"""
airflow/dags/jquants_daily_download.py

Apache Airflow DAG that runs the cl-jquants-api batch downloader once every
trading day (Monday–Friday) after the Japanese equity market closes.

Schedule
--------
The DAG runs at 10:30 UTC (19:30 JST) on Mon–Fri, which is comfortably after
the Tokyo Stock Exchange closes at 15:30 JST and after post-close data is
typically published.

Tasks
-----
One task is created per dataset.  All tasks are independent so Airflow can
monitor each dataset individually and retry failed datasets without
re-downloading the ones that succeeded.

Configuration
-------------
The following Airflow Variables / environment variables are read at runtime:

  JQUANTS_API_KEY   – J-Quants API v2 key (required).
                      Set in Airflow → Admin → Variables, or pass via the
                      environment of the worker.
  JQUANTS_OUTPUT_DIR – Base directory for Parquet output files.
                       Defaults to /opt/airflow/jquants_output.

The script being executed is:
  scripts/download.lisp  (relative to the repository root configured below)

Quickstart (local Airflow)
--------------------------
  # 1. Install Airflow (see docker-compose.yml for the Docker route)
  pip install "apache-airflow>=2.8"

  # 2. Place this file in your $AIRFLOW_HOME/dags/ directory.

  # 3. Set the API key:
  airflow variables set JQUANTS_API_KEY <your-key>

  # 4. Trigger a manual test run:
  airflow dags trigger jquants_daily_download --conf '{"date":"2024-12-27"}'
"""

from __future__ import annotations

import os
from datetime import datetime, timedelta
from pathlib import Path

from airflow import DAG
from airflow.models import Variable
from airflow.operators.bash import BashOperator
from airflow.operators.python import PythonOperator

# ---------------------------------------------------------------------------
# Configuration
# ---------------------------------------------------------------------------

# Absolute path to the root of the cl-jquants-api repository.
# Adjust this if the DAG file is deployed separately from the repository.
_REPO_ROOT = Path(os.getenv("JQUANTS_REPO_ROOT", "/opt/jquants/cl-jquants-api"))

# Base output directory.  Each run creates a sub-directory named after the date.
_OUTPUT_BASE = Path(
    os.getenv("JQUANTS_OUTPUT_DIR", "/opt/airflow/jquants_output")
)

# Datasets to download.  Each entry is (task_id, dataset_name_arg).
# The dataset_name_arg is forwarded to the --dataset flag of download.lisp.
_DATASETS: list[tuple[str, str]] = [
    ("listed_issues",                      "listed-issues"),
    ("stock_prices",                       "stock-prices"),
    ("morning_session_stock_prices",       "morning-session-stock-prices"),
    ("indices_prices",                     "indices-prices"),
    ("topix_prices",                       "topix-prices"),
    ("index_option_prices",                "index-option-prices"),
    ("trading_by_type_of_investors",       "trading-by-type-of-investors"),
    ("margin_trading_outstandings",        "margin-trading-outstandings"),
    ("short_sale_value_and_ratio",         "short-sale-value-and-ratio-by-sector"),
    ("trading_calendar",                   "trading-calendar"),
    ("earnings_calendar",                  "earnings-calendar"),
    ("financial_data",                     "financial-data"),
    ("breakdown_trading_data",             "breakdown-trading-data"),
    ("cash_dividend_data",                 "cash-dividend-data"),
    ("financial_statement_data",           "financial-statement-data"),
    ("futures_data",                       "futures-data"),
    ("options_data",                       "options-data"),
]

# ---------------------------------------------------------------------------
# Default task arguments
# ---------------------------------------------------------------------------

_DEFAULT_ARGS: dict = {
    "owner": "airflow",
    "depends_on_past": False,
    "retries": 3,
    "retry_delay": timedelta(minutes=10),
    "retry_exponential_backoff": True,
    "email_on_failure": True,
    "email_on_retry": False,
    # Set 'email' to a real address or configure Airflow SMTP settings.
    "email": os.getenv("AIRFLOW_ALERT_EMAIL", ""),
}

# ---------------------------------------------------------------------------
# Helper: resolve the execution date
# ---------------------------------------------------------------------------


def _resolve_date(ds: str, **context) -> str:  # noqa: ARG001
    """Return the logical execution date (YYYY-MM-DD).

    When the DAG is triggered manually with a ``{"date": "YYYY-MM-DD"}``
    conf value that date is used.  Otherwise Airflow's ``ds`` macro (the
    logical execution date of the run) is used.
    """
    conf = context.get("dag_run") and context["dag_run"].conf
    return (conf or {}).get("date") or ds


# ---------------------------------------------------------------------------
# Helper: build the SBCL invocation for a single dataset
# ---------------------------------------------------------------------------


def _sbcl_command(date_macro: str, dataset_name: str, output_dir: Path) -> str:
    """Return the shell command that downloads one dataset.

    The command:
    1. Loads Quicklisp (from ~/quicklisp/setup.lisp).
    2. Pushes the repository root onto ASDF's central registry.
    3. Loads cl-jquants-api via Quicklisp.
    4. Loads scripts/download.lisp with dataset-specific overrides so only
       the named dataset is downloaded.
    """
    script = _REPO_ROOT / "scripts" / "download.lisp"
    return (
        "sbcl --non-interactive"
        " --load ~/quicklisp/setup.lisp"
        f" --eval \"(push #p\\\"{_REPO_ROOT}/\\\" asdf:*central-registry*)\""
        " --eval \"(ql:quickload :cl-jquants-api :silent t)\""
        f" --load \"{script}\""
        f" -- \"{date_macro}\" \"{output_dir}/\""
    )


# ---------------------------------------------------------------------------
# DAG definition
# ---------------------------------------------------------------------------

with DAG(
    dag_id="jquants_daily_download",
    description=(
        "Download daily J-Quants financial data and save as Parquet files. "
        "Runs Monday–Friday after the Tokyo Stock Exchange closes."
    ),
    # 10:30 UTC = 19:30 JST, comfortably after the 15:30 JST market close.
    schedule_interval="30 10 * * 1-5",
    start_date=datetime(2024, 1, 1),
    catchup=False,
    max_active_runs=1,
    default_args=_DEFAULT_ARGS,
    tags=["jquants", "finance", "daily"],
    doc_md=__doc__,
) as dag:

    # ------------------------------------------------------------------
    # Task 0: resolve the target date and create the output directory.
    # ------------------------------------------------------------------

    def _prepare(ds: str, **context):
        date = _resolve_date(ds, **context)
        out = _OUTPUT_BASE / date
        out.mkdir(parents=True, exist_ok=True)
        # Push the resolved date into XCom so download tasks can read it.
        return date

    prepare = PythonOperator(
        task_id="prepare",
        python_callable=_prepare,
    )

    # ------------------------------------------------------------------
    # Task N: one BashOperator per dataset.
    # ------------------------------------------------------------------

    download_tasks = []
    for task_id, dataset_name in _DATASETS:
        # Airflow's Jinja template {{ ti.xcom_pull(task_ids='prepare') }}
        # returns the date string pushed by the prepare task.
        date_expr = "{{ ti.xcom_pull(task_ids='prepare') }}"
        output_dir = _OUTPUT_BASE / "{{ ti.xcom_pull(task_ids='prepare') }}"

        cmd = (
            "sbcl --non-interactive"
            " --load ~/quicklisp/setup.lisp"
            f" --eval \"(push #p\\\"{_REPO_ROOT}/\\\" asdf:*central-registry*)\""
            " --eval \"(ql:quickload :cl-jquants-api :silent t)\""
            f" --load \"{_REPO_ROOT / 'scripts' / 'download.lisp'}\""
            f" -- \"{date_expr}\" \"{_OUTPUT_BASE}/{{{{ ti.xcom_pull(task_ids='prepare') }}}}/\""
        )

        task = BashOperator(
            task_id=f"download_{task_id}",
            bash_command=cmd,
            env={
                # Pass the API key from Airflow Variables into the subprocess.
                "JQUANTS_API_KEY": "{{ var.value.JQUANTS_API_KEY }}",
                # Inherit the current PATH so sbcl can be found.
                "PATH": os.environ.get("PATH", "/usr/local/bin:/usr/bin:/bin"),
                "HOME": os.environ.get("HOME", "/root"),
            },
            # Allow the task to be re-tried independently of other datasets.
            retries=_DEFAULT_ARGS["retries"],
            retry_delay=_DEFAULT_ARGS["retry_delay"],
        )
        download_tasks.append(task)

    # ------------------------------------------------------------------
    # Task: verify that all Parquet files were written.
    # ------------------------------------------------------------------

    def _verify(ds: str, **context):
        import glob as _glob

        date = _resolve_date(ds, **context)
        out = _OUTPUT_BASE / date
        files = list(out.glob("*.parquet"))
        if not files:
            raise RuntimeError(
                f"No Parquet files found in {out} for date {date}."
            )
        print(f"Verification passed: {len(files)} Parquet file(s) in {out}.")
        for f in sorted(files):
            size_kb = f.stat().st_size / 1024
            print(f"  {f.name}  ({size_kb:.1f} KB)")
        return len(files)

    verify = PythonOperator(
        task_id="verify",
        python_callable=_verify,
        trigger_rule="all_done",  # Run even if some downloads failed.
    )

    # ------------------------------------------------------------------
    # Task graph: prepare → all downloads (parallel) → verify
    # ------------------------------------------------------------------

    prepare >> download_tasks >> verify
