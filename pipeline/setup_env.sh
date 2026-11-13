#!/usr/bin/env bash
# Create (or update) the project virtual environment .venv/ used by pipeline/R/run_set.R and pipeline/py/*.py
# usage (repo root):  pipeline/setup_env.sh [python]      default python: /opt/homebrew/bin/python3, else python3
set -euo pipefail
PY=${1:-$( [ -x /opt/homebrew/bin/python3 ] && echo /opt/homebrew/bin/python3 || command -v python3 )}
"$PY" -m venv .venv
.venv/bin/pip install -q --upgrade pip
.venv/bin/pip install -q -r pipeline/py/requirements.txt
.venv/bin/python -c "import yaml, numpy, networkx, openpyxl, PIL, scipy, matplotlib; print('venv ok:', '$PY')"
