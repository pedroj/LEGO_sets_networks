#!/usr/bin/env bash
# Regression for one migrated set: re-run the shared pipeline on a scratch copy and compare with the committed outputs.
# usage (repo root):  pipeline/tests/regress.sh 6628 [py|R|all]     default: all
# Checks: checked matrix + link audit (python audit), xlsx cell values (python), nodes/edges CSVs (R plot; coords to 1e-9).
set -uo pipefail
S=$1; WHAT=${2:-all}; ROOT=$(pwd); W=$(mktemp -d); trap 'rm -rf "$W"' EXIT
mkdir -p "$W/output" "$W/data" "$W/pipeline"
cp -r "output/${S}_lineart" "$W/output/"; cp -r data/lego_compat_out "$W/data/"; ln -s "$ROOT/data/icons" "$W/data/icons"; [ -d .venv ] && ln -s "$ROOT/.venv" "$W/.venv"
[ -d data/icons_small ] && cp -r data/icons_small "$W/data/"; cp -r pipeline/R pipeline/py "$W/pipeline/"
REF="$ROOT/output/${S}_lineart"; cd "$W"; fail=0
same() { if cmp -s "$REF/$1" "output/${S}_lineart/$1"; then echo "  ok   $1"; else echo "  DIFF $1"; fail=1; fi; }
if [ "$WHAT" != R ]; then
  echo "python audit + xlsx"; Rscript pipeline/R/run_set.R "$S" audit,xlsx >/dev/null 2>&1 || { echo "  run failed"; fail=1; }
  same "${S}_adjacency_matrix_checked.csv"
  diff -q <(sort "$REF/${S}_link_audit.csv") <(sort "output/${S}_lineart/${S}_link_audit.csv") >/dev/null && echo "  ok   ${S}_link_audit.csv" || { echo "  DIFF ${S}_link_audit.csv"; fail=1; }
  python3 - "$REF/${S}_adjacency_matrix_icons.xlsx" "output/${S}_lineart/${S}_adjacency_matrix_icons.xlsx" <<'PY' || fail=1
import sys; from openpyxl import load_workbook
a, b = (load_workbook(f) for f in sys.argv[1:3])
d = sum(x.value != y.value for sa, sb in zip(a.worksheets, b.worksheets) for ra, rb in zip(sa.iter_rows(), sb.iter_rows()) for x, y in zip(ra, rb))
print("  %s   xlsx cells (%d differing)" % ("ok" if d == 0 else "DIFF", d)); sys.exit(d > 0)
PY
fi
if [ "$WHAT" != py ]; then
  echo "R plot"; Rscript pipeline/R/run_set.R "$S" plot >/dev/null 2>&1 || { echo "  run failed"; fail=1; }
  for f in nodes edges coords_edges; do same "${S}_${f}.csv"; done
  Rscript -e 'a<-read.csv(commandArgs(TRUE)[1]);b<-read.csv(commandArgs(TRUE)[2]);n<-sapply(a,is.numeric);ok<-identical(a[!n],b[!n])&&max(abs(as.matrix(a[n])-as.matrix(b[n])))<1e-9;cat(if(ok)"  ok  " else "  DIFF"," coords_nodes.csv (tol 1e-9)\n");quit(status=!ok)' \
    "$REF/${S}_coords_nodes.csv" "output/${S}_lineart/${S}_coords_nodes.csv" || fail=1
fi
[ $fail = 0 ] && echo "PASS $S" || echo "FAIL $S"; exit $fail
