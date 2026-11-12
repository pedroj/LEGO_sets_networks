#!/bin/bash
# LEGO Sets & Parts Explorer launcher (v1.2)
# The app's R code lives INSIDE this bundle (Contents/Resources/app), so changes to
# loose .R files in the project folder do not affect it. Data tables (.rds) are read
# from DATA_DIR. Always opens in Safari. R stops a few seconds after the tab closes.

DATA_DIR="/Users/pedro/Documents/Working/~RCode/MyRCode/LEGO/LEGO_sets_networks/code/LEGO_parts_explorer"
CODE_DIR="$(cd "$(dirname "$0")/../Resources/app" && pwd)"
PORT=8766
URL="http://127.0.0.1:${PORT}"
LOG="$HOME/Library/Logs/LEGOSetsPartsExplorer.log"
TITLE="LEGO Sets & Parts Explorer"

# stop an older copy of the app (e.g. a previous version) still using the port
OLD=$(lsof -ti tcp:${PORT} 2>/dev/null)
[ -n "$OLD" ] && kill $OLD 2>/dev/null && sleep 1

for R in /Library/Frameworks/R.framework/Resources/bin/Rscript \
         /usr/local/bin/Rscript /opt/homebrew/bin/Rscript; do
    [ -x "$R" ] && RSCRIPT="$R" && break
done
if [ -z "$RSCRIPT" ]; then
    osascript -e "display alert \"$TITLE\" message \"Rscript was not found. Install R from CRAN or edit the path in the app.\""
    exit 1
fi

if [ -f "$DATA_DIR/parts_index.rds" ]; then
    osascript -e "display notification \"Starting…\" with title \"$TITLE\""
else
    osascript -e "display notification \"First launch: downloading Rebrickable tables (1–3 minutes)…\" with title \"$TITLE\""
fi

cd "$CODE_DIR" || exit 1
LEGO_DATA_DIR="$DATA_DIR" LEGO_DESKTOP=1 "$RSCRIPT" -e "
for (p in c('shiny','DT','data.table','later')) if (!requireNamespace(p, quietly = TRUE)) install.packages(p, repos = 'https://cloud.r-project.org')
shiny::runApp('.', port = ${PORT}, launch.browser = FALSE)" > "$LOG" 2>&1 &
RPID=$!
trap 'kill $RPID 2>/dev/null' TERM INT EXIT

for i in $(seq 1 360); do
    if curl -s -o /dev/null "$URL"; then open -a Safari "$URL"; break; fi
    if ! kill -0 $RPID 2>/dev/null; then
        osascript -e "display alert \"$TITLE\" message \"The app failed to start. See the log: $LOG\""
        exit 1
    fi
    sleep 1
done

wait $RPID
