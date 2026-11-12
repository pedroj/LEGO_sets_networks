#!/bin/bash
# Start the LEGO Sets & Parts Explorer from Terminal (works wherever this folder is).
cd "$(dirname "$0")"
for R in /Library/Frameworks/R.framework/Resources/bin/Rscript /usr/local/bin/Rscript /opt/homebrew/bin/Rscript; do
    [ -x "$R" ] && exec "$R" run_app.R
done
echo "Rscript not found"; exit 1
