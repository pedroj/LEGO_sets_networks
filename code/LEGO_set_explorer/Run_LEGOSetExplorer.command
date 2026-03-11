#!/bin/bash

RSCRIPT="/Library/Frameworks/R.framework/Resources/bin/Rscript"

APP_DIR="/Users/pedro//Documents/Working/~RCode/MyRCode/LEGO/LEGO_sets_networks/code/LEGO_set_explorer"

cd "$APP_DIR"
"$RSCRIPT" run_lego_app.R
