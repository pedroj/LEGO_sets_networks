#!/bin/bash
# build_app.sh - build "LEGO Sets & Parts Explorer.app" (macOS) with the database inside.
#   ./build_app.sh            -> dist/LEGO Sets & Parts Explorer.app
# The database (Data/lego_explorer.sqlite) is created by:
#   Rscript tools/rds_to_sqlite.R <folder with the .rds files>
set -euo pipefail
cd "$(dirname "$0")"
APP="dist/LEGO Sets & Parts Explorer.app"
DB="Data/lego_explorer.sqlite"
[ -f "$DB" ] || { echo "Missing $DB - run tools/rds_to_sqlite.R first" >&2; exit 1; }

swift build -c release --product LEGOExplorer
BIN="$(swift build -c release --show-bin-path)/LEGOExplorer"

rm -rf "$APP"
mkdir -p "$APP/Contents/MacOS" "$APP/Contents/Resources"
cp "$BIN" "$APP/Contents/MacOS/LEGOExplorer"
cp "$DB" Resources/icon.png Resources/sta_ana_bricks.png Resources/AppIcon.icns "$APP/Contents/Resources/"
cat > "$APP/Contents/Info.plist" <<PLIST
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0">
<dict>
    <key>CFBundleName</key><string>LEGO Sets &amp; Parts Explorer</string>
    <key>CFBundleDisplayName</key><string>LEGO Sets &amp; Parts Explorer</string>
    <key>CFBundleIdentifier</key><string>es.pjordano.legoexplorer.swiftui</string>
    <key>CFBundleVersion</key><string>2.0</string>
    <key>CFBundleShortVersionString</key><string>2.0</string>
    <key>CFBundlePackageType</key><string>APPL</string>
    <key>CFBundleExecutable</key><string>LEGOExplorer</string>
    <key>CFBundleIconFile</key><string>AppIcon</string>
    <key>LSMinimumSystemVersion</key><string>14.0</string>
    <key>NSHighResolutionCapable</key><true/>
    <key>NSPrincipalClass</key><string>NSApplication</string>
</dict>
</plist>
PLIST
codesign --force --deep --sign - "$APP" >/dev/null 2>&1 || echo "(ad-hoc signing skipped)"
echo "Built: $APP ($(du -sh "$APP" | cut -f1))"
