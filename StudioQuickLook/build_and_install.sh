#!/bin/bash
set -e

cd "$(dirname "$0")"

echo "Building StudioQL..."
xattr -cr .

xcodebuild -project StudioQL.xcodeproj \
    -target StudioQL \
    -configuration Release \
    CODE_SIGN_IDENTITY="" \
    CODE_SIGNING_REQUIRED=NO \
    CODE_SIGNING_ALLOWED=NO \
    -quiet

BUILD_DIR="build/Release"
mkdir -p "$BUILD_DIR/StudioQL.app/Contents/PlugIns"
cp -R "$BUILD_DIR/StudioQLThumbnail.appex" "$BUILD_DIR/StudioQL.app/Contents/PlugIns/"
cp -R "$BUILD_DIR/StudioQLPreview.appex" "$BUILD_DIR/StudioQL.app/Contents/PlugIns/"

# Clean extended attributes and ad-hoc sign
xattr -cr "$BUILD_DIR/StudioQL.app"
codesign -s - -f "$BUILD_DIR/StudioQL.app/Contents/PlugIns/StudioQLThumbnail.appex"
codesign -s - -f "$BUILD_DIR/StudioQL.app/Contents/PlugIns/StudioQLPreview.appex"
codesign -s - -f "$BUILD_DIR/StudioQL.app"

echo "Installing to /Applications..."
killall StudioQL 2>/dev/null || true
sleep 1
rm -rf /Applications/StudioQL.app
cp -R "$BUILD_DIR/StudioQL.app" /Applications/

echo "Launching app to register extensions..."
open /Applications/StudioQL.app

echo "Resetting QuickLook..."
qlmanage -r
qlmanage -r cache

echo ""
echo "Done! StudioQL.app installed to /Applications."
echo ""
echo "NOTE: On macOS 13+, QuickLook extensions require a valid code signature"
echo "from an Apple Developer account to be registered by the system."
echo "If extensions don't appear, you may need to:"
echo "  1. Sign with a Developer ID: codesign -s 'Developer ID' --deep /Applications/StudioQL.app"
echo "  2. Or open the project in Xcode and build with automatic signing."
echo ""
echo "In the meantime, you can use the standalone thumbnail extractor:"
echo "  ./extract_thumbnail.py /path/to/file.io"
