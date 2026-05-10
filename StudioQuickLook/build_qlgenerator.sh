#!/bin/bash
set -e

cd "$(dirname "$0")"

BUNDLE="StudioIO.qlgenerator"
BUNDLE_DIR="build/$BUNDLE"
CONTENTS="$BUNDLE_DIR/Contents"
MACOS="$CONTENTS/MacOS"

echo "Building qlgenerator..."

rm -rf "$BUNDLE_DIR"
mkdir -p "$MACOS"

# Compile the shared IOFileReader + the qlgenerator C entry point
swiftc -O \
    -module-name StudioIOQL \
    -emit-library \
    -o "$MACOS/StudioIO" \
    IOFileReader.swift \
    QLGenerator.swift \
    -framework Cocoa \
    -framework QuickLook \
    -Xlinker -bundle

# Write Info.plist
cat > "$CONTENTS/Info.plist" << 'PLIST'
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0">
<dict>
    <key>CFBundleDevelopmentRegion</key>
    <string>en</string>
    <key>CFBundleDocumentTypes</key>
    <array>
        <dict>
            <key>CFBundleTypeRole</key>
            <string>QLGenerator</string>
            <key>LSItemContentTypes</key>
            <array>
                <string>com.bricklink.io</string>
            </array>
        </dict>
    </array>
    <key>CFBundleExecutable</key>
    <string>StudioIO</string>
    <key>CFBundleIdentifier</key>
    <string>com.studioql.qlgenerator</string>
    <key>CFBundleInfoDictionaryVersion</key>
    <string>6.0</string>
    <key>CFBundleName</key>
    <string>StudioIO</string>
    <key>CFBundlePackageType</key>
    <string>BNDL</string>
    <key>CFBundleShortVersionString</key>
    <string>1.0</string>
    <key>CFBundleVersion</key>
    <string>1</string>
    <key>QLNeedsToBeRunInMainThread</key>
    <false/>
    <key>QLPreviewHeight</key>
    <real>600</real>
    <key>QLPreviewWidth</key>
    <real>800</real>
    <key>QLThumbnailMinimumSize</key>
    <real>16</real>
    <key>QLSupportsConcurrentRequests</key>
    <true/>
</dict>
</plist>
PLIST

echo "Installing to ~/Library/QuickLook/..."
mkdir -p ~/Library/QuickLook
rm -rf ~/Library/QuickLook/$BUNDLE
cp -R "$BUNDLE_DIR" ~/Library/QuickLook/

echo "Resetting QuickLook..."
qlmanage -r
qlmanage -r cache

echo ""
echo "Done! $BUNDLE installed to ~/Library/QuickLook/"
echo "Test with: qlmanage -t -s 512 /path/to/file.io -o /tmp/"
