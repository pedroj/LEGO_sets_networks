# StudioQL — QuickLook for BrickLink Studio .io Files

A macOS QuickLook plugin that shows thumbnails and previews for BrickLink Studio 2.0 `.io` files directly in Finder and Quick Look.

## What it does

- **Thumbnails**: Shows the model thumbnail in Finder icon view, column view, and Cover Flow
- **Preview**: Press Space on a `.io` file to see the full model preview with part count info

## Build & Install

```bash
./build_and_install.sh
```

Or manually:

```bash
xcodebuild -project StudioQL.xcodeproj -target StudioQL -configuration Release \
    CODE_SIGN_IDENTITY="-" CODE_SIGNING_REQUIRED=NO CODE_SIGNING_ALLOWED=NO
    
# Copy extensions into app bundle
cp -R build/Release/StudioQLThumbnail.appex build/Release/StudioQL.app/Contents/PlugIns/
cp -R build/Release/StudioQLPreview.appex build/Release/StudioQL.app/Contents/PlugIns/

# Install
cp -R build/Release/StudioQL.app /Applications/
qlmanage -r
```

You may need to log out and back in, or restart Finder, for QuickLook to pick up the new extensions.

## How it works

`.io` files are ZIP archives (encrypted with PKZIP traditional encryption) containing:
- `thumbnail.png` — rendered model preview
- `model.ldr` — LDraw model data
- `.info` — JSON metadata (version, part count)

The plugin extracts and displays the embedded thumbnail.

## Requirements

- macOS 12.0+
- Xcode 15+
