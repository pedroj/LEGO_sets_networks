# LEGO Sets & Parts Explorer — native SwiftUI app

Rewrite of the Shiny app in `../LEGO_parts_explorer` (shinyapps.io: lego-sets-parts-explorer).
Same features, but native, offline and instant: all tables live in one SQLite database
that ships inside the app. Only the set/part pictures load from Rebrickable.

## Layout
- Left: **Sets / Parts** tabs with their filters (and the parts-list options).
- Middle: results list (sortable).
- Right: detail with pictures — a set's parts list (CSV export), or a part's colours and sets
  ("Show set" jumps to the Sets tab).
- Banner with the icon, title and the “Designed by PedroJ @ Santa Ana Bricks” link.
- **Data** button (bottom left, or ⇧⌘U): shows the build info and **Update tables now**, which
  downloads Rebrickable's public CSVs (no API key), rebuilds the database on the device and
  swaps it in. The updated copy lives in `~/Library/Application Support/LEGO Sets & Parts Explorer/`;
  “Restore data shipped with the app” goes back to the bundled one.

## Build
```
Rscript tools/rds_to_sqlite.R "<folder with set_parts.rds, lego_sets.rds, parts_index.rds>"   # -> Data/lego_explorer.sqlite (~80 MB)
./build_app.sh                                                                               # -> dist/LEGO Sets & Parts Explorer.app
```
Needs Xcode 16+ / Swift 6 toolchain, macOS 14+.

## Code
- `Sources/ExplorerCore/PartClassifier.swift` — port of `classify_part.R` (same categories/subcategories).
- `Sources/ExplorerCore/TableBuilder.swift` — port of `build_data.R`: download, gunzip (Compression
  framework), CSV → SQLite staging, SQL transforms, classification. Same schema as `rds_to_sqlite.R`.
- `Sources/ExplorerCore/ExplorerDatabase.swift` — queries.
- `Sources/LEGOExplorer/` — SwiftUI app (`NavigationSplitView`, 3 columns).
- `Sources/explorer-tool/` — command-line checks.

## Checks
```
swift build -c release --product explorer-tool
.build/release/explorer-tool classify Data/lego_explorer.sqlite   # Swift classifier vs R categories: 0 mismatches / 64,955 parts
.build/release/explorer-tool build <csv folder> out.sqlite        # build from Rebrickable CSVs (~15 s)
.build/release/explorer-tool selftest Data/lego_explorer.sqlite
```
Database built by Swift from the same CSVs is identical to the R one (set_parts, parts, categories,
counts). Known difference: 25 set names where the old R `fread` kept doubled quotes (`14""`); the
Swift CSV reader unescapes them correctly.

## iPad
The code is cross-platform (`#if os(macOS)` only around window sizing and `VSplitView`; the data layer uses
only Foundation, SQLite3 and Compression). To run on iPad, add an iOS app target in Xcode that includes
`Sources/LEGOExplorer`, links the `ExplorerCore` package product and has `Data/lego_explorer.sqlite`,
`Resources/icon.png` and `Resources/sta_ana_bricks.png` in its bundle. This has not been built or run for iPadOS yet.
