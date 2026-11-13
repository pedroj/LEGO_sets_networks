# LEGO Explorer (iPad)

Native SwiftUI rewrite of the Shiny app in `../LEGO_parts_explorer` (LEGO Sets & Parts
Explorer). iPad only, iOS 17+, no third-party dependencies.

- **Offline data**: `set_parts.rds`, `lego_sets.rds` and `parts_index.rds` are converted into one
  SQLite database (`LEGOExplorer/Resources/lego.sqlite`, ~87 MB) that ships inside the app.
  Only the part and set pictures load from Rebrickable (cached on the iPad).
- **Interface**: three-column split view. Sets / Parts tabs and filters on the left, results in the
  middle, detail with pictures on the right. Banner, icon and "Designed by PedroJ @ Santa Ana Bricks"
  link are kept. Tap a part in a set to jump to it in the Parts tab, and vice versa.
- **Classifier**: `Core/PartClassifier.swift` is a line-by-line port of `classify_part.R`.
- **Update tables**: Data sheet (cloud icon, top of the filter column) downloads Rebrickable's public
  CSVs (no API key) and rebuilds the database on the iPad (`Core/DataBuilder.swift`, port of
  `build_data.R`), about a minute. "Go back to the bundled tables" undoes it.

## Build

```bash
# 1. make the database (not committed; needs R with DBI, RSQLite, data.table)
cd code/LEGO_explorer_ipad
Rscript tools/make_sqlite.R ../LEGO_parts_explorer LEGOExplorer/Resources/lego.sqlite
# 2. open LEGOExplorer.xcodeproj in Xcode, pick an iPad, Run
```

Without `lego.sqlite` the app still builds; it opens empty and the Data sheet can download everything.

## Checks (run on the Mac, no simulator needed)

```bash
tools/verify_classifier.sh        # Swift classifier vs. categories stored from R: expect 0 mismatches
```

Verified on 2026-11-13: 64,955 / 64,955 parts match. `DataBuilder` run on the repo's
`rebrickable_csv/` reproduced the R build's counts and totals exactly (28,588 sets, 64,955 parts,
1,467,900 set-part rows, same quantity / spare / minifig / image sums).

## Layout

`tools/make_sqlite.R` RDS to SQLite. `Core/` SQLite wrapper, schema, streaming gzip + CSV, classifier,
builder, store (all queries), update service, app model. `Views/` SwiftUI.
The schema lives in `tools/make_sqlite.R` and `Core/Schema.swift`; keep them in sync.
