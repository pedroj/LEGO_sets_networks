# LEGO_sets_networks — Claude Code guide

Network analysis of LEGO set part connectivity from LDraw MPD/LDR files (R primary, Python for geometry checks).
Owner: Pedro Jordano (Rebrickable user PedroJ). Repo: github.com/pedroj/LEGO_sets_networks.

## Non-negotiable rules
- **Real data only.** Part names, categories and links come from parsing actual files (MPD, Rebrickable CSVs, Parts.lst). Never invent parts, categories or connections.
- **Compatibility beats proximity.** Part-type pairs the subtype compatibility matrix says cannot join are NOT drawn in networks; they stay in the edges CSV with `joint = check`, weight 0.
- Reference compatibility matrix = Pedro's hand-revised sheet `compat_revPJ` in `data/lego_compat_out/compat_matrix_subtype_icons.xlsx` (it supersedes `compat_matrix_subtype_codes.csv`/`_binary.csv`). Seats/chairs (4079) count as Minifig accessories.
- **Before regenerating a set's outputs**, move the old outputs into `output/<set>_lineart/bckp/`. Never overwrite them silently.
- Keep observed adjacency (from MPD) separate from theoretical compatibility (category-level, from LEGO interface knowledge).
- New sets must match the 75192 reference widget design. Widget tooltips go inline (no `position: fixed`, which collapses the iframe and cuts off the degree bar).
- Defensive category lookups: map `NA` to `"other"` so no node is left uncoloured.

## Pipeline (one set)
1. Parse the MPD subfiles: keep type-1 part lines and **filter out LDraw primitives** (e.g. `4-4cyli.dat`). Missing primitives in the filter list are a recurring bug.
2. Candidate links: spatial proximity (≤ 80 LDU / bbox contact, stud stacking) plus step co-occurrence, capped at 15–25 parts per step. Some sets (e.g. 4485) have no STEP markers, so use proximity only.
3. Build the kernel adjacency (`run_adj*.R` → `<set>_adjacency_matrix_kernel.csv`).
4. Audit: `make_bboxes.py` → `check_geometry.py` (writes `pairs.pkl`) → `decide.py` → `check_with_instructions.py`, which uses the PDF building-instruction step images to catch false positives and writes `<set>_adjacency_matrix_checked.csv`, `<set>_link_audit.csv` and `<set>_link_audit_summary.md`.
5. Plot: `run_plot_coords.R` sources `plot_lego_network.R` (`plot_lego_network_coords()`), using icons from `data/icons/`.
6. Excel: `build_adjacency_xlsx.py` → `<set>_adjacency_matrix_icons.xlsx`.

Parts.lst parsing: split on the first `=`. Image lookup goes in two passes: the exact part number, then with trailing letters stripped (`3062a → 3062`).

## Layout
- `data/models/` holds MPD files (`600_series/`, `starwars/`); set images are in `data/models/set_images/<series>_set_images/` (look there first).
- `data/models_adj/` holds edge lists `<set>-<name>-el.csv`.
- `data/lego_compat_out/` holds the compatibility matrices, interfaces, mating rules and `make_compat_codes.R`.
- `data/icons/` is the **single shared, cumulative** icon store. Render only the missing icons into it; no per-set icon folders.
- `output/<set>_lineart/` holds per-set scripts and outputs; `bckp/` keeps old versions and `scratch/` holds throwaway files.
- `functions/` holds R helpers (Rebrickable downloads, set/part images, plotting).
- `code/` holds apps and side projects (`LEGO_parts_explorer` Shiny app, `LEGO_set_explorer`, `lego_mpd_adjacency`, network-embedding comparisons).

## External resources (outside the repo)
- LDraw library: `/Users/pedro/Documents/LEGO Creations/Utils/ldraw/` (`parts/`, `p/`, `Parts.lst`); alternative copy in `../pybricks_ldraw/ldraw`.
- Rebrickable tables: `../rebrickable-sqlite` (also a local MySQL database). API key in env `REBRICKABLE_API_KEY`; the API only patches what local files lack (local-first).
- Sibling output folder: `../LEGO_Parts/`.

## Conventions
- R: tidyverse, ggplot2, igraph, ggraph, bipartite. Python: numpy, networkx, matplotlib, openpyxl (avoid pandas in audit scripts).
- Scripts run from inside `output/<set>_lineart/` with relative paths (`../../data/...`).
- Write long R scripts in logical sections.
- Commit per set or per logical change; never commit `.RData`, `__pycache__/`, `*.pkl` or `scratch/`.
