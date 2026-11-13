# LEGO_sets_networks — Claude Code guide

Network analysis of LEGO set part connectivity from LDraw MPD/LDR files (R
primary, Python for geometry checks). Owner: Pedro Jordano (Rebrickable user
PedroJ). Repo: github.com/pedroj/LEGO_sets_networks.

## Non-negotiable rules

-   **Real data only.** Part names, categories and links come from parsing
    actual files (MPD, Rebrickable CSVs, Parts.lst). Never invent parts,
    categories or connections.
-   **Compatibility beats proximity.** Part-type pairs the subtype compatibility
    matrix says cannot join are NOT drawn in networks; they stay in the edges
    CSV with `joint = check`, weight 0.
-   Reference compatibility matrix = Pedro's hand-revised sheet `compat_revPJ`
    in `data/lego_compat_out/compat_matrix_subtype_icons.xlsx` (it supersedes
    `compat_matrix_subtype_codes.csv`/`_binary.csv`). Seats/chairs (4079) count
    as Minifig accessories.
-   **Before regenerating a set's outputs**, move the old outputs into
    `output/<set>_lineart/bckp/`. Never overwrite them silently.
-   Keep observed adjacency (from MPD) separate from theoretical compatibility
    (category-level, from LEGO interface knowledge).
-   New sets must match the 75192 reference widget design. Widget tooltips go
    inline (no `position: fixed`, which collapses the iframe and cuts off the
    degree bar).
-   Defensive category lookups: map `NA` to `"other"` so no node is left
    uncoloured.

## Pipeline (one set)
Shared code lives in `pipeline/` (R driver in `pipeline/R/`, Python audit in `pipeline/py/`). Everything set-specific goes in `output/<set>_lineart/set_config.yaml`: subtype/category overrides, `pair_allow`, `manual_add` (by node name, e.g. `3829a#1`), `flexible_parts`, audit notes and plot title. **Never put set constants in shared code, and never copy scripts into a set folder.** Template: `output/6628_lineart/set_config.yaml`.

Python dependencies live in the project virtual environment `.venv/` (gitignored). Create or update it with `pipeline/setup_env.sh` (packages in `pipeline/py/requirements.txt`); `run_set.R` and `regress.sh` use it automatically, and `lego_lineart.py` runs with `.venv/bin/python`.

One entry point, run from the repo root:
```
Rscript pipeline/R/run_set.R <set> [steps]        # default steps: audit,plot,xlsx
```
Steps:
- `instances`: MPD → `_instances.csv`, `_node_coords.csv`, `_adjacency_matrix_kernel.csv` (vendored `pipeline/R/kernel.R`; primitives filtered). Also writes the starting matrix if it does not exist yet.
- `bboxes`: LDraw bounding boxes → `part_bbox.csv` (env `LDRAW_DIR` overrides the library path).
- `audit`: starts from **`<set>_adjacency_matrix.csv`** (the curated starting matrix). Geometry + compatibility (`compat_matrix_subtype_codes.csv`) + strict stud rule (2–5.5 LDU insertion; flush contact is not a joint) + hand checks → `_adjacency_matrix_checked.csv`, `_link_audit.csv`, `_link_audit_summary.md`.
- `promote`: back up the starting matrix to `bckp/` and replace it with `_checked.csv`. Only after Pedro has reviewed the audit.
- `plot`: network figures from `<set>_adjacency_matrix.csv` → `_network.pdf`, `_network_coords.pdf/.png`, `_nodes/_edges/_coords_*.csv`.
- `xlsx`: `_adjacency_matrix_icons.xlsx`.
- `all`: every step in order.

After ANY change in `pipeline/`, run `pipeline/tests/regress.sh <set>` for every migrated set (see MIGRATION.md). A FAIL means the change altered results: explain why before keeping it.

Parts.lst parsing: split on the first `=`. Image lookup goes in two passes: the
exact part number, then with trailing letters stripped (`3062a → 3062`).

## Layout

-   `data/models/` holds MPD files (`600_series/`, `starwars/`); set images are
    in `data/models/set_images/<series>_set_images/` (look there first).
-   `data/models_adj/` holds edge lists `<set>-<name>-el.csv`.
-   `data/lego_compat_out/` holds the compatibility matrices, interfaces, mating
    rules and `make_compat_codes.R`.
-   `data/icons/` is the **single shared, cumulative** icon store. Render only
    the missing icons into it; no per-set icon folders.
-   `output/<set>_lineart/` holds per-set scripts and outputs; `bckp/` keeps old
    versions and `scratch/` holds throwaway files.
-   `functions/` holds R helpers (Rebrickable downloads, set/part images,
    plotting).
-   `code/` holds apps and side projects (`LEGO_parts_explorer` Shiny app,
    `LEGO_set_explorer`, `lego_mpd_adjacency`, network-embedding comparisons).

## External resources (outside the repo)

-   LDraw library: `/Users/pedro/Documents/LEGO Creations/Utils/ldraw/`
    (`parts/`, `p/`, `Parts.lst`); alternative copy in
    `../pybricks_ldraw/ldraw`.
-   Rebrickable tables: `../rebrickable-sqlite` (also a local MySQL database).
    API key in env `REBRICKABLE_API_KEY`; the API only patches what local files
    lack (local-first).
-   Sibling output folder: `../LEGO_Parts/`.

## Conventions

-   R: tidyverse, ggplot2, igraph, ggraph, bipartite. Python: numpy, networkx,
    matplotlib, openpyxl (avoid pandas in audit scripts).
-   Scripts run from inside `output/<set>_lineart/` with relative paths
    (`../../data/...`).
-   Write long R scripts in logical sections.
-   Commit per set or per logical change; never commit `.RData`, `__pycache__/`,
    `*.pkl` or `scratch/`.