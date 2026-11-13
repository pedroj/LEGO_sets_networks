---
description: Run the full connectivity pipeline for a new LEGO set
argument-hint: <set-number> [mpd path] [instructions pdf]
---
Analyse LEGO set $ARGUMENTS with the shared pipeline (see CLAUDE.md). Work directly on `main` (no worktree, no branch) and use the project venv `.venv` (run `pipeline/setup_env.sh` if it is missing).

1. Inputs
   - MPD: locate it in `data/models/` (e.g. `data/models/starwars/<series>/`); if it is missing, list it for Pedro and stop for that set.
   - Booklet PDF: look in the Apple Books folder `~/Library/Mobile Documents/iCloud~com~apple~iBooks/Documents/` by set-number prefix (then `~/Documents/LEGO Creations/Building Instructions/`); ask for the path if not found. Read only the pages of this model.
   - Create `output/<set>_lineart/` with the MPD (`<set>-1.mpd`), `set_info.json` (name, year, num_parts from the local Rebrickable `sets.csv`; `image` = absolute path in the main checkout) and the set image (`data/rebrickable_set_images/<set>-1.jpg`, or Pedro's cropped one; it must be a real JPEG).
   - Do NOT create a per-set `rebrickable/set_parts.csv`: part names/categories come from the single shared catalogue `data/extdata/rebrickable-repo/parts_catalog.csv` (`run_set.R` sets `LEGO_PARTS_CATALOG`). Rebuild the catalogue from `../rebrickable-sqlite/tables` only if Rebrickable data is refreshed.
2. Write `set_config.yaml` from the template `output/6628_lineart/set_config.yaml` (set, name, set_num, empty override lists).
3. `Rscript pipeline/R/run_set.R <set> instances,bboxes,plot,audit` (the `plot` step is needed once to create `<set>_nodes.csv`, which the audit reads). Report link counts (starting, kept, added, rejected, components, isolated parts). Notes: LDraw primitives and MLCad helpers (`her.dat`) are not parts; unknown parts without a bounding box go to `flexible_parts` (linked by `manual_add` only).
4. Compare the audit with the MPD STEP lines and the booklet. Look for false positives (flush contacts), false negatives (pin/axle/hinge/clip joints, components that should join) and subtype misclassifications; test fixes as a dry run with `LEGO_SET_CONFIG=<file>`. Propose `manual_add`, `manual_remove`, subtype overrides and `pair_allow` pairs to Pedro with step numbers as evidence. Put them in `set_config.yaml` only after he approves, then re-run `audit`.
5. Once Pedro accepts the audit: `promote,plot,xlsx,widget` (old outputs go to `bckp/` first). Render only the icons missing from `data/icons/` (`.venv/bin/python pipeline/py/lego_lineart.py --parts <file> --out data/icons --ldraw-dir "/Users/pedro/Documents/LEGO Creations/Utils/ldraw"`). Open the widget `<set>_adjacency.html` in the browser pane and check the stat cards against `_nodes.csv`/`_edges.csv`, that no part is in a colourless category (`NA` becomes `other`), and that the set image inset shows in the plots.
   To refresh only the widget of an already-processed set: `Rscript pipeline/R/run_set.R <set> widget`.
6. Summary: parts, links, false positives removed, parts in "other", widget path. Clean up (`Rplots.pdf`, `pairs.pkl`, `scratch/`). Then commit and push to `main` without asking (standing permission): stage only the set outputs and new `data/icons_small` icons, never `.RData`, `__pycache__/`, `*.pkl` or `scratch/`; report the commit hash.
After any change in `pipeline/`, run `pipeline/tests/regress.sh <set>` for every migrated set.
