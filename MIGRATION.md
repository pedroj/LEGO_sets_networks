# Migration to Claude Code: from per-set script copies to a shared pipeline

## Why
Each `output/<set>_lineart/` folder carried its own copy of the pipeline, and the copies drifted apart
(`check_geometry.py`, `check_with_instructions.py` and `build_adjacency_xlsx.py` differ in all 7 sets, mostly in the set number and a few
per-set corrections). Set-specific knowledge (subtype overrides, allowed pairs) was written twice: once in Python, once in R.

## Target layout
```
pipeline/R/         run_set.R (entry point), set_config.R, instances.R, kernel.R (vendored from the lego-mpd-adjacency skill), plot_lego_network.R (10174 version)
pipeline/py/        audit + helpers called by run_set.R: setcfg.py, make_bboxes.py, check_geometry.py, decide.py (strict stud rule), check_with_instructions.py, build_adjacency_xlsx.py, lego_lineart.py
pipeline/tests/     regress.sh <set>: shared code must reproduce the committed outputs
output/<set>_lineart/set_config.yaml   the ONLY set-specific input: overrides, pair_allow, manual_add (by node name), notes, plot title
```
Run from the repo root: `Rscript pipeline/R/run_set.R <set> [instances,bboxes,audit,promote,plot,xlsx|all]`.

## Status
- [x] CLAUDE.md, `/new-set` command, .gitignore; old settings.local.json deleted
- [x] Decisions (13 Nov): starting matrix = `<set>_adjacency_matrix.csv`; 6628 door joints in `pair_allow`; audit reads `compat_matrix_subtype_codes.csv`; shared stud rule = strict 2-5.5 LDU (6628/4488/10174 version; 75181's looser -1..5.5 dropped); R driver + Python audit
- [x] `pipeline/R/` with run_set.R, kernel.R vendored, shared plot_lego_network.R; `pipeline/py/` with parameterised make_bboxes/xlsx
- [x] 6628 migrated: checked matrix, xlsx, nodes/edges CSVs reproduced exactly (coords within 1e-12). Old scripts in `output/6628_lineart/bckp/old_scripts_20261113/`
- [x] (done 13 Nov) 6628: run `Rscript pipeline/R/run_set.R 6628 audit` once to refresh `_link_audit.csv`/`.md` (labels now relative to the curated starting matrix; same 56 links)
- [x] 4488 and 10174 ported (13 Nov): set_config.yaml written; checked matrix, xlsx, nodes/edges CSVs reproduced exactly; `_link_audit.csv/.md` refreshed (same kept links; labels now relative to the curated starting matrix; old copies and old scripts in `bckp/old_scripts_20261113/`)
  - Shared code gained: `manual_remove` (4488), `bridge_subassemblies: true` (10174), `pair_allow_plot_only` (4488: 3705|3961ps0 axle, R plots only), MANUAL_REMOVE count in the audit summary
  - 10174: `run_adj.R` (custom MPD sub-files, hose collapsing, segment-wise hose contacts) and `mk_set_parts.R` stay in the set folder because the shared `instances` step does not reproduce them
  - `regress.sh` passes for 4488 and 10174. It needs a python3 with pyyaml, openpyxl, numpy, pillow: `python3 -m venv ~/.venvs/lego && ~/.venvs/lego/bin/pip install pyyaml openpyxl numpy pillow`, then `source ~/.venvs/lego/bin/activate` (or prefix `PATH=~/.venvs/lego/bin:$PATH`)
- [x] 75181 (13 Nov): `set_config.yaml` written, strict audit run: starting matrix 15816 links (curated = kernel) -> checked 3040 links (1814 kept, 1226 added), 42 components, 8 isolated; the previous checked matrix (looser rule) had 6005. **Promoted 13 Nov (Pedro)**; plots, xlsx, widget regenerated; the checked matrix is the starting matrix now. Old outputs in `bckp/pre_port_20261113/`
- [x] 75344 (13 Nov): ported; audit starts from `scratch/75344_start_577b.csv` (the curated matrix uses Rebrickable 64567 for LDraw 577b; set in `files.kernel`). Curated 105 links -> 97 (89 kept, 8 added, 16 dropped), promoted 13 Nov. Node 577b (LDraw) replaces 64567 everywhere (alias row in `rebrickable/set_parts.csv`, icon 577b.png rendered); `instances` now works for its MPD (empty header block). The icon-repulsion loop of its old plot is still not in the shared plot code
- [x] 4010 and 4485 ported (13 Nov). New generic config keys read by `pipeline/R/instances.R`: `mpd_block_alias`, `extra_lines`, `dims_override` (+ blocks without parts are never the root).
  - 4010: hull sub-model aliased to LDraw 2372c01, the 2 missing 298c04 levers added as `extra_lines` (flexible_parts, linked by manual_add), starting matrix = old checked matrix (119 links). The old hand knowledge is in `manual_add` (hull rests, minifig whitelist, 19 adds) and `manual_remove` (hand-torso). Shared audit gives 121 links: the 119 plus 2 new to review (2348a#1-4625#1 hinge, 3004#4-4623#1 bar). Part 4289 (mast) is still `Other`.
  - 4485: starting matrix = old checked matrix (67 nodes, 65 links, every joint decided by hand from the booklet) -> `manual_add` (65) + `manual_remove` (21 geometric contacts the hand audit rejected); the shared audit reproduces the 65 exactly. `mpd: 4485-1.mpd` (the folder also holds the single-podracer MPDs). Sub-model variants (_anakin, _sebulba, _nominifig, run_plot_split*.R) keep their set-specific scripts.
  - Old `<set>_adjacency_matrix.csv` of both sets were stale (neither checked nor kernel); backups in `bckp/pre_port_20261113/`.
- [x] `widget` step (13 Nov): `pipeline/R/widget.R` + `run_set.R widget`; regenerated `<set>_adjacency.html` for 75181, 75344, 4010, 4485 (old ones in `bckp/pre_port_20261113/`). Added to `/new-set` and CLAUDE.md
- [ ] Copy the `lego-mpd-adjacency` and `lego-network-icons` skills into `.claude/skills/` (or rely on pipeline/ and retire them)
## Issues found during migration
1. **Starting matrix (decided: `<set>_adjacency_matrix.csv`).** The old check_geometry.py read `<set>_adjacency_matrix.csv` as the kernel, but that file was later overwritten:
   it equals `_checked` in 6628, 4488 and 10174, equals `_kernel` in 75181, and matches neither in 4485, 75344 and 4010.
   Pedro chose `<set>_adjacency_matrix.csv` as the starting matrix, so re-running a set refines its current curated matrix. Override per set with `files: {kernel: ...}` in set_config.yaml.
2. **Python/R allow-lists differed** (resolved: one `pair_allow` list) (6628: the door pairs 3623/3821p60/3822p60/3823 are allowed in R only; the audit adds them through manual_add).
   They are kept separate as `pair_allow` / `pair_allow_plot_only` for now, to reproduce the old outputs; they could be merged later.
3. **Compatibility source (decided: `compat_matrix_subtype_codes.csv`).** The audit reads `compat_matrix_subtype_codes.csv`; the reference is sheet `compat_revPJ` in the xlsx.
   Confirm that codes.csv is regenerated from compat_revPJ (make_compat_codes.R), or point `files.compat` at an export of it.
4. Manual links are now stored by node name (e.g. `3829a#1`), not by row index, so they survive changes in instance order.
