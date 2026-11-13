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
- [ ] 6628: run `Rscript pipeline/R/run_set.R 6628 audit` once to refresh `_link_audit.csv`/`.md` (labels now relative to the curated starting matrix; same 56 links)
- [x] 4488 and 10174 ported (13 Nov): set_config.yaml written; checked matrix, xlsx, nodes/edges CSVs reproduced exactly; `_link_audit.csv/.md` refreshed (same kept links; labels now relative to the curated starting matrix; old copies and old scripts in `bckp/old_scripts_20261113/`)
  - Shared code gained: `manual_remove` (4488), `bridge_subassemblies: true` (10174), `pair_allow_plot_only` (4488: 3705|3961ps0 axle, R plots only), MANUAL_REMOVE count in the audit summary
  - 10174: `run_adj.R` (custom MPD sub-files, hose collapsing, segment-wise hose contacts) and `mk_set_parts.R` stay in the set folder because the shared `instances` step does not reproduce them
  - `regress.sh` passes for 4488 and 10174. It needs a python3 with pyyaml, openpyxl, numpy, pillow: `python3 -m venv ~/.venvs/lego && ~/.venvs/lego/bin/pip install pyyaml openpyxl numpy pillow`, then `source ~/.venvs/lego/bin/activate` (or prefix `PATH=~/.venvs/lego/bin:$PATH`)
- [ ] 75181: port and re-audit with the strict rule (expect fewer stud links; review the dropped ones)
- [ ] 4010, 4485, 75344: older generation (no decide.py; 75344/4485 plots had an icon-repulsion loop that the shared plot code lacks). Port them, or freeze them with a note
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
