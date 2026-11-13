---
description: Run the full connectivity pipeline for a new LEGO set
argument-hint: <set-number> [mpd path] [instructions pdf]
---
Analyse LEGO set $ARGUMENTS with the shared pipeline (see CLAUDE.md).

1. Locate the MPD in data/models/ and the PDF building instructions (ask if either is missing). Create output/<set>_lineart/ with the MPD, rebrickable/set_parts.csv (from the local Rebrickable tables), set_info.json and the set image.
2. Write set_config.yaml from the template output/6628_lineart/set_config.yaml (set, name, set_num, empty override lists).
3. `Rscript pipeline/R/run_set.R <set> instances,bboxes,audit` and report link counts (starting, kept, added, rejected, components, isolated parts).
4. Compare the audit with the instruction-booklet steps. Propose `manual_add` entries, subtype overrides and `pair_allow` pairs for Pedro to approve, with step numbers as evidence. Put them in set_config.yaml only after he approves, then re-run `audit`.
5. Once Pedro accepts the audit: `promote,plot,xlsx`. Render only the icons missing from data/icons/ (pipeline/py/lego_lineart.py).
6. Summary: parts, links, false positives removed, parts in "other". Do not commit; show `git status` and propose a commit message.
