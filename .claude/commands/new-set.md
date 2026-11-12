---
description: Run the full connectivity pipeline for a new LEGO set
argument-hint: <set-number> [mpd path] [instructions pdf]
---
Analyse LEGO set $ARGUMENTS following the pipeline in CLAUDE.md.

1. Locate the MPD in data/models/ and the PDF building instructions (ask if either is missing). Fetch the set parts from the local Rebrickable tables.
2. Create output/<set>_lineart/. Copy the scripts from the most recent completed set (check `git log`), then adapt the set-specific constants only.
3. Run the kernel adjacency → geometry audit → instruction check → plot → xlsx steps. After each step, report link counts (kernel, kept, added, removed).
4. Render only the icons missing from data/icons/.
5. Finish with a short summary: number of parts, number of links, false positives removed, and any parts that fell into "other".
Do not commit; show `git status` and propose a commit message.
