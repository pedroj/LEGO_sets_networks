# Shared Rebrickable parts catalogue

`parts_catalog.csv` (part_num, part_name, part_category) is the ONE part-metadata table used by every set
(`pipeline/R/run_set.R` exports it as `LEGO_PARTS_CATALOG`). Do not create per-set `rebrickable/set_parts.csv` files.

Source: local Rebrickable tables (`../rebrickable-sqlite/tables/parts.csv` + `part_categories.csv`), 64,955 parts.
Rebuild it only when the Rebrickable data are refreshed.

Alias rows (LDraw number = Rebrickable part under another number), appended at the end:
- `577b` = Rebrickable `64567` (lightsaber hilt with bottom ring), needed by set 75344.
