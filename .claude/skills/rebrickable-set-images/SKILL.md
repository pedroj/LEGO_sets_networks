---
name: rebrickable-set-images
description: Download or refresh LEGO set images (e.g. 4010-1.png) from rebrickable.com for the Star Wars theme (or another theme / specific sets) into data/rebrickable_set_images. Use when the user wants set images updated, new Star Wars sets fetched, or a set image is missing for a network plot.
---

# Rebrickable set images

Downloads set images via the Rebrickable API and saves them as `<set_num>.jpg` and `<set_num>.png`
(e.g. `4010-1.png`) in
`/Users/pedro/Research/~RCode/MyRCode/LEGO/LEGO_sets_networks/data/rebrickable_set_images`
(override with env `REBRICKABLE_IMG_DIR` or `--dir`). Plotting skills read set images from there.
The images are gitignored; `_manifest.csv` (set, name, year, status, date) is tracked.

Script: `functions/rebrickable_set_images.R` (functions `update_set_images()`, `rb_theme_sets()`).

## Requirements
- `REBRICKABLE_API_KEY` in `~/.Renviron` (never in the repo). If unset, tell Pedro; do not invent a key.
- R packages: httr2, dplyr, readr, magick (png conversion; not needed with `--format jpg`).

## Run (repo root)
```
Rscript functions/rebrickable_set_images.R --dry-run     # list sets still missing an image
Rscript functions/rebrickable_set_images.R               # download only what is missing (Star Wars + subthemes)
Rscript functions/rebrickable_set_images.R --sets 4010-1,4485-1
Rscript functions/rebrickable_set_images.R --theme "Technic" --format jpg
Rscript functions/rebrickable_set_images.R --force       # re-download everything
```
Re-running is incremental: existing files are skipped, so run it periodically to pick up new sets.

## After a run
Report counts per status (`downloaded`, `exists`, `no_image_on_rebrickable`, `download_failed`)
and list the newly downloaded set numbers. Sets with no image on Rebrickable are listed in `_manifest.csv`.
Do not commit images. Commit only script/manifest changes if asked.
