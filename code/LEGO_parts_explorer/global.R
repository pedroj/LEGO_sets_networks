# global.R - LEGO Sets & Parts Explorer
# Data come from Rebrickable's public CSV downloads (no API key needed).
# Run build_data.R to refresh them; on the very first launch it runs by itself.

suppressPackageStartupMessages({
    library(shiny)
    library(DT)
    library(data.table)
})

# Data folder: set by the launcher app (LEGO_DATA_DIR); otherwise the app folder
DATA_DIR <- Sys.getenv("LEGO_DATA_DIR", ".")
dpath <- function(f) file.path(DATA_DIR, f)

if (!file.exists(dpath("parts_index.rds"))) {
    message("No data yet: building tables from Rebrickable (about 1-2 minutes) ...")
    source("build_data.R")
    build_lego_data(out_dir = DATA_DIR, app_dir = ".")
}

# Fast-start cache: only the columns the app uses, stored as factors (~8 MB, loads
# in a fraction of a second). Rebuilt automatically whenever set_parts.rds is newer.
APP_COLS <- c("set_num", "part_num", "color_id", "color_name", "rgb", "quantity",
              "is_spare", "minifig", "part_img_url", "part_name", "part_category")
cache_f <- dpath("explorer_cache.rds")
if (file.exists(cache_f) && file.mtime(cache_f) >= file.mtime(dpath("set_parts.rds"))) {
    set_parts <- readRDS(cache_f)
} else {
    message("Creating fast-start cache (once) ...")
    set_parts <- as.data.table(readRDS(dpath("set_parts.rds")))
    if (!"minifig" %in% names(set_parts)) set_parts[, minifig := NA_character_]
    set_parts <- set_parts[, ..APP_COLS]
    for (v in names(set_parts)) if (is.character(set_parts[[v]])) set(set_parts, j = v, value = factor(set_parts[[v]]))
    try(saveRDS(set_parts, cache_f), silent = TRUE)
}
for (v in names(set_parts)) if (is.factor(set_parts[[v]])) set(set_parts, j = v, value = as.character(set_parts[[v]]))
lego_sets   <- as.data.table(readRDS(dpath("lego_sets.rds")))
parts_index <- as.data.table(readRDS(dpath("parts_index.rds")))
build_info  <- if (file.exists(dpath("build_info.rds"))) readRDS(dpath("build_info.rds")) else list()
if (!"theme_group" %in% names(lego_sets)) lego_sets[, theme_group := theme_name]

# row indices per set and per part, for fast look-ups
rows_by_set  <- split(seq_len(nrow(set_parts)), set_parts$set_num)
rows_by_part <- split(seq_len(nrow(set_parts)), set_parts$part_num)

theme_groups <- sort(unique(na.omit(lego_sets$theme_group)))
year_range   <- range(lego_sets$year, na.rm = TRUE)
categories   <- sort(unique(parts_index$category))
rb_categories <- sort(unique(na.omit(parts_index$part_category)))

# ---- small HTML helpers ------------------------------------------------------
img_tag <- function(url, h = 60) {
    ifelse(is.na(url) | !nzchar(url), "",
           sprintf("<img src='%s' style='max-height:%dpx;max-width:%dpx' loading='lazy'>",
                   url, h, round(1.4 * h)))
}
swatch <- function(rgb, name) {
    sprintf("<span style='display:inline-block;width:13px;height:13px;background:#%s;border:1px solid #888;vertical-align:middle;margin-right:5px'></span>%s",
            ifelse(is.na(rgb), "FFFFFF", rgb), name)
}

# Set-number matching: "75144-1" exact; "75144" all its variants; otherwise partial
match_set_num <- function(ids, q) {
    q <- tolower(trimws(q))
    if (!nzchar(q)) return(rep(TRUE, length(ids)))
    lid <- tolower(ids)
    hit <- lid == q | startsWith(lid, paste0(q, "-"))
    if (!any(hit)) hit <- grepl(q, lid, fixed = TRUE)
    hit
}
# Part-number matching: exact first, then partial
match_part_num <- function(ids, q) {
    q <- tolower(trimws(q))
    if (!nzchar(q)) return(rep(TRUE, length(ids)))
    lid <- tolower(ids)
    hit <- lid == q
    if (!any(hit)) hit <- startsWith(lid, q)
    if (!any(hit)) hit <- grepl(q, lid, fixed = TRUE)
    hit
}

# Desktop mode (launched from "LEGO Sets & Parts Explorer.app"): stop R a few seconds
# after the last browser tab closes. A page reload reconnects in time.
.desktop_sessions <- 0
