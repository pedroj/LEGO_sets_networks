# =============================================================================
# rebrickable_set_images.R
# Download / refresh Rebrickable set images for a theme (default: Star Wars).
#
# Images are saved as <set_num>.jpg / <set_num>.png (e.g. 4010-1.png) in the
# shared folder read by the network-plotting pipeline. Re-running only fetches
# sets whose image is missing, so it is safe to run periodically.
#
# Usage (from the repo root):
#   Rscript functions/rebrickable_set_images.R                # Star Wars, new only
#   Rscript functions/rebrickable_set_images.R --dry-run      # list what is missing
#   Rscript functions/rebrickable_set_images.R --force        # re-download all
#   Rscript functions/rebrickable_set_images.R --theme "Technic" --format jpg
#   Rscript functions/rebrickable_set_images.R --sets 4010-1,4485-1
#
# Options:
#   --theme NAME     Rebrickable theme name, subthemes included (default "Star Wars")
#   --format F       both | jpg | png   (default both; CDN serves jpg, png is converted)
#   --sets A,B       only these set numbers (skips the theme query for the list)
#   --dir PATH       output folder (default: env REBRICKABLE_IMG_DIR or the path below)
#   --force          re-download even if the file exists
#   --dry-run        only report what would be downloaded
#
# API key: env REBRICKABLE_API_KEY (put it in ~/.Renviron). Never hard-code it.
# =============================================================================

suppressPackageStartupMessages({
  library(httr2)
  library(dplyr)
  library(readr)
})

# --- 0. Config ----------------------------------------------------------------
API_BASE    <- "https://rebrickable.com/api/v3"
DEFAULT_DIR <- "/Users/pedro/Research/~RCode/MyRCode/LEGO/LEGO_sets_networks/data/rebrickable_set_images"

`%||%` <- function(x, y) if (is.null(x)) y else x

get_api_key <- function() {
  key <- Sys.getenv("REBRICKABLE_API_KEY")
  if (!nzchar(key)) key <- Sys.getenv("REBRICKABLE_KEY")  # name used in ~/.Renviron
  if (!nzchar(key)) {
    stop("REBRICKABLE_API_KEY is not set. Add `REBRICKABLE_API_KEY=<your key>` to ",
         "~/.Renviron (key from rebrickable.com > Settings > API) and restart R.",
         call. = FALSE)
  }
  key
}

# --- 1. API helpers -----------------------------------------------------------
# Paginated GET; follows `next` until exhausted. Throttled (~1 req/s) and
# retried on 429/5xx, as the Rebrickable API rate-limits.
rb_get_all <- function(path, query = list(), key = get_api_key()) {
  url <- paste0(API_BASE, path)
  out <- list()
  repeat {
    req <- request(url) |>
      req_headers(Authorization = paste("key", key), Accept = "application/json") |>
      req_throttle(rate = 1) |>
      req_retry(max_tries = 5, is_transient = \(r) resp_status(r) %in% c(429, 500, 502, 503, 504))
    if (length(query)) req <- do.call(req_url_query, c(list(req), query))
    resp <- req_perform(req)
    body <- resp_body_json(resp, simplifyVector = FALSE)
    out  <- c(out, body$results)
    if (is.null(body$`next`)) break
    url   <- body$`next`   # absolute URL that already carries the query
    query <- list()
  }
  out
}

rb_theme_ids <- function(theme_name) {
  th <- rb_get_all("/lego/themes/", list(page_size = 1000))
  th <- tibble(id = vapply(th, \(x) x$id, 1L),
               parent_id = vapply(th, \(x) x$parent_id %||% NA_integer_, 1L),
               name = vapply(th, \(x) x$name, ""))
  root <- th$id[tolower(th$name) == tolower(theme_name) & is.na(th$parent_id)]
  if (!length(root)) stop("Top-level theme not found: ", theme_name, call. = FALSE)
  ids <- root
  repeat {  # collect all descendants
    kids <- th$id[th$parent_id %in% ids & !(th$id %in% ids)]
    if (!length(kids)) break
    ids <- c(ids, kids)
  }
  message(sprintf("Theme '%s': id %s + %d subthemes", theme_name, root[1], length(ids) - 1))
  ids
}

rb_theme_sets <- function(theme_ids) {
  res <- lapply(theme_ids, \(id) rb_get_all("/lego/sets/", list(theme_id = id, page_size = 1000)))
  res <- unlist(res, recursive = FALSE)
  tibble(
    set_num   = vapply(res, \(x) x$set_num, ""),
    name      = vapply(res, \(x) x$name, ""),
    year      = vapply(res, \(x) x$year %||% NA_integer_, 1L),
    theme_id  = vapply(res, \(x) x$theme_id, 1L),
    num_parts = vapply(res, \(x) x$num_parts %||% NA_integer_, 1L),
    img_url   = vapply(res, \(x) x$set_img_url %||% NA_character_, "")
  ) |> distinct(set_num, .keep_all = TRUE) |> arrange(set_num)
}

# --- 2. Download --------------------------------------------------------------
download_one <- function(set_num, img_url, dir, format, force) {
  jpg <- file.path(dir, paste0(set_num, ".jpg"))
  png <- file.path(dir, paste0(set_num, ".png"))
  need_jpg <- format %in% c("both", "jpg")
  need_png <- format %in% c("both", "png")
  have <- (!need_jpg || file.exists(jpg)) && (!need_png || file.exists(png))
  if (have && !force) return("exists")
  if (is.na(img_url) || !nzchar(img_url)) return("no_image_on_rebrickable")

  tmp <- tempfile(fileext = ".jpg")
  err <- NULL
  ok <- tryCatch({
    request(img_url) |>
      req_retry(max_tries = 6, backoff = \(i) min(60, 5 * 2^(i - 1)),
                is_transient = \(r) resp_status(r) %in% c(403, 429, 500, 502, 503, 504)) |>
      req_throttle(rate = 1.5) |>
      req_perform(path = tmp)
    file.size(tmp) > 0
  }, error = \(e) { err <<- conditionMessage(e); FALSE })
  if (!ok) return(paste0("download_failed: ", substr(err %||% "empty file", 1, 80)))

  if (need_jpg) file.copy(tmp, jpg, overwrite = TRUE)
  if (need_png) {
    conv <- tryCatch({
      img <- magick::image_read(tmp)
      magick::image_write(img, png, format = "png"); TRUE
    }, error = \(e) FALSE)
    if (!conv) return("png_conversion_failed")
  }
  unlink(tmp)
  "downloaded"
}

# --- 3. Driver ----------------------------------------------------------------
update_set_images <- function(theme = "Star Wars", sets = NULL,
                              dir = Sys.getenv("REBRICKABLE_IMG_DIR", DEFAULT_DIR),
                              format = "both", force = FALSE, dry_run = FALSE) {
  format <- match.arg(format, c("both", "jpg", "png"))
  if (format != "jpg" && !requireNamespace("magick", quietly = TRUE))
    stop("Package 'magick' is needed for png output (or use --format jpg).", call. = FALSE)
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)

  if (is.null(sets)) {
    tbl <- rb_theme_sets(rb_theme_ids(theme))
  } else {
    key <- get_api_key()
    tbl <- bind_rows(lapply(sets, \(s) {
      x <- request(paste0(API_BASE, "/lego/sets/", s, "/")) |>
        req_headers(Authorization = paste("key", key)) |> req_throttle(rate = 1) |>
        req_perform() |> resp_body_json()
      tibble(set_num = x$set_num, name = x$name, year = x$year, theme_id = x$theme_id,
             num_parts = x$num_parts, img_url = x$set_img_url %||% NA_character_)
    }))
  }
  message(sprintf("%d sets listed on Rebrickable.", nrow(tbl)))

  if (dry_run) {
    miss <- tbl |> filter(!file.exists(file.path(dir, paste0(set_num, if (format == "png") ".png" else ".jpg"))))
    message(sprintf("DRY RUN: %d sets missing images in %s", nrow(miss), dir))
    print(miss |> select(set_num, name, year), n = 50)
    return(invisible(miss))
  }

  tbl$status <- NA_character_
  for (i in seq_len(nrow(tbl))) {
    tbl$status[i] <- download_one(tbl$set_num[i], tbl$img_url[i], dir, format, force)
    if (tbl$status[i] != "exists")
      message(sprintf("[%d/%d] %s  %s", i, nrow(tbl), tbl$set_num[i], tbl$status[i]))
  }

  # Manifest: merge with previous so --sets runs don't drop other rows.
  mf_path <- file.path(dir, "_manifest.csv")
  tbl$checked_at <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  if (file.exists(mf_path)) {
    old <- read_csv(mf_path, show_col_types = FALSE) |>
      mutate(across(everything(), as.character)) |> filter(!set_num %in% tbl$set_num)
    tbl_out <- bind_rows(old, mutate(tbl, across(everything(), as.character)))
  } else tbl_out <- tbl
  write_csv(arrange(tbl_out, set_num), mf_path)

  print(table(tbl$status))
  invisible(tbl)
}

# --- 4. CLI -------------------------------------------------------------------
if (sys.nframe() == 0 && !interactive()) {
  a <- commandArgs(trailingOnly = TRUE)
  opt <- function(flag, default = NULL) {
    i <- match(flag, a); if (is.na(i) || i == length(a)) default else a[i + 1]
  }
  sets <- opt("--sets"); if (!is.null(sets)) sets <- strsplit(sets, ",")[[1]]
  update_set_images(
    theme   = opt("--theme", "Star Wars"),
    sets    = sets,
    dir     = opt("--dir", Sys.getenv("REBRICKABLE_IMG_DIR", DEFAULT_DIR)),
    format  = opt("--format", "both"),
    force   = "--force" %in% a,
    dry_run = "--dry-run" %in% a
  )
}
