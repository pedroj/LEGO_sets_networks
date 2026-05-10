## ============================================================
## parse_mpd_parts.R
##
## Extract LEGO part filenames, human names, and categories
## from an LDraw .mpd file.
##
## Sources for part metadata (tried in order):
##   1. Sub-parts defined inline within the MPD itself
##   2. LDraw parts library on disk (if LDRAW_DIR is set)
##   3. Rebrickable parts CSV  (parts.csv + part_categories.csv)
##
## Usage:
##   source("parse_mpd_parts.R")
##   df <- parse_mpd(
##     mpd_path         = "311-1-Ferry.mpd",
##     rebrickable_dir  = "path/to/rebrickable/csvs"   # optional
##   )
## ============================================================

library(tidyverse)

## ------------------------------------------------------------
## 1.  Low-level helpers
## ------------------------------------------------------------

#' Read all lines from an MPD file, stripping CR if present
read_mpd_lines <- function(path) {
  readLines(path, warn = FALSE) |>
    str_remove("\r$")           # handle CRLF files
}

#' Extract the filename from a "0 FILE ..." line
file_line_to_name <- function(line) {
  str_remove(line, "^0 FILE\\s+") |>   # strip the prefix
    str_trim() |>
    basename()                          # drop any sub-folder prefix (e.g. s\)
}

## ------------------------------------------------------------
## 2.  Parse the MPD: collect referenced parts + inline defs
## ------------------------------------------------------------

#' Parse an MPD and return two tibbles:
#'   $refs  – every unique part filename referenced (line type 1)
#'   $defs  – parts defined inline (0 FILE blocks inside the MPD)
parse_mpd_raw <- function(lines) {

  ## --- 2a. All parts referenced on type-1 lines ---
  ## Format: 1 <colour> x y z a b c d e f g h i <filename>
  refs <- lines |>
    str_subset("^1\\s") |>
    str_extract("[^\\s]+$") |>         # last token = filename
    str_to_lower() |>
    basename() |>
    unique() |>
    sort()

  ## --- 2b. Inline sub-part definitions ---
  ## Each 0 FILE block has:
  ##   0 FILE  <filename>
  ##   0 <human description>          <- first non-empty meta line
  ##   0 Name: ...
  ##   0 !CATEGORY <category>         <- optional; rare in MPDs
  file_idx <- which(str_detect(lines, "^0 FILE\\s"))

  defs <- map_dfr(seq_along(file_idx), function(i) {
    start <- file_idx[i]
    end   <- if (i < length(file_idx)) file_idx[i + 1] - 1L else length(lines)
    block <- lines[start:end]

    filename <- file_line_to_name(block[1]) |> str_to_lower()

    ## Human name: first meta line after FILE that is NOT blank,
    ## NOT "Name:", NOT "Author:", NOT a flag (0 !)
    name_line <- block[-1] |>
      str_subset("^0\\s") |>
      str_subset("^0 Name:|^0 Author:|^0 !|^0 BFC|^0 //"
                 , negate = TRUE) |>
      head(1)

    human_name <- if (length(name_line) > 0)
      str_remove(name_line, "^0\\s+") |> str_trim()
    else
      NA_character_

    ## Category: explicit 0 !CATEGORY line (often absent in MPDs)
    cat_line <- block |> str_subset("^0 !CATEGORY\\s") |> head(1)
    category <- if (length(cat_line) > 0)
      str_remove(cat_line, "^0 !CATEGORY\\s+") |> str_trim()
    else
      NA_character_

    tibble(filename = filename, human_name = human_name, category = category)
  })

  list(refs = refs, defs = defs)
}

## ------------------------------------------------------------
## 3.  LDraw library lookup (optional)
## ------------------------------------------------------------

#' Try to find and read a .dat file from a local LDraw library.
#' Returns list(human_name, category) or NULL if not found.
lookup_ldraw <- function(filename, ldraw_dir) {
  if (is.null(ldraw_dir) || !dir.exists(ldraw_dir)) return(NULL)

  candidates <- c(
    file.path(ldraw_dir, "parts",  filename),
    file.path(ldraw_dir, "p",      filename),
    file.path(ldraw_dir, "parts", "s", filename)
  )
  hit <- candidates[file.exists(candidates)]
  if (length(hit) == 0) return(NULL)

  dat_lines <- readLines(hit[1], warn = FALSE) |> str_remove("\r$")

  ## Human name: first "0 <text>" line that is not a flag
  name_line <- dat_lines |>
    str_subset("^0\\s") |>
    str_subset("^0 !|^0 BFC|^0 //"
               , negate = TRUE) |>
    head(1)

  human_name <- if (length(name_line) > 0)
    str_remove(name_line, "^0\\s+") |> str_trim()
  else
    NA_character_

  ## Category: 0 !CATEGORY or inferred from human name prefix
  cat_line <- dat_lines |> str_subset("^0 !CATEGORY\\s") |> head(1)
  category <- if (length(cat_line) > 0)
    str_remove(cat_line, "^0 !CATEGORY\\s+") |> str_trim()
  else
    NA_character_

  list(human_name = human_name, category = category)
}

## ------------------------------------------------------------
## 4.  Rebrickable CSV lookup (optional)
## ------------------------------------------------------------

#' Load Rebrickable parts.csv and part_categories.csv from a directory.
#' Expects files named exactly "parts.csv" and "part_categories.csv".
load_rebrickable <- function(rebrickable_dir) {
  if (is.null(rebrickable_dir) || !dir.exists(rebrickable_dir)) return(NULL)

  parts_path  <- file.path(rebrickable_dir, "parts.csv")
  cats_path   <- file.path(rebrickable_dir, "part_categories.csv")

  if (!file.exists(parts_path)) {
    message("Rebrickable: parts.csv not found in ", rebrickable_dir)
    return(NULL)
  }

  parts <- read_csv(parts_path, col_types = cols(.default = "c"), show_col_types = FALSE)

  ## Rebrickable part_num uses the bare number without extension
  ## (e.g. "3001", not "3001.dat")
  parts <- parts |>
    mutate(filename = str_to_lower(paste0(part_num, ".dat")))

  if (file.exists(cats_path)) {
    cats <- read_csv(cats_path, col_types = cols(.default = "c"), show_col_types = FALSE) |>
      rename(cat_name = name)
    parts <- parts |>
      left_join(cats, by = c("part_cat_id" = "id"))
  } else {
    message("Rebrickable: part_categories.csv not found; categories will be NA")
    parts <- parts |> mutate(cat_name = NA_character_)
  }

  parts |> select(filename, human_name = name, category = cat_name)
}

## ------------------------------------------------------------
## 5.  Main function
## ------------------------------------------------------------

#' Parse an MPD file and return a tidy dataframe with one row
#' per unique referenced part.
#'
#' @param mpd_path        Path to the .mpd file.
#' @param ldraw_dir       (optional) Path to LDraw library root.
#' @param rebrickable_dir (optional) Path to folder with
#'                        parts.csv and part_categories.csv from
#'                        https://rebrickable.com/downloads/
#' @return A tibble with columns:
#'   filename   – LDraw part filename  (e.g. "3002.dat")
#'   human_name – Descriptive name     (e.g. "Brick 1 x 2")
#'   category   – Part category        (e.g. "Brick")
#'   source     – Where the name came from
parse_mpd <- function(mpd_path,
                      ldraw_dir       = Sys.getenv("LDRAW_DIR", unset = NA),
                      rebrickable_dir = NULL) {

  if (is.na(ldraw_dir) || ldraw_dir == "") ldraw_dir <- NULL

  message("Reading: ", mpd_path)
  lines  <- read_mpd_lines(mpd_path)
  parsed <- parse_mpd_raw(lines)
  refs   <- parsed$refs
  defs   <- parsed$defs

  message(sprintf("  %d unique part references found", length(refs)))
  message(sprintf("  %d sub-parts defined inline",     nrow(defs)))

  ## --- Load Rebrickable lookup table once ---
  rb <- load_rebrickable(rebrickable_dir)

  ## --- Build result row-by-row ---
  result <- map_dfr(refs, function(fn) {

    ## Priority 1: inline definition in the MPD
    inline <- defs |> filter(filename == fn)
    if (nrow(inline) > 0) {
      return(tibble(
        filename   = fn,
        human_name = inline$human_name[1],
        category   = inline$category[1],
        source     = "mpd_inline"
      ))
    }

    ## Priority 2: local LDraw library
    if (!is.null(ldraw_dir)) {
      ld <- lookup_ldraw(fn, ldraw_dir)
      if (!is.null(ld)) {
        return(tibble(
          filename   = fn,
          human_name = ld$human_name,
          category   = ld$category,
          source     = "ldraw_library"
        ))
      }
    }

    ## Priority 3: Rebrickable CSV
    if (!is.null(rb)) {
      hit <- rb |> filter(filename == fn)
      if (nrow(hit) > 0) {
        return(tibble(
          filename   = fn,
          human_name = hit$human_name[1],
          category   = hit$category[1],
          source     = "rebrickable"
        ))
      }
    }

    ## Fallback: no metadata found
    tibble(filename = fn, human_name = NA_character_,
           category = NA_character_, source = "not_found")
  })

  message(sprintf("  %d / %d parts resolved",
                  sum(!is.na(result$human_name)), nrow(result)))

  result
}

## ------------------------------------------------------------
## 6.  Example run (edit paths as needed)
## ------------------------------------------------------------

## Minimal call – inline MPD parts only:
# df <- parse_mpd("311-1-Ferry.mpd")

## With a local LDraw library:
# df <- parse_mpd("311-1-Ferry.mpd", ldraw_dir = "~/ldraw")

## With Rebrickable CSVs (download from https://rebrickable.com/downloads/):
# df <- parse_mpd(
#   mpd_path         = "311-1-Ferry.mpd",
#   rebrickable_dir  = "~/data/rebrickable"
# )

## Quick preview
# print(df, n = Inf)
# df |> count(category, sort = TRUE)
