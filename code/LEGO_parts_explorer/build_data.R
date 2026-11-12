# build_data.R - rebuild the LEGO Parts Explorer tables from Rebrickable CSVs
# -----------------------------------------------------------------------------
# No API key is needed: Rebrickable publishes its full database as CSV files.
#
#   source("build_data.R")
#   build_lego_data()                        # download fresh CSVs from Rebrickable
#   build_lego_data(src = "/path/to/csvs")   # use CSVs you already have locally
#
# From Terminal:  Rscript build_data.R [optional/path/to/csv/folder]
#
# Needed tables (.csv or .csv.gz): themes, colors, part_categories, parts, sets,
# inventories, inventory_parts. Optional: inventory_minifigs (adds the parts of
# the minifigures that come with each set).
#
# Writes into out_dir (the app folder by default):
#   set_parts.rds    one row per set x part x colour, same 21-column layout as the
#                    previous set_parts.rds (+ column `minifig`)
#   lego_sets.rds    one row per set (+ column `theme_group`, the top-level theme)
#   parts_index.rds  one row per part: names, Rebrickable category, category and
#                    subcategory (classify_part.R), image, number of sets
#   build_info.rds   date and source of the build
# -----------------------------------------------------------------------------

suppressPackageStartupMessages(library(data.table))

RB_TABLES   <- c("themes", "colors", "part_categories", "parts", "sets",
                 "inventories", "inventory_parts")
RB_OPTIONAL <- c("inventory_minifigs")
RB_URL      <- "https://cdn.rebrickable.com/media/downloads/%s.csv.gz"

# columns that must stay text (part numbers like 3001 would otherwise become numbers)
.RB_CHAR <- c("set_num", "part_num", "fig_num", "name", "img_url", "rgb",
              "is_trans", "is_spare", "part_material")

.find_table <- function(src, t) {
  f <- file.path(src, paste0(t, c(".csv.gz", ".csv")))
  f[file.exists(f)][1]
}

read_rb <- function(f) {
  hdr <- names(fread(if (grepl("\\.gz$", f)) cmd_gz(f) else f, nrows = 0))
  cc  <- intersect(.RB_CHAR, hdr)
  args <- list(colClasses = list(character = cc), encoding = "UTF-8",
               na.strings = "NA", showProgress = FALSE)
  if (grepl("\\.gz$", f)) do.call(fread, c(list(cmd = cmd_gz(f)), args))
  else do.call(fread, c(list(file = f), args))
}
cmd_gz <- function(f) paste("gzip -dc", shQuote(f))

download_rb <- function(dir) {
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  for (t in c(RB_TABLES, RB_OPTIONAL)) {
    dest <- file.path(dir, paste0(t, ".csv.gz"))
    message("  downloading ", t, " ...")
    ok <- tryCatch({
      utils::download.file(sprintf(RB_URL, t), dest, mode = "wb", quiet = TRUE); TRUE
    }, error = function(e) FALSE, warning = function(w) FALSE)
    if (!ok && t %in% RB_TABLES) stop("Could not download ", t, " from Rebrickable.")
  }
  dir
}

.as_bool_txt <- function(x) ifelse(toupper(substr(as.character(x), 1, 1)) %in% c("T", "1"), "True", "False")

build_lego_data <- function(src = NULL, out_dir = ".", app_dir = ".") {
  t0 <- Sys.time()
  if (is.null(src) || !nzchar(src)) {
    message("Downloading Rebrickable tables (no API key needed) ...")
    src <- download_rb(file.path(out_dir, "rebrickable_csv"))
    source_label <- "cdn.rebrickable.com (downloaded)"
  } else {
    source_label <- normalizePath(src)
  }
  miss <- RB_TABLES[is.na(vapply(RB_TABLES, function(t) .find_table(src, t), ""))]
  if (length(miss)) stop("Missing tables in ", src, ": ", paste(miss, collapse = ", "))
  rb <- lapply(setNames(RB_TABLES, RB_TABLES), function(t) read_rb(.find_table(src, t)))
  fm <- .find_table(src, "inventory_minifigs")
  if (!is.na(fm)) rb$inventory_minifigs <- read_rb(fm)
  message("Tables read: ", paste(names(rb), collapse = ", "))

  # ---- themes: name, direct parent, top-level theme (theme group) -------------
  th <- rb$themes[, .(id, name, parent_id)]
  root <- th$id
  repeat {
    p <- th$parent_id[match(root, th$id)]
    upd <- !is.na(p) & p %in% th$id
    if (!any(upd)) break
    root[upd] <- p[upd]
  }
  th[, theme_group := th$name[match(root, th$id)]]

  # ---- sets -------------------------------------------------------------------
  st <- rb$sets[, .(set_num, set_name = name, year, theme_id, num_parts, set_img_url = img_url)]
  st[, `:=`(theme_name  = th$name[match(theme_id, th$id)],
            parent_id   = th$parent_id[match(theme_id, th$id)],
            theme_group = th$theme_group[match(theme_id, th$id)])]

  # ---- inventories: latest version of every set (and minifig) ----------------
  inv <- rb$inventories[order(set_num, -version)][!duplicated(set_num)]

  ip <- rb$inventory_parts
  if (!"img_url" %in% names(ip)) ip[, img_url := NA_character_]
  ip <- ip[, .(inventory_id, part_num, color_id, quantity, is_spare, part_img_url = img_url)]

  # parts that sit directly in each set's inventory
  sp <- merge(inv[, .(inventory_id = id, set_num, version)], ip, by = "inventory_id")
  sp[, minifig := NA_character_]

  # parts of the minifigures included in each set (optional table)
  if (!is.null(rb$inventory_minifigs)) {
    im <- rb$inventory_minifigs[, .(inventory_id, fig_num, fig_qty = quantity)]
    im <- merge(inv[, .(inventory_id = id, set_num, version)], im, by = "inventory_id")
    fig_inv <- inv[grepl("^fig-", set_num), .(fig_inventory_id = id, fig_num = set_num)]
    im <- merge(im, fig_inv, by = "fig_num")
    fp <- merge(im, ip, by.x = "fig_inventory_id", by.y = "inventory_id")
    fp[, quantity := quantity * fig_qty]
    fp <- fp[, .(inventory_id, set_num, version, part_num, color_id, quantity, is_spare,
                 part_img_url, minifig = fig_num)]
    sp <- rbind(sp, fp[set_num %in% st$set_num], use.names = TRUE)
  }
  sp <- sp[set_num %in% st$set_num]

  # ---- join part, category, colour and set information ------------------------
  pt <- rb$parts[, .(part_num, part_name = name, part_cat_id, part_material)]
  pc <- rb$part_categories[, .(part_cat_id = id, part_category = name)]
  co <- rb$colors[, .(color_id = id, color_name = name, rgb, is_trans)]
  sp <- merge(sp, pt, by = "part_num", all.x = TRUE)
  sp <- merge(sp, pc, by = "part_cat_id", all.x = TRUE)
  sp <- merge(sp, co, by = "color_id", all.x = TRUE)
  sp <- merge(sp, st[, .(set_num, set_name, year, theme_id, theme_name, parent_id, set_img_url)],
              by = "set_num", all.x = TRUE)
  sp[, `:=`(is_spare = .as_bool_txt(is_spare), is_trans = .as_bool_txt(is_trans))]
  sp[is.na(part_img_url), part_img_url := ""]
  setcolorder(sp, c("theme_name", "theme_id", "set_img_url", "set_num", "set_name", "year",
                    "quantity", "inventory_id", "part_cat_id", "part_category", "part_num",
                    "part_name", "part_img_url", "part_material", "color_id", "color_name",
                    "is_trans", "rgb", "parent_id", "is_spare", "version", "minifig"))
  setorder(sp, set_num, part_category, part_num, color_id)

  # ---- one row per part, with category / subcategory --------------------------
  message("Classifying ", nrow(pt), " parts into categories and subcategories ...")
  source(file.path(app_dir, "classify_part.R"), local = TRUE)
  pidx <- merge(pt, pc, by = "part_cat_id", all.x = TRUE)
  cls  <- classify_parts(pidx$part_name, pidx$part_category)
  pidx[, `:=`(category = cls$category, subcategory = cls$subcategory)]
  # representative image: the most used colour that has a photo
  img <- sp[nzchar(part_img_url), .(n = .N), by = .(part_num, part_img_url)][order(part_num, -n)]
  img <- img[!duplicated(part_num)]
  use <- sp[, .(n_sets = uniqueN(set_num), n_colors = uniqueN(color_id),
                                       total_qty = sum(quantity)), by = part_num]
  pidx <- merge(pidx, img[, .(part_num, part_img_url)], by = "part_num", all.x = TRUE)
  pidx <- merge(pidx, use, by = "part_num", all.x = TRUE)
  pidx[is.na(n_sets), `:=`(n_sets = 0L, n_colors = 0L, total_qty = 0L)]
  pidx[is.na(part_img_url), part_img_url := ""]
  setcolorder(pidx, c("part_num", "part_name", "category", "subcategory", "part_category",
                      "part_cat_id", "part_material", "part_img_url", "n_sets", "n_colors", "total_qty"))
  setorder(pidx, -n_sets, part_num)

  # ---- save -------------------------------------------------------------------
  lego_sets <- as.data.frame(st[, .(set_num, set_name, year, theme_id, num_parts, set_img_url,
                                    theme_name, parent_id, theme_group)])
  for (f in c("set_parts.rds", "lego_sets.rds")) {          # keep one previous copy
    old <- file.path(out_dir, f)
    if (file.exists(old)) file.copy(old, file.path(out_dir, sub("\\.rds$", "_previous.rds", f)),
                                    overwrite = TRUE)
  }
  saveRDS(as.data.frame(sp), file.path(out_dir, "set_parts.rds"))
  saveRDS(lego_sets, file.path(out_dir, "lego_sets.rds"))
  saveRDS(as.data.frame(pidx), file.path(out_dir, "parts_index.rds"))
  info <- list(built = Sys.time(), source = source_label, n_sets = nrow(lego_sets),
               n_parts = nrow(pidx), n_rows = nrow(sp),
               minifig_parts = !is.null(rb$inventory_minifigs))
  saveRDS(info, file.path(out_dir, "build_info.rds"))
  message(sprintf("Done in %.0f s: %d sets, %d parts, %d set-part rows.",
                  as.numeric(difftime(Sys.time(), t0, units = "secs")),
                  info$n_sets, info$n_parts, info$n_rows))
  invisible(info)
}

# Rscript build_data.R [csv folder]
if (sys.nframe() == 0) {
  a <- commandArgs(TRUE)
  build_lego_data(src = if (length(a)) a[1] else NULL)
}
