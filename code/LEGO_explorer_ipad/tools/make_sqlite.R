# make_sqlite.R - convert the Shiny app's RDS tables into the iPad app's SQLite database
#
#   Rscript tools/make_sqlite.R [rds_dir] [out_file]
#
# Defaults: rds_dir = ../LEGO_parts_explorer, out_file = LEGOExplorer/Resources/lego.sqlite
# Reads set_parts.rds, lego_sets.rds, parts_index.rds (+ build_info.rds) and writes ONE
# normalised database. The schema must match LEGOExplorer/Core/Schema.swift (the app
# rebuilds the same layout from Rebrickable's CSVs when you press "Update tables").
suppressPackageStartupMessages({ library(DBI); library(RSQLite); library(data.table) })

args <- commandArgs(TRUE)
rds_dir <- if (length(args) >= 1) args[1] else "../LEGO_parts_explorer"
out_f   <- if (length(args) >= 2) args[2] else "LEGOExplorer/Resources/lego.sqlite"
rd <- function(f) as.data.table(readRDS(file.path(rds_dir, f)))

message("Reading RDS tables from ", normalizePath(rds_dir))
sp  <- rd("set_parts.rds")
st  <- rd("lego_sets.rds")
pi  <- rd("parts_index.rds")      # already ordered by -n_sets, part_num
bi  <- if (file.exists(file.path(rds_dir, "build_info.rds"))) readRDS(file.path(rds_dir, "build_info.rds")) else list()
if (!"minifig" %in% names(sp)) sp[, minifig := NA_character_]
if (!"theme_group" %in% names(st)) st[, theme_group := theme_name]

if (file.exists(out_f)) file.remove(out_f)
dir.create(dirname(out_f), showWarnings = FALSE, recursive = TRUE)
con <- dbConnect(SQLite(), out_f)
invisible(dbExecute(con, "PRAGMA journal_mode=OFF")); invisible(dbExecute(con, "PRAGMA synchronous=OFF"))

ddl <- c(
 "CREATE TABLE sets(set_id INTEGER PRIMARY KEY, set_num TEXT NOT NULL COLLATE NOCASE, set_name TEXT, year INTEGER, theme_id INTEGER, num_parts INTEGER, set_img_url TEXT, theme_name TEXT, theme_group TEXT)",
 "CREATE TABLE parts(part_id INTEGER PRIMARY KEY, part_num TEXT NOT NULL COLLATE NOCASE, part_name TEXT, category TEXT, subcategory TEXT, part_category TEXT, part_cat_id INTEGER, part_material TEXT, part_img_url TEXT, n_sets INTEGER, n_colors INTEGER, total_qty INTEGER)",
 "CREATE TABLE colors(color_id INTEGER PRIMARY KEY, color_name TEXT, rgb TEXT, is_trans INTEGER)",
 "CREATE TABLE figs(fig_id INTEGER PRIMARY KEY, fig_num TEXT)",
 "CREATE TABLE set_parts(set_id INTEGER NOT NULL, part_id INTEGER NOT NULL, color_id INTEGER, quantity INTEGER, is_spare INTEGER, fig_id INTEGER)",
 "CREATE TABLE part_color_img(part_id INTEGER NOT NULL, color_id INTEGER NOT NULL, img_url TEXT, PRIMARY KEY(part_id, color_id)) WITHOUT ROWID",
 "CREATE TABLE build_info(key TEXT PRIMARY KEY, value TEXT)")
for (s in ddl) invisible(dbExecute(con, s))

# ---- sets (set_id in C-locale set_num order) -------------------------------------
setorder(st, set_num)
st[, set_id := .I]
dbWriteTable(con, "sets", st[, .(set_id, set_num, set_name, year, theme_id, num_parts,
                                 set_img_url, theme_name, theme_group)], append = TRUE)
# ---- parts (part_id = row order of parts_index: -n_sets, part_num) ----------------
pi[, part_id := .I]
dbWriteTable(con, "parts", pi[, .(part_id, part_num, part_name, category, subcategory,
        part_category, part_cat_id, part_material, part_img_url, n_sets, n_colors, total_qty)],
        append = TRUE)
# ---- colours ---------------------------------------------------------------------
co <- unique(sp[, .(color_id, color_name, rgb, is_trans = as.integer(is_trans == "True"))], by = "color_id")
dbWriteTable(con, "colors", co, append = TRUE)
# ---- minifigure ids --------------------------------------------------------------
figs <- data.table(fig_num = sort(unique(na.omit(sp$minifig))))
figs[, fig_id := .I]
dbWriteTable(con, "figs", figs[, .(fig_id, fig_num)], append = TRUE)
# ---- set_parts -------------------------------------------------------------------
sp[, `:=`(set_id = st$set_id[match(set_num, st$set_num)],
          part_id = pi$part_id[match(part_num, pi$part_num)],
          fig_id = figs$fig_id[match(minifig, figs$fig_num)])]
lost <- sp[is.na(set_id) | is.na(part_id), .N]
if (lost) message("Skipping ", lost, " rows whose set or part is missing from the index tables")
sp <- sp[!is.na(set_id) & !is.na(part_id)]
setorder(sp, set_id, part_id, color_id)
dbWriteTable(con, "set_parts", sp[, .(set_id, part_id, color_id, quantity,
                                      is_spare = as.integer(is_spare == "True"), fig_id)], append = TRUE)
# ---- one picture per part + colour -------------------------------------------------
im <- sp[!is.na(part_img_url) & nzchar(part_img_url), .(img_url = part_img_url[1]), by = .(part_id, color_id)]
setorder(im, part_id, color_id)
dbWriteTable(con, "part_color_img", im, append = TRUE)
# ---- info ------------------------------------------------------------------------
info <- c(built = format(if (is.null(bi$built)) Sys.time() else bi$built, "%Y-%m-%d %H:%M"),
          source = if (is.null(bi$source)) "RDS tables" else paste0(bi$source, " (converted from RDS)"),
          n_sets = nrow(st), n_parts = nrow(pi), n_rows = nrow(sp),
          minifig_parts = if (isTRUE(bi$minifig_parts) || nrow(figs) > 0) "1" else "0")
dbWriteTable(con, "build_info", data.frame(key = names(info), value = unname(info)), append = TRUE)

for (s in c("CREATE INDEX sets_num ON sets(set_num)",
            "CREATE INDEX sets_theme ON sets(theme_group, theme_name)",
            "CREATE INDEX parts_num ON parts(part_num)",
            "CREATE INDEX set_parts_set ON set_parts(set_id)",
            "CREATE INDEX set_parts_part ON set_parts(part_id)")) invisible(dbExecute(con, s))
invisible(dbExecute(con, "ANALYZE"))
dbDisconnect(con)
message(sprintf("Wrote %s (%.1f MB): %d sets, %d parts, %d set-part rows",
                out_f, file.size(out_f) / 1e6, nrow(st), nrow(pi), nrow(sp)))
