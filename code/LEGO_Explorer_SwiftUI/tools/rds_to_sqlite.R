# rds_to_sqlite.R - convert the Shiny app's RDS tables into the SQLite database
# that ships inside the SwiftUI app.
#
#   Rscript tools/rds_to_sqlite.R [rds_dir] [out.sqlite]
#
# rds_dir defaults to ../LEGO_parts_explorer (set_parts.rds, lego_sets.rds,
# parts_index.rds, build_info.rds). The schema is the same one the app's own
# "Update tables" builder (Sources/ExplorerCore/TableBuilder.swift) writes.
suppressPackageStartupMessages({ library(DBI); library(RSQLite); library(data.table) })

args <- commandArgs(TRUE)
here <- tryCatch(dirname(normalizePath(sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)))), error = function(e) ".")
rds_dir <- if (length(args) >= 1) args[1] else file.path(here, "..", "..", "LEGO_parts_explorer")
out     <- if (length(args) >= 2) args[2] else file.path(here, "..", "Data", "lego_explorer.sqlite")
dp <- function(f) file.path(rds_dir, f)

PFX <- "https://cdn.rebrickable.com/media/"
strip_pfx <- function(u) { u <- ifelse(is.na(u), "", u); ifelse(startsWith(u, PFX), substr(u, nchar(PFX) + 1, nchar(u)), u) }

message("Reading RDS files from ", normalizePath(rds_dir))
sp  <- as.data.table(readRDS(dp("set_parts.rds")))
st  <- as.data.table(readRDS(dp("lego_sets.rds")))
pix <- as.data.table(readRDS(dp("parts_index.rds")))
bi  <- if (file.exists(dp("build_info.rds"))) readRDS(dp("build_info.rds")) else list()
if (!"minifig" %in% names(sp)) sp[, minifig := NA_character_]
if (!"theme_group" %in% names(st)) st[, theme_group := theme_name]

# sets: newest first (same order the Shiny app shows)
setorder(st, -year, set_num, na.last = TRUE)
st[, id := .I]
# parts: most used first (parts_index.rds order)
pix[, id := .I]

stopifnot(!anyNA(match(sp$set_num, st$set_num)), !anyNA(match(sp$part_num, pix$part_num)))

colors <- unique(sp[, .(color_id, name = color_name, rgb, is_trans = is_trans == "True")], by = "color_id")
setorder(colors, color_id)

setparts <- sp[, .(set_id = st$id[match(set_num, st$set_num)],
                   part_id = pix$id[match(part_num, pix$part_num)],
                   color_id, quantity, is_spare = as.integer(is_spare == "True"), minifig)]
setorder(setparts, set_id, part_id, color_id)

pimg <- sp[nzchar(part_img_url), .(set_num, part_num, color_id, url = strip_pfx(part_img_url))]
pimg <- unique(pimg, by = c("part_num", "color_id"))
pimg <- pimg[, .(part_id = pix$id[match(part_num, pix$part_num)], color_id, url)]
setorder(pimg, part_id, color_id)

if (file.exists(out)) file.remove(out)
dir.create(dirname(out), showWarnings = FALSE, recursive = TRUE)
con <- dbConnect(SQLite(), out)
dbExecute(con, "PRAGMA journal_mode = OFF")
for (s in c(
 "CREATE TABLE meta (key TEXT PRIMARY KEY, value TEXT)",
 "CREATE TABLE sets (id INTEGER PRIMARY KEY, set_num TEXT NOT NULL, set_name TEXT, year INTEGER, theme_id INTEGER, num_parts INTEGER, img_url TEXT, theme_name TEXT, theme_group TEXT)",
 "CREATE TABLE parts (id INTEGER PRIMARY KEY, part_num TEXT NOT NULL, part_name TEXT, category TEXT, subcategory TEXT, part_category TEXT, part_material TEXT, img_url TEXT, n_sets INTEGER, n_colors INTEGER, total_qty INTEGER)",
 "CREATE TABLE colors (color_id INTEGER PRIMARY KEY, name TEXT, rgb TEXT, is_trans INTEGER)",
 "CREATE TABLE set_parts (set_id INTEGER NOT NULL, part_id INTEGER NOT NULL, color_id INTEGER NOT NULL, quantity INTEGER, is_spare INTEGER, minifig TEXT)",
 "CREATE TABLE part_images (part_id INTEGER NOT NULL, color_id INTEGER NOT NULL, url TEXT, PRIMARY KEY (part_id, color_id)) WITHOUT ROWID"
)) dbExecute(con, s)

dbWriteTable(con, "sets", as.data.frame(st[, .(id, set_num, set_name, year, theme_id, num_parts,
                                             img_url = strip_pfx(set_img_url), theme_name, theme_group)]), append = TRUE)
dbWriteTable(con, "parts", as.data.frame(pix[, .(id, part_num, part_name, category, subcategory, part_category,
                                               part_material, img_url = strip_pfx(part_img_url), n_sets, n_colors, total_qty)]), append = TRUE)
dbWriteTable(con, "colors", as.data.frame(colors), append = TRUE)
dbWriteTable(con, "set_parts", as.data.frame(setparts), append = TRUE)
dbWriteTable(con, "part_images", as.data.frame(pimg), append = TRUE)

meta <- c(built = format(if (!is.null(bi$built)) bi$built else Sys.time(), "%Y-%m-%d %H:%M"),
          source = if (!is.null(bi$source)) bi$source else "RDS tables",
          n_sets = nrow(st), n_parts = nrow(pix), n_rows = nrow(setparts),
          minifig_parts = if (isTRUE(bi$minifig_parts)) "1" else "0",
          image_prefix = PFX, schema_version = "1")
dbWriteTable(con, "meta", data.frame(key = names(meta), value = unname(as.character(meta))), append = TRUE)

for (s in c("CREATE INDEX idx_sp_set  ON set_parts(set_id)",
            "CREATE INDEX idx_sp_part ON set_parts(part_id)",
            "CREATE INDEX idx_sets_num ON sets(set_num)",
            "CREATE INDEX idx_parts_num ON parts(part_num)")) dbExecute(con, s)
dbExecute(con, "ANALYZE")
dbDisconnect(con)
dbDisconnect(dbConnect(SQLite(), out), shutdown = TRUE)
con <- dbConnect(SQLite(), out); dbExecute(con, "VACUUM"); dbDisconnect(con)
message(sprintf("Wrote %s (%.1f MB)", normalizePath(out), file.size(out) / 1e6))
