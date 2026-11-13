# deploy_shinyapps.R - publish the LEGO Sets & Parts Explorer to shinyapps.io
# -----------------------------------------------------------------------------
# Run in RStudio (or: Rscript deploy_shinyapps.R). Uses the shinyapps.io account
# already set up on this Mac (z2leso-pedro-jordano). The old "lego-set-explorer"
# app stays as it is; this publishes a separate app "lego-sets-parts-explorer".
#
# Only what the web app needs is uploaded (~16 MB): the code, the banner images,
# the fast-start cache and the small tables. set_parts.rds (25 MB) and the raw
# Rebrickable CSVs stay on the Mac.
# To publish updated data: rebuild the tables on the Mac (Data tab of the desktop
# app, or source("build_data.R"); build_lego_data()), open the desktop app once so
# explorer_cache.rds is refreshed, then run this script again.
# -----------------------------------------------------------------------------

if (!requireNamespace("rsconnect", quietly = TRUE)) install.packages("rsconnect")

app_dir  <- "/Users/pedro/Research/~RCode/MyRCode/LEGO/LEGO_sets_networks/code/LEGO_parts_explorer"
app_name <- "lego-sets-parts-explorer"

acc <- rsconnect::accounts()
if (is.null(acc) || !nrow(acc)) stop("No shinyapps.io account set up in rsconnect on this Mac.")
print(acc)

files <- c("ui.R", "server.R", "global.R", "build_data.R", "classify_part.R",
           "www/icon.png", "www/favicon.png", "www/sta_ana_bricks.png",
           "explorer_cache.rds", "lego_sets.rds", "parts_index.rds", "build_info.rds")
missing <- files[!file.exists(file.path(app_dir, files))]
if (length(missing)) stop("Missing files: ", paste(missing, collapse = ", "),
                          "\n(open the desktop app once to create explorer_cache.rds)")

rsconnect::deployApp(
    appDir     = app_dir,
    appFiles   = files,
    appName    = app_name,
    appTitle   = "LEGO Sets & Parts Explorer",
    account    = acc$name[1],
    server     = acc$server[1],
    forceUpdate = TRUE,
    launch.browser = TRUE
)
cat("\nPublished at: https://", acc$name[1], ".shinyapps.io/", app_name, "/\n", sep = "")
