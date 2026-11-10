# run_app.R - start the LEGO Sets & Parts Explorer from R / RStudio
# First launch downloads the Rebrickable tables (1-2 minutes); later launches are fast.

for (p in c("shiny", "DT", "data.table", "later")) {
    if (!requireNamespace(p, quietly = TRUE))
        install.packages(p, repos = "https://cloud.r-project.org")
}

app_dir <- "/Users/pedro/Documents/Working/~RCode/MyRCode/LEGO/LEGO_sets_networks/code/LEGO_parts_explorer"
shiny::runApp(appDir = app_dir, port = 8766, launch.browser = TRUE)
