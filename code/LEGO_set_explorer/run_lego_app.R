# run_lego_app.R

if (!requireNamespace("shiny", quietly = TRUE)) {
    install.packages("shiny", repos = "https://cloud.r-project.org")
}

# Change this to your actual path
app_dir <- "/Users/pedro//Documents/Working/~RCode/MyRCode/LEGO/LEGO_sets_networks/code/LEGO_set_explorer"

setwd(app_dir)
shiny::runApp(appDir = ".", launch.browser = TRUE)
