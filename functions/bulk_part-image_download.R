library(furrr)
library(purrr)

# Read your URLs (one per row, column "url")
# parts_df <- read.csv("lego_urls.csv", stringsAsFactors = FALSE)
parts_df <- data.frame(as.character(lego_parts$part_img_url))
colnames(parts_df)<- c("url", "part_id")
parts_df <- data.frame(cbind(parts_df), as.character(lego_parts$part_num))

# Use URL basenames as filenames, e.g. "1234.png"
parts_df$filename <- basename(parts_df$url)

# path to folder where you want to store images
img_dir <- "lego_parts_img"

# create directory if it doesn't exist
if (!dir.exists(img_dir)) {
    dir.create(img_dir, recursive = TRUE)
}

# example: parts_df has columns `url` and `part_id`
# if you don't already have filenames, build them, e.g. "<part_id>.png"
# parts_df$filename <- paste0(parts_df$part_id, ".png")

download_timeout <- 60  # seconds per file, adjust as needed

for (i in seq_len(nrow(parts_df))) {
    
    url  <- parts_df$url[i]
    f    <- parts_df$filename[i]
    dest <- file.path(img_dir, f)
    
    # skip if file already exists
    if (file.exists(dest)) {
        next
    }
    
    # try downloading; choose mode="wb" for images on Windows
    try(
        download.file(
            url      = url,
            destfile = dest,
            mode     = "wb",
            timeout  = download_timeout
        ),
        silent = TRUE
    )
    
}

