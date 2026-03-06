convert_jpg_to_png <- function(jpg_dir = "images/part_images", 
                               png_dir = "images/downloaded_images",
                               pattern = "\\.jpg$") {
    # Create output dir
    # dir.create(png_dir, showWarnings = FALSE)
    
    # Find all JPGs
    jpg_files <- list.files(jpg_dir, pattern = pattern, full.names = TRUE)
    cat("Found", length(jpg_files), "JPG files to convert\n")
    
    for (jpg_path in jpg_files) {
        # Read JPG
        img <- image_read(jpg_path)
        
        # PNG destination (same basename)
        png_path <- file.path(png_dir, sub("\\.jpg$", ".png", basename(jpg_path)))
        
        # Skip if PNG exists
        if (!file.exists(png_path)) {
            # Write as PNG
            image_write(img, png_path, format = "png")
            cat("Converted:", basename(jpg_path), "→", basename(png_path), "\n")
        } else {
            cat("Skipped (exists):", basename(png_path), "\n")
        }
    }
}
