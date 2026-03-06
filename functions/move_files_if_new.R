# Usage examples
#
# Move all PNGs from source to dest, skipping duplicates
# move_files_if_new("images/source", "images/final")
move_files_if_new <- function(from_dir, to_dir, pattern = NULL, ...) {
    if (!dir.exists(to_dir)) dir.create(to_dir, recursive = TRUE)
    
    files <- list.files(from_dir, pattern = pattern, full.names = TRUE)
    
    for (file in files) {
        dest_file <- file.path(to_dir, basename(file))
        if (!file.exists(dest_file)) {
            file.copy(file, dest_file, overwrite = FALSE)
            file.remove(file)
            cat("Moved:", basename(file), "\n")
        } else {
            cat("Skipped (exists):", basename(file), "\n")
        }
    }
}
