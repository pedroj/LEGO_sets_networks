# Function to extract set name from an .mpd file.
# Example usage
# set_name <- extract_mpd_set_name("path/to/your_file.mpd")
# print(set_name)  # "7110 - Landspeeder."
# 
library(stringr)

extract_mpd_set_name <- function(mpd_path) {
    lines <- readLines(mpd_path, warn = FALSE)
    
    name_idx <- grep("^0\\s+Name:\\s+", lines)[1]
    if (is.na(name_idx)) return(NA_character_)
    
    line   <- lines[name_idx]
    tokens <- unlist(strsplit(trimws(line), "\\s+"))
    if (length(tokens) < 3 || tokens[1] != "0" || tokens[2] != "Name:") {
        return(NA_character_)
    }
    
    set_name <- paste(tokens[3:length(tokens)], collapse = " ")
    set_name <- stringr::str_squish(set_name)
    set_name <- gsub("\\s-\\s", "-", set_name)
    set_name <- gsub("\\.(mpd|ldr)$", "", set_name, ignore.case = TRUE)
    
    if (grepl("^[0-9]+-", set_name)) {
        set_name <- sub("^([0-9]+)-", "\\1-1-", set_name)
    }
}

