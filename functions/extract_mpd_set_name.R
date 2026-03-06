# Function to extract set name from an .mpd file.
# Example usage
# set_name <- extract_mpd_set_name("path/to/your_file.mpd")
# print(set_name)  # "7110 - Landspeeder."
# 
extract_mpd_set_name <- function(mpd_path) {
    # Read all lines
    lines <- readLines(mpd_path)
    
    # Find first 0 FILE line (usually line 3+ after headers)
    file_line_idx <- grep("^0\\s+FILE\\s+", lines)[1]
    
    if (is.na(file_line_idx)) {
        return(NA_character_)
    }
    
    # Extract filename from that line
    line <- lines[file_line_idx]
    tokens <- unlist(strsplit(trimws(line), "\\s+"))
    
    if (length(tokens) < 3 || tokens[1] != "0" || tokens[2] != "FILE") {
        return(NA_character_)
    }
    
    # Set name is everything after "FILE"
    set_name <- paste(tokens[3:length(tokens)], collapse = " ")
    
    # Optionally strip .ldr extension
    gsub("\\.ldr$", "", set_name, ignore.case = TRUE)
}

