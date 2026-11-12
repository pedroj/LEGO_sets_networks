# Recursive function to extract ALL primary part types (.dat only) from MPD tree
# Returns tibble with unique part_raw across entire hierarchy
# Usage
# primary_parts_all <- get_LEGO_all_primary_parts_for_set(mpd_lines, files_df)
# cat("Total unique primary parts:", nrow(primary_parts_all), "\n")
# print(primary_parts_all$part_raw)
#
get_LEGO_all_primary_parts_for_set <- function(mpd_path) {
    library(dplyr)
    library(purrr)
    library(stringr)
    library(tibble)
    
    mpd_lines <- readLines(mpd_path)
    
    # 1) Detect FILE blocks in MPD
    file_idx   <- grep("^0\\s+FILE\\s+", mpd_lines)
    if (length(file_idx) == 0) {
        # Single-model .ldr/.dat case: treat whole file as one block
        file_idx   <- 1L
        file_names <- basename(mpd_path)
    } else {
        file_names <- sub("^0\\s+FILE\\s+", "", mpd_lines[file_idx])
    }
    file_start <- file_idx
    file_end   <- c(file_idx[-1] - 1, length(mpd_lines))
    
    files_df <- tibble(
        file_start = file_start,
        file_end   = file_end,
        file_name  = file_names
    )
    
    # 2) Recursive extraction of primary parts (.dat)
    all_parts <- tibble()
    
    process_file_block <- function(start_idx, end_idx, file_name, depth = 0) {
        block_lines <- mpd_lines[start_idx:end_idx]
        
        # Grab all "1 ..." lines
        part_lines <- block_lines[grepl("^1\\s", block_lines)]
        if (length(part_lines) > 0) {
            tokens_list <- str_split(part_lines, "\\s+")
            
            block_parts <- map_dfr(tokens_list, function(tok) {
                tok <- unlist(tok)
                # For your examples: length(tok) == 15
                # tok[1] = "1"
                # tok[2] = color, tok[3:5] = x y z, tok[6:14] = a..i, tok[15] = part
                if (length(tok) < 15) return(tibble(part_raw = character(0)))
                
                part_raw <- tok[15]  # single token like "2540.dat"
                
                tibble(
                    file_name  = file_name,
                    part_raw   = part_raw,
                    color_code = tok[2],
                    x          = as.numeric(tok[3]),
                    y          = as.numeric(tok[4]),
                    z          = as.numeric(tok[5]),
                    depth      = depth
                )
            }) %>%
                filter(grepl("\\.dat$", part_raw, ignore.case = TRUE))
            
            if (nrow(block_parts) > 0) {
                all_parts <<- bind_rows(all_parts, block_parts)
            }
        }
        
        # Recurse into nested FILE blocks
        nested_files <- files_df %>%
            filter(file_start > start_idx, file_end <= end_idx)
        
        if (nrow(nested_files) > 0) {
            walk(seq_len(nrow(nested_files)), ~{
                process_file_block(
                    nested_files$file_start[.x],
                    nested_files$file_end[.x],
                    nested_files$file_name[.x],
                    depth + 1
                )
            })
        }
    }
    
    # Start at first FILE (or whole file if no FILE meta)
    process_file_block(files_df$file_start[1], files_df$file_end[1], files_df$file_name[1])
    
    # Unique primary part types
    all_parts %>%
        distinct(part_raw) %>%
        mutate(part_id = str_remove(part_raw, "\\.dat$")) %>%
        arrange(part_id)
}
