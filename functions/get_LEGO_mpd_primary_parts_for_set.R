# Recursive MPD primary parts extraction
#
get_LEGO_mpd_primary_parts_for_set <- function(mpd_file_path) {
    require(dplyr)
    require(purrr)
    require(stringr)
    require(tibble)
    mpd_lines <- readLines(mpd_file_path)
        
        # All "1 ..." lines
        part_lines <- grep("^1\\s", mpd_lines, value = TRUE)
        
        if (length(part_lines) == 0) {
            return(tibble(part_raw = character(), part_id = character()))
        }
        
        parts_df <- map_dfr(str_split(part_lines, "\\s+"), function(tok) {
            tok <- unlist(tok)
            if (length(tok) < 15) {
                return(tibble(part_raw = character(0), color = character(0)))
            }
            
            part_raw <- paste(tok[15:length(tok)], collapse = " ")
            
            tibble(
                color    = tok[2],
                part_raw = part_raw
            )
        }) %>%
            # keep only .dat (primary parts), drop .ldr submodels
            filter(grepl("\\.dat$", tolower(part_raw)))
        
        if (nrow(parts_df) == 0) {
            return(tibble(part_raw = character(), part_id = character()))
        }
        
        parts_df %>%
            distinct(part_raw) %>%
            mutate(part_id = str_remove(part_raw, "\\.dat$")) %>%
            arrange(part_id)
    }
    