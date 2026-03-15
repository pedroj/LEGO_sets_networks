# Download parts images by specifying sets or set groups.
library(httr)
library(jsonlite)
library(purrr)
library(dplyr)
library(tibble)
library(fs)

api_key  <- Sys.getenv("REBRICKABLE_KEY")
base_url <- "https://rebrickable.com/api/v3/lego"

auth_hdr <- function() add_headers(
    Authorization = paste("key", api_key),
    Accept        = "application/json"
)
#
# Location of downloaded images
img_root <- here::here("images/rebrickable_set_images")
fs::dir_create(img_root)

lego_get_paged <- function(url, query = list()) {
    auth <- paste("key", api_key)
    res  <- GET(url, query = query,
                add_headers(Authorization = auth, Accept = "application/json"))
    stop_for_status(res)
    dat <- content(res, as = "text", encoding = "UTF-8")
    dat <- fromJSON(dat, simplifyVector = FALSE)
    
    out      <- dat$results
    next_url <- dat$`next`
    
    while (!is.null(next_url) && nzchar(next_url)) {
        Sys.sleep(1)
        res  <- GET(next_url,
                    add_headers(Authorization = auth, Accept = "application/json"))
        stop_for_status(res)
        dat <- content(res, as = "text", encoding = "UTF-8")
        dat <- fromJSON(dat, simplifyVector = FALSE)
        out <- c(out, dat$results)
        next_url <- dat$`next`
    }
    out
}


download_set_part_images <- function(set_id) {
    message("Processing set: ", set_id)
    
    url <- paste0(base_url, "/sets/", set_id, "/parts/")
    parts_list <- lego_get_paged(url, query = list(page_size = 1000))
    
    if (length(parts_list) == 0L) {
        warning("No parts returned for set ", set_id)
        # return an empty tibble with expected columns
        return(tibble(
            part_num     = character(),
            color_id     = integer(),
            quantity     = integer(),
            is_spare     = logical(),
            part_img_url = character()
        ))
    }
    
    parts_df <- map_dfr(parts_list, function(x) {
        tibble(
            part_num     = x$part$part_num,
            color_id     = x$color$id,
            quantity     = x$quantity,
            is_spare     = x$is_spare,
            part_img_url = x$part$part_img_url %||% NA_character_
        )
    })
    
    # ensure the column exists even if all entries were NULL
    if (!"part_img_url" %in% names(parts_df)) {
        parts_df$part_img_url <- NA_character_
    }
    
    parts_df <- parts_df[
        !is.na(parts_df$part_img_url) & parts_df$part_img_url != "",
    ]
    
    parts_df <- parts_df[
        !duplicated(parts_df[c("part_num", "color_id", "part_img_url")]),
    ]
    
    if (nrow(parts_df) == 0L) {
        warning("No images for parts in set ", set_id)
        # return empty tibble instead of NULL
        return(tibble(
            part_num     = character(),
            color_id     = integer(),
            quantity     = integer(),
            is_spare     = logical(),
            part_img_url = character()
        ))
    }
    
    set_dir <- fs::path(img_root, set_id)
    dir_create(set_dir)
    
    pwalk(
        .l = parts_df,
        .f = function(part_num, color_id, quantity, is_spare, part_img_url) {
            ext <- tolower(fs::path_ext(part_img_url))
            if (ext == "") ext <- "jpg"
            fname <- sprintf("%s_c%s.%s", part_num, color_id, ext)
            dest  <- fs::path(set_dir, fname)
            
            if (file_exists(dest)) return(invisible(NULL))
            
            Sys.sleep(0.2)
            
            try({
                resp <- GET(part_img_url)
                if (status_code(resp) == 200) {
                    writeBin(content(resp, "raw"), dest)
                } else {
                    warning("Failed image: ", part_img_url,
                            " status: ", status_code(resp))
                }
            }, silent = TRUE)
        }
    )
    
    csv_path <- fs::path(set_dir, paste0(set_id, "_parts.csv"))
    readr::write_csv(parts_df, csv_path)
    
    invisible(parts_df)
}

## Example usage
# set_ids <- c("21310-1")
# results <- lapply(set_ids, download_set_part_images)
set_ids <- unique(Starwars$set_num)
results1 <- lapply(set_ids[1:100], download_set_part_images)
results2 <- lapply(set_ids[101:200], download_set_part_images)
results3 <- lapply(set_ids[201:300], download_set_part_images)
results4 <- lapply(set_ids[301:400], download_set_part_images)
results5 <- lapply(set_ids[401:500], download_set_part_images)
results6 <- lapply(set_ids[501:600], download_set_part_images)
results7 <- lapply(set_ids[601:700], download_set_part_images)
results8 <- lapply(set_ids[701:791], download_set_part_images)
