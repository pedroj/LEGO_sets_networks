# install.packages(c("httr", "jsonlite", "purrr", "fs"))

library(httr)
library(jsonlite)
library(purrr)
library(fs)

# 1. Your Brickset credentials --------------------------------------------
api_key   <- "3-T2mN-wEiE-Vt0ek"
username  <- "PedroJ"
password  <- "5oPNETadOY"
# Brickset API: 3-T2mN-wEiE-Vt0ek

# 2. Authenticate and get a session token ---------------------------------
auth_resp <- POST(
    url = "https://brickset.com/api/v3.asmx",
    body = list(
        apiKey   = api_key,
        username = username,
        password = password
    ),
    encode = "json"
)

stop_for_status(auth_resp)

auth_json <- content(auth_resp, as = "text", encoding = "UTF-8") |>
    fromJSON()

session_id <- auth_json$hash
session_id

get_instructions_for_set <- function(set_number) {
    resp <- POST(
        url = "https://brickset.com/api/v3.asmx/getInstructions",
        body = list(
            apiKey    = api_key,
            userHash  = session_id,
            setNumber = set_number
        ),
        encode = "json"
    )
    stop_for_status(resp)
    
    dat <- content(resp, as = "text", encoding = "UTF-8") |>
        fromJSON()
    
    # Brickset typically returns a list with elements like "URL" and "description"
    if (is.null(dat$instructions) || length(dat$instructions) == 0) return(NULL)
    
    tibble::tibble(
        set_number  = set_number,
        url         = vapply(dat$instructions, `[[`, "", "URL"),
        description = vapply(dat$instructions, `[[`, "", "description")
    )
}

#####
# Bulk query and download:
    
# 3. Define the sets you want
set_numbers <- c("21309-1", "21310-1", "10179-1")  # example set numbers

# 4. Get instruction URLs for all sets
instr_tbl <- map_dfr(set_numbers, safely(get_instructions_for_set)) |>
    purrr::transpose() |>
    (\(x) x$result)() |>
    compact() |>
    list_rbind()

instr_tbl

# 5. Download PDFs ---------------------------------------------------------
out_dir <- "lego_instructions"
dir_create(out_dir)

download_instruction <- function(set_number, url, description) {
    # Try to infer file extension (default pdf)
    ext <- tools::file_ext(url)
    if (ext == "") ext <- "pdf"
    
    safe_desc <- gsub("[^A-Za-z0-9_\\-]", "_", description)
    fname <- sprintf("%s_%s.%s", set_number, safe_desc, ext)
    dest  <- path(out_dir, fname)
    
    message("Downloading ", url, " -> ", dest)
    
    try(
        download.file(url, destfile = dest, mode = "wb", quiet = TRUE),
        silent = TRUE
    )
    
    dest
}

pwalk(
    instr_tbl,
    ~ download_instruction(..1, ..2, ..3)
)

# This will create a lego_instructions/ folder and save all available PDFs for your set list with reasonably informative filenames.


# Notes and variants
# Brickset’s API documentation (methods like getInstructions) explains the exact JSON structure; adjust the field names above if their schema changes.


# If you already have a large CSV of sets (e.g. exported from Brickset), just feed the set_numbers vector from that file and reuse the same functions.

# If you wanted to bypass Brickset entirely and scrape LEGO’s own “building instructions” pages, you’d need to reverse‑engineer their search URLs and PDF links, respect robots.txt, and throttle requests; conceptually similar code in R with rvest + download.file would work, but is less stable than the documented Brickset API.

