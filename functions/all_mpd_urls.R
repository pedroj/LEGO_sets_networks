# ============================================================
# Download all LDraw OMR Star Wars .mpd files
# Source: https://library.ldraw.org/omr/sets
# Confirmed: 67 Star Wars sets as of March 2026
# ============================================================

# Install if needed:
# install.packages(c("httr", "rvest", "xml2"))

library(httr)
library(rvest)
library(xml2)

base_url  <- "https://library.ldraw.org"
out_dir   <- "ldraw_starwars_mpd"
dir.create(out_dir, showWarnings = FALSE)

# ----------------------------------------------------------
# 1. All 67 Star Wars OMR internal set IDs
#    (scraped from /omr/sets?search=Star+Wars, 100 per page)
# ----------------------------------------------------------
set_ids <- c(
    873, 125, 126, 127, 128, 129, 130, 131, 132, 133,
    998, 997,  56,  57,  58,  59, 516, 517, 518, 519,
    520, 521, 522, 523, 524, 342, 525, 526, 527, 528,
    108, 754, 109, 110, 111, 112, 113, 114, 115, 116,
    117, 118, 341, 343, 996, 894, 895, 896, 897,1205,
    898, 899,1116,1213,1212,1211, 762, 119,1214, 937,
    939, 938, 120, 121, 122, 123, 999
)

# ----------------------------------------------------------
# 2. Scrape download links from each set page
# ----------------------------------------------------------
get_mpd_links <- function(set_id) {
    url  <- paste0(base_url, "/omr/sets/", set_id)
    page <- tryCatch(read_html(url), error = function(e) NULL)
    if (is.null(page)) {
        message("  [WARN] Could not read set page: ", set_id)
        return(character(0))
    }
    # All <a> elements whose href ends in .mpd
    links <- page |>
        html_elements("a[href$='.mpd']") |>
        html_attr("href")
    paste0(links)
}

# ----------------------------------------------------------
# 3. Download each .mpd file
# ----------------------------------------------------------
download_mpd <- function(url, dest_dir) {
    filename <- basename(url)
    dest     <- file.path(dest_dir, filename)
    if (file.exists(dest)) {
        message("  [SKIP] Already exists: ", filename)
        return(invisible(dest))
    }
    resp <- tryCatch(
        GET(url, write_disk(dest, overwrite = TRUE),
            timeout(60), progress()),
        error = function(e) {
            message("  [ERROR] ", filename, ": ", e$message)
            return(NULL)
        }
    )
    if (!is.null(resp) && status_code(resp) == 200) {
        sz <- file.info(dest)$size
        message(sprintf("  [OK] %-45s  %.1f KB", filename, sz / 1024))
    } else {
        file.remove(dest)   # remove incomplete file
        message("  [FAIL] HTTP ", status_code(resp), "  ", basename(url))
    }
    invisible(dest)
}

# ----------------------------------------------------------
# 4. Main loop
# ----------------------------------------------------------
all_mpd_urls <- character(0)

message("=== Collecting download links for ", length(set_ids), " sets ===")
for (id in set_ids) {
    Sys.sleep(0.5)           # polite delay between page reads
    links <- get_mpd_links(id)
    message("Set ", id, ": ", length(links), " model(s) found")
    all_mpd_urls <- c(all_mpd_urls, links)
}

message("\n=== Total .mpd files to download: ", length(all_mpd_urls), " ===\n")

for (url in all_mpd_urls) {
    download_mpd(url, out_dir)
    Sys.sleep(1)             # polite 1 s delay between downloads
}

# ----------------------------------------------------------
# 5. Summary
# ----------------------------------------------------------
downloaded <- list.files(out_dir, pattern = "\\.mpd$", full.names = TRUE)
total_mb   <- sum(file.info(downloaded)$size, na.rm = TRUE) / 1024^2

message("\n=== Done ===")
message("Files downloaded : ", length(downloaded))
message(sprintf("Total size       : %.2f MB", total_mb))
message("Saved to         : ", normalizePath(out_dir))

