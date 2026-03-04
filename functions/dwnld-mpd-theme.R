# ================================================================
# LDraw OMR – Download all .mpd files for a chosen theme
# FIXED: avoids SPA list page entirely; scrapes SSR set detail pages
# ================================================================

# install.packages(c("httr", "rvest"))
library(httr)
library(rvest)

# ----------------------------------------------------------------
# USER CONFIG
# ----------------------------------------------------------------
theme_query <- "Technic"           # partial match, case-insensitive
out_dir     <- "ldraw_technic_mpd" # output folder
id_range    <- 1:1400              # OMR set IDs to scan (sequential ints)
# ----------------------------------------------------------------

base_url <- "https://library.ldraw.org"
dir.create(out_dir, showWarnings = FALSE)

# ----------------------------------------------------------------
# Helper: scrape one set page → returns list(theme, mpd_urls)
# Returns NULL if 404 or theme doesn't match
# ----------------------------------------------------------------
scrape_set <- function(set_id, theme_filter) {
    url  <- paste0(base_url, "/omr/sets/", set_id)
    resp <- tryCatch(GET(url, timeout(20)), error = function(e) NULL)
    
    # Skip if request failed or page not found
    if (is.null(resp) || status_code(resp) != 200) return(NULL)
    
    html <- tryCatch(
        content(resp, as = "text", encoding = "UTF-8") |> read_html(),
        error = function(e) NULL
    )
    if (is.null(html)) return(NULL)
    
    # Page title is e.g. "LDraw.org Official Model Repository - Display Team Jet"
    # Theme is embedded in the page as plain text (SSR)
    # Look for theme text – it appears as a link with text like "Technic"
    page_text <- html |> html_text2()
    
    # Check theme match (partial, case-insensitive)
    if (!grepl(theme_filter, page_text, ignore.case = TRUE)) return(NULL)
    
    # Extract all .mpd download links
    mpd_links <- html |>
        html_elements("a[href$='.mpd']") |>
        html_attr("href")
    
    if (length(mpd_links) == 0) return(NULL)
    
    list(
        set_id    = set_id,
        page_text = substr(page_text, 1, 80),
        mpd_urls  = paste0(base_url, mpd_links)
    )
}

# ----------------------------------------------------------------
# Helper: download one .mpd with skip/resume logic
# ----------------------------------------------------------------
download_mpd <- function(url, dest_dir) {
    fname <- basename(url)
    dest  <- file.path(dest_dir, fname)
    
    if (file.exists(dest) && file.info(dest)$size > 500) {
        message("  [SKIP] ", fname)
        return(invisible(dest))
    }
    
    resp <- tryCatch(
        GET(url, write_disk(dest, overwrite = TRUE), timeout(120)),
        error = function(e) {
            message("  [ERROR] ", fname, ": ", e$message)
            NULL
        }
    )
    
    if (!is.null(resp) && status_code(resp) == 200) {
        kb <- file.info(dest)$size / 1024
        message(sprintf("  [OK]   %-50s  %6.1f KB", fname, kb))
    } else {
        if (file.exists(dest)) file.remove(dest)  # remove partial file
        code <- if (!is.null(resp)) status_code(resp) else "???"
        message("  [FAIL] HTTP ", code, "  ", fname)
    }
    invisible(dest)
}

# ----------------------------------------------------------------
# Phase 1: Scan set IDs, collect matching sets + their .mpd URLs
# ----------------------------------------------------------------
message("=== Phase 1: Scanning set pages for theme '",
        theme_query, "' ===")
message("    ID range: ", min(id_range), " – ", max(id_range), "\n")

all_mpd_urls  <- character(0)
matched_sets  <- 0
consecutive_404 <- 0          # stop early if IDs run out

for (id in id_range) {
    result <- scrape_set(id, theme_query)
    
    if (is.null(result)) {
        # Distinguish 404 (end of IDs) from theme mismatch
        url  <- paste0(base_url, "/omr/sets/", id)
        resp <- tryCatch(GET(url, timeout(10)), error = function(e) NULL)
        code <- if (!is.null(resp)) status_code(resp) else 0
        
        if (code == 404) {
            consecutive_404 <- consecutive_404 + 1
            if (consecutive_404 >= 20) {
                message("  20 consecutive 404s – assuming end of IDs. Stopping scan.")
                break
            }
        } else {
            consecutive_404 <- 0   # reset on theme mismatch (200 but wrong theme)
        }
    } else {
        consecutive_404 <- 0
        matched_sets    <- matched_sets + 1
        n_files         <- length(result$mpd_urls)
        message(sprintf("  [%4d] Set ID %4d: %d file(s)  |  %s",
                        matched_sets, id, n_files,
                        substr(result$page_text, 1, 60)))
        all_mpd_urls <- c(all_mpd_urls, result$mpd_urls)
    }
    
    Sys.sleep(0.4)   # polite delay
}

message("\n  Matched sets : ", matched_sets)
message("  Total .mpd   : ", length(all_mpd_urls))

# ----------------------------------------------------------------
# Phase 2: Download all collected .mpd files
# ----------------------------------------------------------------
message("\n=== Phase 2: Downloading ", length(all_mpd_urls),
        " files to '", out_dir, "/' ===\n")

for (url in all_mpd_urls) {
    download_mpd(url, out_dir)
    Sys.sleep(0.8)
}

# ----------------------------------------------------------------
# Summary
# ----------------------------------------------------------------
done     <- list.files(out_dir, pattern = "\\.mpd$", full.names = TRUE)
total_mb <- sum(file.info(done)$size, na.rm = TRUE) / 1024^2

message("\n========== DONE ==========")
message("Theme          : ", theme_query)
message("Sets matched   : ", matched_sets)
message("Files saved    : ", length(done))
message(sprintf("Total size     : %.2f MB", total_mb))
message("Output folder  : ", normalizePath(out_dir))
