# Get a list with the LEGO set names for Starwars group
# To get the list of LEGO Star Wars set names programmatically, the clean way is to query the Rebrickable v3 API for all sets where theme_id corresponds to “Star Wars”, then extract their names.
# 
# Below is end‑to‑end R code that does this.
# Explanation:
#     
# /lego/themes/ gives all themes; we grep for “Star Wars” to find its theme id rather than hard‑coding it.
# 
# /lego/sets/?theme_id=... returns all sets for that theme, with name and set_num
# 
# The code optionally pulls subthemes whose parent_id is the Star Wars theme, so you get all Star Wars sets, not just the top‑level theme.
# 
library(httr)
library(jsonlite)
library(dplyr)
library(purrr)
library(tibble)
#
# api_key  <- Sys.getenv("REBRICKABLE_API_KEY")  # or "your_key_here"
api_key <- "40c6692458e8b4c27074486ab114d6cc"
base_url <- "https://rebrickable.com/api/v3/lego"

# Helper to GET with auth and follow pagination
lego_get_paged <- function(path, query = list()) {
    auth <- paste("key", api_key)
    url  <- paste0(base_url, path)
    
    res  <- GET(url, query = query,
                add_headers(Authorization = auth, Accept = "application/json"))
    stop_for_status(res)
    dat <- content(res, as = "text", encoding = "UTF-8")
    dat <- fromJSON(dat, simplifyVector = FALSE)
    
    out      <- dat$results
    next_url <- dat$`next`
    
    while (!is.null(next_url) && nzchar(next_url)) {
        Sys.sleep(0.5)
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

# 1) Find the Star Wars theme_id
themes_raw <- lego_get_paged("/themes/", query = list(page_size = 1000))
themes_df  <- map_dfr(themes_raw, ~tibble(
    id        = .x$id,
    name      = .x$name,
    parent_id = .x$parent_id
))

starwars_themes <- themes_df %>%
    dplyr::filter(grepl("Star Wars", name, ignore.case = TRUE))

starwars_themes
# Pick the main "Star Wars" row (usually id 18, but we don’t hard‑code it) [web:27]

main_sw_id <- starwars_themes %>%
#    dplyr::arrange(id) %>%           # crude preference for the base theme
    dplyr::slice(1) %>%
    dplyr::pull(id)

main_sw_id

# 2) Get all sets in (sub)themes under Star Wars
# Option A: just theme_id == main_sw_id
sets_sw_main <- lego_get_paged(
    "/sets/",
    query = list(theme_id = main_sw_id, page_size = 1000)
)

sets_sw_main_df <- map_dfr(sets_sw_main, ~tibble(
    set_num = .x$set_num,
    name    = .x$name,
    year    = .x$year,
    theme_id = .x$theme_id
))

# If you also want all sets from Star Wars subthemes, collect their ids:
sw_theme_ids <- themes_df %>%
    dplyr::filter(parent_id == main_sw_id | id == main_sw_id) %>%
    dplyr::pull(id)

# Fetch sets for each of those theme_ids and bind
sets_sw_all <- map(sw_theme_ids, function(tid) {
    lego_get_paged("/sets/", query = list(theme_id = tid, page_size = 1000))
})

sets_sw_all_df <- map_dfr(sets_sw_all, function(lst) {
    map_dfr(lst, ~tibble(
        set_num = .x$set_num,
        name    = .x$name,
        year    = .x$year,
        theme_id = .x$theme_id
    ))
}) %>%
    dplyr::distinct(set_num, .keep_all = TRUE) %>%
    dplyr::arrange(year, set_num)

# 3) Vector of set Ids (set_num), or keep the tibble
starwars_set_num <- sets_sw_all_df$set_num
head(starwars_set_names, 20)

###################################################################################