library(rvest)
library(purrr)
library(stringr)
library(utils)

theme_to_url <- function(theme) {
    base <- "https://library.ldraw.org/omr/sets?search="
    paste0(base, URLencode(theme, reserved = TRUE))
}

get_set_codes_page <- function(url) {
    page <- read_html(url)
    
    tab <- html_element(page, "table")
    if (is.na(tab)) return(character(0))
    
    tab |>
        html_elements("tr td:nth-child(2) a") |>
        html_text2()
}

get_theme_set_codes <- function(theme, max_pages = 50) {
    base_url <- theme_to_url(theme)
    
    urls <- c(
        base_url,
        paste0(base_url, "&page=", 2:max_pages)
    )
    
    codes_list <- map(urls, get_set_codes_page)
    lens <- lengths(codes_list)
    
    if (any(lens == 0)) {
        last_full <- which(lens == 0)[1] - 1
        codes_list <- codes_list[seq_len(last_full)]
    }
    
    unique(unlist(codes_list))
}

technic_codes <- get_theme_set_codes("Technic")
head(technic_codes)


technic_numbers <- str_match(technic_codes, "^(\\d+)")[, 2]
technic_numbers <- unique(technic_numbers[!is.na(technic_numbers)])
