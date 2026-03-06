get_LEGO_set_parts <- function(set_num= "4489-1", api_key= "40c6692458e8b4c27074486ab114d6cc") {
    url <- paste0(
        "https://rebrickable.com/api/v3/lego/sets/",
        set_num,
        "/parts/?page=1&page_size=1000"
    )
    resp <- GET(
        url,
        add_headers(Authorization = paste("key", api_key))
    )
    stop_for_status(resp)
    
    dat <- content(resp, as = "text", encoding = "UTF-8") |>
        fromJSON()
    parts <- dat$results %>%
        unnest_wider(part,  names_sep = "_") %>%
        unnest_wider(color, names_sep = "_") %>%
        dplyr::select(
            part_num   = part_part_num,
            part_name  = part_name,
            color_id   = color_id,
            color_name = color_name,
            img_url    = part_part_img_url,
            quantity
        )
   # if (!is.null(color_id_filter)) {
   #     parts <- parts %>% filter(color_id %in% color_id_filter)
   #}
    parts
}