get_LEGO_part_images <- function(my_set="4489-1",api_key="40c6692458e8b4c27074486ab114d6cc") {
  # Use BrickLink/Rebrickable to download image files fpr LEGO parts. Then map the V(g)$name (assume Design IDs like "3001") to images, then integrate in ggraph.
  # Fetch Set 7131 Parts (R Code)
  # Download unique parts data:
    require(jsonlite)  # For API
    require(here)
    require(dplyr)
    require(tidyr)
    require(purrr)
    require(httr)
    require(ggimage)
  # NOT RUN ------------------------------------------------------------------------------
  # Rebrickable API (free key at rebrickable.com/api/): replace 'YOUR_KEY'
  # parts_json <- fromJSON("https://rebrickable.com/api/v3/lego/sets/21319-1/parts/?key=YOUR_KEY&includecolor=yes")
  # top20_parts <- parts_json$results %>%
  #    count(part_id, color_id, sort = TRUE) %>%
  #    head(20)  # Top 20 unique parts
  #    parts_7131.1_json <- fromJSON("https://rebrickable.com/api/v3/lego/sets/7131-1/parts/?key=40c6692458e8b4c27074486ab114d6cc&includecolor=yes")
  #    set_7131_parts<- parts_7131.1_json$results$part
  # NOT RUN ------------------------------------------------------------------------------
  #
  ## 1. Download and cache images locally
  # Assume we already have a parts `data.frame` like `parts_4489` from function 
  # `get_set_parts()`:
  #     `parts_4489` columns: `part_num`, `part_name`, 
  #     `color_id`, `color_name`, `img_url`, `quantity`
  
  # {r dowload_part_images, echo=TRUE}
  # CSV download with function get_set_parts.R: 
  # https://cdn.rebrickable.com/media/downloads/sets.csv.zip?
 # api_key <- "40c6692458e8b4c27074486ab114d6cc"
  source(here::here("functions/get_set_parts.R"))
  
  # Dataframe with parts list for a set.
  my_parts <- get_set_parts(my_set, api_key)
  head(my_parts)
  # my_parts <- my_parts %>%
  #     dplyr::mutate(
  #         part_num_trim = sub("\\.dat$", "", part_num)
  #     )
  # my_partsnew <- my_parts %>%
  #     dplyr::filter(part_num_trim %in% V(g)$part_id)
  #
  download_part_images <- function(urls, destdir = "images/downloaded_images") {
      dir.create(destdir, showWarnings = FALSE)
      paths <- file.path(destdir, basename(urls))
      mapply(
          function(u, p) {
              if (!file.exists(p) && !is.na(u) && nzchar(u)) {
                  try(download.file(u, p, mode = "wb"), silent = TRUE)
              }
          },
          urls, paths
      )
      
      paths
  }
  
  my_parts$img_local_path <- download_part_images(my_parts$img_url, 
                                              destdir = "images/part_images")
#  my_parts
  cat("Downloaded", nrow(my_parts), "parts for set", my_set, "\n")
  head(my_parts)
  
# Subsetting the parts file to just unique part types (no color or duplications).
#  parts_4489_unique <- parts_4489 %>%
#      distinct(part_num, .keep_all = TRUE)
  
  # NO parts_4489_unique$part_num en V(g)$part_id: 44567,30000,4592,4593,4489 - Head.ldr,
  # exclude_parts <- c("44567", "30000", "4592", "4593", "4489 - Head.ldr")
  
  # parts_4489_filtered <- parts_4489_unique %>%
  #     dplyr::filter(!part_num %in% exclude_parts)
  
  # Basename(urls) keeps names simple; we can choose more informative filenames if we 
  # want (e.g., paste `part_num` and `color_id`).
  #
  # After this step, `parts_4489_filtered$img_path` points to local PNG files.
  
  #
  # Assume V(g)$name are part_ids like "3001"; map the nodes. 
  # V(g)$part_id <- sub("\\.dat$", "", V(g)$name) #V(g)$name  # Or your mapping
  # V(g)$color_id <- 7  # Black example; get from parts_json$results$color_id
  # V(g)$image <- paste0("https://img.bricklink.com/ItemImage/P/", V(g)$part_id, V(g)$color_id, ".png")
  return(my_parts)
}

