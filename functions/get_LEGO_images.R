# Use BrickLink/Rebrickable to map your V(g)$name (assume Design IDs like "3001") to images, then integrate in ggraph.
# Fetch Set 4489 Parts (R Code)
# Download unique parts data:
library(jsonlite)  # For API
library(here)
library(dplyr)
library(tidyr)
library(purrr)
library(httr)
library(ggimage)

# NOT RUN ------------------------------------------------------------------------------
# Rebrickable API (free key at rebrickable.com/api/): replace 'YOUR_KEY'
# parts_json <- fromJSON("https://rebrickable.com/api/v3/lego/sets/21319-1/parts/?key=YOUR_KEY&includecolor=yes")
# top20_parts <- parts_json$results %>%
#    count(part_id, color_id, sort = TRUE) %>%
#    head(20)  # Top 20 unique parts
#    parts_4489.1_json <- fromJSON("https://rebrickable.com/api/v3/lego/sets/4489-1/parts/?key=40c6692458e8b4c27074486ab114d6cc&includecolor=yes")
#    set_4489_parts<- parts_4489.1_json$results$part
# NOT RUN ------------------------------------------------------------------------------

# CSV download with function get_set_parts.R: 
# https://cdn.rebrickable.com/media/downloads/sets.csv.zip?
api_key <- "40c6692458e8b4c27074486ab114d6cc"
source(here::here("./functions/get_set_parts.R"))

# Dataframe with parts list for a set.
parts_4489 <- get_set_parts("4489-1", api_key)
head(parts_4489)
parts_4489 <- parts_4489 %>% 
    # dplyr::mutate(
    #     part_num_trim = sub("\\.dat$", "", part_num)  %>% 
    dplyr::mutate(
        partcol_code = paste0(part_num, "_", color_id))

#parts_4489new <- parts_4489 %>%
#    dplyr::filter(part_num_trim %in% V(g)$part_id)
#
# Assume V(g)$name are part_ids like "3001"; map 20 nodes
V(g)$part_name<- unique(parts_4489$part_name)
V(g)$part_id <- sub("\\.dat$", "", V(g)$name) #V(g)$name  # Or your mapping
V(g)$color_id <- parts_4489$color_id  # Black example; get from parts_json$results$color_id
V(g)$image <- paste0("https://img.bricklink.com/ItemImage/P/", V(g)$part_id, #V(g)$color_id, 
                     ".png")
# Example for color 0 (White): https://img.bricklink.com/ItemImage/P/3001/0.png


# 1. Download and cache images locally
# Assume you already have a parts data.frame like parts_4489 from get_set_parts():
# parts_4489 columns: part_num, part_name, color_id, color_name, img_url, quantity
download_part_images <- function(part_num, color_id, urls, destdir = "images/part_images/") {
    dir.create(destdir, showWarnings = FALSE)
    filenames <- paste0(part_num, "_", color_id, ".png")
    paths <- file.path(destdir, filenames)
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

parts_4489$img_path <- download_part_images(
    part_num = parts_4489new$part_num,
    color_id = parts_4489$color_id,
    urls     = parts_4489$img_url,
    destdir  = "images/part_images/"
)

# parts_4489$img_path <- download_part_images(parts_4489$img_url, destdir = "images")

#Check part names include .dat
#parts_4489 <- parts_4489 %>%
#    dplyr::mutate(part_num2= paste0(part_num,".dat"))

# download.file(V(g)$image, destdir = "part_imgs/")
V(g)$loc.image <- paste0("images/part_images/", V(g)$part_id,"_", V(g)$color_id,".png")


# 3. Plot with ggraph + ggimage using local files
# 1) Layout
coords <- layout_with_fr(g) * 1.5

# 2) Node data frame with coords + attributes
nodes <- data.frame(
    x        = coords[, 1],
    y        = coords[, 2],
    name     = V(g)$name,
    img_path = V(g)$loc.image,
    degree   = degree(g)
)

# 3) Plot: ggraph uses g + manual layout; geom_image uses nodes with its own aes
ggraph(g, layout = "manual", x = coords[,1], y = coords[,2]) +
    geom_edge_link(width = 0.1, alpha = 0.8) +
    geom_node_point(size = 0, alpha = 0) +  # invisible base nodes
    geom_image(
        data = nodes,
        aes(x = x, y = y, image = img_path),
        size = 0.13,                 # fixed, reasonable size
        inherit.aes = FALSE
    ) +
    geom_node_text(aes(label = name),
                   vjust = 1.8,
                   size = 3,
                   color = "black") +
    theme_graph() +
    labs(title = "LEGO Parts Network (Images)")

# Key points:
#    nodes must contain columns literally named x and y.

# In geom_image(...) we set data = nodes and aes(x = x, y = y, image = img_path, size = degree) so all needed aesthetics exist in that data.

# inherit.aes = FALSE stops it from looking for x/y in the ggraph layout environment, so there is no ambiguity.

# size is relative to the plot; adjust (e.g. 0.07–0.12) to fit ~20 nodes.

# 4. Optional: safer filename scheme
# To avoid clashes when two URLs share the same basename:

download_part_images <- function(part_num, color_id, urls, destdir = "part_imgs") {
    dir.create(destdir, showWarnings = FALSE)
    filenames <- paste0(part_num, "_", color_id, ".png")
    paths <- file.path(destdir, filenames)
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

parts_4489$img_path <- download_part_images(
    part_num = parts_4489$part_num,
    color_id = parts_4489$color_id,
    urls     = parts_4489$img_url,
    destdir  = "images/part_images"
)

# That keeps our cache stable and unique per part/color.

# Tune size = 0.12 for visibility with 20 nodes.
# Quick R Mapping for Your 20 Nodes
# Assume V(g)$name = c("3024", "3001", ...)  # Your 20 Design IDs
# V(g)$part_id <- V(g)$name
# V(g)$color_id <- c(7, 0, 7, 1, ...)  # From table or API
# V(g)$image <- paste0("https://img.bricklink.com/ItemImage/P/", 
                     # V(g)$part_id, "/", V(g)$color_id, ".png")


