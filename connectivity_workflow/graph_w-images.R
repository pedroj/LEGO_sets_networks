# Below is an end‑to‑end pattern tailored to a Rebrickable parts table and LEGO‑network workflow.

# 1. Download and cache images locally
# Assume you already have a parts data.frame like parts_4489 from get_set_parts():
# parts_4489 columns: part_num, part_name, color_id, color_name, img_url, quantity

download_part_images <- function(part_num, color_id, urls, destdir = "images/part_images") {
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

parts_4489$img_path <- download_part_images(parts_4489$img_url, destdir = "images")

#Check part names include .dat
parts_4489 <- parts_4489 %>%
    dplyr::mutate(part_num2= paste0(part_num,".dat"))

# basename(urls) keeps names simple; we can choose more informative filenames if we want 
# (e.g., paste part_num and color_id).

# After this step, parts_4489$img_path points to local PNG files.

# 2. Attach image paths to igraph nodes
# Assume:
#    The igraph object is g.
# Each node name equals a part_num (e.g., "3001", "3024").
# Ensure node names are part_nums
# V(g)$name already something like "3001", "3024", ...

# Join local image paths from parts table to nodes
node_df <- data.frame(
    name = V(g)$name,
    stringsAsFactors = FALSE
)

# Now each node has V(g)$img_path pointing to a local file.

# 3. Plot with ggraph + ggimage using local files

set.seed(1)
coords <- layout_with_fr(g) * 1.5

nodes <- data.frame(
    x        = coords[,1],
    y        = coords[,2],
    name     = V(g)$name,
    img_path = V(g)$img_path,
    degree   = igraph::degree(g),
    quantity = V(g)$quantity
)

ggraph(g, layout = "manual", x = coords[,1], y = coords[,2]) +
    geom_edge_link(width = 0.1, alpha = 0.8) +
    geom_node_point(size = 0, alpha = 0) +  # invisible base nodes
    geom_image(
        data = node_df,
        aes(x = x, y = y, image = img_path, size = degree),  # or quantity
        inherit.aes = FALSE
    ) +
    geom_node_text(aes(label = name),
                   vjust = 1.8,
                   size = 3,
                   color = "black") +
    scale_size(range = c(0.06, 0.16)) +  # min/max image size
    theme_graph() +
    labs(title = "LEGO Parts Network (Images scaled by degree)")

# Key points:
# In geom_image(), we can pass the vector directly or construct a tibble with node 
# attributes and map aes(image = img_path).

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
    destdir  = "part_imgs"
)

# That keeps our cache stable and unique per part/color.


