# Below is an end‑to‑end pattern tailored to a Rebrickable parts table and LEGO‑network workflow.

# 1. Download and cache images locally
# Assume you already have a parts data.frame like parts_4489 from get_set_parts():
# parts_4489 columns: part_num, part_name, color_id, color_name, img_url, quantity

download_part_images <- function(urls, destdir = "images/part_images") {
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

parts_4489$img_path <- download_part_images(parts_4489$img_url, 
                                            destdir = "images/part_images")

# Subsetting the parts file to just unique part types (no color or duplications).
parts_4489_unique <- parts_4489 %>%
    distinct(part_num, .keep_all = TRUE)

# NO parts_4489_unique$part_num en V(g)$part_id: 44567,30000,4592,4593,4489 - Head.ldr,
exclude_parts <- c("44567", "30000", "4592", "4593", "4489 - Head.ldr")

parts_4489_filtered <- parts_4489_unique %>%
    dplyr::filter(!part_num %in% exclude_parts)

# basename(urls) keeps names simple; we can choose more informative filenames if we want 
# (e.g., paste part_num and color_id).

# After this step, parts_4489$img_path points to local PNG files.

# 2. Attach image paths to igraph nodes
# Assume:
#    The igraph object is g.
# Each node name equals a part_num (e.g., "3001", "3024").
# Ensure node names are part_nums
# V(g)$name already something like "3001", "3024", ...
# Assume V(g)$name are part_ids like "3001"; map 20 nodes
V(g)$part_id <- sub("\\.dat$", "", V(g)$name) #V(g)$name  # Or your mapping
V(g)$color_id <- 7# parts_4489new$color_id  # Black example; get from parts_json$results$color_id
V(g)$image <-parts_4489_filtered$img_url


# Vertex table
nodes <- tibble::tibble(part_id = V(g)$part_id)

nodes <- nodes %>%
    left_join(
        parts_4489_filtered %>%
            select(part_num, img_url = img_url),
        by = c("part_id" = "part_num")
    )

stopifnot(nrow(nodes) == vcount(g))

V(g)$img_url <- nodes$img_url

# 2) Compute layout and node data frame

set.seed(1)
coords <- layout_with_fr(g) * 1.5

nodes_plot <- data.frame(
    x       = coords[,1],
    y       = coords[,2],
    name    = V(g)$part_id,  # or V(g)$name
    img_url = V(g)$img_url
)

# 3) Test plot with Rebrickable URLs

ggraph(g, layout = "manual", x = coords[,1], y = coords[,2]) +
    geom_edge_link(width = 0.1, alpha = 0.8) +
    geom_node_point(size = 0, alpha = 0) +
    geom_image(
        data = nodes_plot,
        aes(x = x, y = y, image = img_url),
        size = 0.10,
        inherit.aes = FALSE
    ) +
    geom_node_text(aes(label = name),
                   vjust = 1.8,
                   size = 3,
                   color = "black") +
    theme_graph() +
    labs(title = "LEGO Parts Network (Rebrickable Images)")


##################################################################

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


