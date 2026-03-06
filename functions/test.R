## 1. Download and cache images locally
# Assume we already have a parts `data.frame` like `parts_4489` from function 
# `get_set_parts()`:
# `parts_4489` columns: `part_num`, `part_name`, `color_id`, `color_name`, 
# `img_url`, `quantity`

# CSV download with function get_set_parts.R: 
# https://cdn.rebrickable.com/media/downloads/sets.csv.zip?
api_key <- "40c6692458e8b4c27074486ab114d6cc"
source(here::here("./code/connectivity_workflow/functions/get_set_parts.R"))

target_set<- c("4489-1")

# Dataframe with parts list for a set.
target_set_parts <- get_set_parts(target_set, api_key)
head(target_set_parts) # part_num contains the parts numbers.

# parts_4489 <- parts_4489 %>%
#     dplyr::mutate(
#         part_num_trim = sub("\\.dat$", "", part_num)
#     )
# parts_4489new <- parts_4489 %>%
#     dplyr::filter(part_num_trim %in% V(g)$part_id)
#
source(here::here("functions/get_LEGO_part_images_for_set.R"))

# download_part_images <- function(urls, destdir = "images/part_images") {
#     dir.create(destdir, showWarnings = FALSE)
#     paths <- file.path(destdir, basename(urls))
#     
#     mapply(
#         function(u, p) {
#             if (!file.exists(p) && !is.na(u) && nzchar(u)) {
#                 try(download.file(u, p, mode = "wb"), silent = TRUE)
#             }
#         },
#         urls, paths
#     )
#     
#     paths
# }
parts_target <- get_LEGO_part_images_for_set(target_set)
parts_target <- parts_target %>%
    distinct(part_num, .keep_all = TRUE)

# NO parts_4489_unique$part_num en V(g)$part_id: 44567,30000,4592,4593,4489 - Head.ldr,
# exclude_parts <- c("44567", "30000", "4592", "4593", "4489 - Head.ldr")
# parts_4489_filtered <- parts_4489_unique %>%
#    dplyr::filter(!part_num %in% exclude_parts)

# Basename(urls) keeps names simple; we can choose more informative filenames if we want 
# (e.g., paste `part_num` and `color_id`).
# After this step, `parts_4489_filtered$img_path` points to local PNG files.

## 2. Attach image paths to igraph nodes and rebuild the network

# Assume:
#     The igraph object is g.
# Each node name equals a part_num (e.g., "3001", "3024").
# Ensure node names are part_nums
# V(g)$name already something - like "3001", "3024", ...
# Assume V(g)$name are part_ids like "3001"; map 20 nodes

# This sets url of part images to the nodes identified by their part number.

V(g)$part_id <- sub("\\.dat$", "", V(g)$name) #V(g)$name  # Or your mapping
V(g)$color_id <- 7         # parts_4489new$color_id  
# Light_grey example; get from parts_json$results$color_id
# V(g)$image <-parts_4489_filtered$img_url

# 1) Build node table from graph vertices
nodes <- tibble::tibble(part_id = V(g)$part_id)

# 2) Left-join image URLs from filtered parts by part number
nodes <- nodes %>%
    left_join(
        parts_target %>%
            dplyr::select(part_num, img_local_path),
        by = c("part_id" = "part_num")
    )

# 3) Check alignment
nrow(nodes)          # should be 31
vcount(g)            # should also be 31

# 4) Assign to graph
V(g)$img_url <- nodes$img_local_path


## 3. Compute layout and node data frame and plot with Rebrickable URLs, using ggraph + ggimage using local files

# Now let's each node having a `V(g)$img_path` pointing to a local file.

### Key points:
# In `geom_image()`, we can pass the vector directly or construct a tibble with node attributes and map `aes(image = img_path)`.

# `size` is relative to the plot; adjust (e.g. 0.07–0.12) to fit ~20 nodes.

# Also, I use a safer filename scheme. To avoid clashes when two URLs share the same basename:
# download.file(V(g)$image, destdir = "part_imgs/")
V(g)$loc.image <- paste0("images/part_images/", V(g)$part_id, ".png")

# 3. Plot with ggraph + ggimage using local files
# 1) Layout
# coords <- layout_with_fr(g) * 1.5
diag_coords_OK<- coords

# 2) Node data frame with coords + attributes
nodes <- data.frame(
    x        = diag_coords_OK[, 1],
    y        = diag_coords_OK[, 2],
    name     = V(g)$part_id,  # or V(g)$name
    img_path = V(g)$loc.image,
    degree   = degree(g)
)

# 3) Plot: ggraph uses g + manual layout; geom_image uses nodes with its own aes
ggp<- ggraph(g, layout = "manual", x = diag_coords_OK[,1], y = diag_coords_OK[,2]) +
  geom_edge_bend0(width = 0.38, alpha = 0.8) +
# geom_node_point(size = 0, alpha = 0) +
  geom_node_point(aes(size = degree(g)), color = "blue", alpha= 0.65) +
  geom_edge_link(width = 0.1, alpha = 0.8) +
  geom_image(
        data = nodes,
        aes(x = x, y = y, image = img_path), use_cache=FALSE,
        size = 0.05,                 # fixed, reasonable size
        inherit.aes = FALSE
    ) +
#  geom_node_text(aes(label = name), repel = TRUE) +
  geom_node_text(aes(label = name),
                   vjust = 4.25,
                   size = 5,
                   color = "black") +
    theme_graph() +
    labs(title = "LEGO Parts Network (4489-1 AT-AT-Mini)")

# readPNG function to load an image that we have stored on our computer:
img_array <- readPNG("images/4489-1-AT-AT-Mini.png", native = TRUE)
my_image_grob <- rasterGrob(img_array, interpolate = TRUE)

# If still see overlap, shrink further:
# decrease width: increase left to 0.1 or decrease right to 0.3
# decrease height: increase bottom to 0.7
ggp_image <- ggp +
  inset_element(
    my_image_grob,
    left   = 0.15,  # 15% from left
    right  = 0.15,  # 65% from left -> 30% width
    bottom = 0.90,  # 35% from bottom
    top    = 0.6,   # 65% from bottom -> 30% height
    align_to = "full",
    clip     = F
  )

ggp_image           # Draw combined plot

# Key points:
#    nodes must contain columns literally named x and y.

# In geom_image(...) we set data = nodes and aes(x = x, y = y, image = img_path, size = degree) so all needed aesthetics exist in that data.

# inherit.aes = FALSE stops it from looking for x/y in the ggraph layout environment, so there is no ambiguity.

# size is relative to the plot; adjust (e.g. 0.07–0.12) to fit ~20 nodes.
# Using ggraph (Recommended)
# ggraph extends ggplot2 for networks and works directly with igraph.

## Session Info

session_info()
