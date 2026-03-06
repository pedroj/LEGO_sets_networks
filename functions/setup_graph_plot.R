setup_graph_plot <- function(target_set) {
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
}
