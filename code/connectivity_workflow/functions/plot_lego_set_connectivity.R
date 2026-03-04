## -------------------------------------------------------------------
## Function: plot_lego_set_connectivity
## -------------------------------------------------------------------
# Requires:
# - Rebrickable API key in Sys.getenv("REBRICKABLE_KEY")
# - Libraries: dplyr, purrr, httr, jsonlite, igraph, ggraph, ggimage, tibble, tidyr, stringr
# 
# A compact function that, given a LEGO set number, builds a part–part co‑occurrence network, downloads part images from Rebrickable, and plots the network with ggraph using node images. You can drop this into your R Markdown after the setup chunk in LEGO_model_connection_networks.Rmd and then call it as shown at the end.
# 
plot_lego_set_connectivity <- function(set_num,
                                       img_destdir = "images/part_images",
                                       min_quantity = 1) {
    
    # 1) Helper: get parts for a set from Rebrickable -------------------
    get_set_parts <- function(set_num) {
        api_key <- Sys.getenv("REBRICKABLE_KEY")
        if (api_key == "") {
            stop("Please define REBRICKABLE_KEY in your environment.")
        }
        
        base_url <- "https://rebrickable.com/api/v3/lego/sets/"
        url <- paste0(base_url, set_num, "/parts/?page_size=1000")
        
        res <- httr::GET(
            url,
            httr::add_headers(Authorization = paste("key", api_key))
        )
        httr::stop_for_status(res)
        
        dat   <- jsonlite::fromJSON(httr::content(res, as = "text", encoding = "UTF-8"))
        parts <- dat$results
        
        safe_chr <- function(x, nm) {
            if (is.null(x) || !is.list(x) || is.null(x[[nm]]) || length(x[[nm]]) == 0) {
                NA_character_
            } else {
                as.character(x[[nm]][1])
            }
        }
        
        safe_int_scalar <- function(v) {
            if (is.null(v) || length(v) == 0) NA_integer_ else as.integer(v[1])
        }
        
        rows <- lapply(seq_along(parts$part), function(i) {
            p <- parts$part[[i]]
            
            tibble::tibble(
                part_num   = safe_chr(p, "part_num"),
                part_name  = safe_chr(p, "name"),
                # quantity is scalar in results; fall back to 1 if missing
                quantity   = safe_int_scalar(parts$quantity[i]),
                img_url    = safe_chr(p, "part_img_url")
            )
        })
        
        df <- dplyr::bind_rows(rows)
        
        n_total      <- nrow(df)
        n_miss_pnum  <- sum(is.na(df$part_num))
        n_miss_pname <- sum(is.na(df$part_name))
        n_miss_img   <- sum(is.na(df$img_url) | df$img_url == "")
        
        message(
            "Set ", set_num, ": ",
            n_total, " part rows; ",
            n_miss_pnum, " with missing part_num; ",
            n_miss_pname, " with missing part_name; ",
            n_miss_img, " with missing img_url."
        )
        
        df
    }
    
    # 2) Get Rebrickable parts for the set ------------------------------
    parts_df <- get_set_parts(set_num)
    
    parts_df <- get_set_parts(set_num) %>%
        dplyr::filter(!is.na(part_num), quantity >= min_quantity)
    
    # Optionally filter very rare parts
#    parts_df <- parts_df %>%
#        dplyr::filter(quantity >= min_quantity)
    
    # 3) Build a simple co-occurrence network ---------------------------
    # Here, we use a "fully connected" co-occurrence: any two distinct
    # part types in the same set are connected. You can refine later to
    # use LDraw MPD info and submodels. [file:1]
    
#    part_vec <- unique(parts_df$part_num)
    
#    if (length(part_vec) < 2) {
#        stop("Not enough distinct parts to build a network.")
#    }

    part_vec <- unique(parts_df$part_num)
    
    if (length(part_vec) == 0) {
        stop("No parts available for this set after filtering.")
    } else if (length(part_vec) == 1) {
        warning("Only one distinct part; plotting a single-node network.")
        g <- igraph::make_empty_graph(n = 1, directed = FALSE)
        igraph::V(g)$name     <- part_vec
        igraph::V(g)$part_id  <- part_vec
        igraph::V(g)$quantity <- parts_df$quantity[match(part_vec, parts_df$part_num)]
        # continue to layout + plotting using g and its single node
        # (coords <- layout_with_fr(g) will just give one point)
    } else {
        comb <- t(combn(part_vec, 2))
        edges_df <- tibble::tibble(
            from = comb[, 1],
            to   = comb[, 2]
        )
        g <- igraph::graph_from_data_frame(edges_df, directed = FALSE)
    }
        
    # All unordered pairs of distinct parts
    comb <- t(combn(part_vec, 2))
    edges_df <- tibble::tibble(
        from = comb[, 1],
        to   = comb[, 2]
    )
    
    # 4) Create igraph object -------------------------------------------
    g <- igraph::graph_from_data_frame(edges_df, directed = FALSE)
    
    # Attach basic attributes from parts_df (quantity, color) -----------
    node_tbl <- tibble::tibble(part_id = igraph::V(g)$name) %>%
        dplyr::left_join(
            parts_df %>%
                dplyr::select(part_num, part_name, quantity),
            by = c("part_id" = "part_num")
        )
    
    igraph::V(g)$part_id   <- node_tbl$part_id
    igraph::V(g)$part_name <- node_tbl$part_name
    igraph::V(g)$quantity  <- node_tbl$quantity
    
    # 5) Download images and attach paths -------------------------------
    img_paths <- download_part_images(
        part_num = parts_df$part_num,
        color_id = parts_df$color_id,
        urls     = parts_df$img_url,
        destdir  = img_destdir
    )
    
    parts_df$img_path <- img_paths
    
    node_tbl <- node_tbl %>%
        dplyr::left_join(
            parts_df %>%
                dplyr::select(part_num, img_path),
            by = c("part_id" = "part_num")
        )
    
    igraph::V(g)$img_path <- node_tbl$img_path
    
    # 6) Compute layout and build node data frame for plotting ----------
    set.seed(1)
    coords <- igraph::layout_with_fr(g) * 1.5
    
    node_plot_df <- data.frame(
        x        = coords[, 1],
        y        = coords[, 2],
        name     = igraph::V(g)$part_id,
        img_path = igraph::V(g)$img_path,
        degree   = igraph::degree(g),
        quantity = igraph::V(g)$quantity
    )
    
    # 7) Plot with ggraph + ggimage -------------------------------------
    p <- ggraph::ggraph(g, layout = "manual",
                        x = coords[, 1], y = coords[, 2]) +
        ggraph::geom_edge_link(width = 0.1, alpha = 0.6) +
        ggraph::geom_node_point(size = 0, alpha = 0) +
        ggimage::geom_image(
            data = node_plot_df,
            ggplot2::aes(x = x, y = y, image = img_path, size = degree),
            inherit.aes = FALSE
        ) +
        ggraph::geom_node_text(
            ggplot2::aes(label = name),
            vjust = 1.8,
            size = 3,
            color = "black"
        ) +
        ggplot2::scale_size(range = c(0.07, 0.16)) +
        ggraph::theme_graph() +
        ggplot2::labs(
            title = paste("LEGO Parts Network (set", set_num, ")"),
            subtitle = "Node images scaled by degree (connectivity)"
        )
    
    print(p)
    
    # Return the igraph object invisibly for further analysis -----------
    invisible(g)
}

