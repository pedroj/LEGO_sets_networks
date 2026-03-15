plot_bend_net <- function(g, coords, my_image) {
    # 3. Plot with ggraph + ggimage using local files
    # 1) Layout
    
    # 2) Node data frame with coords + attributes
    nodes <- data.frame(
        x        = coords[, 1],
        y        = coords[, 2],
        name     = V(g)$part_id,  # or V(g)$name
        img_path = V(g)$loc.image,
        degree   = degree(g)
    )
    
    # 3) Plot: ggraph uses g + manual layout; geom_image uses nodes with its own aes
    ggp<- ggraph(g, layout = "manual", x = coords[,1], y = coords[,2]) +
        geom_edge_bend0(width = 0.48, alpha = 0.6) +
        # geom_node_point(size = 0, alpha = 0) +
        geom_node_point(aes(size = degree(g)), color = "blue", alpha= 0.65) +
        # geom_edge_link(width = 0.1, alpha = 0.8) +
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
    img_array <- my_image
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
}
