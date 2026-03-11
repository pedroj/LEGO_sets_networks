library(plotly)

# tt must contain: set_num, set_name, H
p <- ggplot(head(stw_H,100), aes(x = reorder(set_num, H), y = H)) +
    geom_col(
        aes(text = paste0(
            "Set: ", set_num, "\n",
            "Name: ", set_name, "\n",
            "Shannon H: ", round(H, 4)
        )),
        fill = "steelblue"
    ) +
    coord_flip() +
    labs(
        x = "Set number",
        y = "Shannon diversity H",
        title = "Shannon diversity across 50 Star Wars sets"
    ) +
    theme_minimal()

ggplotly(p, tooltip = "text")
