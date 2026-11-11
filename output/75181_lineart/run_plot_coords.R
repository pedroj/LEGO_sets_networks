source("plot_lego_network.R")
plot_lego_network_coords("75181_adjacency_matrix.csv", "75181_node_coords.csv", "../../data/icons",
                         parts_meta = "rebrickable/set_parts.csv", out_prefix = "75181", set_info = "set_info.json",
                         panel_title = "UCS Y-wing Starfighter (links checked against geometry and building steps)",
                         compat_csv = "../../data/lego_compat_out/compat_matrix_subtype_codes.csv")
