source("plot_lego_network.R")
.PART_OVERRIDE <- data.frame(part = character(0), category = character(0), subtype = character(0))
.PAIR_ALLOW <- character(0)
plot_lego_network_coords("10174_adjacency_matrix.csv", "10174_node_coords.csv", "../../data/icons",
                         parts_meta = "rebrickable/set_parts.csv", out_prefix = "10174", set_info = "set_info.json",
                         panel_title = "Imperial AT-ST (links checked against geometry and building instructions)",
                         compat_csv = "../../data/lego_compat_out/compat_matrix_subtype_codes.csv")
