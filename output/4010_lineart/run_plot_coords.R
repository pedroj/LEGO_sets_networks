# Coordinate-based network (real top-view positions). NOTE: the two 298c04 levers are not in the MPD; their position is indicative only.
source("plot_lego_network.R")
plot_lego_network_coords("4010_adjacency_matrix.csv", "4010_node_coords.csv", "../../data/icons",
                         parts_meta = "rebrickable/set_parts.csv", out_prefix = "4010", set_info = "set_info.json",
                         panel_title = "Police Rescue Boat (links checked against building instructions)",
                         compat_csv = "../../data/lego_compat_out/compat_matrix_subtype_codes.csv")
