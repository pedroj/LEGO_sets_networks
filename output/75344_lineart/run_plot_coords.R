source("plot_lego_network.R")
.PART_OVERRIDE <- data.frame(part = "11458", category = "Technic", subtype = "Technic plate (with holes)")   # plate with pin hole on top
.PAIR_ALLOW <- c("20482|61184" = "bar", "20482|64567" = "bar")   # bar of pin 61184 / hilt ring in hollow-bar tile (checked in geometry, step 41)
plot_lego_network_coords("75344_adjacency_matrix.csv", "75344_node_coords.csv", "../../data/icons",
                         parts_meta = "rebrickable/set_parts.csv", out_prefix = "75344", set_info = "set_info.json",
                         panel_title = "Boba Fett's Starship Microfighter (links checked against geometry and building instructions)",
                         compat_csv = "../../data/lego_compat_out/compat_matrix_subtype_codes.csv")
