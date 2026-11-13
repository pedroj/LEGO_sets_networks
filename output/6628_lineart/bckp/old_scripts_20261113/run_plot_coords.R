source("plot_lego_network.R")
.PART_OVERRIDE <- data.frame(part = c("4211", "3788", "3136"), category = c("Brick", "Brick", "Plate"),
                             subtype = c("Brick basic", "Brick basic", "Plate jumper"))   # vehicle base, studded mudguard, tow hook with studs
.PAIR_ALLOW <- c("122c01|4084" = "wheel", "3623|3821p60" = "door", "3623|3822p60" = "door",
                 "3821p60|3823" = "door", "3822p60|3823" = "door")   # tyre on wheel plate; doors between plate below and windscreen above
plot_lego_network_coords("6628_adjacency_matrix.csv", "6628_node_coords.csv", "../../data/icons",
                         parts_meta = "rebrickable/set_parts.csv", out_prefix = "6628", set_info = "set_info.json",
                         panel_title = "Shell Tow Truck (links checked against geometry and building instructions)",
                         compat_csv = "../../data/lego_compat_out/compat_matrix_subtype_codes.csv")
