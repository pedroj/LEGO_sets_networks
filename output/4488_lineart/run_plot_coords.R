source("plot_lego_network.R")
.PART_OVERRIDE <- data.frame(part = c("4599a", "4032a"), category = c("Brick", "Technic"),
                             subtype = c("Brick basic", "Technic plate (with holes)"))   # tap = 1x1 brick-like; round plate with axle hole takes the axle
.PAIR_ALLOW <- c("3705|4150" = "axle", "3705|4150ps0" = "axle", "3705|3961ps0" = "axle", "4592|4593" = "hinge", "4599a|4740" = "stud",
                 "30367a|4274" = "pin", "2444|43093" = "pin")   # sorted-string keys; lever on base, tap spout takes dish pin, bottom-cross tile on axle, pin 1/2 in dome
plot_lego_network_coords("4488_adjacency_matrix.csv", "4488_node_coords.csv", "../../data/icons",
                         parts_meta = "rebrickable/set_parts.csv", out_prefix = "4488", set_info = "set_info.json",
                         panel_title = "Millennium Falcon (mini) (links checked against geometry and building instructions)",
                         compat_csv = "../../data/lego_compat_out/compat_matrix_subtype_codes.csv")
