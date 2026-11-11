source("plot_lego_network.R")
.PART_OVERRIDE <- data.frame(part = c("4211", "3788", "3136"), category = c("Brick", "Brick", "Plate"),
                             subtype = c("Brick basic", "Brick basic", "Plate jumper"))   # vehicle base, studded mudguard, tow hook with studs
.PAIR_ALLOW <- c("122c01|4084" = "wheel", "3623|3821p60" = "door", "3623|3822p60" = "door",
                 "3821p60|3823" = "door", "3822p60|3823" = "door")   # tyre on wheel plate; doors between plate below and windscreen above
res <- plot_lego_network("6628_adjacency_matrix.csv", "../../data/icons", set_info = "set_info.json",
                         parts_meta = "rebrickable/set_parts.csv", out_prefix = "6628", label = "part",
                         compat_csv = "../../data/lego_compat_out/compat_matrix_subtype_codes.csv")
e <- res$edges_all; print(table(e$system))
print(e[e$system == "check", ])
print(res$nodes[res$nodes$category == "Other", c("part","desc")])
print(table(res$nodes$category))
file.rename("6628_icons.pdf", "6628_network.pdf")
