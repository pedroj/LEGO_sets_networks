source("plot_lego_network.R")
.PART_OVERRIDE <- data.frame(part = character(0), category = character(0), subtype = character(0))
.PAIR_ALLOW <- character(0)
res <- plot_lego_network("10174_adjacency_matrix.csv", "../../data/icons", set_info = "set_info.json",
                         parts_meta = "rebrickable/set_parts.csv", out_prefix = "10174", label = "part",
                         compat_csv = "../../data/lego_compat_out/compat_matrix_subtype_codes.csv")
e <- res$edges_all; print(table(e$system))
print(e[e$system == "check", ])
print(res$nodes[res$nodes$category == "Other", c("part","desc")])
print(table(res$nodes$category))
file.rename("10174_icons.pdf", "10174_network.pdf")
