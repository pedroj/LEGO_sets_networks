Sys.setlocale("LC_ALL", "en_US.UTF-8")
source("plot_lego_network.R")
res <- plot_lego_network("4010_adjacency_matrix.csv", "../../data/icons", set_info = "set_info.json",
                         parts_meta = "rebrickable/set_parts.csv", out_prefix = "4010", label = "part",
                         compat_csv = "../../data/lego_compat_out/compat_matrix_subtype_codes.csv")
print(res$edges_all[res$edges_all$system == "check", ])
print(res$nodes[res$nodes$category == "Other", c("part","desc")])
print(table(res$nodes$category))
