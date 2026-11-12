source("plot_lego_network.R")
cc <- "../../data/lego_compat_out/compat_matrix_subtype_codes.csv"
for (k in list(c("sebulba", "Sebulba's Podracer"), c("anakin", "Anakin's Podracer"))) {
  pre <- paste0("4485_", k[1])
  res <- plot_lego_network(paste0(pre, "_adjacency_matrix.csv"), "../../data/icons", parts_meta = "rebrickable/set_parts.csv",
                           out_prefix = pre, label = "part", compat_csv = cc, title = k[2])
  cat(k[2], ": check edges\n"); print(res$edges_all[res$edges_all$system == "check", c("from","to","contacts_proximity")])
}
