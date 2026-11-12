source("plot_lego_network.R")
res <- plot_lego_network("4485_adjacency_matrix.csv","../../data/icons", set_info="set_info.json",
  parts_meta="rebrickable/set_parts.csv", out_prefix="4485", label="part",
  compat_csv="../../data/lego_compat_out/compat_matrix_subtype_codes.csv")
e <- res$edges_all; print(table(e$system))
nm <- setNames(res$nodes$desc,res$nodes$part)
ck <- e[e$system=="check",]; ck <- ck[order(-ck$contacts_proximity),]
ck$a <- substr(nm[ck$from],1,26); ck$b <- substr(nm[ck$to],1,26)
cat("\nCHECK (top 30):\n"); print(head(ck[,c("from","a","to","b","contacts_proximity")],30), row.names=FALSE)
cat("\nOTHER:\n"); print(res$nodes[res$nodes$category=="Other", c("part","desc","instances")])
print(table(res$nodes$category))
file.rename("4485_icons.pdf","4485_network.pdf")
