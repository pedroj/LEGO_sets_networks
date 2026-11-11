source("plot_lego_network.R")
res <- plot_lego_network("10174_adjacency_matrix.csv","icons", set_info="set_info.json",
  parts_meta="rebrickable/set_parts.csv", out_prefix="10174", label="part",
  compat_csv="../../data/lego_compat_out/compat_matrix_subtype_codes.csv")
e <- res$edges_all; print(table(e$system)); 
cat("\nCHECK:\n"); ck<-e[e$system=="check",]; ck<-ck[order(-ck$contacts_proximity),]; nm<-setNames(res$nodes$desc,res$nodes$part); ck$a<-substr(nm[ck$from],1,28); ck$b<-substr(nm[ck$to],1,28); print(head(ck[,c("from","a","to","b","contacts_proximity")],45), row.names=FALSE)
cat("\nOTHER:\n"); print(res$nodes[res$nodes$category=="Other", c("part","desc","instances")])
print(table(res$nodes$category))
file.rename("10174_icons.pdf","10174_network.pdf")
