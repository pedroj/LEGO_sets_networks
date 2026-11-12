library(data.table)
T <- "/Users/pedro/Research/~RCode/MyRCode/LEGO/rebrickable-sqlite/tables"
p <- fread(file.path(T,"parts.csv"),header=FALSE,col.names=c("part_num","name","part_cat_id","material")); cat <- fread(file.path(T,"part_categories.csv"),header=FALSE)
names(cat) <- c("part_cat_id","part_category")
m <- merge(p, cat, by="part_cat_id", all.x=TRUE)
fwrite(m[, .(part_num, part_name=name, part_category)], "rebrickable/set_parts.csv")
cat(nrow(m), "parts\n")
