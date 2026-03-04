stopifnot(dir.exists("../../data")
stopifnot(exists("lego_parts"))
save(lego_parts, file="../../data/lego_parts.rda")
