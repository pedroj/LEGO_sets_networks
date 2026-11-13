K <- Sys.glob("~/.claude/skills/synced/*/lego-mpd-adjacency/kernel.R")[1]
source(K)
.exp <- expand_model
is_prim <- function(d) { d <- gsub("\\\\", "/", tolower(d)); grepl("^(48/|s/|stug|stud|rect|box|[0-9]+-[0-9]+)", d) }
expand_model <- function(...) Filter(function(p) !is_prim(p$dat), .exp(...))
.rm <- resolve_meta
resolve_meta <- function(dat, meta) {
  m <- .rm(dat, meta)
  if (!is.na(m$name)) return(m)
  base <- sub("[a-z]+$", "", dat)
  for (c in unique(c(paste0(dat, c("a", "b", "c")), base, paste0(base, c("a", "b", "c", "d"))))) {
    if (exists(c, envir = meta, inherits = FALSE)) return(get(c, envir = meta, inherits = FALSE))
  }
  m
}
res <- run_lego_adjacency("4488-1.mpd", "rebrickable", Sys.getenv("PFX", "4488k"))
nm <- ave(seq_along(res$parts), vapply(res$parts, function(p) p$dat, ""), FUN = seq_along)
inst <- do.call(rbind, lapply(seq_along(res$parts), function(i) { p <- res$parts[[i]]
  data.frame(i = i, node = paste0(p$dat, "#", nm[i]), dat = p$dat, x = p$x, y = p$y, z = p$z, col = p$colour, step = p$step,
             grp = p$group, fam = p$fam, nm = substr(p$name, 1, 40), w = p$dims$w, d = p$dims$d, h = p$dims$h,
             R = paste(round(p$R, 2), collapse = " ")) }))
write.csv(inst, "4488_instances.csv", row.names = FALSE)
