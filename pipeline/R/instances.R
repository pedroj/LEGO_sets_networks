# Step 1 (new sets): parse the MPD with the vendored kernel and write
#   <set>_instances.csv, <set>_node_coords.csv, <set>_adjacency_matrix_kernel.csv
#   and <set>_adjacency_matrix.csv (the starting matrix) ONLY if it does not exist yet.
# Wraps kernel.R with the two fixes every run_adj*.R script carried: drop LDraw primitives, and resolve
# part metadata by trying letter suffixes (3062 -> 3062a/b/c, 3062a -> 3062).
make_instances <- function(cfg, pipeline_dir) {
  source(file.path(pipeline_dir, "kernel.R"), local = FALSE)
  .exp <- expand_model
  is_prim <- function(d) { d <- gsub("\\\\", "/", tolower(d)); grepl("^(48/|s/|stug|stud|rect|box|[0-9]+-[0-9]+)", d) }
  assign("expand_model", function(...) Filter(function(p) !is_prim(p$dat), .exp(...)), envir = globalenv())
  .rm <- resolve_meta
  assign("resolve_meta", function(dat, meta) {
    m <- .rm(dat, meta)
    if (!is.na(m$name)) return(m)
    base <- sub("[a-z]+$", "", dat)
    for (c in unique(c(paste0(dat, c("a", "b", "c")), base, paste0(base, c("a", "b", "c", "d")))))
      if (exists(c, envir = meta, inherits = FALSE)) return(get(c, envir = meta, inherits = FALSE))
    m
  }, envir = globalenv())
  S <- cfg$set
  mpd <- if (!is.null(cfg$mpd)) cfg$mpd else Sys.glob(paste0(S, "*.mpd"))[1]
  stopifnot("no MPD file found in the set folder (set `mpd:` in set_config.yaml)" = !is.na(mpd) && file.exists(mpd))
  pfx <- paste0(S, "k")                                   # kernel's own outputs: <set>k_*.png/.html/.csv
  res <- run_lego_adjacency(mpd, "rebrickable", pfx)
  nm <- ave(seq_along(res$parts), vapply(res$parts, function(p) p$dat, ""), FUN = seq_along)
  inst <- do.call(rbind, lapply(seq_along(res$parts), function(i) { p <- res$parts[[i]]
    data.frame(i = i, node = paste0(p$dat, "#", nm[i]), dat = p$dat, x = p$x, y = p$y, z = p$z, col = p$colour, step = p$step,
               grp = p$group, fam = p$fam, nm = substr(p$name, 1, 40), w = p$dims$w, d = p$dims$d, h = p$dims$h,
               R = paste(round(p$R, 2), collapse = " ")) }))
  write.csv(inst, paste0(S, "_instances.csv"), row.names = FALSE)
  write.csv(data.frame(node = inst$node, part = inst$dat, x = inst$x, y = inst$y, z = inst$z),
            paste0(S, "_node_coords.csv"), row.names = FALSE, quote = FALSE)
  file.copy(paste0(pfx, "_adjacency_matrix.csv"), paste0(S, "_adjacency_matrix_kernel.csv"), overwrite = TRUE)
  start <- paste0(S, "_adjacency_matrix.csv")
  if (!file.exists(start)) file.copy(paste0(S, "_adjacency_matrix_kernel.csv"), start)
  else message(start, " already exists: kept as the starting matrix (kernel saved as _kernel.csv)")
  writeLines(unique(inst$dat), "parts_list.txt")
  invisible(inst)
}
