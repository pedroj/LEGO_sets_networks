# Kernel run on the 75344 MPD (instance level), plus instance dump for the link check
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
# The first FILE block ("... (Slave I).mpd") is an empty header; the parts are in the second block, which must be the root.
.pm <- parse_mpd
parse_mpd <- function(path) {
  s <- .pm(path)
  has <- vapply(s, function(b) any(grepl("^1 ", b)), TRUE)
  s[order(!has)]
}
ggsave <- function(...) try(ggplot2::ggsave(...), silent = TRUE)

# Use the CHECKED instance-level links (geometry + instruction cross-check, see check_with_instructions.py)
CHK <- as.matrix(read.csv("75344_adjacency_matrix_checked.csv", row.names = 1, check.names = FALSE))
.ba2 <- build_adjacency
build_adjacency <- function(parts, tol = TOL) {
  ad <- .ba2(parts, tol)
  nm <- ave(seq_along(parts), vapply(parts, function(p) p$dat, ""), FUN = seq_along)
  nm <- paste0(vapply(parts, function(p) p$dat, ""), "#", nm)
  stopifnot(all(nm %in% rownames(CHK)))
  ad$A[,] <- 0L; ad$crit[,] <- NA
  ad$A[,] <- CHK[nm, nm]
  ad$crit[ad$A == 1] <- "checked"
  ad
}
res <- run_lego_adjacency("75344-1.mpd", "rebrickable", "75344")
