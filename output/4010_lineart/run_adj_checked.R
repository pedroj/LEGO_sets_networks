K <- Sys.glob("~/.claude/skills/synced/*/lego-mpd-adjacency/kernel.R")[1]
source(K)

# 1. Drop LDraw primitives (stud, rect, box ...) so only real parts are counted
.exp <- expand_model
is_prim <- function(d) grepl("^(stug|stud|rect|box|4-4|4-8|2-4|1-4|3-8|1-8|4-1|s/)", tolower(d)) | grepl("^[0-9]+-[0-9]+", d) | grepl("^(stud|stug)", tolower(d))
expand_model <- function(...) Filter(function(p) !is_prim(p$dat), .exp(...))

# 2. The MPD defines the hull as a custom sub-model made only of raw triangles ("4010 - 2372c01.dat"),
#    which expands to no parts. Use the official LDraw part 2372c01 (full hull, no stickers) instead.
.pm <- parse_mpd
parse_mpd <- function(path) {
  s <- .pm(path)
  s <- s[!tolower(names(s)) %in% "4010 - 2372c01.dat"]
  s <- lapply(s, function(b) sub("4010 - 2372c01\\.dat$", "2372c01.dat", b, ignore.case = TRUE))
  # two 298c04 levers are missing from the MPD; add them near the mast (position only indicative: links come from the checked matrix)
  s[[1]] <- c(s[[1]], "1 15 -20 -110 0 1 0 0 0 1 0 0 0 1 298c04.dat", "1 15 20 -110 0 1 0 0 0 1 0 0 0 1 298c04.dat")
  s
}

# 3. Hull contacts. A single bounding box of the hull would touch every part inside it, so the hull
#    is joined only to the parts that REST on its upward-facing surfaces (deck top y = -4 and cockpit
#    floor y = 76 in LDU): the part's bottom face (y + height, LDraw y points down) lies within TOL of a
#    hull surface AND its footprint overlaps that surface. Surfaces come from the LDraw 2372c01 geometry
#    (hull_up_faces.csv: x/z extent and y of every upward-facing triangle). Minifig parts are excluded.
HULL_FACES <- read.csv("hull_up_faces.csv")
.dims <- dims_of
dims_of <- function(name, fam) {
  if (!is.na(name) && grepl("^Boat 25 x 10", name)) return(list(w = 214.4, d = 540.3, h = 104))
  .dims(name, fam)
}
.ba <- build_adjacency
build_adjacency <- function(parts, tol = TOL) {
  ad <- .ba(parts, tol)
  hi <- which(vapply(parts, function(p) p$dat == "2372c01", TRUE))
  if (!length(hi)) return(ad)
  for (h in hi) {
    ad$A[h, ] <- 0L; ad$A[, h] <- 0L; ad$crit[h, ] <- NA; ad$crit[, h] <- NA
    for (j in setdiff(seq_along(parts), h)) {
      p <- parts[[j]]
      if (p$fam %in% c("minifig", "tyre", "wheel")) next
      d <- p$dims; R <- p$R
      ex <- abs(R[1, 1]) * d$w / 2 + abs(R[1, 3]) * d$d / 2 - 2     # footprint, shrunk 2 LDU
      ez <- abs(R[3, 1]) * d$w / 2 + abs(R[3, 3]) * d$d / 2 - 2
      bottom <- p$y + d$h
      F <- HULL_FACES[abs(HULL_FACES$y - bottom) <= tol, ]
      if (!nrow(F)) next
      if (any(F$xmin <= p$x + ex & F$xmax >= p$x - ex & F$zmin <= p$z + ez & F$zmax >= p$z - ez)) {
        ad$A[h, j] <- ad$A[j, h] <- 1L; ad$crit[min(h, j), max(h, j)] <- "rests on hull"
      }
    }
  }
  ad
}

# 4. Use the CHECKED links (geometry + building-instruction cross-check, see check_with_instructions.py)
CHK <- as.matrix(read.csv("4010_adjacency_matrix_checked.csv", row.names = 1, check.names = FALSE))
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
res <- run_lego_adjacency("4010_-_Police_rescue_boat.mpd", "rebrickable", "4010")
