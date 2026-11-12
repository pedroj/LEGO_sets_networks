K <- Sys.glob("~/.claude/skills/synced/*/lego-mpd-adjacency/kernel.R")[1]
source(K)

# 1. Drop LDraw primitives (stud, stug, rect, box, 48/..., s/...) so only real parts are counted
.exp <- expand_model
is_prim <- function(d) { d <- gsub("\\\\", "/", tolower(d)); grepl("^(48/|s/|stug|stud|rect|box|[0-9]+-[0-9]+)", d) }

# parts dropped from the analysis (env EXCL, comma separated): here the minifig-category parts 3626b (head) and 4599a (tap)
EXCL <- strsplit(Sys.getenv("EXCL", ""), ",")[[1]]
expand_model <- function(...) Filter(function(p) !is_prim(p$dat) && !p$dat %in% EXCL, .exp(...))

# 3. Variant part numbers (LDraw 44301 / 32123a / 4287 ...) are Rebrickable's 44301a / 32123b / 4287b:
#    if the exact number has no Rebrickable name, try the neighbouring letter variants.
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

# 4. This model has parts mounted sideways (pod engines, palm leaves, cables), which the kernel's upright
#    box model cannot size. Use the true local bounding box of each part from LDraw geometry (part_bbox.csv,
#    made by make_bboxes.py), rotated with the part's full matrix; contact = boxes overlap within TOL,
#    plus the kernel's family compatibility rule.
BB <- read.csv("part_bbox.csv", stringsAsFactors = FALSE); rownames(BB) <- BB$part
build_adjacency <- function(parts, tol = TOL) {
  N <- length(parts)
  bx <- lapply(parts, function(p) {
    b <- BB[sub("^(.*)$", "\\1", p$dat), ]
    if (is.na(b$part)) b <- BB[sub("[a-z]+$", "", p$dat), ]
    cr <- as.matrix(expand.grid(c(b$xmin, b$xmax), c(b$ymin, b$ymax), c(b$zmin, b$zmax)))
    w <- t(p$R %*% t(cr)) + matrix(c(p$x, p$y, p$z), nrow(cr), 3, byrow = TRUE)
    list(lo = apply(w, 2, min), hi = apply(w, 2, max), fam = p$fam)
  })
  A <- matrix(0L, N, N); crit <- matrix(NA_character_, N, N)
  for (i in seq_len(N - 1)) for (j in (i + 1):N) {
    a <- bx[[i]]; b <- bx[[j]]
    if (!compatible(a$fam, b$fam)) next
    if (all(a$lo - tol <= b$hi & b$lo - tol <= a$hi)) { A[i, j] <- A[j, i] <- 1L; crit[i, j] <- "touch" }
  }
  list(A = A, crit = crit)
}

# the instance-level heatmap is too large (1000+ parts) for ggsave: skip failed kernel figures
ggsave <- function(...) try(ggplot2::ggsave(...), silent = TRUE)
res <- run_lego_adjacency(Sys.getenv("MPD", "4485-1.mpd"), "rebrickable", Sys.getenv("PFX", "4485"))

# real coordinates of every instance (same node names as the matrix: <part>#<n>), for the coordinate network plot
pp <- res$parts; dv <- vapply(pp, `[[`, "", "dat"); nid <- paste0(dv, "#", ave(seq_along(dv), dv, FUN = seq_along))
write.csv(data.frame(node = nid, part = dv, x = vapply(pp, `[[`, 0, "x"), y = vapply(pp, `[[`, 0, "y"),
                     z = vapply(pp, `[[`, 0, "z")), paste0(Sys.getenv("PFX", "4485"), "_instances.csv"), row.names = FALSE)
