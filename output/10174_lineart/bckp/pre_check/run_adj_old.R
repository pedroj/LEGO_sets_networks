K <- Sys.glob("~/.claude/skills/synced/*/lego-mpd-adjacency/kernel.R")[1]
source(K)
PFX <- "10174 - "

# 1. Drop LDraw primitives (stud, stug, rect, box, 48/..., s/...) so only real parts are counted
.exp <- expand_model
is_prim <- function(d) { d <- gsub("\\\\", "/", tolower(d)); grepl("^(48/|s/|stug|stud|rect|box|[0-9]+-[0-9]+)", d) }

# 2. Parts defined INSIDE the MPD as custom ".dat" sub-files made of raw geometry (here "10174 - 30658.dat",
#    the turntable top) would expand to nothing. Use the official LDraw part (same number without the prefix).
#    The three flex hoses (sub-files flexHose2-2/3/4, ~150 tiny segments 750/754/755/756 each) are collapsed
#    into ONE hose part per hose, 73590c02a (Rebrickable inventory: 3 x 73590c02a). The segments are kept in
#    HOSE_SEGS so that the hose's contacts are still tested segment by segment (see build_adjacency below).
HOSE_FILES <- c("10174 - flexHose2-2.ldr", "10174 - flexHose2-3.ldr", "10174 - flexHose2-4.ldr")
HOSE_SEGS <- list()
.pm <- parse_mpd
parse_mpd <- function(path) {
  s <- .pm(path)
  cust <- names(s)[grepl("\\.dat$", names(s), ignore.case = TRUE) & startsWith(tolower(names(s)), tolower(PFX))]
  s <- s[!names(s) %in% cust]
  for (h in HOSE_FILES) {
    id <- sub("^.*flexHose", "", sub("\\.ldr$", "", h))
    HOSE_SEGS[[id]] <<- Filter(Negate(is.null), lapply(s[[h]], parse_type1))
    s[[h]] <- sprintf("1 16 0 0 0 1 0 0 0 1 0 0 0 1 73590c02a@%s.dat", id)
  }
  lapply(s, function(b) {
    for (c in cust) b <- sub(paste0(" ", c, "$"), paste0(" ", sub(PFX, "", c, fixed = TRUE)), b, ignore.case = TRUE)
    b
  })
}
expand_model <- function(...) {
  ps <- Filter(function(p) !is_prim(p$dat), .exp(...))
  lapply(ps, function(p) {
    if (grepl("^73590c02a@", p$dat)) { p$hose <- sub("^.*@", "", p$dat); p$dat <- "73590c02a" }
    p
  })
}

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

# 4. A hose touches whatever any of its segments touches: its row/column is rebuilt from the segment boxes
.ba <- build_adjacency
build_adjacency <- function(parts, tol = TOL) {
  ad <- .ba(parts, tol)
  hi <- which(vapply(parts, function(p) !is.null(p$hose), TRUE))
  for (h in hi) {
    P <- parts[[h]]; segs <- HOSE_SEGS[[P$hose]]
    sp <- lapply(segs, function(s) list(dat = sub("\\.dat$", "", s$file), x = NA, y = NA, z = NA,
                                         R = P$R %*% s$R, fam = "other", dims = list(w = 20, d = 20, h = 8),
                                         .t = as.numeric(P$R %*% s$t + c(P$x, P$y, P$z))))
    ad$A[h, ] <- 0L; ad$A[, h] <- 0L; ad$crit[h, ] <- NA; ad$crit[, h] <- NA
    for (j in setdiff(seq_along(parts), hi)) {
      for (s in sp) {
        s$x <- s$.t[1]; s$y <- s$.t[2]; s$z <- s$.t[3]
        a2 <- .ba(list(s, parts[[j]]), tol)
        if (a2$A[1, 2] == 1L) { ad$A[h, j] <- ad$A[j, h] <- 1L; ad$crit[min(h, j), max(h, j)] <- a2$crit[1, 2]; break }
      }
    }
  }
  ad
}

# the instance-level heatmap is too large (1000+ parts) for ggsave: skip failed kernel figures
ggsave <- function(...) try(ggplot2::ggsave(...), silent = TRUE)
res <- run_lego_adjacency("10174-1.mpd", "rebrickable", "10174")
