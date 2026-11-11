K <- Sys.glob("~/.claude/skills/synced/*/lego-mpd-adjacency/kernel.R")[1]
source(K)

# 1. Drop LDraw primitives (stud, stug, rect, box, 48/..., s/...) so only real parts are counted
.exp <- expand_model
is_prim <- function(d) { d <- gsub("\\\\", "/", tolower(d)); grepl("^(48/|s/|stug|stud|rect|box|[0-9]+-[0-9]+)", d) }

# 2. Parts defined INSIDE the MPD as custom ".dat" sub-files made of raw geometry (here "10174 - 30658.dat",
#    the turntable top) would expand to nothing. Use the official LDraw part (same number without the prefix).
#    The three flex hoses (sub-files flexHose2-2/3/4, ~150 tiny segments 750/754/755/756 each) are collapsed
#    into ONE hose part per hose, 73590c02a (Rebrickable inventory: 3 x 73590c02a). The segments are kept in
#    HOSE_SEGS so that the hose's contacts are still tested segment by segment (see build_adjacency below).
PFX <- "75181 - Y-Wing Starfighter - "
.pm <- parse_mpd
parse_mpd <- function(path) {
  s <- .pm(path)
  cust <- names(s)[grepl("\\.dat$", names(s), ignore.case = TRUE) & startsWith(tolower(names(s)), tolower(PFX))]
  s <- s[!names(s) %in% cust]
  main <- grep("main\\.ldr$", names(s))[1]
  # the display stand is not part of the model: drop its reference from the main file
  s[[main]] <- s[[main]][!grepl("stand\\.ldr\\s*$", s[[main]], ignore.case = TRUE)]
  lapply(s, function(b) {
    for (c in cust) b <- sub(paste0(" ", c, "$"), paste0(" ", sub(PFX, "", c, fixed = TRUE)), b, ignore.case = TRUE)
    b
  })
}
expand_model <- function(...) Filter(function(p) !is_prim(p$dat), .exp(...))

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

# the instance-level heatmap is too large (1000+ parts) for ggsave: skip failed kernel figures
ggsave <- function(...) try(ggplot2::ggsave(...), silent = TRUE)
res <- run_lego_adjacency("75181-1.mpd", "rebrickable", Sys.getenv("PFX", "75181k"))

nm <- ave(seq_along(res$parts), vapply(res$parts, function(p) p$dat, ""), FUN = seq_along)
inst <- do.call(rbind, lapply(seq_along(res$parts), function(i) { p <- res$parts[[i]]
  data.frame(i = i, node = paste0(p$dat, "#", nm[i]), dat = p$dat, x = p$x, y = p$y, z = p$z, col = p$colour, step = p$step,
             grp = p$group, fam = p$fam, nm = substr(p$name, 1, 40), w = p$dims$w, d = p$dims$d, h = p$dims$h,
             R = paste(round(p$R, 2), collapse = " ")) }))
write.csv(inst, "75181_instances.csv", row.names = FALSE)
