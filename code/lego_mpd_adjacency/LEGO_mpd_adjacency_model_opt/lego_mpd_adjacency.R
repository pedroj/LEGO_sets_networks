# =============================================================================
# lego_mpd_adjacency.R  —  general-purpose part adjacency matrix for ANY LEGO
#                          model, straight from its LDraw .mpd/.ldr file.
#
# Generalises towtruck_adjacency.R: instead of a hand-coded table of part
# dimensions / roles, every part's bounding box and connector FAMILY are
# derived from the Rebrickable catalogue (name + category). Two part
# instances are connected (1) when they are BOTH:
#     (a) physically PROXIMATE   — 3-D bounding boxes touch, stud-stack, or
#                                  (wheel/tyre) hubs are within range, AND
#     (b) connector-COMPATIBLE   — their families can actually mate
#                                  (stud system stacks; tyre↔wheel; etc.).
#
# Usage:
#   Rscript lego_mpd_adjacency.R <model.mpd> [rebrickable_dir] [out_prefix]
#     <model.mpd>        path to the LDraw MPD/LDR file           (required)
#     [rebrickable_dir]  folder holding set_parts.csv for names/categories
#                        (optional; without it parts fall back to family
#                         "other" and default dimensions)
#     [out_prefix]       prefix for the 4 output files (default = model stem)
#
# Or interactively:  source("lego_mpd_adjacency.R");
#                    run_lego_adjacency("600-1.mpd", "…/data/rebrickable")
#
# Outputs (out_prefix = e.g. "6600-1"):
#   <prefix>_adjacency_matrix.csv   symmetric 0/1 part-instance matrix
#   <prefix>_edges.csv              edge list (+ names, families, criterion)
#   <prefix>_adjacency_matrix.png   heatmap, instances grouped by family
#   <prefix>_network.png            ggrepel network, node colour = family
#
# Dependencies: igraph, ggplot2, ggrepel, reshape2, data.table
# =============================================================================

suppressPackageStartupMessages({
  library(igraph); library(ggplot2); library(ggrepel)
  library(reshape2); library(data.table)
})

# ----------------------------------------------------------------------------
# Tunables (LDU = LDraw Units; 1 stud = 20 LDU wide, 1 brick = 24 LDU tall)
# ----------------------------------------------------------------------------
STUD        <- 20     # LDU per stud (horizontal footprint pitch)
PLATE_H     <- 8      # plate / tile height
BRICK_H     <- 24     # brick height
TOL         <- 6      # tolerance (LDU) for "boxes touch"
STACK_DY    <- 24     # max vertical gap counted as a stud-stack (one brick)
HUB_D       <- 60     # tyre-to-wheel-hub max centre distance

# =============================================================================
# 1. PARSE THE MPD / LDR INTO NAMED SUB-ASSEMBLIES
# =============================================================================
# LDraw MPD files bundle several sub-files, each opened by "0 FILE <name>".
# A type-1 line is a placed reference:
#   1 <colour> <x> <y> <z> <a b c d e f g h i> <file>
# where <file> is either a real part ("*.dat") or another sub-file ("*.ldr").
parse_mpd <- function(path) {
  split_blocks <- function(lines) {
    subfiles <- list(); cur <- NULL; buf <- character(0)
    flush <- function() if (!is.null(cur)) subfiles[[cur]] <<- buf
    for (ln in lines) {
      s <- trimws(ln)
      if (startsWith(s, "0 FILE ")) { flush(); cur <- trimws(sub("^0 FILE ", "", s)); buf <- character(0) }
      else if (s == "0 NOFILE")     { flush(); cur <- NULL; buf <- character(0) }
      else if (!is.null(cur))       buf <- c(buf, s)
    }
    flush(); subfiles
  }

  lines    <- readLines(path, warn = FALSE)
  subfiles <- split_blocks(lines)
  # Single-model file with no "0 FILE" header → treat the whole thing as one model.
  if (length(subfiles) == 0) subfiles[["__root__"]] <- trimws(lines)

  # Studio/BrickLink split exports: external sibling .ldr/.mpd referenced but
  # not defined inline are loaded from the model's directory (case-insensitive)
  # and merged in, repeating until no new externals appear.
  base_dir  <- dirname(path)
  disk      <- list.files(base_dir)
  disk_lc   <- setNames(disk, tolower(disk))
  key       <- function(s) tolower(trimws(s))
  repeat {
    have <- key(names(subfiles))
    refs <- character(0)
    for (blk in subfiles) for (ln in blk) {
      p <- parse_type1(ln); if (is.null(p)) next
      if (grepl("\\.(ldr|mpd)$", p$file, ignore.case = TRUE)) refs <- c(refs, p$file)
    }
    refs <- unique(refs[!key(refs) %in% have])
    added <- FALSE
    for (rf in refs) {
      hit <- if (key(rf) %in% names(disk_lc)) disk_lc[[key(rf)]] else NULL
      if (is.null(hit)) next
      ext <- split_blocks(readLines(file.path(base_dir, hit), warn = FALSE))
      for (nm in names(ext)) if (!key(nm) %in% key(names(subfiles))) {
        subfiles[[nm]] <- ext[[nm]]; added <- TRUE
      }
    }
    if (!added) break
  }
  subfiles
}

# Parse one type-1 line into a record (NULL if not a type-1 line).
parse_type1 <- function(ln) {
  t <- strsplit(trimws(ln), "\\s+")[[1]]
  if (length(t) < 15 || t[1] != "1") return(NULL)
  list(colour = suppressWarnings(as.integer(t[2])),
       t = as.numeric(t[3:5]),                       # translation (x,y,z)
       R = matrix(as.numeric(t[6:14]), 3, 3, byrow = TRUE),  # rotation
       file = paste(t[15:length(t)], collapse = " ")) # ref filename
}

# =============================================================================
# 2. RECURSIVELY EXPAND NESTED SUB-FILES → FLAT LIST OF PLACED REAL PARTS
# =============================================================================
# Composes transforms down the tree so every real .dat part gets GLOBAL
# coordinates. Also records the build STEP index (per leaf sub-file) in case
# it is useful downstream. Guards against reference cycles.
expand_model <- function(subfiles, root = NULL, R = diag(3), t = c(0,0,0),
                         step0 = 0L, seen = character(0),
                         group = NULL, depth = 0L) {
  key <- function(s) tolower(trimws(s))
  names_lc <- setNames(names(subfiles), key(names(subfiles)))
  if (is.null(root)) root <- names(subfiles)[1]      # first FILE = top model
  rk <- key(root)
  if (!rk %in% names(names_lc) || rk %in% seen) return(list())
  seen <- c(seen, rk)
  out <- list(); step <- step0
  for (ln in subfiles[[ names_lc[[rk]] ]]) {
    if (grepl("^0 STEP", ln)) { step <- step + 1L; next }
    p <- parse_type1(ln); if (is.null(p)) next
    gt <- as.numeric(R %*% p$t + t)                  # global translation
    gR <- R %*% p$R                                  # global rotation
    is_sub <- key(p$file) %in% names(names_lc) ||
              grepl("\\.ldr$", p$file, ignore.case = TRUE)
    if (is_sub && key(p$file) %in% names(names_lc)) {
      # the FIRST level of sub-models below the root defines a part's "group"
      # (the named .mpd sub-assembly it belongs to); deeper levels inherit it
      child_group <- if (depth == 0L) sub("\\.(ldr|mpd)$", "", p$file, ignore.case = TRUE)
                     else group
      out <- c(out, expand_model(subfiles, p$file, gR, gt, step, seen,
                                 group = child_group, depth = depth + 1L))
    } else {
      dat <- sub("\\.dat$", "", p$file, ignore.case = TRUE)
      g   <- if (is.null(group)) "(root)" else group
      out <- c(out, list(list(dat = dat, colour = p$colour,
                              x = gt[1], y = gt[2], z = gt[3],
                              R = gR, step = step, group = g)))
    }
  }
  out
}

# =============================================================================
# 3. PART METADATA FROM REBRICKABLE  (part_num → name, category)
# =============================================================================
# set_parts.csv is the denormalised catalogue; we keep one row per part_num.
load_part_meta <- function(rebrickable_dir) {
  meta <- new.env(parent = emptyenv())
  if (is.null(rebrickable_dir)) return(meta)
  f <- file.path(rebrickable_dir, "set_parts.csv")
  if (!file.exists(f)) { warning("set_parts.csv not found; names/categories unavailable"); return(meta) }
  dt <- fread(f, select = c("part_num", "part_name", "part_category"),
              showProgress = FALSE)
  dt <- unique(dt, by = "part_num")
  for (i in seq_len(nrow(dt)))
    assign(dt$part_num[i], list(name = dt$part_name[i], cat = dt$part_category[i]), envir = meta)
  meta
}

# Resolve an LDraw .dat stem to a catalogue entry via progressive fallback
# (strip assembly "c01", print "pXX/prc", trailing-letter variants).
resolve_meta <- function(dat, meta) {
  cands <- unique(c(dat,
                    sub("c[0-9]+$", "", dat),
                    sub("p.*$", "",   dat),
                    sub("[a-z].*$", "", dat)))
  cands <- cands[!is.na(cands) & nzchar(cands)]
  for (c in cands) if (exists(c, envir = meta, inherits = FALSE))
    return(get(c, envir = meta, inherits = FALSE))
  list(name = NA_character_, cat = NA_character_)
}

# =============================================================================
# 4. FAMILY + BOUNDING BOX FROM NAME / CATEGORY   (the generalisation)
# =============================================================================
# Family drives BOTH the node colour and the compatibility rule. Derived from
# the Rebrickable category string (falls back to the part name, then "other").
family_of <- function(name, cat) {
  s <- tolower(paste(cat, name))
  # Wheel-related parts, disambiguated by structural noun:
  #   "Brick/Plate ... with Wheels"  → wheel_holder (stud-connects AND holds a tyre)
  #   "Tyre ..."                     → tyre
  #   "Wheel ..." (rim only)         → wheel
  if (grepl("wheel|tyre|tire", s)) {
    if (grepl("brick|plate", s))                     return("wheel_holder")
    if (grepl("tyre|tire", s))                       return("tyre")
    return("wheel")
  }
  # word boundaries so structural parts don't false-match minifig tokens:
  #   "Headlight" !~ head, "Hinges, Arms..." !~ arm, "Side Handle" !~ hand.
  # \\bminifig (prefix only) still catches "Minifig Head/Hand" and any part
  # whose Rebrickable category is "Minifig ..." (hair, headwear, weapons).
  if (grepl("\\bminifig|\\bfigure\\b|\\btorso\\b|\\bhips\\b|\\barm\\b|\\bhand\\b|\\bhead\\b|\\bhat\\b|\\bcap\\b|\\bhelmet\\b|\\bleg\\b", s)) return("minifig")
  if (grepl("baseplate", s))                         return("baseplate")
  if (grepl("technic|axle|\\bpin\\b|gear|beam|bush", s)) return("technic")
  if (grepl("tile",  s))                             return("tile")
  if (grepl("slope|wedge|roof",   s))                return("slope")
  if (grepl("cone",  s))                             return("cone")
  if (grepl("round", s) & grepl("brick", s))         return("round")
  if (grepl("panel|windscreen|windshield|door|window|glass|windscreen", s)) return("panel")
  if (grepl("bar|clip|bracket|hinge", s))            return("bracket")
  if (grepl("plate", s))                             return("plate")
  if (grepl("brick", s))                             return("brick")
  "other"
}

# Height (LDU) from family/name; footprint (studs) parsed from "N x M" in name.
# Wheels/tyres/minifig parts skip the stud-footprint parse (their names carry
# millimetre sizes like "Tyre 15 x 6" that must NOT be read as studs).
dims_of <- function(name, fam) {
  s <- tolower(paste(fam, name))
  h <- if (fam %in% c("brick","round","cone","slope","technic")) BRICK_H
       else if (fam == "wheel_holder") (if (grepl("brick", s)) BRICK_H else PLATE_H)
       else if (fam == "baseplate") PLATE_H
       else PLATE_H
  # default footprint (1 stud) for parts we cannot size from the name
  w <- STUD; d <- STUD
  if (!fam %in% c("tyre","wheel","minifig","other") && !is.na(name)) {
    m <- regmatches(name, regexpr("([0-9]+)\\s*[xX]\\s*([0-9]+)", name))
    if (length(m) == 1) {
      nn <- as.numeric(strsplit(m, "\\s*[xX]\\s*")[[1]])
      w <- nn[1] * STUD; d <- nn[2] * STUD
    }
  }
  if (fam == "tyre")  { w <- 30; d <- 30; h <- 16 }
  if (fam == "wheel") { w <- 30; d <- 30; h <- 16 }
  list(w = w, d = d, h = h)
}

# =============================================================================
# 5. COMPATIBILITY POLICY  (editable — the "in addition to proximity" gate)
# =============================================================================
# STUD_SYS families all mate with each other (they connect via studs/anti-studs
# when stacked). Special mating pairs handled explicitly. "other" is permissive
# (unknown parts are not dropped). tyre↔tyre never connects.
# wheel_holder is a stud-system part that ALSO accepts a tyre/wheel on its axle.
STUD_SYS <- c("plate","tile","slope","brick","round","baseplate","cone",
              "panel","bracket","wheel_holder")
# pairs that mate via an axle/hub rather than studs (proximity = hub distance)
is_hub_pair <- function(fa, fb)
  setequal(c(fa,fb), c("tyre","wheel")) ||
  ("tyre"  %in% c(fa,fb) && "wheel_holder" %in% c(fa,fb)) ||
  ("wheel" %in% c(fa,fb) && "wheel_holder" %in% c(fa,fb))
compatible <- function(fa, fb) {
  if ("other" %in% c(fa, fb))                         return(TRUE)   # permissive default
  if (fa %in% STUD_SYS && fb %in% STUD_SYS)           return(TRUE)
  if (is_hub_pair(fa, fb))                            return(TRUE)
  if ("technic" %in% c(fa,fb) && any(c(fa,fb) %in% c(STUD_SYS,"technic"))) return(TRUE)
  if ("minifig" %in% c(fa,fb) && any(c(fa,fb) %in% c(STUD_SYS,"minifig"))) return(TRUE)
  if (fa == fb && fa != "tyre")                       return(TRUE)
  FALSE
}

# =============================================================================
# 6. BUILD THE ADJACENCY MATRIX  (proximity AND compatibility)
# =============================================================================
build_adjacency <- function(parts, tol = TOL) {
  N <- length(parts)
  # bounding box per instance
  bx <- lapply(parts, function(p) {
    d <- p$dims; R <- p$R
    hx <- d$w/2; hz <- d$d/2
    ex <- abs(R[1,1])*hx + abs(R[1,3])*hz            # rotated half-extent x
    ez <- abs(R[3,1])*hx + abs(R[3,3])*hz            # rotated half-extent z
    list(xmin=p$x-ex, xmax=p$x+ex, zmin=p$z-ez, zmax=p$z+ez,
         ymin=p$y-d$h, ymax=p$y, cx=p$x, cy=p$y, cz=p$z, fam=p$fam)
  })
  touch <- function(a,b) (a$xmin-tol<=b$xmax && b$xmin-tol<=a$xmax &&
                          a$ymin-tol<=b$ymax && b$ymin-tol<=a$ymax &&
                          a$zmin-tol<=b$zmax && b$zmin-tol<=a$zmax)
  xz    <- function(a,b) (a$xmin-tol<=b$xmax && b$xmin-tol<=a$xmax &&
                          a$zmin-tol<=b$zmax && b$zmin-tol<=a$zmax)
  ygap  <- function(a,b) max(0, max(a$ymin-b$ymax, b$ymin-a$ymax))
  cdist <- function(a,b) sqrt((a$cx-b$cx)^2+(a$cy-b$cy)^2+(a$cz-b$cz)^2)

  A <- matrix(0L, N, N); crit <- matrix(NA_character_, N, N)
  for (i in seq_len(N)) for (j in seq_len(N)) {
    if (j <= i) next
    a <- bx[[i]]; b <- bx[[j]]
    if (!compatible(a$fam, b$fam)) next
    if (is_hub_pair(a$fam, b$fam)) {
      prox <- cdist(a,b) < HUB_D; c <- "tyre-hub"
    } else {
      tt <- touch(a,b); st <- xz(a,b) && ygap(a,b) <= STACK_DY
      prox <- tt || st; c <- if (tt) "touch" else if (st) "stack" else NA_character_
    }
    if (prox) { A[i,j] <- 1L; A[j,i] <- 1L; crit[i,j] <- c }
  }
  list(A = A, crit = crit)
}

# =============================================================================
# 7. DRIVER
# =============================================================================
# Interactive menu of the set's named sub-models; returns the group names the
# user picks to exclude (in addition to any auto-selected minifig groups).
.prompt_exclusions <- function(gtab, preselected) {
  cat("\nSub-models in this set (a 'part' is one placed instance):\n")
  for (i in seq_len(nrow(gtab))) {
    mk <- if (gtab$group[i] %in% preselected) "  [auto-excluded: minifig]" else ""
    cat(sprintf("  %2d. %-48s %4d parts%s\n", i, gtab$group[i], gtab$n[i], mk))
  }
  cat("\nEnter the numbers of ADDITIONAL sub-models to exclude\n")
  cat("(comma/space separated; blank = keep all the rest): ")
  ans <- trimws(readline())
  if (!nzchar(ans)) return(character(0))
  idx <- suppressWarnings(as.integer(strsplit(ans, "[ ,]+")[[1]]))
  idx <- idx[!is.na(idx) & idx >= 1 & idx <= nrow(gtab)]
  gtab$group[idx]
}

run_lego_adjacency <- function(mpd_path, rebrickable_dir = NULL, out_prefix = NULL,
                               TOL = 10, exclude = NULL,
                               exclude_minifigs = FALSE, ask = FALSE) {
  stopifnot(file.exists(mpd_path))
  if (is.null(out_prefix))
    out_prefix <- sub("\\.[^.]*$", "", basename(mpd_path))

  subfiles <- parse_mpd(mpd_path)
  raw      <- expand_model(subfiles)
  if (length(raw) == 0) stop("No placed parts found in ", mpd_path)

  meta <- load_part_meta(rebrickable_dir)
  for (k in seq_along(raw)) {
    m   <- resolve_meta(raw[[k]]$dat, meta)
    fam <- family_of(m$name, m$cat)
    raw[[k]]$name <- if (is.na(m$name)) raw[[k]]$dat else m$name
    raw[[k]]$cat  <- m$cat
    raw[[k]]$fam  <- fam
    raw[[k]]$dims <- dims_of(m$name, fam)
  }

  # --- exclusions: drop whole minifig / ancillary sub-models -----------------
  # Each part carries $group = the named .mpd sub-model it descends from.
  # Exclusion is GROUP-based (a minifigure is a sub-model, not a loose part),
  # so an excluded figure takes its accessories (blasters, tools) with it.
  grp_v   <- vapply(raw, function(p) if (is.null(p$group)) "(root)" else p$group, "")
  fam_all <- vapply(raw, `[[`, "", "fam")
  gtab <- data.frame(group = names(table(grp_v)),
                     n     = as.integer(table(grp_v)),
                     stringsAsFactors = FALSE)
  # A sub-model is a "minifig" if its name says so, or if most of its parts
  # are minifig-family (catches figures whose sub-model name is generic).
  name_is_mf <- grepl("minifig|figure|droid", gtab$group, ignore.case = TRUE)
  frac_mf    <- vapply(gtab$group, function(g) mean(fam_all[grp_v == g] == "minifig"), 0)
  minifig_groups <- gtab$group[name_is_mf | frac_mf >= 0.5]

  if (ask) {
    auto <- if (exclude_minifigs) minifig_groups else character(0)
    exclude <- unique(c(exclude, auto, .prompt_exclusions(gtab, auto)))
    exclude_minifigs <- FALSE   # already folded any auto picks into `exclude`
  }

  drop_groups <- character(0)
  if (exclude_minifigs) drop_groups <- c(drop_groups, minifig_groups)
  if (length(exclude))  drop_groups <- c(drop_groups, exclude)
  drop_groups <- unique(drop_groups)
  drop <- tolower(trimws(grp_v)) %in% tolower(trimws(drop_groups))
  if (any(drop)) {
    cat(sprintf("Excluded         : %d parts in %d sub-model(s)  (%s)\n",
                sum(drop), length(unique(grp_v[drop])),
                paste(sort(unique(grp_v[drop])), collapse = ", ")))
    raw <- raw[!drop]
    if (length(raw) == 0) stop("All parts were excluded — nothing to analyse.")
  }

  # instance-unique node labels: "<part_num>#<k>" ; display label = part_num
  dat_v  <- vapply(raw, `[[`, "", "dat")
  idx    <- ave(seq_along(dat_v), dat_v, FUN = seq_along)
  node   <- paste0(dat_v, "#", idx)
  part_num <- dat_v
  fam_v  <- vapply(raw, `[[`, "", "fam")
  name_v <- vapply(raw, `[[`, "", "name")

  ad   <- build_adjacency(raw, tol = TOL)
  A    <- ad$A; crit <- ad$crit
  dimnames(A) <- list(node, node)
  deg  <- rowSums(A)

  cat(sprintf("Model            : %s\n", out_prefix))
  cat(sprintf("Part instances   : %d\n", length(raw)))
  cat(sprintf("Connections      : %d\n", sum(A)/2))
  cat(sprintf("Isolated parts   : %d  (%s)\n", sum(deg==0),
              paste(node[deg==0], collapse=", ")))
  cat(sprintf("Families         : %s\n",
              paste(sort(unique(fam_v)), collapse=", ")))

  # ---- family colour palette (stable, colour-blind-aware) ------------------
  FAM_COL <- c(brick="#4E79A7", plate="#59A14F", tile="#8CD17D", slope="#B07AA1",
               round="#499894", cone="#86BCB6", baseplate="#A0CBE8",
               panel="#F1CE63", bracket="#B6992D", technic="#E15759",
               wheel="#79706E", wheel_holder="#D37295", tyre="#4B4B4B",
               minifig="#FF9D9A", other="#BAB0AC")
  fam_col_v <- FAM_COL[fam_v]; fam_col_v[is.na(fam_col_v)] <- FAM_COL["other"]

  # ---- (A) adjacency heatmap, instances grouped by family ------------------
  ord   <- order(match(fam_v, names(FAM_COL)), part_num)
  As    <- A[ord, ord]
  labs  <- part_num[ord]                       # part-number axis labels
  rownames(As) <- colnames(As) <- node[ord]
  hm <- reshape2::melt(As, varnames = c("A","B"), value.name = "v")
  hm$Ai <- match(hm$A, node[ord]); hm$Bi <- match(hm$B, node[ord])
  axis_col <- fam_col_v[ord]
  p1 <- ggplot(hm, aes(Bi, Ai, fill = factor(v))) +
    geom_tile(colour = "white", linewidth = 0.3) +
    scale_fill_manual(values = c("0"="#F4F4F2","1"="#2C6E8F"),
                      labels = c("not connected","connected"), name = NULL) +
    scale_x_continuous(breaks = seq_along(labs), labels = labs, expand = c(0,0)) +
    scale_y_reverse(breaks = seq_along(labs), labels = labs, expand = c(0,0)) +
    coord_equal() +
    labs(title = sprintf("%s — part adjacency matrix", out_prefix),
         subtitle = sprintf("%d instances · %d connections · proximity AND compatibility · grouped by family",
                            length(raw), sum(A)/2), x = NULL, y = NULL) +
    theme_minimal(base_size = 9) +
    theme(axis.text.x = element_text(angle = 90, vjust = .5, hjust = 1, size = 6, colour = axis_col),
          axis.text.y = element_text(size = 6, colour = axis_col),
          panel.grid = element_blank(), legend.position = "top")
  ggsave(paste0(out_prefix, "_adjacency_matrix.png"), p1,
         width = max(7, 0.32*length(raw)), height = max(6.5, 0.32*length(raw)),
         dpi = 200, limitsize = FALSE)

  # ---- (B) network with ggrepel labels, node colour = family ---------------
  g <- graph_from_adjacency_matrix(A, mode = "undirected", diag = FALSE)
  set.seed(42); lay <- layout_with_fr(g)
  nd <- data.frame(x = lay[,1], y = lay[,2], part = part_num, fam = fam_v)
  ee <- as.data.frame(as_edgelist(g, names = FALSE))
  ed <- data.frame(x = lay[ee[,1],1], y = lay[ee[,1],2],
                   xend = lay[ee[,2],1], yend = lay[ee[,2],2])
  p2 <- ggplot() +
    geom_segment(data = ed, aes(x, y, xend = xend, yend = yend),
                 colour = "#CCCCCC", linewidth = 0.5) +
    geom_point(data = nd, aes(x, y, fill = fam), shape = 21, size = 5,
               colour = "white", stroke = 0.8) +
    ggrepel::geom_text_repel(data = nd, aes(x, y, label = part),
                             size = 2.4, max.overlaps = Inf, seed = 42,
                             segment.size = 0.2, segment.colour = "#999999",
                             min.segment.length = 0, box.padding = 0.3) +
    scale_fill_manual(values = FAM_COL, name = "part family") +
    coord_equal() +
    labs(title = sprintf("%s — part connectivity network", out_prefix),
         subtitle = "node = part instance · label = part number · colour = family") +
    theme_void(base_size = 10) +
    theme(legend.position = "right",
          plot.title = element_text(face = "bold"),
          plot.subtitle = element_text(colour = "grey40"))
  ggsave(paste0(out_prefix, "_network.png"), p2,
         width = 10, height = 8, dpi = 200)

  # ---- (D) network on REAL model coordinates (top view: x vs z) ------------
  cx <- vapply(raw, `[[`, 0, "x"); cz <- vapply(raw, `[[`, 0, "z")
  ndc <- data.frame(x = cx, z = cz, part = part_num, fam = fam_v)
  eec <- as.data.frame(as_edgelist(g, names = FALSE))
  edc <- if (nrow(eec)) data.frame(x = cx[eec[,1]], z = cz[eec[,1]],
                                   xend = cx[eec[,2]], zend = cz[eec[,2]]) else
         data.frame(x=numeric(), z=numeric(), xend=numeric(), zend=numeric())
  p3 <- ggplot() +
    geom_segment(data = edc, aes(x, z, xend = xend, yend = zend),
                 colour = "#CCCCCC", linewidth = 0.5) +
    geom_point(data = ndc, aes(x, z, fill = fam), shape = 21, size = 5,
               colour = "white", stroke = 0.8) +
    ggrepel::geom_text_repel(data = ndc, aes(x, z, label = part),
                             size = 2.4, max.overlaps = Inf, seed = 42,
                             segment.size = 0.2, segment.colour = "#999999",
                             min.segment.length = 0, box.padding = 0.3) +
    scale_fill_manual(values = FAM_COL, name = "part family") +
    scale_y_reverse() + coord_equal() +
    labs(title = sprintf("%s — part connectivity network (real coordinates)", out_prefix),
         subtitle = "nodes at real model coordinates (top view) · label = part number · colour = family",
         x = "x (LDU)", y = "z (LDU)") +
    theme_minimal(base_size = 10) +
    theme(legend.position = "right",
          plot.title = element_text(face = "bold"),
          plot.subtitle = element_text(colour = "grey40"),
          panel.grid.minor = element_blank())
  ggsave(paste0(out_prefix, "_network_coords.png"), p3,
         width = 10, height = 8, dpi = 200)

  # ---- (C) exports ---------------------------------------------------------
  write.csv(A, paste0(out_prefix, "_adjacency_matrix.csv"))
  N <- length(raw); el <- list()
  for (i in seq_len(N)) for (j in seq_len(N)) if (j > i && A[i,j] == 1L)
    el[[length(el)+1]] <- data.frame(
      part_A = node[i], num_A = part_num[i], fam_A = fam_v[i], name_A = name_v[i],
      part_B = node[j], num_B = part_num[j], fam_B = fam_v[j], name_B = name_v[j],
      criterion = crit[i,j], stringsAsFactors = FALSE)
  edges <- if (length(el)) do.call(rbind, el) else
           data.frame(part_A=character(),num_A=character(),fam_A=character(),name_A=character(),
                      part_B=character(),num_B=character(),fam_B=character(),name_B=character(),
                      criterion=character())
  write.csv(edges, paste0(out_prefix, "_edges.csv"), row.names = FALSE)

  cat(sprintf("Wrote: %s_{adjacency_matrix.csv, edges.csv, adjacency_matrix.png, network.png, network_coords.png}\n",
              out_prefix))
  invisible(list(A = A, edges = edges, parts = raw))
}

# =============================================================================
# 8. MODEL-ONLY DRIVER  (exclude minifigures + ancillary sub-models)
# =============================================================================
# Same analysis as run_lego_adjacency() but scoped to the built model without
# its figures/accessories. By default it auto-drops every minifig sub-model
# and, when run interactively (ask = TRUE, the default), prints a numbered menu
# of the set's named sub-models so you can also exclude ancillary structures
# (flagpoles, stands, spare parts, etc.). Pass ask = FALSE for non-interactive/
# batch use, optionally with an explicit `exclude =` vector of sub-model names.
run_lego_model_adjacency <- function(mpd_path, rebrickable_dir = NULL,
                                     out_prefix = NULL, TOL = 10,
                                     exclude = NULL, exclude_minifigs = TRUE,
                                     ask = interactive()) {
  run_lego_adjacency(mpd_path, rebrickable_dir = rebrickable_dir,
                     out_prefix = out_prefix, TOL = TOL,
                     exclude = exclude, exclude_minifigs = exclude_minifigs,
                     ask = ask)
}

# ---- CLI entry point --------------------------------------------------------
if (sys.nframe() == 0 || identical(environment(), globalenv())) {
  a <- commandArgs(trailingOnly = TRUE)
  if (length(a) >= 1) {
    run_lego_adjacency(a[1],
                       if (length(a) >= 2) a[2] else NULL,
                       if (length(a) >= 3) a[3] else NULL)
  }
}
