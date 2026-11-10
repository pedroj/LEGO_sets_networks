# =============================================================================
# plot_lego_network.R - LEGO part connection network with line-art part icons
# plot_lego_network(adj, icons_dir, parts_meta = NULL, out_prefix = "lego_network", ...)
# Nodes = part types as icons on white discs; ring colour = category; grouped by category,
# ordered by subcategory. Edge colour = joint inferred from subcategory compatibility;
# pairs the rules say cannot join are dark red dashed ("check").
# Writes <out_prefix>_icons.png (300 dpi), _icons.pdf, _nodes.csv, _edges.csv
# =============================================================================
suppressPackageStartupMessages({ library(igraph); library(ggplot2); library(png) })

classify_part <- function(name, category = "") {
  n <- tolower(gsub("\\s+", " ", paste(name)))
  ct <- tolower(paste(category))
  has <- function(p, x = n) grepl(p, x, perl = TRUE)
  out <- function(c, s) c(category = c, subtype = s)
  if (has("^[~=_]")) n <- sub("^[~=_|]+", "", n)
  if (has("\\bminifig") || has("^minifig", ct)) {
    if (has("\\bhead\\b(?!gear|dress|band)") && !has("hair|hat|helmet|cap\\b|headgear|crown|hood"))
      return(out("Minifig", "Minifig head"))
    if (has("torso")) return(out("Minifig", "Minifig torso (with arms)"))
    if (has("^arm\\b|\\barm, (left|right)")) return(out("Minifig", "Minifig arm"))
    if (has("\\bhand\\b")) return(out("Minifig", "Minifig hand"))
    if (has("hips|legs?\\b")) return(out("Minifig", "Minifig hips and legs"))
    if (has("hair|hat|helmet|\\bcap\\b|headgear|headdress|crown|hood|headwear") || has("headwear", ct))
      return(out("Minifig", "Minifig headgear or hair"))
    if (has("neck|backpack|cape|armou?r|vest|airtanks")) return(out("Minifig", "Minifig neckwear"))
    return(out("Minifig", "Minifig accessory (tool; weapon)"))
  }
  # Seats and chairs are minifig accessories (studded seat, S|A)
  if (has("\\bseat\\b|\\bchair\\b")) return(out("Minifig", "Minifig seat or chair"))
  if (has("^arm, (left|right)")) return(out("Minifig", "Minifig arm"))
  # Boat hulls are large studded bases (the hull 2372c01 carries the build on its deck)
  if (has("boat hull|^boat \\d")) return(out("Baseplate", "Baseplate"))
  if (has("steering (stand|wheel)")) return(out("Special", "Bracket (SNOT)"))
  if (has("\\bty(re|res)\\b|\\btire")) return(out("Special", "Tyre"))
  if (has("(brick|plate).*wheel|wheels? holder") && !has("^bar\\b"))
    return(out("Special", "Wheel holder"))
  if (has("\\bwheel|\\brim\\b")) return(out("Special", "Wheel rim"))
  if (has("technic") || has("technic", ct)) {
    if (has("\\bpin\\b") && has("ball")) return(out("Technic", "Technic pin with ball"))
    if (has("axle pin|pin.*axle|axle.*pin")) return(out("Technic", "Technic pin with axle"))
    if (has("\\bpin\\b") && !has("connector|joiner|hole")) return(out("Technic", "Technic pin"))
    if (has("\\bbush")) return(out("Technic", "Technic bush"))
    if (has("gear|worm|\\brack\\b|differential|turntable")) return(out("Technic", "Technic gear"))
    if (has("chain|tread|track")) return(out("Technic", "Technic chain or tread link"))
    if (has("connector|joiner|block|cross|angle|hub|steering|shock|link")) return(out("Technic", "Technic connector (cross block; angle)"))
    if (has("\\baxle\\b")) return(out("Technic", "Technic axle"))
    if (has("panel|fairing")) return(out("Technic", "Technic panel or fairing"))
    if (has("\\bbrick\\b")) return(out("Technic", "Technic brick (studded beam)"))
    if (has("\\bplate\\b")) return(out("Technic", "Technic plate (with holes)"))
    if (has("beam|liftarm")) return(out("Technic", "Technic beam (liftarm)"))
    return(out("Technic", "Technic connector (cross block; angle)"))
  }
  if (has("hinge")) return(out("Special", "Click hinge (plate or brick)"))
  if (has("turntable")) return(out("Special", "Turntable"))
  if (has("socket|ball cup")) return(out("Special", "Ball-joint plate (socket)"))
  if (has("tow ball|ball joint|with ball|\\bball\\b")) return(out("Special", "Ball-joint plate (ball)"))
  if (has("bracket")) return(out("Special", "Bracket (SNOT)"))
  if (has("\\bclip")) return(out("Special", "Clip plate or tile"))
  if (has("handle|bar holder|with bar\\b")) return(out("Special", "Bar holder or handle plate"))
  if (has("\\bdish\\b|radar")) return(out("Special", "Dish or radar"))
  if (has("\\bflag")) return(out("Special", "Flag"))
  if (has("hose|flex|\\btube\\b|\\bstring")) return(out("Special", "Hose or flex bar"))
  if (has("plant|leaves|flower|stem|grass|\\btree|vine|foliage")) return(out("Special", "Plant or foliage"))
  if (has("^(bar|antenna)|\\bbar \\d|antenna|lightsaber|whip|\\brod\\b")) return(out("Special", "Bar or antenna"))
  if (has("baseplate")) return(out("Baseplate", "Baseplate"))
  if (has("windscreen|windshield|canopy")) return(out("Panel", "Windscreen"))
  if (has("glass|pane\\b|shutter|door\\b(?! ?frame)|\\bgate\\b"))
    return(out("Panel", "Window or door leaf"))
  if (has("window|door ?frame|\\bframe\\b")) return(out("Panel", "Window frame"))
  if (has("\\bpanel\\b|\\bwall\\b|\\bfence\\b")) return(out("Panel", "Panel"))
  if (has("^tile|\\btile\\b") || has("^tiles", ct)) {
    if (has("round|circle|quarter")) return(out("Tile", "Tile round"))
    if (has("grille|slope|modified|curved|corner")) return(out("Tile", "Tile modified (grille; slope tile)"))
    return(out("Tile", "Tile basic"))
  }
  if (has("slope|curved|\\bcurve\\b") || has("slopes", ct)) {
    if (has("inverted")) return(out("Slope", "Slope inverted"))
    if (has("curved|curve")) return(out("Slope", "Slope curved"))
    return(out("Slope", "Slope standard"))
  }
  if (has("\\bcone\\b")) return(out("Brick", "Cone"))
  if (has("\\barch\\b")) return(out("Brick", "Brick arch"))
  if (has("\\bwedge\\b") && has("plate")) return(out("Plate", "Plate wedge"))
  if (has("\\bwedge\\b")) return(out("Slope", "Slope standard"))
  if (has("^plate|\\bplate\\b") || has("^plates", ct)) {
    if (has("jumper|with 1 stud|1 x 2 with groove|centre stud|center stud"))
      return(out("Plate", "Plate jumper"))
    if (has("round|circle")) return(out("Plate", "Plate round"))
    return(out("Plate", "Plate basic"))
  }
  if (has("^brick|\\bbrick\\b") || has("^bricks", ct)) {
    if (has("round|cylinder")) return(out("Brick", "Brick round"))
    if (has("with (pin|axle)|with technic")) return(out("Brick", "Brick with pin or axle"))
    if (has("studs? on (\\d+ )?(side|sides)|headlight|with side studs?|snot")) return(out("Brick", "Brick modified SNOT"))
    return(out("Brick", "Brick basic"))
  }
  out("Other", "Other")
}

.SUBTYPE_IF <- c(
  "Brick basic" = "S|A", "Brick modified SNOT" = "S|A", "Brick with pin or axle" = "S|A|P",
  "Brick round" = "S|HS|A|XH", "Brick arch" = "S|A", "Cone" = "S|HS|A",
  "Plate basic" = "S|A", "Plate jumper" = "S|A", "Plate round" = "S|HS|A", "Plate wedge" = "S|A",
  "Tile basic" = "A", "Tile round" = "A", "Tile modified (grille; slope tile)" = "A",
  "Slope standard" = "S|A", "Slope inverted" = "S|A", "Slope curved" = "A",
  "Baseplate" = "S", "Panel" = "S|A", "Window frame" = "S|A|DHf", "Window or door leaf" = "DHm",
  "Windscreen" = "S|A",
  "Technic pin" = "P", "Technic pin with axle" = "P|X", "Technic pin with ball" = "P|BL",
  "Technic axle" = "X", "Technic beam (liftarm)" = "PH|XH", "Technic brick (studded beam)" = "S|A|PH",
  "Technic plate (with holes)" = "S|A|PH", "Technic connector (cross block; angle)" = "PH|XH|P",
  "Technic bush" = "XH", "Technic gear" = "XH|G", "Technic panel or fairing" = "PH",
  "Technic chain or tread link" = "TR",
  "Click hinge (plate or brick)" = "S|A|HG", "Clip plate or tile" = "S|A|C",
  "Bar holder or handle plate" = "S|A|B", "Ball-joint plate (ball)" = "S|A|BL",
  "Ball-joint plate (socket)" = "S|A|SK", "Turntable" = "S|A|TT", "Bracket (SNOT)" = "S|A",
  "Dish or radar" = "A|HS", "Bar or antenna" = "B", "Flag" = "C", "Hose or flex bar" = "B",
  "Plant or foliage" = "S|A|B|C", "Wheel holder" = "S|A|P", "Wheel rim" = "PH|XH|RIM", "Tyre" = "TY",
  "Minifig head" = "S|A", "Minifig torso (with arms)" = "S|AHm|MJ", "Minifig hand" = "AHf|C",
  "Minifig hips and legs" = "MJ|A", "Minifig headgear or hair" = "A",
  "Minifig accessory (tool; weapon)" = "B", "Minifig neckwear" = "S|A", "Minifig seat or chair" = "S|A", "Minifig arm" = "AHf|AHm",
  "Other" = "S|A|B|C|P|PH|X|XH")
.RULES <- data.frame(
  a = c("S", "HS", "HS", "A", "C", "P", "X", "X", "BL", "HG", "TT", "G", "G", "TR", "RIM", "DHm", "AHm", "MJ"),
  b = c("A", "A", "B", "B", "B", "PH", "XH", "PH", "SK", "HG", "TT", "G", "TR", "TR", "TY", "DHf", "AHf", "MJ"),
  system = c("stud", "stud", "bar", "bar", "clip", "pin", "axle", "axle", "ball", "hinge", "turntable",
             "gear", "gear", "track", "wheel", "door", "minifig", "minifig"),
  stringsAsFactors = FALSE)
.SYS_PRIORITY <- c("pin", "axle", "ball", "hinge", "turntable", "clip", "bar", "wheel", "gear",
                   "track", "door", "minifig", "stud")

.COMPAT <- NULL   # subtype x subtype code matrix (compat_matrix_subtype_codes.csv); NULL = embedded tables
joint_system <- function(sa, sb) {
  if (!is.null(.COMPAT) && sa %in% rownames(.COMPAT) && sb %in% colnames(.COMPAT)) {
    code <- .COMPAT[sa, sb]
    if (is.na(code) || !nzchar(code)) return("check")   # matrix says they cannot join
    s <- unique(strsplit(gsub("\\?", "", code), "\\+")[[1]])
    return(s[order(match(s, .SYS_PRIORITY))][1])
  }
  ia <- strsplit(.SUBTYPE_IF[[sa]], "\\|")[[1]]; ib <- strsplit(.SUBTYPE_IF[[sb]], "\\|")[[1]]
  hit <- (.RULES$a %in% ia & .RULES$b %in% ib) | (.RULES$b %in% ia & .RULES$a %in% ib)
  s <- unique(.RULES$system[hit])
  if (!length(s)) return("check")
  s[order(match(s, .SYS_PRIORITY))][1]
}

plot_lego_network <- function(adj, icons_dir, parts_meta = NULL, out_prefix = "lego_network",
                              title = NULL, label = c("part", "part+subtype", "none"),
                              cat_col = NULL, width_in = NULL,
                              set_info = NULL, set_num = NULL, set_name = NULL, set_image = NULL,
                              inset_width = 0.20, compat_csv = NULL) {
  label <- match.arg(label)
  if (!is.null(compat_csv)) {   # compatibility from lego_theoretical_compatibility.R output
    stopifnot(file.exists(compat_csv))
    .COMPAT <<- as.matrix(read.csv(compat_csv, row.names = 1, check.names = FALSE, colClasses = "character"))
  }
  si <- list()
  if (!is.null(set_info) && file.exists(set_info)) si <- jsonlite_read(set_info)
  set_num   <- set_num   %||% si$set_num
  set_name  <- set_name  %||% si$name
  set_image <- set_image %||% si$image
  if (is.null(title)) {
    title <- if (!is.null(set_num) && !is.null(set_name))
      sprintf("LEGO %s — %s", sub("-1$", "", set_num), set_name) else
      if (!is.null(set_num)) sprintf("LEGO %s", sub("-1$", "", set_num)) else
      "LEGO part connection network"
  }
  if (is.character(adj)) adj <- as.matrix(read.csv(adj, row.names = 1, check.names = FALSE))
  adj <- as.matrix(adj); adj[is.na(adj)] <- 0
  stopifnot(nrow(adj) == ncol(adj))
  colnames(adj) <- rownames(adj)
  ptype <- sub("#.*$", "", rownames(adj))
  n_inst <- table(ptype)
  idx <- which(upper.tri(adj) & adj > 0, arr.ind = TRUE)
  ed <- data.frame(from = ptype[idx[, 1]], to = ptype[idx[, 2]], w = adj[idx])
  ed <- ed[ed$from != ed$to, ]
  key <- ifelse(ed$from < ed$to, paste(ed$from, ed$to, sep = "\t"), paste(ed$to, ed$from, sep = "\t"))
  ew <- tapply(ed$w, key, sum)
  ed <- data.frame(do.call(rbind, strsplit(names(ew), "\t")), weight = as.numeric(ew))
  names(ed)[1:2] <- c("from", "to")

  man <- read.csv(file.path(icons_dir, "manifest.csv"), stringsAsFactors = FALSE, colClasses = "character")
  man[is.na(man)] <- ""
  nodes <- data.frame(part = names(n_inst), instances = as.integer(n_inst), stringsAsFactors = FALSE)
  m <- match(nodes$part, man$part_num)
  nodes$desc <- ifelse(is.na(m), "", man$description[m])
  nodes$rb_cat <- ifelse(is.na(m), "", man$ldraw_category[m])
  nodes$icon <- ifelse(is.na(m), "", man$icon[m])
  if (!is.null(parts_meta)) {
    if (is.character(parts_meta)) parts_meta <- read.csv(parts_meta, stringsAsFactors = FALSE)
    nm <- intersect(c("part_name", "name"), names(parts_meta))[1]
    cc <- intersect(c("part_category", "category", "part_cat_name"), names(parts_meta))[1]
    k <- match(nodes$part, parts_meta$part_num)
    if (!is.na(nm)) nodes$desc <- ifelse(!is.na(k) & nzchar(parts_meta[[nm]][k]), parts_meta[[nm]][k], nodes$desc)
    if (!is.na(cc)) nodes$rb_cat <- ifelse(!is.na(k), parts_meta[[cc]][k], nodes$rb_cat)
  }
  cls <- t(mapply(classify_part, nodes$desc, nodes$rb_cat, USE.NAMES = FALSE))
  nodes$category <- cls[, "category"]; nodes$subtype <- cls[, "subtype"]

  st <- setNames(nodes$subtype, nodes$part)
  ed$system <- mapply(joint_system, st[ed$from], st[ed$to])
  # compatibility rules prevail over proximity: pairs the rules say cannot join are false
  # positives. They are not drawn and get weight 0, but stay in the edge table as joint = check
  # (contacts_proximity keeps the proximity-based contact count).
  ed$contacts_proximity <- ed$weight
  ed$weight[ed$system == "check"] <- 0
  ed_all <- ed
  ed <- ed[ed$system != "check", , drop = FALSE]
  g <- graph_from_data_frame(ed, directed = FALSE, vertices = nodes)
  nodes$degree <- degree(g)[nodes$part]
  nodes$strength <- strength(g, weights = E(g)$weight)[nodes$part]

  cats_present <- names(sort(table(nodes$category), decreasing = TRUE))
  ord <- character(0); lo <- 1; hi <- length(cats_present)
  while (lo <= hi) {
    ord <- c(ord, cats_present[lo]); lo <- lo + 1
    if (lo <= hi) { ord <- c(ord, cats_present[hi]); hi <- hi - 1 }
  }
  K <- length(ord); sz <- as.numeric(table(nodes$category)[ord])
  local_xy <- function(m) {
    if (m == 1) return(cbind(0, 0))
    if (m <= 14) { rr <- 0.85 / sin(pi / max(m, 2)); a <- pi + 2 * pi * (seq_len(m) - 1) / m
                   return(cbind(rr * cos(a), rr * sin(a))) }
    xy <- NULL; rad <- 1.4; left <- m
    while (left > 0) {
      cap <- max(1, floor(2 * pi * rad / 1.75)); k <- min(cap, left)
      if (left - k > 0 && left - k < 4) { k <- left; rad <- max(rad, k * 1.75 / (2 * pi)) }
      a <- pi + 2 * pi * (seq_len(k) - 1) / k
      xy <- rbind(xy, cbind(rad * cos(a), rad * sin(a)))
      left <- left - k; rad <- rad + 1.8
    }
    xy
  }
  loc <- lapply(sz, local_xy)
  rr <- vapply(loc, function(z) max(sqrt(rowSums(z^2))), 0)
  if (K == 1) { R <- 0 } else {
    need <- (rr + c(rr[-1], rr[1]) + 3.2) / (2 * sin(pi / K))
    R <- max(need)
  }
  ang <- setNames(pi / 2 - 2 * pi * (seq_len(K) - 1) / K, ord)
  nodes$x <- nodes$y <- NA_real_
  for (k in seq_len(K)) {
    ii <- which(nodes$category == ord[k])
    ii <- ii[order(nodes$subtype[ii], -nodes$degree[ii], nodes$part[ii])]
    rot <- matrix(c(cos(ang[k]), sin(ang[k]), -sin(ang[k]), cos(ang[k])), 2)
    z <- loc[[k]] %*% t(rot)
    nodes$x[ii] <- R * cos(ang[k]) + z[, 1]
    nodes$y[ii] <- R * sin(ang[k]) + z[, 2]
  }
  cat_lab <- data.frame(category = ord, x = (R + rr + 1.9) * cos(ang), y = (R + rr + 1.9) * sin(ang))
  if (K == 1) cat_lab[, c("x", "y")] <- c(0, rr + 1.4)

  default_col <- c(Brick = "#C91A09", Plate = "#0055BF", Tile = "#C8AE7A", Slope = "#FE8A18",
                   Baseplate = "#4B9F4A", Panel = "#6C96BF", Technic = "#8A8D91",
                   Special = "#A5499C", Minifig = "#E0B000", Other = "#555555")
  if (!is.null(cat_col)) default_col[names(cat_col)] <- cat_col
  sys_col <- c(stud = "grey60", bar = "#F58518", clip = "#E45756", pin = "#2A9D8F", axle = "#54A24B",
               ball = "#EECA3B", hinge = "#B279A2", turntable = "#FF9DA6", gear = "#9D755D",
               track = "#7F7F7F", wheel = "#1B9E77", door = "#D95F02", minifig = "#7570B3",
               check = "#8B0000")
  xy <- as.matrix(nodes[, c("x", "y")]); rownames(xy) <- nodes$part
  E <- data.frame(x = xy[ed$from, 1], y = xy[ed$from, 2], xend = xy[ed$to, 1], yend = xy[ed$to, 2],
                  system = factor(ed$system, levels = names(sys_col)), weight = ed$weight)
  E <- E[order(E$system == "stud", decreasing = TRUE), ]
  ew_max <- max(ed$weight)           # weighted network: edge width grows linearly with number of contacts
  rd <- 0.60
  r  <- rd
  circ <- function(cx, cy, rad, id, n = 60) {
    t <- seq(0, 2 * pi, length.out = n)
    data.frame(x = rep(cx, each = n) + rad * cos(t), y = rep(cy, each = n) + rad * sin(t),
               id = rep(id, each = n))
  }
  ring <- circ(nodes$x, nodes$y, rd + 0.07, nodes$part); ring$category <- rep(nodes$category, each = 60)
  disc <- circ(nodes$x, nodes$y, rd, nodes$part)

  ext <- max(abs(c(nodes$x, nodes$y, cat_lab$x, cat_lab$y))) + 1.2
  win <- if (is.null(width_in)) min(max(2 * ext * 0.62, 9), 30) else width_in
  pt <- win / (2 * ext) * 25.4
  p <- ggplot() +
    geom_segment(data = E, aes(x, y, xend = xend, yend = yend, colour = system,
                 linewidth = weight, linetype = system == "check"),
                 alpha = ifelse(E$system == "stud", 0.35, ifelse(E$system == "check", 0.4, 0.85)), lineend = "round") +
    geom_polygon(data = ring, aes(x, y, group = id, fill = category), colour = NA) +
    geom_polygon(data = disc, aes(x, y, group = id), fill = "white", colour = NA) +
    scale_colour_manual(values = sys_col, drop = TRUE, name = "Joint (inferred)") +
    scale_fill_manual(values = default_col, name = "Category") +
    scale_linewidth(range = c(0.25, 2.6), limits = c(0, ew_max), name = "Contacts (edge width)",
                    breaks = unique(round(seq(1, ew_max, length.out = min(5, ew_max))))) +
    guides(linewidth = guide_legend(order = 3, override.aes = list(colour = "grey40", linetype = "solid"))) +
    scale_linetype_manual(values = c(`FALSE` = "solid", `TRUE` = "22"), guide = "none") +
    coord_fixed(xlim = c(-ext, ext), ylim = c(-ext, ext), clip = "off") +
    labs(title = NULL, subtitle = NULL,
         caption = sprintf("Icons rendered from the LDraw library (line art). %d proximity link(s) between part types that the compatibility rules say cannot join directly are treated as false positives and not drawn.", sum(ed_all$system == "check"))) +
    theme_void(base_size = 11) +
    theme(plot.background = element_rect(fill = "white", colour = NA), legend.position = "right",
          plot.margin = margin(1.15 * 72, 14, 14, 14))
  for (k in seq_len(nrow(nodes))) {
    f <- file.path(icons_dir, nodes$icon[k])
    if (nzchar(nodes$icon[k]) && file.exists(f))
      p <- p + annotation_raster(readPNG(f), nodes$x[k] - r, nodes$x[k] + r,
                                 nodes$y[k] - r, nodes$y[k] + r, interpolate = TRUE)
  }
  if (label != "none") {
    nodes$lab <- if (label == "part") ifelse(nodes$instances > 1, sprintf("%s (%d)", nodes$part, nodes$instances), nodes$part) else
      paste0(nodes$part, "\n", vapply(strwrap(nodes$subtype, 16, simplify = FALSE), paste, "", collapse = "\n"))
    p <- p + geom_text(data = nodes, aes(x, y - rd - 0.12, label = lab), vjust = 1,
                       size = min(3.2, 0.24 * pt), colour = "grey20", lineheight = 0.85)
  }
  p <- p + geom_text(data = cat_lab, aes(x, y, label = category), fontface = "bold",
                     size = min(7, 0.55 * pt), colour = default_col[cat_lab$category])

  hgt <- win * 0.88
  has_inst <- any(grepl("#", rownames(adj)))
  subtitle <- if (has_inst)
    sprintf("%d part types, %d instances, %d connected pairs. Edge width = instance-level connections; colour = joint inferred from subcategories.",
            nrow(nodes), sum(nodes$instances), nrow(ed)) else
    sprintf("%d part types, %d connected pairs. Edge width = number of recorded connections; colour = joint inferred from subcategories.",
            nrow(nodes), nrow(ed))
  x0 <- unit(0.3, "in")
  head <- grid::grobTree(
    grid::textGrob(title, x = x0, y = unit(hgt, "in") - unit(0.28, "in"), just = c("left", "top"),
                   gp = grid::gpar(fontsize = 24, fontface = "bold")),
    grid::textGrob(subtitle, x = x0, y = unit(hgt, "in") - unit(0.72, "in"), just = c("left", "top"),
                   gp = grid::gpar(fontsize = 11, col = "grey30")))
  out <- grid::grobTree(ggplotGrob(p), head)
  if (!is.null(set_image) && file.exists(set_image)) {
    img <- if (grepl("\\.jpe?g$", set_image, ignore.case = TRUE)) jpeg::readJPEG(set_image) else readPNG(set_image)
    asp <- dim(img)[1] / dim(img)[2]
    w_in <- min(max(inset_width * win, 3), 6)
    w_in <- min(w_in, 0.26 * hgt / asp)
    h_in <- w_in * asp
    y0 <- unit(hgt, "in") - unit(1.15, "in")
    lab <- sprintf("Set %s%s", set_num %||% "",
                   if (isTRUE(si$source_image %in% c("img_url", "cdn"))) " · image: Rebrickable" else "")
    inset <- grid::grobTree(
      grid::rectGrob(x = x0, y = y0, width = unit(w_in, "in") + unit(8, "pt"),
                     height = unit(h_in, "in") + unit(8, "pt"), just = c("left", "top"),
                     gp = grid::gpar(fill = "white", col = "grey70", lwd = 0.8)),
      grid::rasterGrob(img, x = x0 + unit(4, "pt"), y = y0 - unit(4, "pt"),
                       width = unit(w_in, "in"), height = unit(h_in, "in"),
                       just = c("left", "top"), interpolate = TRUE),
      grid::textGrob(lab, x = x0, y = y0 - unit(h_in, "in") - unit(14, "pt"), just = c("left", "top"),
                     gp = grid::gpar(fontsize = 9, col = "grey35")))
    out <- grid::grobTree(out, inset)
  }
  ggsave(paste0(out_prefix, "_icons.png"), out, width = win, height = hgt, dpi = 300,
         bg = "white", limitsize = FALSE)
  ggsave(paste0(out_prefix, "_icons.pdf"), out, width = win, height = hgt,
         bg = "white", device = cairo_pdf, limitsize = FALSE)
  write.csv(nodes[order(nodes$category, nodes$subtype, -nodes$degree),
                  c("part", "desc", "category", "subtype", "instances", "degree", "strength", "icon")],
            paste0(out_prefix, "_nodes.csv"), row.names = FALSE)
  write.csv(ed_all, paste0(out_prefix, "_edges.csv"), row.names = FALSE)
  message(sprintf("%d part types, %d edges; unclassified: %d; 'check' edges removed: %d; missing icons: %d",
                  nrow(nodes), nrow(ed), sum(nodes$category == "Other"),
                  sum(ed_all$system == "check"), sum(!nzchar(nodes$icon))))
  invisible(list(graph = g, nodes = nodes, edges = ed, edges_all = ed_all, plot = p))
}

`%||%` <- function(a, b) if (is.null(a) || (length(a) == 1 && is.na(a))) b else a

jsonlite_read <- function(f) {
  if (requireNamespace("jsonlite", quietly = TRUE)) return(jsonlite::fromJSON(f))
  txt <- paste(readLines(f, warn = FALSE), collapse = " ")
  kv <- regmatches(txt, gregexpr('"([^"]+)"\\s*:\\s*("([^"]*)"|[-0-9.]+|null|true|false)', txt))[[1]]
  out <- list()
  for (x in kv) {
    k <- sub('^"([^"]+)".*$', "\\1", x); v <- sub('^"[^"]+"\\s*:\\s*', "", x)
    out[[k]] <- if (v == "null") NULL else if (grepl('^"', v)) gsub('^"|"$', "", v) else as.numeric(v)
  }
  out
}

if (sys.nframe() == 0 && length(commandArgs(TRUE)) >= 2) {
  a <- commandArgs(TRUE)
  plot_lego_network(a[1], a[2], parts_meta = if (length(a) >= 3 && nzchar(a[3])) a[3] else NULL,
                    out_prefix = if (length(a) >= 4) a[4] else "lego_network",
                    set_info = if (length(a) >= 5) a[5] else NULL)
}
