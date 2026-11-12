# ----------------------------------------------------------------------------- 4. network on REAL coordinates
# plot_lego_network_coords(): instance-level network with the nodes placed at the real model coordinates
# (top view, x vs z in LDU), drawn like plot_lego_network(): icon on a white disc, ring colour = category,
# edge colour = joint, same title / subtitle / set-image inset. Pairs the compatibility rules forbid are not
# drawn (kept in _coords_edges.csv as "check", weight 0). Icons that would overlap are pushed apart just enough
# to be legible (thin grey tether + dot mark the true position).
#   adj     instance adjacency matrix (names "3001#2")      coords  CSV with columns node, x, z (LDU)
#   panel_title  bold line under the title (e.g. "Anakin's Podracer")
# Writes <out_prefix>_network_coords.pdf and _network_coords_icons.png, _coords_nodes.csv, _coords_edges.csv
plot_lego_network_coords <- function(adj, coords, icons_dir, parts_meta = NULL, out_prefix = "lego",
                                     title = NULL, panel_title = NULL, label = c("part", "none"),
                                     set_info = NULL, set_num = NULL, set_name = NULL, set_image = NULL,
                                     inset_width = 0.20, compat_csv = NULL, cat_col = NULL, rd_frac = 0.04) {
  label <- match.arg(label)
  if (!is.null(compat_csv)) {
    stopifnot(file.exists(compat_csv))
    .COMPAT <<- as.matrix(read.csv(compat_csv, row.names = 1, check.names = FALSE, colClasses = "character"))
  }
  si <- list(); if (!is.null(set_info) && file.exists(set_info)) si <- jsonlite_read(set_info)
  set_num <- set_num %||% si$set_num; set_name <- set_name %||% si$name; set_image <- set_image %||% si$image
  if (is.null(title)) title <- if (!is.null(set_num) && !is.null(set_name))
    sprintf("LEGO %s — %s", sub("-1$", "", set_num), set_name) else "LEGO part connection network"
  if (is.character(adj)) adj <- as.matrix(read.csv(adj, row.names = 1, check.names = FALSE))
  adj <- as.matrix(adj); adj[is.na(adj)] <- 0; colnames(adj) <- rownames(adj)
  if (is.character(coords)) coords <- read.csv(coords, stringsAsFactors = FALSE)
  nd <- data.frame(node = rownames(adj), part = sub("#.*$", "", rownames(adj)), stringsAsFactors = FALSE)
  k <- match(nd$node, coords$node); stopifnot(!anyNA(k))
  nd$x0 <- coords$x[k]; nd$y0 <- -coords$z[k]            # top view: z grows downwards, as in the kernel plot

  # part-type metadata and classification (as in plot_lego_network)
  man <- read.csv(file.path(icons_dir, "manifest.csv"), stringsAsFactors = FALSE, colClasses = "character"); man[is.na(man)] <- ""
  up <- unique(nd$part); tm <- data.frame(part = up, stringsAsFactors = FALSE)
  m <- match(up, man$part_num)
  tm$desc <- ifelse(is.na(m), "", man$description[m]); tm$rb_cat <- ifelse(is.na(m), "", man$ldraw_category[m])
  tm$icon <- ifelse(is.na(m), "", man$icon[m])
  if (!is.null(parts_meta)) {
    if (is.character(parts_meta)) parts_meta <- read.csv(parts_meta, stringsAsFactors = FALSE)
    nm <- intersect(c("part_name", "name"), names(parts_meta))[1]
    cc <- intersect(c("part_category", "category", "part_cat_name"), names(parts_meta))[1]
    kk <- match(up, parts_meta$part_num)
    if (!is.na(nm)) tm$desc <- ifelse(!is.na(kk) & nzchar(parts_meta[[nm]][kk]), parts_meta[[nm]][kk], tm$desc)
    if (!is.na(cc)) tm$rb_cat <- ifelse(!is.na(kk), parts_meta[[cc]][kk], tm$rb_cat)
  }
  cls <- t(mapply(classify_part, tm$desc, tm$rb_cat, USE.NAMES = FALSE))
  tm$category <- cls[, "category"]; tm$subtype <- cls[, "subtype"]
  j <- match(nd$part, tm$part)
  nd$desc <- tm$desc[j]; nd$category <- tm$category[j]; nd$subtype <- tm$subtype[j]; nd$icon <- tm$icon[j]

  # instance-level edges, joint from subcategory compatibility
  idx <- which(upper.tri(adj) & adj > 0, arr.ind = TRUE)
  ed <- data.frame(from = nd$node[idx[, 1]], to = nd$node[idx[, 2]], stringsAsFactors = FALSE)
  ed$system <- if (nrow(ed)) mapply(joint_system, nd$subtype[idx[, 1]], nd$subtype[idx[, 2]]) else character(0)
  ed$weight <- ifelse(ed$system == "check", 0, 1)
  ed_all <- ed; ed <- ed[ed$system != "check", , drop = FALSE]
  nd$degree <- as.integer(table(factor(c(ed$from, ed$to), levels = nd$node))[nd$node])

  # positions: real coordinates, relaxed only where icons would overlap
  span <- max(diff(range(nd$x0)), diff(range(nd$y0)), 1); rd <- rd_frac * span; md <- 2.35 * rd
  P0 <- cbind(nd$x0, nd$y0); P <- P0; n <- nrow(P)
  for (it in seq_len(800)) {
    mv <- FALSE
    for (a in seq_len(n - 1)) for (b in (a + 1):n) {
      dv <- P[b, ] - P[a, ]; d <- sqrt(sum(dv^2))
      if (d < md) {
        u <- if (d < 1e-6) c(cos(a * 2.4), sin(a * 2.4)) else dv / d
        P[a, ] <- P[a, ] - u * (md - d) / 2; P[b, ] <- P[b, ] + u * (md - d) / 2; mv <- TRUE
      }
    }
    P <- P + 0.015 * (P0 - P)
    if (!mv) break
  }
  nd$x <- P[, 1]; nd$y <- P[, 2]
  nd$moved <- sqrt(rowSums((P - P0)^2)) > 0.6 * rd

  default_col <- c(Brick = "#C91A09", Plate = "#0055BF", Tile = "#C8AE7A", Slope = "#FE8A18", Baseplate = "#4B9F4A",
                   Panel = "#6C96BF", Technic = "#8A8D91", Special = "#A5499C", Minifig = "#E0B000", Other = "#555555")
  if (!is.null(cat_col)) default_col[names(cat_col)] <- cat_col
  sys_col <- c(stud = "grey60", bar = "#F58518", clip = "#E45756", pin = "#2A9D8F", axle = "#54A24B", ball = "#EECA3B",
               hinge = "#B279A2", turntable = "#FF9DA6", gear = "#9D755D", track = "#7F7F7F", wheel = "#1B9E77",
               door = "#D95F02", minifig = "#7570B3", check = "#8B0000")
  xy <- as.matrix(nd[, c("x", "y")]); rownames(xy) <- nd$node
  E <- data.frame(x = xy[ed$from, 1], y = xy[ed$from, 2], xend = xy[ed$to, 1], yend = xy[ed$to, 2],
                  system = factor(ed$system, levels = names(sys_col)))
  E <- E[order(E$system == "stud", decreasing = TRUE), ]
  circ <- function(cx, cy, rad, id, nn = 60) { t <- seq(0, 2 * pi, length.out = nn)
    data.frame(x = rep(cx, each = nn) + rad * cos(t), y = rep(cy, each = nn) + rad * sin(t), id = rep(id, each = nn)) }
  ring <- circ(nd$x, nd$y, rd * 1.12, nd$node); ring$category <- rep(nd$category, each = 60)
  disc <- circ(nd$x, nd$y, rd, nd$node)

  pad <- 3.2 * rd
  xl <- range(c(nd$x, nd$x0)) + c(-pad, pad); yl <- range(c(nd$y, nd$y0)) + c(-pad - rd, pad)
  pw <- min(max(10 * diff(xl) / max(diff(xl), diff(yl)), 6.5), 14); ph <- pw * diff(yl) / diff(xl)
  if (ph > 12) { pw <- pw * 12 / ph; ph <- 12 }
  pw <- max(pw, 6.5); ph <- pw * diff(yl) / diff(xl)
  win <- pw + 2.6 + 0.9; hgt <- ph + 1.6 + 0.9
  mm_rd <- pw / diff(xl) * rd * 25.4
  tether <- nd[nd$moved, , drop = FALSE]
  p <- ggplot() +
    geom_segment(data = tether, aes(x = x0, y = y0, xend = x, yend = y), colour = "grey70", linewidth = 0.35) +
    geom_point(data = tether, aes(x0, y0), colour = "grey55", size = 0.9) +
    geom_segment(data = E, aes(x, y, xend = xend, yend = yend, colour = system), linewidth = 0.9,
                 alpha = ifelse(E$system == "stud", 0.45, 0.9), lineend = "round") +
    geom_polygon(data = ring, aes(x, y, group = id, fill = category), colour = NA) +
    geom_polygon(data = disc, aes(x, y, group = id), fill = "white", colour = NA) +
    scale_colour_manual(values = sys_col, drop = TRUE, name = "Joint (inferred)") +
    scale_fill_manual(values = default_col, name = "Category") +
    coord_fixed(xlim = xl, ylim = yl, expand = FALSE, clip = "off") +
    labs(x = "x (LDU)", y = "z (LDU, top view)",
         caption = sprintf("Icons rendered from the LDraw library (line art). Nodes sit at the real model coordinates (top view); overlapping icons are pushed apart (grey tether and dot = true position). %d proximity link(s) that the compatibility rules say cannot join directly are treated as false positives and not drawn.", nrow(ed_all) - nrow(ed))) +
    scale_y_continuous(labels = function(v) -v) +
    theme_minimal(base_size = 11) +
    theme(plot.background = element_rect(fill = "white", colour = NA), legend.position = "right",
          panel.grid.minor = element_blank(), panel.grid.major = element_line(colour = "grey93"),
          plot.caption = element_text(size = 7, colour = "grey40", hjust = 0),
          plot.margin = margin(1.6 * 72, 14, 14, 14))
  for (q in seq_len(nrow(nd))) {
    f <- file.path(icons_dir, nd$icon[q])
    if (nzchar(nd$icon[q]) && file.exists(f))
      p <- p + annotation_raster(readPNG(f), nd$x[q] - rd, nd$x[q] + rd, nd$y[q] - rd, nd$y[q] + rd, interpolate = TRUE)
  }
  if (label != "none")
    p <- p + geom_text(data = nd, aes(x, y - rd * 1.2, label = part), vjust = 1, size = min(3.2, 0.42 * mm_rd), colour = "grey20")

  subtitle <- sprintf("%d part instances, %d part types, %d connected pairs. Nodes at real coordinates (top view); colour = joint inferred from subcategories.",
                      nrow(nd), length(up), nrow(ed))
  x0 <- unit(0.3, "in")
  head <- grid::grobTree(
    grid::textGrob(title, x = x0, y = unit(hgt, "in") - unit(0.28, "in"), just = c("left", "top"),
                   gp = grid::gpar(fontsize = 24, fontface = "bold")),
    if (!is.null(panel_title)) grid::textGrob(panel_title, x = x0, y = unit(hgt, "in") - unit(0.72, "in"),
                   just = c("left", "top"), gp = grid::gpar(fontsize = 18, fontface = "bold", col = "grey25")),
    grid::textGrob(subtitle, x = x0, y = unit(hgt, "in") - unit(if (is.null(panel_title)) 0.72 else 1.12, "in"),
                   just = c("left", "top"), gp = grid::gpar(fontsize = 11, col = "grey30")))
  out <- grid::grobTree(ggplotGrob(p), head)
  if (!is.null(set_image) && file.exists(set_image)) {
    img <- if (grepl("\\.jpe?g$", set_image, ignore.case = TRUE)) jpeg::readJPEG(set_image) else readPNG(set_image)
    asp <- dim(img)[1] / dim(img)[2]
    w_in <- min(max(inset_width * win, 2.2), 3.2); w_in <- min(w_in, 0.30 * hgt / asp); h_in <- w_in * asp
    y0 <- unit(hgt, "in") - unit(1.55, "in")
    lab <- sprintf("Set %s%s", set_num %||% "", if (isTRUE(si$source_image %in% c("img_url", "cdn"))) " · image: Rebrickable" else "")
    out <- grid::grobTree(out, grid::grobTree(
      grid::rectGrob(x = x0, y = y0, width = unit(w_in, "in") + unit(8, "pt"), height = unit(h_in, "in") + unit(8, "pt"),
                     just = c("left", "top"), gp = grid::gpar(fill = "white", col = "grey70", lwd = 0.8)),
      grid::rasterGrob(img, x = x0 + unit(4, "pt"), y = y0 - unit(4, "pt"), width = unit(w_in, "in"),
                       height = unit(h_in, "in"), just = c("left", "top"), interpolate = TRUE),
      grid::textGrob(lab, x = x0, y = y0 - unit(h_in, "in") - unit(14, "pt"), just = c("left", "top"),
                     gp = grid::gpar(fontsize = 9, col = "grey35"))))
  }
  ggsave(paste0(out_prefix, "_network_coords_icons.png"), out, width = win, height = hgt, dpi = 300, bg = "white", limitsize = FALSE)
  ggsave(paste0(out_prefix, "_network_coords.pdf"), out, width = win, height = hgt, bg = "white", device = cairo_pdf, limitsize = FALSE)
  write.csv(nd[, c("node", "part", "desc", "category", "subtype", "x0", "y0", "x", "y", "moved", "degree")],
            paste0(out_prefix, "_coords_nodes.csv"), row.names = FALSE)
  write.csv(ed_all, paste0(out_prefix, "_coords_edges.csv"), row.names = FALSE)
  message(sprintf("%s: %d instances, %d edges drawn, %d check removed, %d icons displaced",
                  out_prefix, nrow(nd), nrow(ed), nrow(ed_all) - nrow(ed), sum(nd$moved)))
  invisible(list(nodes = nd, edges = ed, edges_all = ed_all, plot = p))
}

