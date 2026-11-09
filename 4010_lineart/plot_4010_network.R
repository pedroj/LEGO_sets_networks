# Network of observed part-type connections in LEGO 4010 (Police Rescue Boat)
# Input : 4010_edges.csv (run_lego_adjacency), icon_map.csv + icons_lineart/*.png (render_ldraw_lineart.py)
# Output: 4010_network_icons.png / .pdf
library(igraph); library(ggplot2); library(png)

SET_NUM <- "4010"; SET_NAME <- "Police Rescue Boat"; SET_YEAR <- 1987   # sets.csv (Rebrickable)
SET_IMG <- "set_4010-1.png"            # https://cdn.rebrickable.com/media/sets/4010-1.jpg
e  <- read.csv("4010_edges.csv", stringsAsFactors = FALSE)
im <- read.csv("icon_map.csv", stringsAsFactors = FALSE)
FAM_COL <- c(brick="#4E79A7", plate="#59A14F", tile="#8CD17D", slope="#B07AA1", round="#499894",
             cone="#86BCB6", baseplate="#A0CBE8", panel="#F1CE63", bracket="#B6992D", technic="#E15759",
             wheel="#79706E", wheel_holder="#D37295", tyre="#4B4B4B", minifig="#FF9D9A", other="#BAB0AC")

# ---- instance edges -> part-type edges (weight = number of instance contacts) ----
ed <- data.frame(a = pmin(e$num_A, e$num_B), b = pmax(e$num_A, e$num_B))
ed <- ed[ed$a != ed$b, ]
ed <- aggregate(list(w = rep(1, nrow(ed))), ed, sum)
nodes <- unique(rbind(data.frame(num = e$num_A, name = e$name_A, fam = e$fam_A),
                      data.frame(num = e$num_B, name = e$name_B, fam = e$fam_B)))
cnt <- table(c(sub("#.*", "", e$part_A), sub("#.*", "", e$part_B)))   # placeholder, replaced below
inst <- read.csv("4010_adjacency_matrix.csv", row.names = 1, check.names = FALSE)
n_inst <- table(sub("#.*", "", rownames(inst)))
nodes$n <- as.integer(n_inst[nodes$num]); nodes$n[is.na(nodes$n)] <- 1L
nodes$fam[is.na(nodes$fam) | !nodes$fam %in% names(FAM_COL)] <- "other"

g <- graph_from_data_frame(ed, directed = FALSE, vertices = nodes)
nodes$deg <- degree(g)[nodes$num]

# ---- layout: families = clusters placed on a big circle (big and small alternate);
#      members of a family sit on a ring sized to its count; labels point radially outward ----
fam_n <- table(nodes$fam)[names(FAM_COL)[names(FAM_COL) %in% nodes$fam]]
srt <- names(sort(fam_n, decreasing = TRUE))
fam_order <- character(0)
while (length(srt)) { fam_order <- c(fam_order, srt[1]); srt <- srt[-1]
                      if (length(srt)) { fam_order <- c(fam_order, srt[length(srt)]); srt <- srt[-length(srt)] } }
nf <- length(fam_order)
ang_f <- setNames(pi / 2 - 2 * pi * (seq_len(nf) - 1) / nf, fam_order)
CEN <- list(minifig = c(.30, .70), plate = c(.78, .74), slope = c(.78, .30), brick = c(.30, .24),
            other = c(.06, .42), panel = c(.52, .47), tile = c(.50, .10), bracket = c(.66, .10),
            technic = c(.97, .52), wheel = c(.62, .90), round = c(.9, .9), cone = c(.9, .1),
            baseplate = c(.1, .1), wheel_holder = c(.1, .9), tyre = c(.9, .5))
nodes$x <- nodes$y <- nodes$cx <- nodes$cy <- NA_real_
fam_lab <- data.frame(fam = fam_order, n = NA_integer_, x = NA_real_, y = NA_real_)
for (i in seq_along(fam_order)) {
  f <- fam_order[i]; ii <- which(nodes$fam == f); m <- length(ii)
  rr <- if (m == 1) 0 else max(0.034, 0.0098 * m)
  cx <- CEN[[f]][1]; cy <- CEN[[f]][2]
  a  <- pi / 2 + 2 * pi * (seq_len(m) - 1) / m
  nodes$x[ii] <- cx + rr * cos(a); nodes$y[ii] <- cy + rr * sin(a)
  nodes$cx[ii] <- cx; nodes$cy[ii] <- cy
  fam_lab$n[i] <- m
  fam_lab$x[i] <- cx; fam_lab$y[i] <- if (m >= 8) cy else cy + rr + 0.085
}

pos <- setNames(seq_len(nrow(nodes)), nodes$num)
seg <- data.frame(x = nodes$x[pos[ed$a]], y = nodes$y[pos[ed$a]],
                  xend = nodes$x[pos[ed$b]], yend = nodes$y[pos[ed$b]], w = ed$w)

r <- 0.0265
p <- ggplot() +
  geom_segment(data = seg, aes(x, y, xend = xend, yend = yend, linewidth = w),
               colour = "grey55", alpha = 0.28, lineend = "round") +
  scale_linewidth(range = c(0.15, 1.6), name = "Instance contacts") +
  geom_point(data = nodes, aes(x, y, fill = fam), shape = 21, size = 16.4, colour = "white", stroke = 0) +
  geom_point(data = nodes, aes(x, y), shape = 21, size = 15.4, fill = "white", colour = "white", stroke = 0) +
  scale_fill_manual(values = FAM_COL, name = "Family", breaks = fam_order) +
  guides(fill = guide_legend(override.aes = list(size = 6))) +
  coord_fixed(xlim = c(-0.26, 1.12), ylim = c(-0.04, 1.0), clip = "off") +
  labs(title = paste0("LEGO ", SET_NUM, " — ", SET_NAME, " (", SET_YEAR, ")  |  observed part connection network"),
       subtitle = sprintf("%d part types | %d type-type links | %d part instances. Edge width = number of instance contacts. Icons rendered from the LDraw library.",
                          nrow(nodes), nrow(ed), sum(nodes$n)),
       caption = "Connections: proximity AND family compatibility (lego-mpd-adjacency). Ring colour = part family. xN = instances in the set.") +
  theme_void(base_size = 11) +
  theme(plot.background = element_rect(fill = "white", colour = NA),
        plot.title = element_text(face = "bold", size = 16), plot.subtitle = element_text(colour = "grey35"),
        legend.position = "right", plot.margin = margin(10, 10, 10, 10))

for (k in seq_len(nrow(nodes))) {
  f <- file.path("icons_lineart", paste0(im$icon[im$subtype == nodes$num[k]], ".png"))
  p <- p + annotation_raster(readPNG(f), nodes$x[k] - r, nodes$x[k] + r, nodes$y[k] - r, nodes$y[k] + r, interpolate = TRUE)
}
# ---- inset: set photo, top-left corner, with thin frame ----
img <- readPNG(SET_IMG); iw <- 0.30; ih <- iw * nrow(img) / ncol(img)
ix0 <- -0.25; iy1 <- 0.995
p <- p + annotate("rect", xmin = ix0 - 0.006, xmax = ix0 + iw + 0.006, ymin = iy1 - ih - 0.006, ymax = iy1 + 0.006,
                  fill = "white", colour = "grey60", linewidth = 0.4) +
  annotation_raster(img, ix0, ix0 + iw, iy1 - ih, iy1, interpolate = TRUE) +
  annotate("text", x = ix0 + iw / 2, y = iy1 - ih - 0.022, size = 3.2, colour = "grey25", fontface = "bold",
           label = paste0("LEGO ", SET_NUM, "-1 ", SET_NAME)) +
  annotate("text", x = ix0 + iw / 2, y = iy1 - ih - 0.042, size = 2.6, colour = "grey40",
           label = paste0(SET_YEAR, " | image: rebrickable.com"))
short <- ifelse(nchar(nodes$name) > 40, paste0(substr(nodes$name, 1, 38), "..."), nodes$name)
nodes$lab <- paste0(nodes$num, " x", nodes$n, "\n",
                    vapply(strwrap(short, 22, simplify = FALSE), function(z) paste(head(z, 3), collapse = "\n"), ""))
dx <- nodes$x - nodes$cx; dy <- nodes$y - nodes$cy
single <- abs(dx) + abs(dy) < 1e-9
dx[single] <- nodes$x[single] - 0.5; dy[single] <- nodes$y[single] - 0.5
th <- atan2(dy, dx)
nodes$lx <- nodes$x + (r + 0.006) * cos(th); nodes$ly <- nodes$y + (r + 0.006) * sin(th)
nodes$hj <- ifelse(cos(th) > 0.35, 0, ifelse(cos(th) < -0.35, 1, 0.5))
nodes$vj <- ifelse(abs(cos(th)) > 0.35, 0.5, ifelse(sin(th) > 0, 0, 1))
p <- p + geom_text(data = nodes, aes(lx, ly, label = lab, hjust = hj, vjust = vj), size = 2.0,
                   colour = "grey15", lineheight = 0.88) +
  geom_text(data = fam_lab, aes(x, y, label = toupper(fam)), fontface = "bold", size = 5.2,
            colour = FAM_COL[fam_lab$fam])

ggsave("4010_network_icons.png", p, width = 16, height = 11.4, dpi = 300, bg = "white")
ggsave("4010_network_icons.pdf", p, width = 16, height = 11.4, bg = "white", device = cairo_pdf)
write.csv(nodes[order(-nodes$deg), c("fam", "num", "name", "n", "deg")], "4010_type_degree.csv", row.names = FALSE)
cat("nodes", vcount(g), "edges", ecount(g), "\n")
