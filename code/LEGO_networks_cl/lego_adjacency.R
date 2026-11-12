# ============================================================
# LEGO Set 75144 — UCS Snowspeeder (Star Wars, 2017)
# Adjacency Matrix Analysis from .mpd file
# ============================================================
# Mirrors the Python pipeline used to produce the interactive
# JSON widget:  parse → step + spatial adjacency → matrix →
# ggplot2 heatmap → degree bar → igraph network + Louvain
# community detection → category block matrix → CSV export.
#
# Dependencies:
#   install.packages(c("igraph","ggplot2","reshape2","dplyr","scales"))
# ============================================================

library(igraph)
library(ggplot2)
library(reshape2)
library(dplyr)
library(scales)   # for label_comma() in the degree chart
library(here)

# ============================================================
# 1. PARSE THE MPD / LDR FILE
# ============================================================
# LDraw MPD files are multi-part archives: each logical
# sub-assembly is delimited by a "0 FILE <name>" header.
# Build steps inside each sub-assembly are marked "0 STEP".
# Physical part references are type-1 lines:
#   1 <color> <x> <y> <z> <3x3 rotation matrix> <partfile>
# We collect every step across every subfile so we can later
# compute both step co-occurrence and 3-D spatial adjacency.

parse_mpd <- function(filepath) {

  lines <- readLines(filepath, warn = FALSE)

  # ── split file into named sub-assemblies ────────────────
  subfiles  <- list()
  cur_file  <- NULL
  cur_lines <- character(0)

  for (line in lines) {
    line <- trimws(line)
    if (startsWith(line, "0 FILE ")) {
      if (!is.null(cur_file)) subfiles[[cur_file]] <- cur_lines
      cur_file  <- trimws(sub("^0 FILE ", "", line))
      cur_lines <- character(0)
    } else if (line == "0 NOFILE") {
      if (!is.null(cur_file)) subfiles[[cur_file]] <- cur_lines
      cur_file  <- NULL
      cur_lines <- character(0)
    } else {
      if (!is.null(cur_file)) cur_lines <- c(cur_lines, line)
    }
  }
  if (!is.null(cur_file)) subfiles[[cur_file]] <- cur_lines

  # ── parse a single type-1 line into a list ───────────────
  parse_type1 <- function(line) {
    tok <- strsplit(trimws(line), "\\s+")[[1]]
    # Need at least 15 tokens; first token must be "1"
    if (length(tok) < 15 || tok[1] != "1") return(NULL)
    list(
      colour   = suppressWarnings(as.integer(tok[2])),
      x        = suppressWarnings(as.numeric(tok[3])),
      y        = suppressWarnings(as.numeric(tok[4])),
      z        = suppressWarnings(as.numeric(tok[5])),
      partfile = tok[15]          # last field is always the filename
    )
  }

  # ── walk subfiles, split on STEP markers ─────────────────
  all_steps <- list()

  for (fname in names(subfiles)) {
    step_parts <- list()

    for (line in subfiles[[fname]]) {
      if (grepl("^0 STEP", line)) {
        # flush the current accumulator as a finished step
        if (length(step_parts) > 0) {
          all_steps <- c(all_steps,
                         list(list(file = fname, parts = step_parts)))
          step_parts <- list()
        }
      } else if (startsWith(line, "1 ")) {
        p <- parse_type1(line)
        # only keep real named-part .dat references
        if (!is.null(p) && grepl("\\.dat$", p$partfile, ignore.case = TRUE))
          step_parts <- c(step_parts, list(p))
      }
    }
    # flush any trailing parts that weren't followed by a STEP marker
    if (length(step_parts) > 0)
      all_steps <- c(all_steps,
                     list(list(file = fname, parts = step_parts)))
  }

  all_steps
}


# ============================================================
# 2. PRIMITIVE FILTER
# ============================================================
# LDraw geometry primitives (cylinder caps, edge loops, etc.)
# are not real LEGO parts; we exclude them by prefix.
# The 75144 file also uses fraction-notation names like
# "1-4cyli.dat", "2-4ndis.dat" which we also strip out.

is_primitive <- function(p) {
  grepl("^(stug|stud|rect|box|4-4|4-8|t04|s\\\\|48\\\\|joint|connhole)",
        p, ignore.case = TRUE) |
  grepl("^[0-9]+-[0-9]+", p)   # matches "1-4cyli", "2-4ndis", etc.
}


# ============================================================
# 3. PART METADATA  (LDraw ID → human label + category)
# ============================================================
# Part names follow the LDraw Parts Library conventions.
# Categories match those used in the widget:
#   plate | tile | slope | technic | special | brick | bar | snot

PART_NAMES <- c(
  "3023.dat"  = "1x2 Plate",
  "6141.dat"  = "1x1 Round Plate",
  "2412b.dat" = "Grille Tile 1x2",
  "3710.dat"  = "1x4 Plate",
  "3020.dat"  = "2x4 Plate",
  "87079.dat" = "Tile 2x4",
  "3623.dat"  = "1x3 Plate",
  "3040b.dat" = "1x2 Slope 45",
  "2780.dat"  = "Technic Pin (fric)",
  "32028.dat" = "Technic Plate 1x2",
  "6636.dat"  = "Tile 1x6",
  "11203.dat" = "Inv. Macaroni",
  "85984.dat" = "1x2 Slope 31",
  "6179.dat"  = "Tile 4x4 Edge Studs",
  "3666.dat"  = "1x6 Plate",
  "93555.dat" = "Bar Clip Vertical",
  "3022.dat"  = "2x2 Plate",
  "4070.dat"  = "SNOT Brick 1x1",
  "3024.dat"  = "1x1 Plate",
  "3021.dat"  = "2x3 Plate",
  "92946.dat" = "Round Arch 1x1x3",
  "15573.dat" = "Plate 1x2 Groove",
  "54200.dat" = "1x1 Slope 31",
  "3069b.dat" = "Tile 1x2",
  "18677.dat" = "Bracket 1x2-1x2 Inv",
  "85861.dat" = "Round Tile 1x1 Stud",
  "60479.dat" = "Plate 1x2 Handle",
  "3937.dat"  = "Hinge 1x2 Base",
  "2412a.dat" = "Grille Tile 1x2 A",
  "4274.dat"  = "Technic Pin 1x2",
  "2431.dat"  = "Tile 1x4",
  "3062b.dat" = "Round Brick 1x1",
  "3460.dat"  = "1x8 Plate",
  "3070b.dat" = "Tile 1x1",
  "2445.dat"  = "2x12 Plate",
  "44301.dat" = "Hinge Plate 1x2 Bot",
  "6180.dat"  = "Tile 4x4 Round",
  "3795.dat"  = "2x6 Plate",
  "2420.dat"  = "2x2 Corner Plate",
  "32054.dat" = "Technic Pin Long",
  "63864.dat" = "Tile 1x3",
  "98138.dat" = "Round Tile 1x1 Print",
  "4162.dat"  = "Tile 1x8",
  "6134.dat"  = "Technic Bush",
  "43093.dat" = "Technic Axle Pin",
  "6558.dat"  = "Technic Pin 3L",
  "2639.dat"  = "Slope Brick 45 1x2",
  "2357.dat"  = "2x2x2 Corner Brick",
  "3298.dat"  = "2x3 Slope 33",
  "63082.dat" = "Panel 1x4",
  "54384.dat" = "Wedge Plate 4x4 L",
  "54383.dat" = "Wedge Plate 4x4 R"
)

PART_CATS <- c(
  "3023.dat"  = "plate",   "6141.dat"  = "tile",    "2412b.dat" = "tile",
  "3710.dat"  = "plate",   "3020.dat"  = "plate",   "87079.dat" = "tile",
  "3623.dat"  = "plate",   "3040b.dat" = "slope",   "2780.dat"  = "technic",
  "32028.dat" = "technic", "6636.dat"  = "tile",    "11203.dat" = "special",
  "85984.dat" = "slope",   "6179.dat"  = "tile",    "3666.dat"  = "plate",
  "93555.dat" = "bar",     "3022.dat"  = "plate",   "4070.dat"  = "snot",
  "3024.dat"  = "plate",   "3021.dat"  = "plate",   "92946.dat" = "special",
  "15573.dat" = "plate",   "54200.dat" = "slope",   "3069b.dat" = "tile",
  "18677.dat" = "special", "85861.dat" = "tile",    "60479.dat" = "bar",
  "3937.dat"  = "special", "2412a.dat" = "tile",    "4274.dat"  = "technic",
  "2431.dat"  = "tile",    "3062b.dat" = "brick",   "3460.dat"  = "plate",
  "3070b.dat" = "tile",    "2445.dat"  = "plate",   "44301.dat" = "special",
  "6180.dat"  = "tile",    "3795.dat"  = "plate",   "2420.dat"  = "plate",
  "32054.dat" = "technic", "63864.dat" = "tile",    "98138.dat" = "tile",
  "4162.dat"  = "tile",    "6134.dat"  = "technic", "43093.dat" = "technic",
  "6558.dat"  = "technic", "2639.dat"  = "slope",   "2357.dat"  = "brick",
  "3298.dat"  = "slope",   "63082.dat" = "special", "54384.dat" = "slope",
  "54383.dat" = "slope"
)

# Canonical category palette — matches the widget colours exactly
CAT_COLORS <- c(
  plate   = "#378ADD",
  tile    = "#888780",
  slope   = "#BA7517",
  technic = "#1D9E75",
  special = "#D85A30",
  brick   = "#5B9BD5",
  bar     = "#AA7744",
  snot    = "#2AACB8",
  other   = "#AAAAAA"
)

PART_COUNTS <- c(
  "3023.dat" = 58, "6141.dat" = 57, "2412b.dat" = 50, "3710.dat" = 48,
  "3020.dat" = 34, "87079.dat" = 32, "3623.dat" = 28, "3040b.dat" = 24,
  "2780.dat" = 24, "32028.dat" = 23, "6636.dat" = 23, "11203.dat" = 22,
  "85984.dat" = 21, "6179.dat" = 21, "3666.dat" = 20, "93555.dat" = 20,
  "3022.dat" = 19, "4070.dat" = 19, "3024.dat" = 18, "3021.dat" = 18,
  "92946.dat" = 18, "15573.dat" = 17, "54200.dat" = 17, "3069b.dat" = 17,
  "18677.dat" = 16, "85861.dat" = 16, "60479.dat" = 15, "3937.dat" = 14,
  "2412a.dat" = 14, "4274.dat" = 13, "2431.dat" = 13, "3062b.dat" = 13,
  "3460.dat" = 12, "3070b.dat" = 12, "2445.dat" = 12, "44301.dat" = 12,
  "6180.dat" = 11, "3795.dat" = 10, "2420.dat" = 10, "32054.dat" = 10,
  "63864.dat" = 10, "98138.dat" = 9, "4162.dat" = 9, "6134.dat" = 9,
  "43093.dat" = 9, "6558.dat" = 9, "2639.dat" = 8, "2357.dat" = 8,
  "3298.dat" = 8, "63082.dat" = 8, "54384.dat" = 8, "54383.dat" = 8
)


# ============================================================
# 4. BUILD ADJACENCY MATRIX
# ============================================================
# Two parts are marked adjacent when EITHER:
#   (a) Step co-occurrence — they appear in the same build step
#       AND the step has ≤ step_cap parts (avoids spurious
#       all-to-all connections in very large assembly steps), OR
#   (b) Spatial proximity — their 3-D coordinates (stored in
#       the type-1 line) are within prox_threshold LDU.
#       80 LDU ≈ 2 LEGO studs, a physically meaningful limit.

build_adjacency <- function(all_steps,
                            known_parts,
                            prox_threshold = 80,
                            step_cap       = 15) {

  n   <- length(known_parts)
  adj <- matrix(0L, nrow = n, ncol = n,
                dimnames = list(known_parts, known_parts))

  for (step in all_steps) {

    # keep only parts that are in our known set
    pl  <- Filter(function(p) p$partfile %in% known_parts, step$parts)
    if (length(pl) < 2) next

    ids <- sapply(pl, `[[`, "partfile")

    # (a) step co-occurrence — only for small, focused steps
    if (length(pl) <= step_cap) {
      pairs <- combn(ids, 2, simplify = FALSE)
      for (pair in pairs) {
        adj[pair[[1]], pair[[2]]] <- 1L
        adj[pair[[2]], pair[[1]]] <- 1L
      }
    }

    # (b) spatial proximity — applied to every step regardless of size
    for (i in seq_along(pl)) {
      for (j in seq_along(pl)) {
        if (i >= j) next
        dx <- pl[[i]]$x - pl[[j]]$x
        dy <- pl[[i]]$y - pl[[j]]$y
        dz <- pl[[i]]$z - pl[[j]]$z
        if (sqrt(dx^2 + dy^2 + dz^2) < prox_threshold) {
          adj[pl[[i]]$partfile, pl[[j]]$partfile] <- 1L
          adj[pl[[j]]$partfile, pl[[i]]$partfile] <- 1L
        }
      }
    }
  }

  diag(adj) <- 0L   # a part is not adjacent to itself
  adj
}


# ============================================================
# 5. RUN THE PIPELINE
# ============================================================

mpd_path  <- here::here("data/models/75144_-_Snowspeeder.mpd")
# ← update path as needed
all_steps <- parse_mpd(mpd_path)

# High-level parse diagnostics
n_subfiles <- length(unique(sapply(all_steps, `[[`, "file")))
cat(sprintf("Subfiles parsed : %d\n", n_subfiles))
cat(sprintf("Build steps     : %d\n", length(all_steps)))

# Collect every real part seen, excluding primitives
all_raw <- unique(unlist(lapply(all_steps,
                                function(s) sapply(s$parts, `[[`, "partfile"))))
all_raw <- all_raw[!sapply(all_raw, is_primitive)]

# Restrict to parts we have metadata for (the top-52 by frequency)
known_parts <- sort(intersect(all_raw, names(PART_NAMES)))
cat(sprintf("Named parts in matrix : %d\n", length(known_parts)))

adj_matrix  <- build_adjacency(all_steps, known_parts)
total_edges <- sum(adj_matrix) / 2
degrees     <- rowSums(adj_matrix)

cat(sprintf("Total connections : %d\n", total_edges))
cat(sprintf("Most connected    : %s  (degree %d)\n",
            PART_NAMES[names(which.max(degrees))], max(degrees)))
cat(sprintf("Isolated parts    : %d\n", sum(degrees == 0)))


# ============================================================
# 6. ADJACENCY HEATMAP  (mirrors the widget colour scheme)
# ============================================================
# Parts are sorted so members of the same category cluster
# visually — this reveals the block structure that the
# category-filter buttons expose in the widget.

nice_labels <- PART_NAMES[known_parts]

# Sort by category so same-category parts group together on axes
cat_order   <- PART_CATS[known_parts]
sorted_idx  <- order(cat_order, nice_labels)          # stable sort
sorted_labs <- nice_labels[sorted_idx]
sorted_parts <- known_parts[sorted_idx]

# Re-index the matrix to sorted order
adj_sorted <- adj_matrix[sorted_parts, sorted_parts]
rownames(adj_sorted) <- colnames(adj_sorted) <- sorted_labs

adj_long <- melt(adj_sorted,
                 varnames = c("Part_A", "Part_B"),
                 value.name = "Connected")
adj_long$Part_A <- factor(adj_long$Part_A, levels = rev(sorted_labs))
adj_long$Part_B <- factor(adj_long$Part_B, levels = sorted_labs)

# Colour each cell by the category of the *column* part when connected,
# and use a light neutral for empty cells — matching the widget blend
adj_long$Cat_B  <- PART_CATS[sorted_parts][
                     match(as.character(adj_long$Part_B), sorted_labs)]
adj_long$FillCol <- ifelse(
  adj_long$Connected == 1,
  CAT_COLORS[adj_long$Cat_B],
  "#F1EFE8"
)

# Category-colour axis tick labels
axis_colors_x <- CAT_COLORS[PART_CATS[sorted_parts]]
axis_colors_y <- CAT_COLORS[PART_CATS[rev(sorted_parts)]]

p_matrix <- ggplot(adj_long,
                   aes(x = Part_B, y = Part_A, fill = FillCol)) +
  geom_tile(colour = "white", linewidth = 0.25) +
  scale_fill_identity() +             # colours are already computed
  theme_minimal(base_size = 7.5) +
  theme(
    axis.text.x  = element_text(angle = 90, hjust = 1, vjust = 0.5,
                                 size = 6,
                                 colour = axis_colors_x),
    axis.text.y  = element_text(size = 6,
                                 colour = axis_colors_y),
    axis.title   = element_blank(),
    panel.grid   = element_blank(),
    legend.position = "none",
    plot.title   = element_text(size = 10, face = "bold",
                                 margin = margin(b = 4)),
    plot.subtitle = element_text(size = 7.5, colour = "grey50"),
    plot.caption  = element_text(size = 6.5, colour = "grey60",
                                  hjust = 0)
  ) +
  labs(
    title    = "LEGO 75144 \u2014 UCS Snowspeeder: Part Adjacency Matrix",
    subtitle = sprintf("%d part types \u00b7 %d connections \u00b7 "
                       , length(known_parts), total_edges) %>%
               paste0("step co-occurrence (\u226415 parts) + "
                      , "spatial proximity (<80 LDU)"),
    caption  = paste0("Axis label colour = part category.  "
                      , "Connected cell colour = column-part category.")
  )

print(p_matrix)
ggsave("75144_adjacency_matrix.png", p_matrix,
        width = 14, height = 13, dpi = 180)


# ============================================================
# 7. DEGREE BAR CHART  (sorted descending, coloured by category)
# ============================================================
# The degree bar in the widget is sorted by degree descending,
# with each bar carrying the colour of its part's category and
# a small usage-count label (×N) — we replicate all of that here.

ord    <- order(degrees, decreasing = TRUE)
deg_df <- data.frame(
  Part   = factor(nice_labels[ord], levels = nice_labels[ord]),
  Degree = degrees[ord],
  Cat    = PART_CATS[known_parts[ord]],
  Count  = PART_COUNTS[known_parts[ord]],
  stringsAsFactors = FALSE
)

p_degree <- ggplot(deg_df, aes(x = Part, y = Degree, fill = Cat)) +
  geom_col(width = 0.75) +
  geom_text(aes(label = paste0("\u00d7", Count)),
            vjust = -0.35, size = 2, colour = "grey40") +
  scale_fill_manual(values = CAT_COLORS, name = "Category") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
  theme_minimal(base_size = 9) +
  theme(
    axis.text.x  = element_text(angle = 55, hjust = 1, size = 6.5),
    axis.title.x = element_blank(),
    legend.position = "top",
    legend.key.size  = unit(0.4, "cm"),
    plot.title   = element_text(size = 10, face = "bold"),
    plot.subtitle = element_text(size = 8, colour = "grey50")
  ) +
  labs(
    title    = "Connection Degree by Part \u2014 LEGO 75144 UCS Snowspeeder",
    subtitle = "Sorted descending \u00b7 bar label = piece count in set",
    y        = "Degree (number of adjacent part types)"
  )

print(p_degree)
ggsave("75144_degree_distribution.png", p_degree,
         width = 14, height = 6, dpi = 180)


# ============================================================
# 8. IGRAPH NETWORK + LOUVAIN COMMUNITY DETECTION
# ============================================================
# Node size is proportional to degree (same scaling as widget).
# Louvain algorithm finds the natural communities, which
# correspond closely to the category clusters.

g <- graph_from_adjacency_matrix(adj_matrix,
                                  mode  = "undirected",
                                  diag  = FALSE)

V(g)$category <- PART_CATS[known_parts]
V(g)$color    <- CAT_COLORS[V(g)$category]
V(g)$label    <- PART_NAMES[known_parts]
V(g)$size     <- 3 + 1.5 * degree(g)

set.seed(42)
comm           <- cluster_louvain(g)
V(g)$community <- membership(comm)

cat(sprintf("\nLouvain communities : %d  |  Modularity : %.3f\n",
            length(comm), modularity(comm)))
cat("Community members:\n")
invisible(lapply(split(V(g)$label, V(g)$community),
                 function(x) cat("  ", paste(x, collapse = ", "), "\n")))

# Network plot
set.seed(99)
par(mar = c(0, 0, 2.5, 0))
plot(
  g,
  layout             = layout_with_fr(g),
  vertex.color       = V(g)$color,
  vertex.size        = V(g)$size,
  vertex.label       = ifelse(degree(g) > 0, V(g)$label, NA),
  vertex.label.cex   = 0.50,
  vertex.label.color = "black",
  vertex.frame.color = "white",
  edge.color         = "#CCCCCC",
  edge.width         = 0.8,
  main               = "LEGO 75144 \u2014 Part Connectivity Network"
)
legend("topright",
       legend = names(CAT_COLORS),
       fill   = CAT_COLORS,
       bty    = "n", cex = 0.70, title = "Category")


# ============================================================
# 9. NETWORK STATISTICS
# ============================================================

# Restrict path-length metrics to the connected subgraph
# (isolated vertices inflate diameter and average distance)
g_conn <- induced_subgraph(g, V(g)[degree(g) > 0])

stats <- list(
  n_parts_total      = vcount(g),
  n_parts_connected  = vcount(g_conn),
  n_isolated         = sum(degree(g) == 0),
  n_connections      = ecount(g),
  graph_density      = round(edge_density(g), 4),
  avg_degree         = round(mean(degree(g)), 2),
  max_degree         = max(degree(g)),
  diameter_connected = diameter(g_conn),
  avg_path_connected = round(mean_distance(g_conn), 3),
  clustering_global  = round(transitivity(g, type = "global"), 4),
  n_communities      = length(comm),
  modularity         = round(modularity(comm), 4)
)

cat("\n=== Network Statistics ===\n")
for (nm in names(stats))
  cat(sprintf("  %-26s %s\n", nm, stats[[nm]]))


# ============================================================
# 10. SUBFILE-LEVEL SUMMARY
# ============================================================
# Shows which named parts appear in each logical sub-assembly,
# useful for identifying which module (cockpit, engine pod,
# wing, etc.) each part type belongs to.

subfile_parts <- list()
for (step in all_steps) {
  fname  <- step$file
  parts  <- sapply(step$parts, `[[`, "partfile")
  parts  <- parts[parts %in% known_parts]
  if (length(parts) > 0)
    subfile_parts[[fname]] <- unique(c(subfile_parts[[fname]], parts))
}

cat("\n=== Parts per subfile ===\n")
for (sf in names(subfile_parts)) {
  nms <- PART_NAMES[subfile_parts[[sf]]]
  cat(sprintf("  %s\n    %s\n", sf, paste(nms, collapse = ", ")))
}


# ============================================================
# 11. CATEGORY-LEVEL BLOCK MATRIX
# ============================================================
# Collapses the full part×part matrix into a category×category
# matrix where each cell is the total number of cross-category
# connections — answers "how much does the plate world talk
# to the slope world?" at a glance.

cat_levels <- names(CAT_COLORS)
cat_adj    <- matrix(0L,
                     nrow = length(cat_levels),
                     ncol = length(cat_levels),
                     dimnames = list(cat_levels, cat_levels))

for (i in seq_along(known_parts)) {
  for (j in seq_along(known_parts)) {
    if (adj_matrix[i, j] == 1L) {
      ci <- PART_CATS[known_parts[i]]
      cj <- PART_CATS[known_parts[j]]
      if (!is.na(ci) && !is.na(cj))
        cat_adj[ci, cj] <- cat_adj[ci, cj] + 1L
    }
  }
}
# Diagonal counts each within-category edge twice — halve it
diag(cat_adj) <- diag(cat_adj) / 2L

cat_long <- melt(cat_adj,
                 varnames = c("From", "To"),
                 value.name = "Count")

p_cats <- ggplot(cat_long, aes(x = To, y = From, fill = Count)) +
  geom_tile(colour = "white", linewidth = 0.6) +
  geom_text(aes(label = ifelse(Count > 0, Count, "")),
            size = 3.2, colour = "white", fontface = "bold") +
  scale_fill_gradient(low  = "#F1EFE8",
                      high = "#378ADD",
                      name = "Connections") +
  theme_minimal(base_size = 10) +
  theme(
    axis.text.x  = element_text(angle = 30, hjust = 1),
    plot.title   = element_text(size = 10, face = "bold"),
    plot.subtitle = element_text(size = 8, colour = "grey50")
  ) +
  labs(
    title    = "Inter-category Connections \u2014 LEGO 75144",
    subtitle = "Cell = total part-to-part connections across the two categories",
    x = NULL, y = NULL
  )

print(p_cats)
# ggsave("75144_category_adjacency.png", p_cats,
#         width = 7, height = 6.5, dpi = 180)


# ============================================================
# 12. EXPORT
# ============================================================

# Edge list with category columns (matches the widget's
# connection metadata shown on cell-click)
adj_long_full <- melt(adj_sorted,
                       varnames = c("Part_A", "Part_B"),
                       value.name = "Connected")

edges_df <- adj_long_full %>%
  dplyr::filter(Connected == 1,
         as.integer(Part_A) < as.integer(Part_B)) %>%
  dplyr::transmute(
    Part_A = as.character(Part_A),
    Part_B = as.character(Part_B),
    Cat_A  = PART_CATS[sorted_parts[match(Part_A, sorted_labs)]],
    Cat_B  = PART_CATS[sorted_parts[match(Part_B, sorted_labs)]],
    Count_A = PART_COUNTS[sorted_parts[match(Part_A, sorted_labs)]],
    Count_B = PART_COUNTS[sorted_parts[match(Part_B, sorted_labs)]]
  )

write.csv(edges_df,
          "75144_edges.csv",
          row.names = FALSE)

write.csv(as.data.frame(adj_sorted),
          "75144_adjacency_matrix.csv")

cat(sprintf("\nExported %d edges  \u2192  75144_edges.csv\n",
            nrow(edges_df)))
cat("Full matrix        \u2192  75144_adjacency_matrix.csv\n")

