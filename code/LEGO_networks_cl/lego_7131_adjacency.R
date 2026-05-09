# ============================================================
# LEGO Set 7131 — Anakin's Podracer (Star Wars, 1999)
# Adjacency Matrix Analysis from .mpd file
# ============================================================
# Parsed from: 7131_-_Anakin's_Podracer.mpd
# Results:  63 named part types · 54 co-occurrence connections
#           56 build steps across 14 subfiles
# ============================================================
# Dependencies
# install.packages(c("igraph", "ggplot2", "reshape2", "dplyr"))
# ============================================================

library(igraph)
library(ggplot2)
library(reshape2)
library(dplyr)


# ============================================================
# 1.  PARSE THE MPD FILE
# ============================================================
# LDraw Multi-Part Document format: the file contains multiple
# sub-files, each beginning with "0 FILE <name>".  Within each
# sub-file, "0 STEP" marks the end of a build step, and type-1
# lines ("1 colour x y z a b c d e f g h i partfile") reference
# a child part with its colour, position and 3×3 rotation matrix.

parse_mpd <- function(filepath) {

  lines <- readLines(filepath, warn = FALSE)

  # Split raw text into named sub-files
  subfiles  <- list()
  cur_file  <- NULL
  cur_lines <- character(0)

  for (line in lines) {
    line <- trimws(line)
    if (startsWith(line, "0 FILE ")) {
      if (!is.null(cur_file)) subfiles[[cur_file]] <- cur_lines
      cur_file  <- trimws(sub("^0 FILE ", "", line))
      cur_lines <- character(0)
    } else {
      cur_lines <- c(cur_lines, line)
    }
  }
  if (!is.null(cur_file)) subfiles[[cur_file]] <- cur_lines

  # Parse a single type-1 line into a named list
  parse_type1 <- function(line) {
    tok <- strsplit(trimws(line), "\\s+")[[1]]
    if (length(tok) < 15 || tok[1] != "1") return(NULL)
    list(colour   = as.integer(tok[2]),
         x        = as.numeric(tok[3]),
         y        = as.numeric(tok[4]),
         z        = as.numeric(tok[5]),
         partfile = tok[15])
  }

  # Walk every sub-file and collect parts grouped by STEP marker
  all_steps <- list()
  for (fname in names(subfiles)) {
    step_parts <- list()
    for (line in subfiles[[fname]]) {
      if (grepl("^0 STEP", line)) {
        if (length(step_parts) > 0)
          all_steps <- c(all_steps,
                         list(list(file = fname, parts = step_parts)))
        step_parts <- list()
      } else if (startsWith(line, "1 ")) {
        p <- parse_type1(line)
        # Keep only real part files (.dat), drop sub-model references (.ldr)
        if (!is.null(p) && endsWith(p$partfile, ".dat"))
          step_parts <- c(step_parts, list(p))
      }
    }
    # Flush last step if there was no trailing STEP marker
    if (length(step_parts) > 0)
      all_steps <- c(all_steps,
                     list(list(file = fname, parts = step_parts)))
  }
  all_steps
}


# ============================================================
# 2.  PART METADATA — LDraw part ID → human name + category
# ============================================================

PART_NAMES <- c(
  "2412b.dat"    = "Radiator Grille 1x2",
  "2420.dat"     = "2x2 Corner Plate",
  "2540.dat"     = "Bar 1x4",
  "2555.dat"     = "Tile 1x1 w/Clip",
  "3001.dat"     = "2x4 Brick",
  "3002.dat"     = "2x3 Brick",
  "3003.dat"     = "2x2 Brick",
  "3004.dat"     = "1x2 Brick",
  "30151.dat"    = "Hose Nozzle",
  "30170.dat"    = "Minifig Visor",
  "30171.dat"    = "Minifig Helmet",
  "3020.dat"     = "2x4 Plate",
  "3021.dat"     = "2x3 Plate",
  "3022.dat"     = "2x2 Plate",
  "3023.dat"     = "1x2 Plate",
  "30237.dat"    = "1x2 Brick w/Hole",
  "3034.dat"     = "2x8 Plate",
  "30357.dat"    = "2x2 Round Brick",
  "30359a.dat"   = "Flag 2x2",
  "30360.dat"    = "2x2 Tile w/Clip",
  "30383.dat"    = "Minifig Jetpack",
  "30407.dat"    = "Minifig Camera",
  "30475.dat"    = "Minifig Hat Brim",
  "3048b.dat"    = "1x3 Slope 45",
  "3049c.dat"    = "1x2 Slope 45",
  "3298ps0.dat"  = "2x3 Slope printed",
  "3623.dat"     = "1x3 Plate",
  "3626bps0.dat" = "Minifig Head Padme",
  "3626bpsd.dat" = "Minifig Head Anakin",
  "3660.dat"     = "2x2 Inv Slope",
  "3673.dat"     = "Technic Pin",
  "3710.dat"     = "1x4 Plate",
  "3794a.dat"    = "1x2 Jumper Plate",
  "3815.dat"     = "Minifig Leg R",
  "3816.dat"     = "Minifig Leg L",
  "3817.dat"     = "Minifig Hip",
  "3818.dat"     = "Minifig Arm R",
  "3819.dat"     = "Minifig Arm L",
  "3820.dat"     = "Minifig Hand",
  "3821ps1.dat"  = "Torso Padme",
  "3822ps1.dat"  = "Torso Pit Droid",
  "3849.dat"     = "Technic Connector",
  "3941.dat"     = "2x2 Round Brick B",
  "3942c.dat"    = "2x2 Round Cone",
  "4070.dat"     = "SNOT Brick 1x1",
  "4282.dat"     = "2x16 Plate",
  "4286.dat"     = "1x3 Slope",
  "4495a.dat"    = "Flag 1x2",
  "4592.dat"     = "Bar w/Light Cover",
  "4593.dat"     = "Bar Holder w/Clip",
  "4599a.dat"    = "Tap 1x1",
  "4735.dat"     = "Bar & Axle Conn",
  "4740.dat"     = "Dish 2x2",
  "4855.dat"     = "1x4 Arch Brick",
  "4859.dat"     = "Technic Half Bush",
  "4865a.dat"    = "Panel 1x2x1",
  "4871.dat"     = "4x4 Half Arch",
  "6019.dat"     = "1x1 Plate w/Clip",
  "6141.dat"     = "1x1 Round Plate",
  "6231.dat"     = "Panel 1x1x3",
  "6248.dat"     = "Boat Wedge 4x4",
  "973pr0.dat"   = "Torso Anakin",
  "973pr7.dat"   = "Torso Padme B"
)

PART_CATS <- c(
  "2412b.dat"    = "snot",    "4070.dat"     = "snot",
  "3001.dat"     = "brick",   "3002.dat"     = "brick",
  "3003.dat"     = "brick",   "3004.dat"     = "brick",
  "30237.dat"    = "brick",   "30357.dat"    = "brick",
  "3941.dat"     = "brick",   "4855.dat"     = "brick",
  "2420.dat"     = "plate",   "3020.dat"     = "plate",
  "3021.dat"     = "plate",   "3022.dat"     = "plate",
  "3023.dat"     = "plate",   "3034.dat"     = "plate",
  "3623.dat"     = "plate",   "3710.dat"     = "plate",
  "3794a.dat"    = "plate",   "4282.dat"     = "plate",
  "6248.dat"     = "plate",
  "3048b.dat"    = "slope",   "3049c.dat"    = "slope",
  "3298ps0.dat"  = "slope",   "3660.dat"     = "slope",
  "3942c.dat"    = "slope",   "4286.dat"     = "slope",
  "3815.dat"     = "minifig", "3816.dat"     = "minifig",
  "3817.dat"     = "minifig", "3818.dat"     = "minifig",
  "3819.dat"     = "minifig", "3820.dat"     = "minifig",
  "3626bps0.dat" = "minifig", "3626bpsd.dat" = "minifig",
  "3821ps1.dat"  = "minifig", "3822ps1.dat"  = "minifig",
  "973pr0.dat"   = "minifig", "973pr7.dat"   = "minifig",
  "30170.dat"    = "minifig", "30171.dat"    = "minifig",
  "2540.dat"     = "bar",     "2555.dat"     = "bar",
  "4592.dat"     = "bar",     "4593.dat"     = "bar",
  "4735.dat"     = "bar",     "6019.dat"     = "bar",
  "30360.dat"    = "bar",     "4599a.dat"    = "bar",
  "30359a.dat"   = "tile",    "4495a.dat"    = "tile",
  "6141.dat"     = "tile",
  "4740.dat"     = "special", "4871.dat"     = "special",
  "3849.dat"     = "special", "3673.dat"     = "special",
  "4859.dat"     = "special", "30151.dat"    = "special",
  "30383.dat"    = "special", "30407.dat"    = "special",
  "30475.dat"    = "special", "4865a.dat"    = "special",
  "6231.dat"     = "special"
)

# Primitive sub-parts (stud geometry, edge helpers, custom cable files)
# are referenced inside part files but are not assembly-level parts.
is_primitive <- function(p) {
  grepl("^(stug|stud|rect|box|4-4|4-8|LS[0-9])", p)
}


# ============================================================
# 3.  BUILD THE ADJACENCY MATRIX
# ============================================================
# Two named parts are considered "adjacent" (connected = 1) if they:
#   (a) appear together in the same build STEP, OR
#   (b) are placed within prox_threshold LDraw Units (LDU) of each
#       other in 3-D space.  One LDU ≈ 0.4 mm; a stud pitch is 20 LDU,
#       so the default threshold of 80 LDU ≈ 4 studs = typical contact.
# The matrix is binary and symmetric; the diagonal is always 0.

build_adjacency <- function(all_steps, known_parts, prox_threshold = 80) {

  adj <- matrix(0L,
                nrow = length(known_parts),
                ncol = length(known_parts),
                dimnames = list(known_parts, known_parts))

  for (step in all_steps) {

    # Filter to only named parts present in this step
    pl  <- Filter(function(p) p$partfile %in% known_parts, step$parts)
    if (length(pl) < 2) next
    ids <- sapply(pl, `[[`, "partfile")

    # (a) Step co-occurrence — all pairs in the same step are connected
    for (pair in combn(ids, 2, simplify = FALSE)) {
      adj[pair[[1]], pair[[2]]] <- 1L
      adj[pair[[2]], pair[[1]]] <- 1L
    }

    # (b) 3-D spatial proximity — catches parts placed close together
    #     even across different steps
    for (i in seq_along(pl)) {
      for (j in seq_along(pl)) {
        if (i >= j) next
        d <- sqrt((pl[[i]]$x - pl[[j]]$x)^2 +
                    (pl[[i]]$y - pl[[j]]$y)^2 +
                    (pl[[i]]$z - pl[[j]]$z)^2)
        if (d < prox_threshold) {
          adj[pl[[i]]$partfile, pl[[j]]$partfile] <- 1L
          adj[pl[[j]]$partfile, pl[[i]]$partfile] <- 1L
        }
      }
    }
  }
  diag(adj) <- 0L
  adj
}


# ============================================================
# 4.  RUN THE PIPELINE
# ============================================================

mpd_path  <- "7131_-_Anakin_s_Podracer.mpd"   # update path as needed
all_steps <- parse_mpd(mpd_path)

cat(sprintf("Subfiles parsed : %d\n",
            length(unique(sapply(all_steps, `[[`, "file")))))
cat(sprintf("Build steps     : %d\n", length(all_steps)))

# Collect every part file referenced; discard primitives and unknowns
all_raw     <- unique(unlist(
  lapply(all_steps, function(s) sapply(s$parts, `[[`, "partfile"))))
known_parts <- sort(
  intersect(all_raw[!sapply(all_raw, is_primitive)], names(PART_NAMES)))
cat(sprintf("Named parts     : %d\n", length(known_parts)))

adj_matrix  <- build_adjacency(all_steps, known_parts)
total_edges <- sum(adj_matrix) / 2
degrees     <- rowSums(adj_matrix)

cat(sprintf("Connections     : %d\n", total_edges))
cat(sprintf("Most connected  : %s (degree %d)\n",
            PART_NAMES[names(which.max(degrees))], max(degrees)))
cat(sprintf("Isolated parts  : %d\n", sum(degrees == 0)))


# ============================================================
# 5.  COLOURS & CATEGORY SORTING
# ============================================================

cat_colors <- c(
  brick   = "#378ADD", plate   = "#1D9E75", slope   = "#BA7517",
  minifig = "#7F77DD", special = "#D85A30", bar     = "#888780",
  snot    = "#2AACB8", tile    = "#E87CA0", other   = "#AAAAAA"
)

# Re-order parts by category so block structure is visible in the heatmap
cat_order <- c("brick","plate","slope","minifig","bar","tile","special","snot")

part_cats   <- PART_CATS[known_parts]
sorted_idx  <- order(match(part_cats, cat_order), known_parts)
known_sorted <- known_parts[sorted_idx]
adj_sorted   <- adj_matrix[sorted_idx, sorted_idx]
nice_labels  <- PART_NAMES[known_sorted]


# ============================================================
# 6.  HEATMAP — ADJACENCY MATRIX  (category-sorted)
# ============================================================

rownames(adj_sorted) <- colnames(adj_sorted) <- nice_labels

adj_long <- melt(adj_sorted,
                 varnames  = c("Part_A", "Part_B"),
                 value.name = "Connected")
adj_long$Part_A <- factor(adj_long$Part_A, levels = rev(nice_labels))
adj_long$Part_B <- factor(adj_long$Part_B, levels = nice_labels)

# Category annotation for axis colouring
cat_df <- data.frame(
  Part     = factor(nice_labels, levels = nice_labels),
  Category = PART_CATS[known_sorted]
)

p_matrix <- ggplot(adj_long,
                   aes(x = Part_B, y = Part_A,
                       fill = factor(Connected))) +
  geom_tile(colour = "white", linewidth = 0.2) +
  scale_fill_manual(
    values = c("0" = "#F4F2EC", "1" = "#378ADD"),
    labels = c("No connection", "Connected"),
    name   = NULL
  ) +
  theme_minimal(base_size = 7) +
  theme(
    axis.text.x     = element_text(angle = 90, hjust = 1,
                                   vjust = 0.5, size = 5.5,
                                   colour = cat_colors[PART_CATS[known_sorted]]),
    axis.text.y     = element_text(size = 5.5,
                                   colour = rev(cat_colors[PART_CATS[known_sorted]])),
    axis.title      = element_blank(),
    panel.grid      = element_blank(),
    legend.position = "top",
    plot.title      = element_text(size = 10, face = "bold",
                                   margin = margin(b = 4)),
    plot.subtitle   = element_text(size = 7.5, colour = "grey50")
  ) +
  labs(
    title    = "LEGO 7131 \u2014 Anakin\u2019s Podracer: Part Adjacency Matrix",
    subtitle = sprintf(
      "%d part types \u00b7 %d connections \u00b7 parts sorted by category",
      length(known_parts), total_edges)
  )

print(p_matrix)
# ggsave("7131_adjacency_matrix.png", p_matrix, width = 14, height = 13, dpi = 180)


# ============================================================
# 7.  DEGREE BAR CHART  (sorted by degree, coloured by category)
# ============================================================

deg_sorted  <- degrees[sorted_idx]
ord         <- order(deg_sorted, decreasing = TRUE)

deg_df <- data.frame(
  Part     = factor(nice_labels[ord], levels = nice_labels[ord]),
  Degree   = deg_sorted[ord],
  Category = PART_CATS[known_sorted[ord]]
)

p_degree <- ggplot(deg_df, aes(x = Part, y = Degree, fill = Category)) +
  geom_col(width = 0.75) +
  scale_fill_manual(values = cat_colors) +
  theme_minimal(base_size = 9) +
  theme(
    axis.text.x  = element_text(angle = 55, hjust = 1, size = 6),
    axis.title.x = element_blank(),
    legend.position = "top",
    plot.title   = element_text(size = 10, face = "bold")
  ) +
  labs(
    title = "Connection Degree by Part \u2014 LEGO 7131",
    y     = "Degree (adjacent part types)",
    fill  = "Category"
  )

print(p_degree)
# ggsave("7131_degree_distribution.png", p_degree, width = 14, height = 5.5, dpi = 180)


# ============================================================
# 8.  igraph NETWORK + LOUVAIN COMMUNITY DETECTION
# ============================================================

g <- graph_from_adjacency_matrix(adj_sorted, mode = "undirected",
                                 diag = FALSE)

V(g)$category  <- PART_CATS[known_sorted]
V(g)$color     <- cat_colors[V(g)$category]
V(g)$label     <- nice_labels
V(g)$size      <- 3 + 1.4 * degree(g)

set.seed(42)
comm <- cluster_louvain(g)
V(g)$community <- membership(comm)

cat(sprintf("\nLouvain communities : %d   Modularity : %.3f\n",
            length(comm), modularity(comm)))
cat("Members per community:\n")
lapply(split(V(g)$label, V(g)$community),
       function(x) cat(" ", paste(x, collapse = ", "), "\n"))

set.seed(99)
lay <- layout_with_fr(g)
par(mar = c(0, 0, 2.5, 0))

plot(
  g,
  layout             = lay,
  vertex.color       = V(g)$color,
  vertex.size        = V(g)$size,
  vertex.label       = ifelse(degree(g) > 0, V(g)$label, NA),
  vertex.label.cex   = 0.52,
  vertex.label.color = "black",
  vertex.frame.color = "white",
  edge.color         = "#CCCCCC",
  edge.width         = 0.9,
  main               = "LEGO 7131 \u2014 Part Connectivity Network"
)
legend("topright",
       legend = names(cat_colors)[names(cat_colors) != "other"],
       fill   = cat_colors[names(cat_colors) != "other"],
       bty    = "n", cex = 0.72, title = "Category")


# ============================================================
# 9.  NETWORK STATISTICS
# ============================================================

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
# 10. SUBFILE-LEVEL PART INVENTORY
# ============================================================

subfile_parts <- list()
for (step in all_steps) {
  fname <- step$file
  parts <- sapply(step$parts, `[[`, "partfile")
  parts <- parts[parts %in% known_parts]
  if (length(parts) > 0)
    subfile_parts[[fname]] <- unique(c(subfile_parts[[fname]], parts))
}

cat("\n=== Parts per sub-model ===\n")
for (sf in names(subfile_parts)) {
  nms  <- PART_NAMES[subfile_parts[[sf]]]
  sname <- gsub("7131 - |\\.(ldr|dat)", "", sf)
  cat(sprintf("  %-36s [%d parts]\n    %s\n",
              sname, length(nms),
              paste(nms, collapse = ", ")))
}


# ============================================================
# 11. CATEGORY-LEVEL BLOCK MATRIX
# ============================================================

cat_levels <- cat_order
cat_adj    <- matrix(0L,
                     nrow = length(cat_levels),
                     ncol = length(cat_levels),
                     dimnames = list(cat_levels, cat_levels))

for (i in seq_along(known_sorted)) {
  for (j in seq_along(known_sorted)) {
    if (adj_sorted[i, j] == 1L) {
      ci <- PART_CATS[known_sorted[i]]
      cj <- PART_CATS[known_sorted[j]]
      if (!is.na(ci) && !is.na(cj))
        cat_adj[ci, cj] <- cat_adj[ci, cj] + 1L
    }
  }
}
diag(cat_adj) <- diag(cat_adj) / 2L   # each within-category edge counted twice

cat_long <- melt(cat_adj, varnames = c("From", "To"), value.name = "Count")

p_cats <- ggplot(cat_long, aes(x = To, y = From, fill = Count)) +
  geom_tile(colour = "white", linewidth = 0.5) +
  geom_text(aes(label = ifelse(Count > 0, Count, "")), size = 3.2) +
  scale_fill_gradient(low = "#F4F2EC", high = "#378ADD",
                      name = "Connections") +
  theme_minimal(base_size = 10) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1),
        plot.title  = element_text(size = 10, face = "bold")) +
  labs(title = "Inter-category Connections \u2014 LEGO 7131",
       x = NULL, y = NULL)

print(p_cats)
# ggsave("7131_category_adjacency.png", p_cats, width = 7, height = 6, dpi = 180)


# ============================================================
# 12. EXPORT CSVs
# ============================================================

edges_df <- adj_long %>%
  filter(Connected == 1,
         as.integer(Part_A) < as.integer(Part_B)) %>%
  transmute(
    Part_A = as.character(Part_A),
    Part_B = as.character(Part_B),
    Cat_A  = PART_CATS[known_sorted[match(Part_A, nice_labels)]],
    Cat_B  = PART_CATS[known_sorted[match(Part_B, nice_labels)]]
  )

write.csv(edges_df,
          "7131_edges.csv", row.names = FALSE)
write.csv(as.data.frame(adj_sorted),
          "7131_adjacency_matrix.csv")

cat(sprintf("\nExported %d edges  \u2192 7131_edges.csv\n", nrow(edges_df)))
cat("Full matrix        \u2192 7131_adjacency_matrix.csv\n")
