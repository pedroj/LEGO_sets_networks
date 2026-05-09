# ============================================================
# LEGO Set 4010 — Police Rescue Boat
# Adjacency Matrix Analysis from .mpd file
# ============================================================
# Dependencies: igraph, ggplot2, reshape2, dplyr, stringr
# Install: install.packages(c("igraph","ggplot2","reshape2","dplyr","stringr"))
# ============================================================

library(igraph)
library(ggplot2)
library(reshape2)
library(dplyr)
library(stringr)


# ============================================================
# 1. PARSE THE MPD FILE
# ============================================================

parse_mpd <- function(filepath) {
  lines <- readLines(filepath, warn = FALSE)
  
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
  
  # Parse type-1 lines: 1 colour x y z a b c d e f g h i partfile
  parse_type1 <- function(line) {
    tokens <- strsplit(trimws(line), "\\s+")[[1]]
    if (length(tokens) < 15 || tokens[1] != "1") return(NULL)
    list(
      colour   = as.integer(tokens[2]),
      x        = as.numeric(tokens[3]),
      y        = as.numeric(tokens[4]),
      z        = as.numeric(tokens[5]),
      partfile = tokens[15]
    )
  }
  
  # Walk all subfiles, split by STEP markers
  all_steps <- list()
  for (fname in names(subfiles)) {
    step_parts <- list()
    for (line in subfiles[[fname]]) {
      if (grepl("^0 STEP", line)) {
        if (length(step_parts) > 0) {
          all_steps <- c(all_steps, list(list(file = fname, parts = step_parts)))
          step_parts <- list()
        }
      } else if (startsWith(line, "1 ")) {
        p <- parse_type1(line)
        if (!is.null(p) && endsWith(p$partfile, ".dat")) {
          step_parts <- c(step_parts, list(p))
        }
      }
    }
    if (length(step_parts) > 0)
      all_steps <- c(all_steps, list(list(file = fname, parts = step_parts)))
  }
  
  all_steps
}


# ============================================================
# 2. PART METADATA
# ============================================================

PART_NAMES <- c(
  "2348a.dat" = "2x2 Slope Corner",    "2349a.dat" = "1x4 Slope Corner",
  "3002.dat"  = "2x3 Brick",           "3003.dat"  = "2x2 Brick",
  "3004.dat"  = "1x2 Brick",           "3005.dat"  = "1x1 Brick",
  "3021.dat"  = "2x3 Plate",           "3023.dat"  = "1x2 Plate",
  "3024.dat"  = "1x1 Plate",           "3034.dat"  = "1x2 Plate thick",
  "3040b.dat" = "1x2 Slope 45",        "3070b.dat" = "1x1 Tile",
  "3297.dat"  = "3x4 Slope",           "3460.dat"  = "1x8 Plate",
  "3624.dat"  = "Minifig Hand",        "3626bp01.dat" = "Minifig Head",
  "3665.dat"  = "1x2 Inv Slope",       "3794a.dat" = "1x2 Jumper Plate",
  "3795.dat"  = "2x6 Plate",           "3815.dat"  = "Minifig Leg R",
  "3816.dat"  = "Minifig Leg L",       "3817.dat"  = "Minifig Hip",
  "3818.dat"  = "Minifig Arm R",       "3819.dat"  = "Minifig Arm L",
  "3820.dat"  = "Minifig Hand B",      "3829c01.dat" = "Steering Wheel",
  "3838.dat"  = "Flippers",            "3842a.dat" = "Life Preserver",
  "3939.dat"  = "2x4 Arch",            "3958.dat"  = "4x4 Plate",
  "3962a.dat" = "Torch",               "4073.dat"  = "1x1 Round Plate",
  "4079.dat"  = "Seat 2x2",            "4286.dat"  = "1x3 Slope",
  "4289.dat"  = "1x1 Brick Stud",      "4349.dat"  = "Boat Bow Stud",
  "4599a.dat" = "Tap 1x1",             "4623.dat"  = "1x2 Rail Plate",
  "4625.dat"  = "4x4 Hinge Plate",     "4740.dat"  = "Dish 2x2",
  "4862.dat"  = "1x2 Brick Knob",      "4863.dat"  = "1x2 Slope Curved",
  "4866.dat"  = "Windscreen",          "4873.dat"  = "Lattice Fence",
  "6141.dat"  = "1x1 Round Plate B",   "73090b.dat" = "Boat Hull",
  "973p1f.dat" = "Torso Police",       "973p1g.dat" = "Torso Civilian"
)

PART_CATS <- c(
  "2348a.dat" = "slope",  "2349a.dat" = "slope",
  "3002.dat"  = "brick",  "3003.dat"  = "brick",  "3004.dat" = "brick",
  "3005.dat"  = "brick",  "3939.dat"  = "brick",  "4862.dat" = "brick",
  "3021.dat"  = "plate",  "3023.dat"  = "plate",  "3024.dat" = "plate",
  "3034.dat"  = "plate",  "3460.dat"  = "plate",  "3794a.dat"= "plate",
  "3795.dat"  = "plate",  "3958.dat"  = "plate",  "4349.dat" = "plate",
  "4623.dat"  = "plate",  "4625.dat"  = "plate",
  "3040b.dat" = "slope",  "3297.dat"  = "slope",  "3665.dat" = "slope",
  "4286.dat"  = "slope",  "4863.dat"  = "slope",
  "3624.dat"  = "minifig","3626bp01.dat"="minifig","3815.dat" = "minifig",
  "3816.dat"  = "minifig","3817.dat"  = "minifig","3818.dat" = "minifig",
  "3819.dat"  = "minifig","3820.dat"  = "minifig","973p1f.dat"= "minifig",
  "973p1g.dat"= "minifig",
  "3829c01.dat"="special","3838.dat"  = "special","3842a.dat"= "special",
  "3962a.dat" = "special","4079.dat"  = "special","4740.dat" = "special",
  "4866.dat"  = "special","4873.dat"  = "special","73090b.dat"= "special",
  "4599a.dat" = "special",
  "3070b.dat" = "tile",   "4073.dat"  = "tile",   "4289.dat" = "tile",
  "6141.dat"  = "tile"
)

# Parts considered LDraw primitives — excluded from the main analysis
is_primitive <- function(part) {
  grepl("^(stug|stud|rect|box|4-4|4-8)", part)
}


# ============================================================
# 3. BUILD ADJACENCY MATRIX
# ============================================================

build_adjacency <- function(all_steps, known_parts, prox_threshold = 80) {
  # Two parts are adjacent if they:
  #   (a) co-occur in the same BUILD STEP  (step-based adjacency)
  #   (b) are within `prox_threshold` LDU in 3D space (spatial adjacency)
  
  adj <- matrix(0L, nrow = length(known_parts), ncol = length(known_parts),
                dimnames = list(known_parts, known_parts))
  
  for (step in all_steps) {
    plist <- Filter(function(p) p$partfile %in% known_parts, step$parts)
    if (length(plist) < 2) next
    
    ids <- sapply(plist, `[[`, "partfile")
    
    # (a) Step co-occurrence
    combos <- combn(ids, 2, simplify = FALSE)
    for (pair in combos) {
      adj[pair[[1]], pair[[2]]] <- 1L
      adj[pair[[2]], pair[[1]]] <- 1L
    }
    
    # (b) Spatial proximity
    for (i in seq_along(plist)) {
      for (j in seq_along(plist)) {
        if (i >= j) next
        dx <- plist[[i]]$x - plist[[j]]$x
        dy <- plist[[i]]$y - plist[[j]]$y
        dz <- plist[[i]]$z - plist[[j]]$z
        if (sqrt(dx^2 + dy^2 + dz^2) < prox_threshold) {
          adj[plist[[i]]$partfile, plist[[j]]$partfile] <- 1L
          adj[plist[[j]]$partfile, plist[[i]]$partfile] <- 1L
        }
      }
    }
  }
  diag(adj) <- 0L
  adj
}


# ============================================================
# 4. RUN THE PIPELINE
# ============================================================

mpd_path  <- "4010_-_Police_rescue_boat.mpd"   # adjust path as needed
all_steps <- parse_mpd(mpd_path)

# Collect non-primitive parts that appear in known metadata
all_raw <- unique(unlist(lapply(all_steps, function(s) sapply(s$parts, `[[`, "partfile"))))
known_parts <- sort(intersect(
  all_raw[!sapply(all_raw, is_primitive)],
  names(PART_NAMES)
))

cat(sprintf("Parts used in matrix: %d\n", length(known_parts)))

adj_matrix <- build_adjacency(all_steps, known_parts)

# Summary stats
total_edges <- sum(adj_matrix) / 2
degrees     <- rowSums(adj_matrix)
cat(sprintf("Total connections: %d\n", total_edges))
cat(sprintf("Most connected part: %s (degree %d)\n",
            PART_NAMES[names(which.max(degrees))], max(degrees)))


# ============================================================
# 5. HEATMAP — ADJACENCY MATRIX
# ============================================================

# Label matrix with human-readable names
nice_labels <- PART_NAMES[known_parts]
rownames(adj_matrix) <- colnames(adj_matrix) <- nice_labels

adj_long <- melt(adj_matrix, varnames = c("Part_A", "Part_B"), value.name = "Connected")
adj_long$Part_A <- factor(adj_long$Part_A, levels = rev(nice_labels))
adj_long$Part_B <- factor(adj_long$Part_B, levels = nice_labels)

# Category colour strip data
cat_df <- data.frame(
  Part = factor(nice_labels, levels = nice_labels),
  Category = PART_CATS[known_parts]
)
cat_colors <- c(brick = "#378ADD", plate = "#1D9E75", slope = "#BA7517",
                minifig = "#7F77DD", special = "#D85A30", tile = "#888780",
                other = "#AAAAAA")

p_matrix <- ggplot(adj_long, aes(x = Part_B, y = Part_A, fill = factor(Connected))) +
  geom_tile(colour = "white", linewidth = 0.3) +
  scale_fill_manual(
    values = c("0" = "#F1EFE8", "1" = "#378ADD"),
    labels = c("No connection", "Connected"),
    name   = NULL
  ) +
  theme_minimal(base_size = 8) +
  theme(
    axis.text.x  = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 6.5),
    axis.text.y  = element_text(size = 6.5),
    axis.title   = element_blank(),
    panel.grid   = element_blank(),
    legend.position = "top",
    plot.title   = element_text(size = 11, face = "bold", margin = margin(b = 6)),
    plot.subtitle= element_text(size = 8, colour = "grey50")
  ) +
  labs(
    title    = "LEGO 4010 — Police Rescue Boat: Part Adjacency Matrix",
    subtitle = sprintf("%d part types · %d connections", length(known_parts), total_edges)
  )

print(p_matrix)
# ggsave("4010_adjacency_matrix.png", p_matrix, width = 12, height = 11, dpi = 180)


# ============================================================
# 6. DEGREE DISTRIBUTION BAR CHART
# ============================================================

deg_df <- data.frame(
  Part     = factor(nice_labels, levels = nice_labels[order(degrees, decreasing = TRUE)]),
  Degree   = degrees[order(degrees, decreasing = TRUE)],
  Category = PART_CATS[known_parts][order(degrees, decreasing = TRUE)]
)

p_degree <- ggplot(deg_df, aes(x = Part, y = Degree, fill = Category)) +
  geom_col(width = 0.75) +
  scale_fill_manual(values = cat_colors) +
  theme_minimal(base_size = 9) +
  theme(
    axis.text.x  = element_text(angle = 55, hjust = 1, size = 7),
    axis.title.x = element_blank(),
    legend.position = "top",
    plot.title   = element_text(size = 11, face = "bold")
  ) +
  labs(
    title = "Connection Degree by Part",
    y     = "Degree (number of adjacent parts)",
    fill  = "Category"
  )

print(p_degree)
# ggsave("4010_degree_distribution.png", p_degree, width = 13, height = 6, dpi = 180)


# ============================================================
# 7. GRAPH NETWORK WITH igraph
# ============================================================

g <- graph_from_adjacency_matrix(adj_matrix, mode = "undirected", diag = FALSE)

# Assign vertex attributes
V(g)$category <- PART_CATS[known_parts]
V(g)$color    <- cat_colors[V(g)$category]
V(g)$label    <- PART_NAMES[known_parts]
V(g)$size     <- 4 + 1.5 * degree(g)   # size proportional to connectivity

# Community detection (Louvain)
communities <- cluster_louvain(g)
V(g)$community <- membership(communities)

cat(sprintf("Communities detected: %d\n", length(communities)))
cat("Community membership:\n")
print(split(V(g)$label, V(g)$community))

# Plot network
par(mar = c(0, 0, 2, 0))
plot(
  g,
  layout          = layout_with_fr(g),
  vertex.color    = V(g)$color,
  vertex.size     = V(g)$size,
  vertex.label    = V(g)$label,
  vertex.label.cex= 0.55,
  vertex.label.color = "black",
  vertex.frame.color = "white",
  edge.color      = "#CCCCCC",
  edge.width      = 0.8,
  main            = "LEGO 4010 — Part Connectivity Network"
)
legend(
  "topright", legend = names(cat_colors), fill = cat_colors,
  bty = "n", cex = 0.75, title = "Category"
)


# ============================================================
# 8. NETWORK STATISTICS
# ============================================================

stats <- list(
  n_parts        = vcount(g),
  n_connections  = ecount(g),
  density        = edge_density(g),
  avg_degree     = mean(degree(g)),
  max_degree     = max(degree(g)),
  diameter       = diameter(g),
  avg_path_len   = mean_distance(g),
  clustering_coef= transitivity(g, type = "global"),
  n_communities  = length(communities),
  modularity     = modularity(communities)
)

cat("\n=== Network Statistics ===\n")
for (nm in names(stats)) {
  cat(sprintf("  %-22s %s\n", nm, round(stats[[nm]], 4)))
}


# ============================================================
# 9. CATEGORY-LEVEL ADJACENCY (BLOCK MATRIX)
# ============================================================
# Which categories of parts most frequently connect to which others?

cat_levels <- names(cat_colors)
n_cats     <- length(cat_levels)
cat_adj    <- matrix(0L, nrow = n_cats, ncol = n_cats,
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
diag(cat_adj) <- diag(cat_adj) / 2L   # avoid double-counting within-category

cat("\n=== Category-Level Connection Counts ===\n")
print(cat_adj)

cat_long <- melt(cat_adj, varnames = c("From", "To"), value.name = "Count")
p_cats <- ggplot(cat_long, aes(x = To, y = From, fill = Count)) +
  geom_tile(colour = "white", linewidth = 0.5) +
  geom_text(aes(label = ifelse(Count > 0, Count, "")), size = 3) +
  scale_fill_gradient(low = "#F1EFE8", high = "#378ADD", name = "Connections") +
  theme_minimal(base_size = 10) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1),
        plot.title  = element_text(size = 11, face = "bold")) +
  labs(title = "Inter-category Connections", x = NULL, y = NULL)

print(p_cats)
# ggsave("4010_category_adjacency.png", p_cats, width = 7, height = 6, dpi = 180)


# ============================================================
# 10. EXPORT EDGE LIST (CSV)
# ============================================================

edges_df <- adj_long %>%
  filter(Connected == 1, as.integer(Part_A) < as.integer(Part_B)) %>%
  transmute(
    Part_A      = as.character(Part_A),
    Part_B      = as.character(Part_B),
    Cat_A       = PART_CATS[known_parts[match(Part_A, nice_labels)]],
    Cat_B       = PART_CATS[known_parts[match(Part_B, nice_labels)]]
  )

write.csv(edges_df, "4010_edges.csv", row.names = FALSE)
cat(sprintf("\nEdge list exported: %d rows → 4010_edges.csv\n", nrow(edges_df)))

# Full adjacency matrix export
write.csv(as.data.frame(adj_matrix), "4010_adjacency_matrix.csv")
cat("Adjacency matrix exported → 4010_adjacency_matrix.csv\n")
