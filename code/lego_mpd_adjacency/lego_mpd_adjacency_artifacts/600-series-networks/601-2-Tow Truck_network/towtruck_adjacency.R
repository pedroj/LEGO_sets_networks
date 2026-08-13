# ============================================================================
# Reproduce: adjacency matrix of LEGO parts in 601-2-Tow_Truck.mpd
# Connection = physical PROXIMITY  AND  connector COMPATIBILITY
# Port of the Python pipeline to base R (+ ggplot2 for figures).
# ============================================================================
suppressPackageStartupMessages({ library(ggplot2) })

# ---- Inputs -----------------------------------------------------------------
# MPD path: pass as arg, else resolve the attached artifact via host$artifact_path().
args <- commandArgs(trailingOnly = TRUE)
mpd_path <- if (length(args) >= 1) args[1] else
  host$artifact_path("6076dc11-ad90-465e-9665-2923941be8b3")
# Optional Rebrickable catalog for human-readable names (nice-to-have).
parts_csv <- "/tmp/parts.csv.gz"

TOL      <- 6.0    # LDU tolerance for "touching"
STACK_DY <- 24.0   # max vertical gap for a stud-stack (one brick height)
HUB_D    <- 60.0   # tyre-to-hub max center distance

# ---- 1. Parse LDraw type-1 (sub-part reference) records ---------------------
raw   <- readLines(mpd_path, warn = FALSE)
lines <- Filter(function(l) grepl("^\\s*1\\s", l), raw)      # only type-1 refs

parse_ref <- function(l) {
  t <- strsplit(trimws(l), "\\s+")[[1]]
  dat <- t[15]
  list(color = as.integer(t[2]),
       x = as.numeric(t[3]), y = as.numeric(t[4]), z = as.numeric(t[5]),
       mat = as.numeric(t[6:14]),                # row-major 3x3: a b c d e f g h i
       dat = sub("\\.dat$", "", dat, ignore.case = TRUE),
       is_ldr = grepl("\\.ldr$", dat, ignore.case = TRUE))
}
refs <- lapply(lines, parse_ref)
# Drop the top-level model wrapper (references a .ldr, not a .dat part)
refs <- Filter(function(r) !r$is_ldr && r$dat != "601-2", refs)
N <- length(refs)

# ---- 2. Part footprints (LDU) and connector roles ---------------------------
# width_x, depth_z, height_y, role.  Origin = bottom face; part extends up (-y).
DIMS <- list(
  "3030"    = list(80,200, 8,  "stud"),          # Plate 4 x 10 (chassis)
  "3010"    = list(20, 80, 24, "stud"),          # Brick 1 x 4
  "3001"    = list(40, 80, 24, "stud"),          # Brick 2 x 4
  "3023"    = list(20, 40, 8,  "stud"),          # Plate 1 x 2
  "3021"    = list(40, 60, 8,  "stud"),          # Plate 2 x 3
  "3020"    = list(40, 80, 8,  "stud"),          # Plate 2 x 4
  "3024"    = list(20, 20, 8,  "stud"),          # Plate 1 x 1
  "3137c01" = list(40, 40, 24, "wheel_holder"),  # 2 x 2 wheel-holder brick
  "3137c02" = list(40, 40, 24, "wheel_holder"),  # 2 x 2 dually wheel-holder
  "3135c01" = list(40, 60, 32, "stud"),          # Slope 2 x 3 w/ tow hook
  "3139"    = list(30, 30, 16, "tyre"),          # Tyre 14 x 4
  "451"     = list(24, 24, 12, "tyre")           # Tyre (dually, small)
)
lookup_dims <- function(pn) {
  cand <- c(pn, sub("p.*$", "", pn), sub("[pc].*$", "", pn))
  for (c in cand) if (!is.null(DIMS[[c]])) return(DIMS[[c]])
  list(20, 20, 8, "stud")                        # default: 1x1-ish stud part
}

# ---- 3. Axis-aligned bounding box from footprint + transform ----------------
aabb <- function(r) {
  d <- lookup_dims(r$dat); w <- d[[1]]; dep <- d[[2]]; h <- d[[3]]; role <- d[[4]]
  m <- r$mat                                     # a b c d e f g h i  (indices 1..9)
  hx <- w/2; hz <- dep/2
  ex <- abs(m[1])*hx + abs(m[3])*hz              # rotated half-extent in x
  ez <- abs(m[7])*hx + abs(m[9])*hz              # rotated half-extent in z
  list(role = role,
       xmin = r$x-ex, xmax = r$x+ex,
       zmin = r$z-ez, zmax = r$z+ez,
       ymin = r$y-h,  ymax = r$y,                # extends up (-y) from bottom face
       cx = r$x, cy = r$y, cz = r$z)
}
boxes <- lapply(refs, aabb)
roles <- vapply(boxes, function(b) b$role, character(1))
dat   <- vapply(refs,  function(r) r$dat,  character(1))

# Instance-unique node labels: "<part>#<k>"
node <- ave(seq_len(N), dat, FUN = seq_along)
node <- paste0(dat, "#", node)

# ---- 4. Proximity predicates ------------------------------------------------
boxes_touch <- function(a, b, tol = TOL)
  (a$xmin-tol <= b$xmax && b$xmin-tol <= a$xmax &&
   a$ymin-tol <= b$ymax && b$ymin-tol <= a$ymax &&
   a$zmin-tol <= b$zmax && b$zmin-tol <= a$zmax)
xz_overlap <- function(a, b, tol = TOL)
  (a$xmin-tol <= b$xmax && b$xmin-tol <= a$xmax &&
   a$zmin-tol <= b$zmax && b$zmin-tol <= a$zmax)
ygap <- function(a, b) max(0, max(a$ymin - b$ymax, b$ymin - a$ymax))
cdist <- function(a, b) sqrt((a$cx-b$cx)^2 + (a$cy-b$cy)^2 + (a$cz-b$cz)^2)

# ---- 5. Compatibility (from the connection ontology) ------------------------
# Studded bricks/plates + wheel-holder bricks all share the stud system.
# Tyre mates only with a wheel_holder hub; tyre-tyre never connects.
STUD_SYS <- c("stud", "wheel_holder")
compatible <- function(ra, rb) {
  if (ra %in% STUD_SYS && rb %in% STUD_SYS) return(TRUE)
  if (setequal(c(ra, rb), c("tyre", "wheel_holder"))) return(TRUE)
  FALSE
}

# ---- 6. Build symmetric adjacency: proximate AND compatible -----------------
A <- matrix(0L, N, N, dimnames = list(node, node))
crit <- matrix(NA_character_, N, N)
for (i in seq_len(N)) for (j in seq_len(N)) {
  if (j <= i) next
  ri <- roles[i]; rj <- roles[j]
  if (!compatible(ri, rj)) next
  if (setequal(c(ri, rj), c("tyre", "wheel_holder"))) {
    prox <- cdist(boxes[[i]], boxes[[j]]) < HUB_D; c <- "tyre-hub"
  } else {
    touch <- boxes_touch(boxes[[i]], boxes[[j]])
    stack <- xz_overlap(boxes[[i]], boxes[[j]]) && ygap(boxes[[i]], boxes[[j]]) <= STACK_DY
    prox  <- touch || stack
    c <- if (touch) "touch" else if (stack) "stack" else NA_character_
  }
  if (prox) { A[i, j] <- 1L; A[j, i] <- 1L; crit[i, j] <- c }
}

# ---- 7. Optional human-readable names --------------------------------------
name_of <- setNames(rep("\u2014", N), node)
if (file.exists(parts_csv)) {
  pc <- read.csv(gzfile(parts_csv), colClasses = "character")
  pmap <- setNames(pc$name, pc$part_num)
  resolve <- function(pn) {
    for (c in c(pn, sub("c\\d+$", "", pn), sub("p.*$", "", pn), sub("[a-z].*$", "", pn)))
      if (!is.na(pmap[c])) return(pmap[c])
    "\u2014"
  }
  name_of <- setNames(vapply(dat, resolve, character(1)), node)
}

# ---- 8. Outputs: matrix + edge list ----------------------------------------
write.csv(A, "towtruck_adjacency_matrix_R.csv")
edges <- data.frame(part_A=character(), name_A=character(),
                    part_B=character(), name_B=character(),
                    criterion=character(), stringsAsFactors = FALSE)
for (i in seq_len(N)) for (j in seq_len(N)) if (j > i && A[i, j] == 1L)
  edges <- rbind(edges, data.frame(part_A=node[i], name_A=name_of[node[i]],
                                   part_B=node[j], name_B=name_of[node[j]],
                                   criterion=crit[i, j], stringsAsFactors = FALSE))
write.csv(edges, "towtruck_edges_R.csv", row.names = FALSE)

deg <- rowSums(A)
cat(sprintf("parts=%d  edges=%d  isolated=%s\n", N, sum(A)/2,
            paste(node[deg == 0], collapse = ", ")))

# ---- 9. Figure: adjacency-matrix heatmap -----------------------------------
short <- node
hm <- expand.grid(i = seq_len(N), j = seq_len(N))
hm$val <- mapply(function(i, j) A[i, j], hm$i, hm$j)
p1 <- ggplot(hm, aes(j, i, fill = factor(val))) +
  geom_tile(color = "white", linewidth = 0.4) +
  scale_fill_manual(values = c("0" = "#F4F4F2", "1" = "#2C6E8F"),
                    labels = c("not connected", "connected"), name = NULL) +
  scale_x_continuous(breaks = seq_len(N), labels = short, expand = c(0, 0)) +
  scale_y_reverse(breaks = seq_len(N), labels = short, expand = c(0, 0)) +
  coord_equal() +
  labs(title = "Tow Truck (601-2) part adjacency matrix",
       subtitle = "1 = physically proximate AND connector-compatible",
       x = NULL, y = NULL) +
  theme_minimal(base_size = 9) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 6),
        axis.text.y = element_text(size = 6),
        panel.grid = element_blank(), legend.position = "top")
ggsave("towtruck_adjacency_matrix_R.png", p1, width = 8.4, height = 8.2, dpi = 300)

# ---- 10. Figure: connectivity network on real (x, z) model coordinates ------
role_col <- c(stud = "#2C6E8F", wheel_holder = "#C9A227", tyre = "#B0592F")
nodes_df <- data.frame(node = node, x = vapply(boxes, function(b) b$cx, 0),
                       z = vapply(boxes, function(b) b$cz, 0), role = roles)
seg <- edges  # reuse; need coords
coord <- setNames(seq_len(N), node)
edf <- data.frame(x1 = nodes_df$x[coord[edges$part_A]], z1 = nodes_df$z[coord[edges$part_A]],
                  x2 = nodes_df$x[coord[edges$part_B]], z2 = nodes_df$z[coord[edges$part_B]])
p2 <- ggplot() +
  geom_segment(data = edf, aes(x1, z1, xend = x2, yend = z2),
               color = "#BBBBBB", linewidth = 0.4) +
  geom_point(data = nodes_df, aes(x, z, fill = role), shape = 21,
             size = 6, color = "white", stroke = 0.8) +
  geom_text(data = nodes_df, aes(x, z, label = sub("\\..*$", "", node)),
            size = 1.8, color = "white") +
  scale_fill_manual(values = role_col, name = "connector role") +
  scale_y_reverse() + coord_equal() +
  labs(title = "Tow Truck connectivity network (top view)",
       subtitle = "nodes at real model coordinates", x = "x (LDU)", y = "z (LDU)") +
  theme_minimal(base_size = 9) + theme(legend.position = "right")
ggsave("towtruck_network_R.png", p2, width = 7.6, height = 7.2, dpi = 300)

cat("wrote: towtruck_adjacency_matrix_R.csv, towtruck_edges_R.csv, ",
    "towtruck_adjacency_matrix_R.png, towtruck_network_R.png\n", sep = "")
