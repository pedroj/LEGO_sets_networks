#!/usr/bin/env Rscript
# =====================================================================
# compare_networks.R
# Size-independent structural comparison of two undirected networks.
#
# Motivation
# ----------
# A Hamming distance compares two adjacency matrices cell-by-cell and
# therefore requires the SAME node set (same size, node-aligned). When
# the two networks differ in size (here 260 vs 185 nodes) that test is
# undefined. This pipeline instead compares the *structure* of the two
# networks with unlabeled, size-independent graph-dissimilarity measures
# and turns the observed dissimilarity into a statistical statement with
# a degree-preserving null model + PERMANOVA.
#
# Primary metric: Portrait Divergence (Bagrow & Bollt, 2019,
#   "An information-theoretic, all-scales approach to comparing
#    networks", Applied Network Science 4:45). The network "portrait"
#   B[l,k] counts, for each shortest-path length l, how many nodes have
#   exactly k nodes at that distance. It is invariant to node labelling,
#   defined for graphs of different sizes, and robust to disconnection
#   (unreachable pairs are simply not counted). Portrait Divergence is
#   the Jensen-Shannon divergence between the two portraits, in [0,1].
#
# Complementary lenses:
#   - Laplacian spectral-density JSD (whole-spectrum: communities,
#     bipartiteness, diffusion), normalized Laplacian eigenvalues in [0,2]
#   - Degree-distribution JSD (local connection-count structure)
#
# Test: degree-preserving edge rewiring builds a null "cloud" around
#   each observed network; PERMANOVA (vegan::adonis2) on the full
#   Portrait-Divergence dissimilarity matrix tests whether the two
#   clouds occupy distinguishable regions of structure space.
#
# Usage:
#   source("compare_networks.R")
#   A <- as.matrix(read.csv("netA.csv", row.names=1, check.names=FALSE))
#   B <- as.matrix(read.csv("netB.csv", row.names=1, check.names=FALSE))
#   res <- compare_networks(A, B, n_null = 250, seed = 42)
#   res$dissimilarities   # 3 scalar metrics
#   res$test              # PERMANOVA + effect size
#   res$plot              # ggplot PCoA ordination (if make_plot=TRUE)
# =====================================================================

suppressMessages({
  library(igraph)
  library(vegan)
})

# ---- portrait matrix B[l+1, k+1] -----------------------------------
.portrait <- function(g){
  D <- distances(g)
  N <- nrow(D)
  Lmax <- max(D[is.finite(D)])
  B <- matrix(0, nrow = Lmax + 1, ncol = N + 1)
  for (l in 0:Lmax){
    counts <- rowSums(D == l)
    B[l + 1, ] <- tabulate(counts + 1, nbins = N + 1)
  }
  B
}

# ---- k-weighted, normalized portrait distribution vector -----------
# padded to common (Lmax, Kmax); weights each cell by k = # node pairs
.portrait_vec <- function(B, Lmax, Kmax){
  P <- matrix(0, Lmax, Kmax)
  P[1:nrow(B), 1:ncol(B)] <- B
  Vk <- matrix(rep(0:(Kmax - 1), each = Lmax), nrow = Lmax)
  W <- P * Vk
  as.vector(W / sum(W))
}

# ---- Jensen-Shannon divergence (bits), symmetric, in [0,1] ---------
.js <- function(p, q){
  p <- p / sum(p); q <- q / sum(q)
  m <- 0.5 * (p + q)
  ip <- p > 0; iq <- q > 0
  0.5 * sum(p[ip] * log2(p[ip] / m[ip])) +
  0.5 * sum(q[iq] * log2(q[iq] / m[iq]))
}

# ---- Portrait Divergence between two graphs ------------------------
portrait_divergence <- function(gx, gy){
  Bx <- .portrait(gx); By <- .portrait(gy)
  Lmax <- max(nrow(Bx), nrow(By)); Kmax <- max(ncol(Bx), ncol(By))
  .js(.portrait_vec(Bx, Lmax, Kmax), .portrait_vec(By, Lmax, Kmax))
}

# ---- Laplacian spectral-density JSD --------------------------------
spectral_jsd <- function(gx, gy, grid = seq(0, 2, length.out = 200), bw = 0.05){
  dens <- function(g){
    L  <- laplacian_matrix(g, normalized = TRUE, sparse = FALSE)
    ev <- pmin(pmax(eigen(L, symmetric = TRUE, only.values = TRUE)$values, 0), 2)
    d  <- sapply(grid, function(x) mean(dnorm(x, mean = ev, sd = bw)))
    d / sum(d)
  }
  .js(dens(gx), dens(gy))
}

# ---- Degree-distribution JSD ---------------------------------------
degree_jsd <- function(gx, gy){
  dx <- degree(gx); dy <- degree(gy)
  kmax <- max(c(dx, dy))
  .js(tabulate(dx + 1, nbins = kmax + 1),
      tabulate(dy + 1, nbins = kmax + 1))
}

# ---- adjacency matrix -> undirected igraph -------------------------
.to_graph <- function(A, name = "network"){
  A <- as.matrix(A); storage.mode(A) <- "numeric"
  if (!isSymmetric(unname(A)))
    stop("adjacency matrix is not symmetric (this pipeline is for undirected networks)")
  A[A != 0] <- 1                 # binarize
  diag(A) <- 0
  g <- graph_from_adjacency_matrix(A, mode = "undirected", diag = FALSE)
  g$name <- name; g
}

# =====================================================================
# MAIN: compare_networks
# =====================================================================
compare_networks <- function(A, B,
                             nameA = "A", nameB = "B",
                             n_null = 250, seed = 42,
                             n_perm = 999, make_plot = TRUE){
  set.seed(seed)
  gA <- .to_graph(A, nameA); gB <- .to_graph(B, nameB)

  # ---- scalar dissimilarities ----
  dissim <- data.frame(
    metric = c("Portrait Divergence (JSD)",
               "Laplacian spectral-density JSD",
               "Degree-distribution JSD"),
    value  = c(portrait_divergence(gA, gB),
               spectral_jsd(gA, gB),
               degree_jsd(gA, gB))
  )

  # ---- degree-preserving null ensembles ----
  ens <- function(g, n){
    m <- ecount(g)
    lapply(seq_len(n), function(i) rewire(g, keeping_degseq(niter = 10 * m)))
  }
  graphs <- c(list(gA), ens(gA, n_null), list(gB), ens(gB, n_null))
  grp    <- factor(c(rep(nameA, n_null + 1), rep(nameB, n_null + 1)),
                   levels = c(nameA, nameB))
  is_obs <- c(TRUE, rep(FALSE, n_null), TRUE, rep(FALSE, n_null))

  # ---- pairwise Portrait-Divergence matrix ----
  ports <- lapply(graphs, .portrait)
  Lmax  <- max(sapply(ports, nrow)); Kmax <- max(sapply(ports, ncol))
  V     <- t(sapply(ports, .portrait_vec, Lmax = Lmax, Kmax = Kmax))
  nG    <- nrow(V)
  Dmat  <- matrix(0, nG, nG)
  for (i in 1:(nG - 1)) for (j in (i + 1):nG){
    d <- .js(V[i, ], V[j, ]); Dmat[i, j] <- d; Dmat[j, i] <- d
  }
  Dd <- as.dist(Dmat)

  # ---- PERMANOVA + dispersion + effect size ----
  ad <- adonis2(Dd ~ grp, permutations = n_perm)
  bd <- permutest(betadisper(Dd, grp), permutations = n_perm)
  iA <- which(grp == nameA); iB <- which(grp == nameB)
  wA <- Dmat[iA, iA][upper.tri(diag(length(iA)))]
  wB <- Dmat[iB, iB][upper.tri(diag(length(iB)))]
  bt <- as.vector(Dmat[iA, iB])
  obs_AB <- Dmat[1, n_null + 2]

  test <- data.frame(
    quantity = c("PERMANOVA pseudo-F", "PERMANOVA R2", "PERMANOVA p",
                 "betadisper F", "betadisper p",
                 "mean within-A", "mean within-B", "mean between",
                 "between/within ratio (effect size)",
                 "observed A-B divergence",
                 "z(obs A-B vs between-null)"),
    value = c(ad$F[1], ad$R2[1], ad$`Pr(>F)`[1],
              bd$tab$F[1], bd$tab$`Pr(>F)`[1],
              mean(wA), mean(wB), mean(bt),
              mean(bt) / mean(c(wA, wB)),
              obs_AB, (obs_AB - mean(bt)) / sd(bt))
  )

  out <- list(dissimilarities = dissim, test = test,
              Dmat = Dmat, group = grp, is_obs = is_obs,
              graphs_meta = list(nameA = nameA, nameB = nameB,
                                 nA = vcount(gA), nB = vcount(gB)))

  # ---- optional PCoA ordination plot ----
  if (make_plot && requireNamespace("ggplot2", quietly = TRUE)){
    library(ggplot2)
    pco <- cmdscale(Dd, k = 2, eig = TRUE)
    ve  <- pco$eig / sum(pco$eig[pco$eig > 0])
    pts <- data.frame(PCo1 = pco$points[, 1], PCo2 = pco$points[, 2],
                      network = grp, observed = is_obs)
    out$plot <- ggplot(pts, aes(PCo1, PCo2, color = network)) +
      stat_ellipse(aes(group = network), level = 0.95, linetype = 2, linewidth = 0.5) +
      geom_point(data = pts[!pts$observed, ], size = 1.1, alpha = 0.35) +
      geom_point(data = pts[pts$observed, ], size = 4, shape = 18) +
      labs(x = sprintf("PCo1 (%.0f%%)", 100 * ve[1]),
           y = sprintf("PCo2 (%.0f%%)", 100 * ve[2]),
           color = NULL,
           title = "Portrait-Divergence PCoA (diamonds = observed)") +
      theme_classic(base_size = 12)
  }
  out
}
