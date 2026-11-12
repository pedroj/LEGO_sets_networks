# Comparing two different-sized networks via graph embeddings

**Question.** Do networks `601-2` (21 nodes) and `6600-1` (29 nodes) differ in
*overall structure*? A node-aligned test such as the Hamming distance is
undefined here because the two graphs have no shared node set and different
sizes. The pipeline below replaces node alignment with a **size-invariant
whole-graph embedding** and tests for difference in that embedding space.

---

## Why embeddings instead of a Hamming test

The Hamming distance counts edge-position mismatches between two adjacency
matrices of identical size and node ordering — it is a *node-aligned* comparison.
For graphs of different size there is no correspondence between rows, so the
distance cannot be formed. A graph embedding sidesteps this by mapping each
*whole network* to a fixed-length vector (a "signature") that depends only on
structure, not on node count or labelling. Two signatures of equal length are
then directly comparable regardless of the original graph sizes, and the
embedding distance plays the role the Hamming distance plays for equal-sized
graphs.

## Embeddings used

1. **NetLSD heat trace** (primary). From the normalized graph Laplacian
   *L = I − D⁻¹ᐟ²AD⁻¹ᐟ²*, we take its eigenvalues {λᵢ} and form the heat-diffusion
   trace *h(t) = Σᵢ e^(−tλᵢ)* sampled at 250 log-spaced timescales
   *t ∈ [10⁻², 10²]*. Dividing by the number of nodes, *h(t)/n*, makes the
   descriptor scale-invariant (it starts at 1 for every graph at *t=0*). Small
   *t* probes local structure (degrees, triangles); large *t* probes global
   structure (components, community layout). This is the multiscale, size-robust
   analogue of the Hamming comparison.

2. **Laplacian spectral density** (cross-check). A kernel-density estimate of the
   normalized-Laplacian eigenvalue spectrum on a fixed grid over [0, 2]. This is
   an independent structural fingerprint that does not depend on node count; it
   confirms the NetLSD result rather than restating it.

## Null model and test

The embedding distance alone has no scale — is 0.27 "large"? To calibrate it we
build a **degree-preserving null ensemble** for each network: 1000 random
rewirings that keep each graph's exact degree sequence but scramble the wiring
(`igraph::rewire`, `keeping_degseq`). Embedding every replicate gives two clouds
of 1000 signatures each, describing the family of graphs structurally consistent
with each observed network. The direct test for a structural difference then has
three complementary readouts:

- **Observed-vs-null distance.** The observed inter-network distance
  *d_obs = ‖sig(601-2) − sig(6600-1)‖* is compared to the within-family
  distances (how far degree-matched randomizations sit from their own centroid).
- **Energy distance test** (`energy::eqdist.etest`, 999 permutations) — a
  distance-based two-sample test of whether the two clouds come from the same
  distribution.
- **PERMANOVA** (`vegan::adonis2`, 999 permutations) — partitions embedding-space
  variance into between- vs within-network components and tests the between term.

---

## Results

| Quantity | Value |
|---|---|
| Observed NetLSD L2 distance (d_obs) | **0.273** |
| Within-family null distance (median) | 0.022 |
| Between-family null distance (median) | 0.134 |
| d_obs vs within-family variation | **p = 0.020** |
| Energy E-statistic (2-sample) | 109.0 |
| Energy test p-value | **0.001** |
| PERMANOVA pseudo-F | 2333 |
| PERMANOVA R² | **0.539** |
| PERMANOVA p-value | **0.001** |

**The two networks are structurally distinct.** All three tests agree:

- The observed distance (0.273) is an order of magnitude larger than the typical
  within-family variation (median 0.022) and exceeds 97.9% of it.
- The energy test and PERMANOVA both reach the permutation floor (p = 0.001);
  network identity explains **54%** of the variance in embedding space (R² = 0.54).
- In the PCA ordination (94.7% of variance on PC1), the two degree-preserving
  clouds occupy cleanly separable regions.

**Where the difference lives.** The heat-trace curves overlap at small *t*
(local connectivity — degrees and triangles — is similar between the two sets)
and separate at large *t*, where 6600-1's trace plateaus higher. This large-*t*
gap reflects global structure: 6600-1 has **two connected components** whereas
601-2 is a single component, and 6600-1 is larger and slightly sparser
(density 0.158 vs 0.190). The observed 6600-1 also sits away from its *own*
rewired cloud — degree-preserving rewiring tends to bridge its two components,
so the real disconnected wiring is atypical even among graphs with its exact
degree sequence.

---

## How to reuse this pipeline

The pipeline is size-agnostic: drop in any two (or more) symmetric 0/1 adjacency
matrices. The embedding step (`netlsd_heat`) and the null generator
(`rewire_embed`) are independent of node count, so the same code compares a
15-node web against a 200-node web without modification. For >2 networks, stack
all ensemble clouds and let PERMANOVA test the group term across all of them.

## Files

- `network_descriptors.csv` — basic descriptors (context only, not the test).
- `embeddings_observed.csv` — NetLSD heat-trace signatures for both networks.
- `spectral_density_observed.csv` — Laplacian spectral-density signatures.
- `null_embeddings.rds` — checkpoint: observed + 2×1000 null-ensemble embeddings.
- `structure_test_results.csv` — energy / PERMANOVA / null-distance results.
- `netlsd_signatures.png` — heat-trace curves for the two networks.
- `embedding_ordination.png` — PCA of the two null ensembles + observed points.
- `null_distance_test.png` — d_obs against the within-family null distribution.

**Environment.** R 4.5.3; `igraph`, `vegan`, `energy`, `ggplot2`. Seeds fixed
(ensemble = 42, tests = 7) for reproducibility.
