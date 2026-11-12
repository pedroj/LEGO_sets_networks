# Structural comparison of two LEGO part-connectivity networks

**Networks:** `7131 Anakin's Podracer` (260 parts, 1807 connections) vs
`10026` (185 parts, 827 connections).
Both are undirected, binary, one-mode part-adjacency networks (a node = a part
instance; an edge = two parts that physically connect).

## The problem with a Hamming test here

A Hamming distance compares two adjacency matrices **cell by cell** and counts
how many entries disagree. It requires the two matrices to describe the **same
node set in the same order** — same size, node-aligned. These two networks have
different sizes (260 vs 185 nodes) and no natural node correspondence, so the
Hamming distance is undefined. We need dissimilarity measures that compare
*structure* without needing a node-to-node map.

## Method

**Primary metric — Portrait Divergence** (Bagrow & Bollt 2019). Each network is
reduced to its *portrait* `B[l,k]`: for every shortest-path length `l`, how many
nodes have exactly `k` nodes at that distance. The portrait is invariant to node
labelling, defined for graphs of different sizes, and robust to disconnection
(unreachable pairs are simply not counted — important here, as both networks are
fragmented). Portrait Divergence is the Jensen–Shannon divergence between the two
portraits, bounded in `[0, 1]` (0 = identical multiscale structure, 1 = maximally
different).

**Complementary lenses.** Two additional size-independent JS divergences isolate
*where* the networks differ:
- **Laplacian spectral-density JSD** — whole-spectrum signature (community
  structure, bipartiteness, diffusion behaviour).
- **Degree-distribution JSD** — purely local connection-count structure.

**Turning a number into a test.** A single dissimilarity value cannot say whether
the two structures are *significantly* different, because we have only one
observed network per set. We build replication with a **degree-preserving null
model**: 250 edge-rewirings of each network that keep every node's exact degree
but otherwise randomize the wiring. This produces two "clouds" of structurally
perturbed replicates. We compute the full Portrait-Divergence dissimilarity
matrix among all 502 graphs (2 observed + 500 nulls) and run **PERMANOVA**
(`vegan::adonis2`, 999 permutations) testing whether cloud membership explains
the structural dissimilarity. `betadisper` checks whether the two clouds have
comparable spread.

## Results

### Scalar dissimilarities

| Metric | Value | Reading |
|---|---:|---|
| Portrait Divergence | **0.547** | Large — global path structure differs strongly |
| Degree-distribution JSD | 0.204 | Moderate local difference |
| Laplacian spectral JSD | 0.044 | Small — overall spectral shape is similar |

The networks differ **most in their global, multiscale connectivity** and least
in their coarse spectral signature.

### Direct test of structural difference

| Quantity | Value |
|---|---:|
| PERMANOVA pseudo-F | 1296 |
| PERMANOVA R² | 0.722 |
| PERMANOVA p | 0.001 |
| mean within-cloud divergence (A / B) | 0.239 / 0.229 |
| mean between-network divergence | 0.597 |
| **between/within ratio (effect size)** | **2.55** |
| betadisper F / p | 3.61 / 0.020 |

**Conclusion.** The two networks are structurally distinguishable
(PERMANOVA p = 0.001). Cloud membership explains **72%** of the variance in
Portrait Divergence, and between-network divergence is **2.5× larger** than the
divergence among degree-preserving randomizations of either network — i.e. the
Podracer and 10026 are far more different from each other than random rewirings
of either are from themselves. The `descriptors.csv` grounding table shows the
structural axes driving this: the Podracer is denser (mean degree 13.9 vs 8.9),
more locally clustered (transitivity 0.70 vs 0.56) and carries higher-degree hub
parts, while 10026 is more fragmented (17 components vs 11) with a larger giant
component fraction.

### Caveat

`betadisper` is marginally significant (p = 0.02): the two null clouds are not
identically dispersed, so a small part of the PERMANOVA signal reflects a
spread difference rather than a pure location difference. With R² = 0.72 the
location (mean-structure) separation overwhelmingly dominates, and panel (b) of
the ordination figure shows the between- and within-distributions do not overlap
— so the conclusion is robust. Portrait Divergence weights shortest-path
structure; if a future question is specifically about community partitioning,
the spectral lens (which was the *most* similar here) would deserve its own
null-model test.

## Files

- `compare_networks.R` — reusable pipeline: `compare_networks(A, B, n_null=)`
  accepts any two adjacency matrices of arbitrary (possibly different) sizes and
  returns the three dissimilarities, the PERMANOVA test, and a PCoA ordination.
- `descriptors.csv` — per-network grounding metrics.
- `dissimilarities.csv` — the three scalar dissimilarities.
- `test_results.csv` — PERMANOVA, dispersion, effect size, z-score.
- `degree_comparison.png` — degree CCDF + distribution.
- `pcoa_ordination.png` — PCoA of the divergence matrix + within/between
  divergence distributions (the main result figure).
