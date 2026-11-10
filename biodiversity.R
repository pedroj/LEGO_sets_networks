# ---- Ecology panels for the LEGO analogy figure ----
# Panels: A2 (species abundances), B2 (forbidden/possible/realized links),
#         C2 (two networks, same species, different links) + beta-diversity inset
# Requires: ggplot2, patchwork, dplyr, tidyr

library(ggplot2); library(patchwork); library(dplyr); library(tidyr)

set.seed(42)

# Okabe-Ito palette (colour-blind safe)
oi <- c("#E69F00","#56B4E9","#009E73","#F0E442",
        "#0072B2","#D55E00","#CC79A7","#999999")

# ---- 1. Species and traits (simulated) -------------------------------
n_pl <- 8; n_an <- 8
plants  <- data.frame(sp = paste0("P", 1:n_pl),
                      fruit = sort(runif(n_pl, 4, 20)))     # fruit width, mm
animals <- data.frame(sp = paste0("A", 1:n_an),
                      gape  = sort(runif(n_an, 6, 26)),     # gape width, mm
                      abund = rev(sort(rpois(n_an, 20) + 5)),
                      col   = oi)

# ---- 2. Link states: forbidden / possible / realized ------------------
# Forbidden if fruit wider than gape (trait mismatch)
possible <- outer(plants$fruit, animals$gape, "<=")
dimnames(possible) <- list(plants$sp, animals$sp)

# Realized links: sample among possible, weighted by animal abundance
realize <- function(possible, animals, p_scale = 0.012) {
    p <- possible * matrix(animals$abund * p_scale,
                           nrow(possible), ncol(possible), byrow = TRUE)
    p <- pmin(p, 0.9)
    matrix(rbinom(length(p), 1, p), nrow(p), dimnames = dimnames(possible))
}
net1 <- realize(possible, animals)
net2 <- realize(possible, animals)

# ---- 3. Panel A2: species abundance bars ------------------------------
pA <- ggplot(animals, aes(reorder(sp, -abund), abund, fill = sp)) +
    geom_col(width = 0.75) +
    scale_fill_manual(values = setNames(animals$col, animals$sp), guide = "none") +
    labs(x = "Species", y = "Abundance",
         title = "Diversity of components") +
    theme_classic(base_size = 8)

# ---- 4. Panel B2: link-state matrix -----------------------------------
state <- expand.grid(plant = plants$sp, animal = animals$sp,
                     stringsAsFactors = FALSE) %>%
    mutate(poss = possible[cbind(plant, animal)],
           real = net1[cbind(plant, animal)],
           state = case_when(real == 1 ~ "Realized",
                             poss ~ "Possible",
                             TRUE ~ "Forbidden"),
           state = factor(state, levels = c("Forbidden","Possible","Realized")),
           plant = factor(plant, levels = rev(plants$sp)),
           animal = factor(animal, levels = animals$sp))

pB <- ggplot(state, aes(animal, plant, fill = state)) +
    geom_tile(colour = "white", linewidth = 0.4) +
    scale_fill_manual(values = c(Forbidden = "grey88",
                                 Possible  = "white",
                                 Realized  = "#0072B2"), name = NULL) +
    geom_tile(data = filter(state, state == "Possible"),
              fill = NA, colour = "grey40", linewidth = 0.3) +
    labs(x = "Animals", y = "Plants", title = "Possible vs realized links") +
    coord_equal() + theme_minimal(base_size = 8) +
    theme(panel.grid = element_blank(), legend.position = "bottom",
          legend.key.size = unit(3, "mm"))

# ---- 5. Panel C2: two bipartite networks, same species ----------------
draw_net <- function(M, title, hub_col = "#D55E00") {
    edges <- as.data.frame(as.table(M)) %>%
        filter(Freq > 0) %>% rename(plant = Var1, animal = Var2) %>%
        mutate(x1 = match(plant, plants$sp), x2 = match(animal, animals$sp))
    deg <- colSums(M)
    nodes_a <- data.frame(sp = animals$sp, x = seq_len(n_an), y = 0,
                          deg = deg, col = animals$col)
    nodes_p <- data.frame(sp = plants$sp, x = seq_len(n_pl), y = 1,
                          deg = rowSums(M))
    hub <- names(which.max(deg))
    ggplot() +
        geom_segment(data = edges, aes(x = x1, xend = x2, y = 1, yend = 0),
                     colour = "grey50", alpha = 0.7, linewidth = 0.3) +
        geom_point(data = nodes_p, aes(x, y, size = deg), colour = "darkgreen") +
        geom_point(data = nodes_a, aes(x, y, size = deg, colour = sp)) +
        geom_point(data = filter(nodes_a, sp == hub), aes(x, y, size = deg),
                   shape = 1, stroke = 0.9, colour = hub_col, fill = NA) +
        scale_colour_manual(values = setNames(animals$col, animals$sp), guide = "none") +
        scale_size_continuous(range = c(1.5, 5), guide = "none") +
        labs(title = title) + theme_void(base_size = 8)
}
pC1 <- draw_net(net1, "Assembly 1")
pC2 <- draw_net(net2, "Assembly 2 (same species)")

# ---- 6. Interaction beta-diversity (Poisot et al. 2012 logic) ---------
# Species identical => turnover is 0, all dissimilarity is rewiring (beta_OS)
beta_links <- function(M1, M2) {
    a <- sum(M1 == 1 & M2 == 1)
    b <- sum(M1 == 1 & M2 == 0)
    c <- sum(M1 == 0 & M2 == 1)
    c(shared = a, only1 = b, only2 = c,
      beta_WN = (b + c) / (a + b + c),         # Whittaker-type, total
      beta_ST = 0,                             # species turnover (same species)
      beta_OS = (b + c) / (a + b + c))         # rewiring among shared species
}
bl <- beta_links(net1, net2)

inset <- data.frame(component = c("Species turnover", "Link rewiring"),
                    value = c(bl["beta_ST"], bl["beta_OS"]))
pI <- ggplot(inset, aes(component, value, fill = component)) +
    geom_col(width = 0.6) +
    scale_fill_manual(values = c("grey60", "#D55E00"), guide = "none") +
    scale_y_continuous(limits = c(0, 1)) +
    labs(x = NULL, y = expression(beta[WN]),
         title = "Interaction beta-diversity") +
    theme_classic(base_size = 7)

# ---- 7. Assemble and save --------------------------------------------
fig <- (pA | pB | (pC1 / pC2 / pI + plot_layout(heights = c(1, 1, 0.9)))) +
    plot_annotation(tag_levels = list(c("A2", "B2", "C2", "", "")))

dir.create("output", showWarnings = FALSE)
ggsave("output/ecology_panels.pdf", fig, width = 180, height = 90,
       units = "mm", device = cairo_pdf)
ggsave("output/ecology_panels.svg", fig, width = 180, height = 90, units = "mm")

print(round(bl, 3))

# ---- Optional: use a real network instead of simulated data -----------
# M <- as.matrix(read.csv("my_network.csv", row.names = 1))
# Binary link matrix of plants (rows) x animals (columns). Then set:
#   plants / animals data frames to match, and net1/net2 to two sites or
#   two time periods with the same species set (or restrict to shared species).
# For species turnover too, compute beta_ST using the full species union.

