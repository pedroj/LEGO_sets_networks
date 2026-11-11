source("plot_lego_network.R")
cc <- "../../data/lego_compat_out/compat_matrix_subtype_codes.csv"
for (nm in c("sebulba", "anakin")) for (suf in c("", "_nominifig")) {
  pre <- paste0("4485_", nm, suf)
  lab <- if (nm == "sebulba") "Sebulba's Podracer" else "Anakin's Podracer"
  if (suf != "") lab <- paste0(lab, " (no minifig parts)")
  plot_lego_network_coords(paste0(pre, "_adjacency_matrix.csv"), paste0(pre, "_instances.csv"), "../../data/icons",
                           parts_meta = "rebrickable/set_parts.csv", out_prefix = pre, set_info = "set_info.json",
                           panel_title = lab, compat_csv = cc)
}
