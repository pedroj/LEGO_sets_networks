library(igraph)
library(plotly)
library(htmlwidgets)

#g <- model75344.g

coords <- layout_with_fr(g, dim = 3)
Xn <- coords[, 1]; Yn <- coords[, 2]; Zn <- coords[, 3]

es <- get.edgelist(g, names = FALSE)
Ne <- nrow(es)

Xe <- Ye <- Ze <- numeric(0)
for (k in seq_len(Ne)) {
    v0 <- es[k, 1]; v1 <- es[k, 2]
    Xe <- c(Xe, Xn[v0], Xn[v1], NA)
    Ye <- c(Ye, Yn[v0], Yn[v1], NA)
    Ze <- c(Ze, Zn[v0], Zn[v1], NA)
}

n <- vcount(g)

name_vec <- if (!is.null(V(g)$name)) V(g)$name else as.character(seq_len(n))
part_vec <- if (!is.null(V(g)$part_id)) V(g)$part_id else rep("", n)

# optional remote image URL (Bricklink) – double‑check their URL scheme
V(g)$image   <- paste0("https://img.bricklink.com/ItemImage/P/",
                       V(g)$color_id, "/", V(g)$part_id, ".png")

# local file under project_root/images/part-images/
V(g)$img_url <- paste0("images/part_images/", V(g)$part_id, "_",V(g)$color_id,".png")

hover_txt <- paste0(
    "ID: ", name_vec, "<br>",
    "Part: ", part_vec, "<br>"
)

img_url <- vertex_attr(g, "img_url", index = V(g))
if (is.null(img_url)) img_url <- rep(NA_character_, n)

fig <- plot_ly(
    type  = "scatter3d",
    mode  = "markers",
    x     = Xn, y = Yn, z = Zn,
    marker = list(size = 8, color = "blue", opacity = 0.3),
    text   = hover_txt,
    customdata   = img_url,
    hovertemplate = "%{text}<extra></extra>"
)

fig <- fig %>%
    add_trace(
        x = Xe, y = Ye, z = Ze,
        type = "scatter3d",
        mode = "lines",
        line = list(color = "black", width = 3, opacity = 0.5),
        hoverinfo = "none",
        showlegend = FALSE,
        inherit = FALSE
    )

fig <- fig %>%
    htmlwidgets::onRender("
function(el, x) {
  var tooltip = document.createElement('div');
  tooltip.style.position = 'absolute';
  tooltip.style.pointerEvents = 'none';
  tooltip.style.background = 'rgba(255,255,255,0.95)';
  tooltip.style.border = '1px solid #ccc';
  tooltip.style.padding = '4px';
  tooltip.style.zIndex = 1000;
  tooltip.style.opacity = 0;
  document.body.appendChild(tooltip);

  el.on('plotly_hover', function(d) {
    var pt  = d.points[0];
    var url = pt.customdata;

    var html = '';
    if (url && url !== 'NA') {
      html += '<img src=\"' + url + '\" width=\"120\"><br>';
    }
    html += pt.text;

    tooltip.innerHTML = html;

    var bbox = el.getBoundingClientRect();
    tooltip.style.left = (bbox.left + 10) + 'px';
    tooltip.style.top  = (bbox.top  + 10) + 'px';
    tooltip.style.opacity = 1;
  });

  el.on('plotly_unhover', function(d) {
    tooltip.style.opacity = 0;
  });
}
")

fig
