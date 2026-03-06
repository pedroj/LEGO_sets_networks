
# Key points:
#    nodes must contain columns literally named x and y.

# In geom_image(...) we set data = nodes and aes(x = x, y = y, image = img_path, size = degree) so all needed aesthetics exist in that data.

# inherit.aes = FALSE stops it from looking for x/y in the ggraph layout environment, so there is no ambiguity.

# size is relative to the plot; adjust (e.g. 0.07–0.12) to fit ~20 nodes.
# Using ggraph (Recommended)
# ggraph extends ggplot2 for networks and works directly with igraph.

## Session Info

session_info()
