# ============================================================
# utils_concentric.R — Concentric network visualization (ggraph)
# ============================================================

plot_concentric_network <- function(g_sub, role_table = NULL,
                                     comm_id = NULL, show_plot = TRUE) {

  if (is.null(E(g_sub)$weight)) {
    E(g_sub)$weight <- 1
  }

  strength_val <- strength(g_sub, weights = E(g_sub)$weight)
  V(g_sub)$Strength <- strength_val

  if (!is.null(role_table)) {
    role_vec <- role_table$Role
    names(role_vec) <- role_table$Herb
    V(g_sub)$Role <- role_vec[V(g_sub)$name]
    V(g_sub)$Role[is.na(V(g_sub)$Role)] <- "Shi"

    V(g_sub)$Layer <- ifelse(
      V(g_sub)$Role == "Jun", 1,
      ifelse(V(g_sub)$Role == "Chen", 2, 3)
    )
  } else {
    q <- quantile(strength_val, probs = c(0.33, 0.66), na.rm = TRUE)
    V(g_sub)$Layer <- ifelse(
      strength_val >= q[2], 1,
      ifelse(strength_val >= q[1], 2, 3)
    )
  }

  layout <- create_layout(g_sub, layout = "kk")
  layout$name <- V(g_sub)$name
  layout$Layer <- V(g_sub)$Layer[match(layout$name, V(g_sub)$name)]
  layout$Strength <- V(g_sub)$Strength[match(layout$name, V(g_sub)$name)]
  layout$Layer[is.na(layout$Layer)] <- 3

  layout$x <- 0
  layout$y <- 0

  layers <- sort(unique(layout$Layer))
  for (l in layers) {
    idx <- which(layout$Layer == l)
    n_l <- length(idx)
    if (n_l == 0) next
    idx <- idx[order(layout$Strength[idx], decreasing = TRUE)]
    theta <- seq(0, 2 * pi, length.out = n_l + 1)[-(n_l + 1)]
    r <- ifelse(l == 1, 0.8, ifelse(l == 2, 1.8, 3))
    layout$x[idx] <- r * cos(theta)
    layout$y[idx] <- r * sin(theta)
  }

  p <- ggraph(layout) +
    geom_edge_link(aes(width = weight, alpha = weight, color = weight)) +
    geom_node_point(aes(size = Strength, color = Strength)) +
    geom_node_point(
      data = subset(layout, Layer == 1),
      aes(size = Strength), color = "#B30000", alpha = 0.6
    ) +
    geom_node_text(aes(label = name), size = 3, repel = TRUE) +
    scale_color_gradientn(colors = c("#FFFFCC", "#FDBB84", "#FC8D59", "#D7301F")) +
    scale_edge_color_gradientn(colors = c("#FEE8C8", "#FDBB84", "#E34A33")) +
    scale_edge_width(range = c(0.1, 1.2)) +
    scale_edge_alpha(range = c(0.05, 0.4)) +
    theme_void()

  if (!is.null(comm_id)) {
    p <- p + ggtitle(paste("Community", comm_id))
  }

  if (show_plot) print(p)
  return(p)
}
