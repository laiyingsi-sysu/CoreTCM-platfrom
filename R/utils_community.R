# ============================================================
# utils_community.R — Louvain community detection
# ============================================================

community_detection <- function(g) {

  comm <- cluster_louvain(g, weights = E(g)$weight)

  membership_vec <- membership(comm)

  membership_data <- data.frame(
    Herb = names(membership_vec),
    Community = membership_vec
  )

  community_list <- list()
  for (c in unique(membership_vec)) {
    nodes <- names(membership_vec[membership_vec == c])
    sub_g <- induced_subgraph(g, vids = nodes)
    community_list[[paste0("Community_", c)]] <- sub_g
  }

  return(list(
    membership  = membership_data,
    modularity  = modularity(comm),
    comm_obj    = comm,
    communities = community_list
  ))
}
