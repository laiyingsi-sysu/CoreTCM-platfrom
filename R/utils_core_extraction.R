# ============================================================
# utils_core_extraction.R — Core formula extraction (k-core & core-periphery)
# ============================================================

extract_core_kcore_freq <- function(g_pmi, community_obj,
                                     k_quantile = 0.7, min_nodes = 3,
                                     too_small_threshold = 5) {

  membership_vec <- membership(community_obj)
  community_ids <- unique(membership_vec)

  core_formula_table <- data.frame(
    Community = numeric(), Core_Formula = character(), Note = character(),
    stringsAsFactors = FALSE
  )
  core_detail_table <- data.frame(
    Community = numeric(), Herb = character(), Strength = numeric(),
    stringsAsFactors = FALSE
  )
  all_strength_table <- data.frame(
    Community = numeric(), Herb = character(), Strength = numeric(),
    stringsAsFactors = FALSE
  )
  small_communities <- integer(0)   # E: track skipped communities

  for (comm_id in community_ids) {
    nodes <- names(membership_vec[membership_vec == comm_id])
    g_sub <- induced_subgraph(g_pmi, nodes)

    # E: skip too-small communities (< too_small_threshold herbs)
    if (vcount(g_sub) < too_small_threshold) {
      small_communities <- c(small_communities, comm_id)
      core_formula_table <- rbind(
        core_formula_table,
        data.frame(Community = comm_id, Core_Formula = "",
                   Note = sprintf("skipped: community size %d < %d",
                                  vcount(g_sub), too_small_threshold),
                   stringsAsFactors = FALSE)
      )
      if (vcount(g_sub) > 0) {
        w <- if (is.null(E(g_sub)$weight)) NULL else E(g_sub)$weight
        s_all <- strength(g_sub, weights = w)
        all_strength_table <- rbind(all_strength_table, data.frame(
          Community = comm_id, Herb = names(s_all), Strength = s_all,
          stringsAsFactors = FALSE))
      }
      next
    }

    if (is.null(E(g_sub)$weight)) E(g_sub)$weight <- 1

    strength_val <- strength(g_sub, weights = E(g_sub)$weight)

    tmp_all <- data.frame(
      Community = comm_id, Herb = names(strength_val),
      Strength = strength_val, stringsAsFactors = FALSE
    )
    all_strength_table <- rbind(all_strength_table, tmp_all)

    # D: iterative shrink with soft guard — break before dropping below min_nodes
    g_core <- g_sub
    iter <- 0; max_iter <- 50
    repeat {
      if (vcount(g_core) <= min_nodes || iter >= max_iter) break
      iter <- iter + 1
      s_val <- strength(g_core, weights = E(g_core)$weight)
      k_thresh <- quantile(s_val, k_quantile, na.rm = TRUE)
      remove_nodes <- names(s_val[s_val < k_thresh])
      if (length(remove_nodes) == 0) break
      # D soft guard: stop if removal would drop below min_nodes
      if (vcount(g_core) - length(remove_nodes) < min_nodes) break
      g_core <- delete_vertices(g_core, remove_nodes)
    }
    core_nodes <- V(g_core)$name

    core_formula_table <- rbind(
      core_formula_table,
      data.frame(Community = comm_id,
                 Core_Formula = paste(core_nodes, collapse = " "),
                 Note = sprintf("extracted: %d herbs after %d iterations",
                                length(core_nodes), iter),
                 stringsAsFactors = FALSE)
    )

    if (length(core_nodes) > 0) {
      core_strength <- strength(g_sub, weights = E(g_sub)$weight)[core_nodes]
      tmp_core <- data.frame(
        Community = comm_id, Herb = names(core_strength),
        Strength = core_strength, stringsAsFactors = FALSE
      ) %>% arrange(desc(Strength))
    } else {
      tmp_core <- data.frame(
        Community = comm_id, Herb = NA, Strength = NA,
        stringsAsFactors = FALSE
      )
    }
    core_detail_table <- rbind(core_detail_table, tmp_core)
  }

  core_detail_table <- core_detail_table %>%
    group_by(Community) %>% arrange(desc(Strength), .by_group = TRUE) %>% ungroup()
  all_strength_table <- all_strength_table %>%
    group_by(Community) %>% arrange(desc(Strength), .by_group = TRUE) %>% ungroup()

  return(list(
    core_formula_table  = core_formula_table,
    core_strength_rank  = core_detail_table,
    all_strength_rank   = all_strength_table,
    small_communities  = small_communities
  ))
}
