# ============================================================
# utils_network_enhance.R — Network Knowledge Enhancement
# ============================================================

# --- Normalize vector to 0-1 ---
safe_normalize <- function(x) {
  # Guard: empty input → empty output (don't fabricate length)
  if (length(x) == 0) return(numeric(0))
  # Guard: all-equal or all-NA → neutral 0.5, preserving length
  rng <- range(x, na.rm = TRUE)
  if (is.infinite(rng[1]) || rng[1] == rng[2]) return(rep(0.5, length(x)))
  (x - rng[1]) / (rng[2] - rng[1])
}

# ============================================================
# calculate_network_weight
# ============================================================
calculate_network_weight <- function(m3, a = 0.5, b = 0.2, c_param = 0.3) {

  # --- A. k-core strength ---
  sr <- m3$core$all_strength_rank
  # Aggregate: max strength per herb (community-level max)
  kcore_df <- aggregate(Strength ~ Herb, data = sr, FUN = max, na.rm = TRUE)
  names(kcore_df)[2] <- "kcore"
  kcore_df$kcore_norm <- safe_normalize(kcore_df$kcore)

  # --- B. Weighted degree ---
  # Compute from pair edge data
  if (!is.null(m3$edge_data) && nrow(m3$edge_data) > 0) {
    edges <- m3$edge_data
    wt_col <- if (!is.null(m3$edge_weight_column) && m3$edge_weight_column %in% names(edges)) {
      m3$edge_weight_column
    } else if ("weight" %in% names(edges)) {
      "weight"
    } else if ("PMI" %in% names(edges)) {
      "PMI"
    } else {
      "freq"
    }
    from_df <- data.frame(Herb = edges$Herb1, weight = edges[[wt_col]], stringsAsFactors = FALSE)
    to_df   <- data.frame(Herb = edges$Herb2, weight = edges[[wt_col]], stringsAsFactors = FALSE)
    all_deg <- rbind(from_df, to_df)

    deg_sum <- aggregate(weight ~ Herb, data = all_deg, FUN = sum, na.rm = TRUE)
    names(deg_sum)[2] <- "weighted_degree"
    deg_sum$count <- aggregate(weight ~ Herb, data = all_deg, FUN = length)[, 2]
    deg_sum$weighted_degree_norm <- safe_normalize(deg_sum$weighted_degree)
  } else {
    deg_sum <- data.frame(Herb = character(0), weighted_degree = numeric(0),
                          count = integer(0), weighted_degree_norm = numeric(0),
                          stringsAsFactors = FALSE)
  }

  # --- C. Pair strength ---
  if (!is.null(m3$edge_data) && nrow(m3$edge_data) > 0) {
    edges <- m3$edge_data
    wt_col <- if (!is.null(m3$edge_weight_column) && m3$edge_weight_column %in% names(edges)) {
      m3$edge_weight_column
    } else if ("weight" %in% names(edges)) {
      "weight"
    } else if ("PMI" %in% names(edges)) {
      "PMI"
    } else {
      "freq"
    }
    edges$h1 <- pmin(edges$Herb1, edges$Herb2)
    edges$h2 <- pmax(edges$Herb1, edges$Herb2)
    edges$pair_key <- paste(edges$h1, edges$h2, sep = "||")

    pair_agg <- aggregate(edges[[wt_col]] ~ pair_key + h1 + h2, data = edges, FUN = max, na.rm = TRUE)
    names(pair_agg)[4] <- "pair_strength"

    # For each herb, avg of its top-5 strongest pair strengths
    pair_list <- rbind(
      data.frame(Herb = pair_agg$h1, pair_str = pair_agg$pair_strength, stringsAsFactors = FALSE),
      data.frame(Herb = pair_agg$h2, pair_str = pair_agg$pair_strength, stringsAsFactors = FALSE)
    )
    pair_df <- pair_list %>%
      group_by(Herb) %>%
      summarise(
        avg_pair_strength = mean(pair_str, na.rm = TRUE),
        max_pair_strength = max(pair_str, na.rm = TRUE),
        .groups = "drop"
      )
    pair_df$pair_norm <- safe_normalize(pair_df$avg_pair_strength)
  } else {
    pair_df <- data.frame(Herb = character(0), avg_pair_strength = numeric(0),
                          max_pair_strength = numeric(0), pair_norm = numeric(0),
                          stringsAsFactors = FALSE)
  }

  # --- D. Community info ---
  memb <- m3$memb
  herb_comm <- setNames(memb$Community, memb$Herb)

  # --- E. Merge all ---
  # Start with all unique herbs
  all_herbs <- sort(unique(c(kcore_df$Herb, deg_sum$Herb, pair_df$Herb, memb$Herb)))
  nw_df <- data.frame(Herb = all_herbs, stringsAsFactors = FALSE)

  nw_df <- left_join(nw_df, kcore_df[, c("Herb", "kcore", "kcore_norm")], by = "Herb")
  nw_df <- left_join(nw_df, deg_sum[, c("Herb", "weighted_degree", "weighted_degree_norm")], by = "Herb")
  nw_df <- left_join(nw_df, pair_df[, c("Herb", "avg_pair_strength", "pair_norm")], by = "Herb")

  # Fill NAs
  nw_df$kcore_norm[is.na(nw_df$kcore_norm)] <- 0
  nw_df$weighted_degree_norm[is.na(nw_df$weighted_degree_norm)] <- 0
  nw_df$pair_norm[is.na(nw_df$pair_norm)] <- 0

  # --- F. Compute NetworkWeight ---
  nw_df$NetworkWeight <- a * nw_df$kcore_norm +
                         b * nw_df$weighted_degree_norm +
                         c_param * nw_df$pair_norm

  nw_df$Community <- herb_comm[nw_df$Herb]
  nw_df$Community[is.na(nw_df$Community)] <- NA_integer_

  return(nw_df)
}
