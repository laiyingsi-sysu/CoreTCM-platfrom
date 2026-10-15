# ============================================================
# utils_network.R — Co-occurrence network construction
# (Restored from Rmd original — proven working code)
# ============================================================

build_cooccurrence_network <- function(data, prescription_col, min_freq = 3,
                                       positive_pmi_only = TRUE) {

  edge_list <- list()
  all_herbs <- c()

  for (i in 1:nrow(data)) {
    text <- data[[prescription_col]][i]
    if (is.na(text) || text == "") next

    herbs <- str_split(text, "\\s+")[[1]]
    herbs <- herbs[herbs != ""]
    herbs <- herbs[nchar(herbs) >= 2]
    herbs <- unique(herbs)

    all_herbs <- c(all_herbs, herbs)

    if (length(herbs) > 1) {
      comb <- t(combn(sort(herbs), 2))
      comb <- as.data.frame(comb, stringsAsFactors = FALSE)
      colnames(comb) <- c("Herb1", "Herb2")
      edge_list[[length(edge_list) + 1]] <- comb
    }
  }

  if (length(edge_list) == 0) stop("No co-occurrence found. Check prescription data.")

  edges <- bind_rows(edge_list)

  edge_weight <- edges %>%
    group_by(Herb1, Herb2) %>%
    summarise(freq = n(), .groups = "drop") %>%
    filter(freq >= min_freq) %>%
    arrange(Herb1, Herb2)

  herb_freq <- table(all_herbs) %>% as.data.frame()
  colnames(herb_freq) <- c("Herb", "freq")

  N <- nrow(data)

  edge_weight <- edge_weight %>%
    left_join(herb_freq %>% rename(freq1 = freq), by = c("Herb1" = "Herb")) %>%
    left_join(herb_freq %>% rename(freq2 = freq), by = c("Herb2" = "Herb")) %>%
    mutate(
      p_ij = freq / N, p_i = freq1 / N, p_j = freq2 / N,
      PMI  = log(p_ij / (p_i * p_j))
    )

  if (positive_pmi_only) edge_weight <- edge_weight %>% filter(PMI > 0)

  g <- graph_from_data_frame(edge_weight, directed = FALSE)
  E(g)$weight <- edge_weight$PMI

  return(list(graph = g, edge_data = edge_weight, node_freq = herb_freq))
}

build_network_pmi_freq <- function(data, prescription_col, min_freq = 3,
                                    alpha = 0.5, beta = 0.5,
                                    positive_pmi_only = FALSE) {

  edge_list <- list()
  all_herbs <- c()

  for (i in 1:nrow(data)) {
    text <- data[[prescription_col]][i]
    if (is.na(text) || text == "") next

    herbs <- str_split(text, "\\s+")[[1]]
    herbs <- herbs[herbs != ""]
    herbs <- herbs[nchar(herbs) >= 2]
    herbs <- unique(herbs)

    all_herbs <- c(all_herbs, herbs)

    if (length(herbs) > 1) {
      comb <- t(combn(sort(herbs), 2))
      comb <- as.data.frame(comb, stringsAsFactors = FALSE)
      colnames(comb) <- c("Herb1", "Herb2")
      edge_list[[length(edge_list) + 1]] <- comb
    }
  }

  edges <- bind_rows(edge_list)

  herb_freq <- table(all_herbs) %>% as.data.frame()
  colnames(herb_freq) <- c("Herb", "freq")

  N <- nrow(data)

  edge_stat <- edges %>%
    group_by(Herb1, Herb2) %>%
    summarise(freq = n(), .groups = "drop") %>%
    filter(freq >= min_freq) %>%
    arrange(Herb1, Herb2) %>%
    left_join(herb_freq %>% rename(freq1 = freq), by = c("Herb1" = "Herb")) %>%
    left_join(herb_freq %>% rename(freq2 = freq), by = c("Herb2" = "Herb")) %>%
    mutate(
      p_ij = freq / N, p_i = freq1 / N, p_j = freq2 / N,
      PMI  = log(p_ij / (p_i * p_j))
    )

  if (positive_pmi_only) edge_stat <- edge_stat %>% filter(PMI > 0)

  edge_stat <- edge_stat %>%
    mutate(
      n_freq = normalize(freq),
      n_pmi  = normalize(PMI),
      weight = alpha * n_pmi + beta * n_freq
    )

  g <- graph_from_data_frame(edge_stat, directed = FALSE)
  E(g)$weight <- edge_stat$weight

  return(list(graph = g, edge_data = edge_stat, node_freq = herb_freq))
}
