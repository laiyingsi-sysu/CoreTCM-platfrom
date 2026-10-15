# ============================================================
# mod_6_comparison.R — Module 6: Comparison Center
# Dimensions 2-4: Pair, Community-Cluster, Core Formula
# ============================================================

mod_6_sidebar <- function(id) {
  ns <- NS(id)
  tagList(
    div(class = "param-section",
      h5(icon("info-circle"), "Data Status"),
      uiOutput(ns("data_status"))
    ),
    div(class = "param-section",
      h5(icon("sliders-h"), "Parameters"),
      sliderInput(ns("m6_pair_top_n"), "Pair Comparison Top-N", min = 5, max = 100, value = 20),
      sliderInput(ns("m6_herb_top_n"), "Herb Comparison Top-N", min = 5, max = 100, value = 20)
    ),
    div(class = "sidebar-action-bar",
      parameter_guide("m6"),
      actionButton(ns("run_m6"), "Run Comparison", icon = icon("play"),
                   class = "btn-primary btn-block"),
      br(),
      actionButton(ns("reset_params"), "Reset to Global", icon = icon("undo"),
                   class = "btn-sm btn-block")
    )
  )
}

mod_6_comparison_ui <- function(id) {
  ns <- NS(id)
  tagList(
    workflow_guide("m6"),
    fluidRow(
      box(title = "Comparison Center", status = "primary", solidHeader = TRUE, width = 12,
        p("Compare traditional and network-based workflows across 3 dimensions.")
      )
    ),
    br(),
    fluidRow(
      box(title = "Results", status = "success", solidHeader = TRUE, width = 12,
        tabBox(width = 12,
          tabPanel("Herb Pair Comparison",
            h5(p("M2 Association Rule Support vs M3 network edge weight for shared herb pairs. Weight reflects M3 Frequency, PMI, or Hybrid mode.")),
            br(),
            textOutput(ns("correlation_text")),
            br(),
            plotlyOutput(ns("plot_pair_scatter"), height = "450px"),
            br(),
            DTOutput(ns("dt_pair_overlap"))
          ),
          tabPanel("Community-Cluster Correspondence",
            fluidRow(
              column(6, plotlyOutput(ns("plot_sankey"), height = "500px")),
              column(6,
                verbatimTextOutput(ns("nmi_ari_text")),
                br(),
                DTOutput(ns("dt_cluster_comm_map"))
              )
            )
          ),
          tabPanel("Core Herbal Combination Comparison",
            h5("Per-community consensus: For each M3 community, the best-matching M2 cluster with its core herbs."),
            p(class="text-muted", style="font-size:11px;",
              "M3 Core = k-core extraction (structure-driven). M2 Core = top herbs in the best-matching cluster (frequency-driven)."),
            br(),
            DTOutput(ns("dt_core_compare"))
          )
        )
      )
    )
  )
}

mod_6_comparison_server <- function(input, output, session, shared_data) {
  datatable <- function(data, ...) localized_datatable(data, ..., lang = runtime_lang(shared_data))


  m6_res <- reactiveVal(NULL)

  output$data_status <- renderUI({
    m2 <- shared_data$module_results$m2
    m3 <- shared_data$module_results$m3
    has_rules <- !is.null(m2) && !is.null(m2$rules) && isTRUE(m2$rules$ok)
    has_cluster <- !is.null(m2) && !is.null(m2$cluster) && !is.null(m2$cluster$df) && nrow(m2$cluster$df) > 0
    has_network <- !is.null(m3) && !is.null(m3$memb)
    if (!has_rules && !has_cluster && !has_network)
      return(div(class = "alert alert-warning", "No comparable M2/M3 results. Run any relevant M2 or M3 workflow first."))
    states <- c(if (has_rules) "M2 rules ready" else "M2 rules unavailable",
                if (has_cluster) "M2 clusters ready" else "M2 clusters unavailable",
                if (has_network) "M3 network ready" else "M3 network unavailable")
    div(class = "alert alert-info", paste(vapply(states, runtime_tr, character(1), lang = runtime_lang(shared_data)), collapse = " · "))
  })

  observeEvent(input$reset_params, {
    p <- shared_data$active_params
    if (is.null(p)) {
      showNotification("No global parameters to reset (run M2 first).", type = "warning")
      return()
    }
    if (!is.null(p$m6_herb_top_n))
      updateSliderInput(session, "m6_herb_top_n", value = p$m6_herb_top_n)
    if (!is.null(p$m6_pair_top_n))
      updateSliderInput(session, "m6_pair_top_n", value = p$m6_pair_top_n)
    showNotification("Parameters reset", type = "message")
  })

  observeEvent(input$run_m6, {
    m2 <- shared_data$module_results$m2
    m3 <- shared_data$module_results$m3
    if (is.null(m2)) m2 <- list()
    if (is.null(m3)) m3 <- list()
    data <- shared_data$clean
    col  <- shared_data$clean_col

    showNotification("Running comparison...", type = "warning", duration = 3)

    # ===== 1. Herb Pair Comparison: M2 Support vs M3 Edge Weight =====
    pair_compare <- NULL
    dim2_status <- "ok"
    ok2 <- !is.null(m2$rules) && isTRUE(m2$rules$ok) && nrow(m2$rules$df) > 0
    ok3 <- !is.null(m3$edge_data) && nrow(m3$edge_data) > 0

    if (!ok2) {
      dim2_status <- "M2 association rules not available — run M2 Association Rules first"
    } else if (!ok3) {
      dim2_status <- "M3 network edges not available — run M3 Network Mining first"
    } else if (ok2 && ok3) {
      # --- M2: parse arules labels; retain rules with exactly one item on each side ---
      m2_pairs <- m2$rules$df
      parse_itemset <- function(x) {
        items <- str_split(str_replace_all(x, "[{}]", ""), "\\s*,\\s*")[[1]]
        items <- str_trim(items)
        items[nzchar(items)]
      }
      lhs_items <- lapply(m2_pairs$LHS, parse_itemset)
      rhs_items <- lapply(m2_pairs$RHS, parse_itemset)
      single_mask <- lengths(lhs_items) == 1L & lengths(rhs_items) == 1L
      m2_single <- m2_pairs[single_mask, , drop = FALSE]
      if (nrow(m2_single) > 0) {
        raw_h1 <- vapply(lhs_items[single_mask], `[`, character(1), 1)
        raw_h2 <- vapply(rhs_items[single_mask], `[`, character(1), 1)
        m2_single$h1 <- pmin(raw_h1, raw_h2)
        m2_single$h2 <- pmax(raw_h1, raw_h2)
        m2_single$pair_key <- paste(m2_single$h1, m2_single$h2, sep = "||")
        m2_agg <- aggregate(support ~ pair_key + h1 + h2, data = m2_single, FUN = max)
        m2_agg$support <- round(m2_agg$support, 4)
      } else {
        m2_agg <- data.frame()
      }

      # --- M3: use weight field matching selected network construction mode ---
      m3_edges <- m3$edge_data
      m3_edges$eh1 <- pmin(m3_edges$Herb1, m3_edges$Herb2)
      m3_edges$eh2 <- pmax(m3_edges$Herb1, m3_edges$Herb2)
      m3_edges$pair_key <- paste(m3_edges$eh1, m3_edges$eh2, sep = "||")
      edge_mode <- if (!is.null(m3$edge_weight_mode)) m3$edge_weight_mode else "PMI"
      wt_col <- if (!is.null(m3$edge_weight_column) && m3$edge_weight_column %in% names(m3_edges)) {
        m3$edge_weight_column
      } else if (edge_mode == "Frequency" && "freq" %in% names(m3_edges)) {
        "freq"
      } else if (edge_mode == "Hybrid" && "weight" %in% names(m3_edges)) {
        "weight"
      } else "PMI"
      edge_label <- paste0("M3 ", edge_mode, " Edge Weight")
      m3_agg <- aggregate(m3_edges[[wt_col]] ~ pair_key + eh1 + eh2, data = m3_edges, FUN = max)
      names(m3_agg)[4] <- "edge_weight"
      m3_agg$edge_weight <- round(m3_agg$edge_weight, 4)

      # Cross-reference
      if (nrow(m2_agg) > 0) {
        shared_keys <- intersect(m2_agg$pair_key, m3_agg$pair_key)
        n_shared <- length(shared_keys)

        # --- Fallback: single-herb overlap when pair overlap is low ---
        m2_herbs <- unique(c(m2_single$h1, m2_single$h2))
        m3_herbs <- unique(c(m3_edges$eh1, m3_edges$eh2))
        herb_shared <- intersect(m2_herbs, m3_herbs)
        n_herb_shared <- length(herb_shared)

        # Build cross-df from shared pairs (main comparison)
        if (n_shared > 0) {
          m2_sh <- m2_agg[match(shared_keys, m2_agg$pair_key), ]
          m3_sh <- m3_agg[match(shared_keys, m3_agg$pair_key), ]
          cross_df <- data.frame(
            Herb1 = m2_sh$h1, Herb2 = m2_sh$h2,
            Support = m2_sh$support, Edge_Weight = m3_sh$edge_weight,
            Label = paste(m2_sh$h1, "+", m2_sh$h2),
            stringsAsFactors = FALSE
          )
          cross_df <- cross_df[!duplicated(cross_df[, c("Herb1", "Herb2")]), ]

          sp_cor <- if (nrow(cross_df) >= 5) round(cor(cross_df$Support, cross_df$Edge_Weight, method = "spearman"), 3) else NA_real_
          pr_cor <- if (nrow(cross_df) >= 5) round(cor(cross_df$Support, cross_df$Edge_Weight, method = "pearson"), 3) else NA_real_

          med_sup <- median(cross_df$Support); med_wt <- median(cross_df$Edge_Weight)
          cross_df$Quadrant <- dplyr::case_when(
            cross_df$Support >= med_sup & cross_df$Edge_Weight >= med_wt ~ "High on both (consensus)",
            cross_df$Support >= med_sup & cross_df$Edge_Weight <  med_wt ~ "Support dominant",
            cross_df$Support <  med_sup & cross_df$Edge_Weight >= med_wt ~ paste(edge_mode, "dominant"),
            TRUE ~ "Low on both"
          )
          # Combined ranking
          cross_df$combined <- cross_df$Support * cross_df$Edge_Weight
          n_top_pairs <- min(input$m6_pair_top_n, nrow(cross_df))
          cross_top <- dplyr::slice_max(cross_df, combined, n = n_top_pairs)

          n_m2_only <- length(setdiff(m2_agg$pair_key, m3_agg$pair_key))
          n_m3_only <- length(setdiff(m3_agg$pair_key, m2_agg$pair_key))
        } else {
          cross_df      <- data.frame()
          cross_top     <- data.frame()
          sp_cor        <- NA_real_
          pr_cor        <- NA_real_
          med_sup       <- NA_real_
          med_wt        <- NA_real_
          n_m2_only     <- nrow(m2_agg)
          n_m3_only     <- nrow(m3_agg)
        }

        pair_compare <- list(
          cross_top  = cross_top,
          cross_all  = cross_df,
          sp_cor     = sp_cor,
          pr_cor     = pr_cor,
          n_shared   = n_shared,
          n_m2_only  = n_m2_only,
          n_m3_only  = n_m3_only,
          med_sup    = med_sup,
          med_wt     = med_wt,
          # Herb-level fallback
          n_herb_shared = n_herb_shared,
          n_m2_herbs    = length(m2_herbs),
          n_m3_herbs    = length(m3_herbs),
          herb_overlap  = herb_shared,
          edge_mode = edge_mode,
          edge_label = edge_label
        )
      } else {
        # M2 has no single-herb rules
        pair_compare <- list(
          n_shared = 0, n_m2_only = 0, n_m3_only = 0,
          msg = "M2 association rules are multi-herb combinations, not single herb pairs. Cannot compare at pair level."
        )
      }
    }

    # ===== Dimension 3: Community-Cluster Correspondence =====
    nmi <- NA_real_; ari <- NA_real_
    n_common_herbs <- 0
    dim3_status <- "ok"
    cluster_comm_map <- NULL
    cluster_df <- if (!is.null(m2$cluster)) m2$cluster$df else NULL
    clust_ok <- !is.null(cluster_df) && nrow(cluster_df) > 0 &&
      all(c("Herb", "Cluster") %in% names(cluster_df))
    comm_ok <- !is.null(m3$memb) && all(c("Herb", "Community") %in% names(m3$memb))

    if (!clust_ok) {
      dim3_status <- "M2 cluster assignment unavailable — run M2 clustering"
    } else if (!comm_ok) {
      dim3_status <- "M3 community membership unavailable — run M3 Network Mining"
    } else {
      cluster_df <- cluster_df[!duplicated(cluster_df$Herb), ]
      common_herbs <- intersect(cluster_df$Herb, m3$memb$Herb)
      n_common_herbs <- length(common_herbs)
      if (n_common_herbs <= 2) {
        dim3_status <- sprintf("Not applicable: need more than 2 common herbs (found %d)", n_common_herbs)
      } else {
        cl <- cluster_df$Cluster[match(common_herbs, cluster_df$Herb)]
        cm <- m3$memb$Community[match(common_herbs, m3$memb$Herb)]
        tbl <- table(cl, cm)
        if (nrow(tbl) < 2 || ncol(tbl) < 2) {
          dim3_status <- "Not applicable: ARI/NMI require at least two M2 clusters and two M3 communities"
        } else {
          n <- sum(tbl)
          row_marg <- rowSums(tbl) / n
          col_marg <- colSums(tbl) / n
          mi <- 0
          for (i in seq_len(nrow(tbl))) for (j in seq_len(ncol(tbl))) {
            if (tbl[i, j] > 0) {
              expected <- row_marg[i] * col_marg[j]
              mi <- mi + (tbl[i, j] / n) * log2(tbl[i, j] / n / expected)
            }
          }
          h_cl <- -sum(ifelse(row_marg > 0, row_marg * log2(row_marg), 0))
          h_cm <- -sum(ifelse(col_marg > 0, col_marg * log2(col_marg), 0))
          if (h_cl <= 0 || h_cm <= 0 || choose(n, 2) == 0) {
            dim3_status <- "Not applicable: ARI/NMI undefined for a degenerate partition"
          } else {
            nmi <- 2 * mi / (h_cl + h_cm)
            sum_nij_2 <- sum(choose(tbl, 2))
            sum_ni_2 <- sum(choose(rowSums(tbl), 2))
            sum_nj_2 <- sum(choose(colSums(tbl), 2))
            expected <- sum_ni_2 * sum_nj_2 / choose(n, 2)
            max_idx <- (sum_ni_2 + sum_nj_2) / 2
            if (max_idx == expected) {
              dim3_status <- "Not applicable: ARI undefined for a degenerate partition"
              nmi <- NA_real_
            } else {
              ari <- (sum_nij_2 - expected) / (max_idx - expected)
            }
          }
        }
        cluster_comm_map <- data.frame(
          Cluster = paste0("Clust_", cl), Community = paste0("C", cm),
          stringsAsFactors = FALSE
        )
      }
    }

    # ===== Dimension 4: Per-Community Core Consensus =====
    core_compare <- NULL
    dim4_status <- "ok"
    comm_ok <- !is.null(m3$memb) && !is.null(m3$core$core_formula_table)
    if (!clust_ok) {
      dim4_status <- "M2 cluster assignment unavailable — run M2 clustering"
    } else if (!comm_ok) {
      dim4_status <- "M3 core formula table unavailable — run M3 Network Mining fully"
    } else {
      memb <- m3$memb
      cf <- m3$core$core_formula_table
      herb_freq <- if ("Freq" %in% names(cluster_df)) cluster_df[, c("Herb", "Freq")] else NULL
      comm_ids <- sort(unique(memb$Community))
      rows <- list()
      for (cid in comm_ids) {
        m3_herbs <- memb$Herb[memb$Community == cid]
        cf_row <- cf[cf$Community == cid, ]
        m3_core <- if (nrow(cf_row) > 0) cf_row$Core_Formula[1] else ""
        common <- intersect(m3_herbs, cluster_df$Herb)
        if (length(common) == 0) next
        cl_for_common <- cluster_df$Cluster[match(common, cluster_df$Herb)]
        cl_tab <- table(cl_for_common)
        best_cl <- names(which.max(cl_tab))
        m2_cluster <- cluster_df[cluster_df$Cluster == as.integer(best_cl), , drop = FALSE]
        if (!is.null(herb_freq)) m2_cluster <- m2_cluster[order(-m2_cluster$Freq, m2_cluster$Herb), ]
        m2_core_herbs <- head(m2_cluster$Herb, input$m6_herb_top_n)
        m2_best_overlap <- sort(intersect(m2_cluster$Herb, m3_herbs))
        rows[[length(rows) + 1]] <- data.frame(
          M3_Community = paste0("C", cid), M3_N_Herbs = length(m3_herbs), M3_Core = m3_core,
          Best_M2_Cluster = paste0("Clust_", best_cl), Overlap_Count = as.integer(cl_tab[best_cl]),
          Overlap_Pct = round(as.integer(cl_tab[best_cl]) / length(m3_herbs) * 100, 1),
          M2_Top_Herbs = paste(m2_core_herbs, collapse = ", "),
          M2_M3_Overlap_Herbs = paste(m2_best_overlap, collapse = ", "), stringsAsFactors = FALSE
        )
      }
      if (length(rows) > 0) {
        core_compare <- do.call(rbind, rows)
        rownames(core_compare) <- NULL
      }
    }

    m6_res(list(
      pair_compare   = pair_compare,
      nmi            = nmi,
      ari            = ari,
      n_common_herbs = n_common_herbs,
      cluster_comm_map = cluster_comm_map,
      core_compare   = core_compare,
      dim2_status    = dim2_status,
      dim3_status    = dim3_status,
      dim4_status    = dim4_status
    ))
    showNotification("Comparison complete!", type = "message")
  })

  output$correlation_text <- renderText({
    lang <- runtime_lang(shared_data)
    r <- m6_res()
    if (is.null(r)) return("Run comparison")
    pc <- r$pair_compare
    if (!is.null(r$dim2_status) && r$dim2_status != "ok")
      return(paste0("Skipped: ", r$dim2_status))
    if (is.null(pc))
      return("No comparison data available. Run both M2 Association Rules and M3 Network Mining first.")
    if (!is.null(pc$msg)) return(pc$msg)

    if (identical(lang, "zh") && pc$n_shared > 0)
      return(sprintf("Spearman：%s | Pearson：%s | 共有药对：%d | 仅 M2：%d | 仅 M3：%d | 共有药物：%d（M2：%d，M3：%d）",
                     pc$sp_cor, pc$pr_cor, pc$n_shared, pc$n_m2_only, pc$n_m3_only,
                     pc$n_herb_shared, pc$n_m2_herbs, pc$n_m3_herbs))
    herb_info <- if (!is.null(pc$n_herb_shared))
      sprintf(" | Herb-level overlap: %d shared / %d M2 herbs / %d M3 herbs",
              pc$n_herb_shared, pc$n_m2_herbs, pc$n_m3_herbs) else ""

    if (pc$n_shared == 0) {
      return(sprintf("0 shared herb pairs found. M2: %d unique pairs, M3: %d unique edges.%s\n
Use herb-level overlap as a fallback comparison.", pc$n_m2_only, pc$n_m3_only, herb_info))
    }

    paste0(
      "Spearman: ", if(!is.na(pc$sp_cor)) pc$sp_cor else "N/A",
      " | Pearson: ", if(!is.na(pc$pr_cor)) pc$pr_cor else "N/A",
      " | Shared pairs: ", pc$n_shared,
      " | M2-only: ", pc$n_m2_only,
      " | M3-only: ", pc$n_m3_only,
      herb_info
    )
  })

  output$plot_pair_scatter <- renderPlotly({
    lang <- runtime_lang(shared_data)
    r <- m6_res(); pc <- if (is.null(r)) NULL else r$pair_compare
    if (is.null(pc) || !is.null(pc$msg))
      return(plotly_empty() %>% layout(title = runtime_tr("Cannot compare at pair level.", lang),
             paper_bgcolor = "#ffffff", plot_bgcolor = "#ffffff"))

    if (pc$n_shared == 0) {
      # Fallback: herb-level overlap heatmap-style
      d <- data.frame(
        Metric = c("M2 Unique Pairs", "M3 Unique Edges", "Shared Herbs", "M2 Unique Herbs", "M3 Unique Herbs"),
        Value = c(pc$n_m2_only, pc$n_m3_only, pc$n_herb_shared, pc$n_m2_herbs - pc$n_herb_shared, pc$n_m3_herbs - pc$n_herb_shared),
        stringsAsFactors = FALSE
      )
      return(plot_ly(d, x = ~reorder(Metric, Value), y = ~Value, type = "bar",
                     marker = list(color = "#3498db")) %>%
        layout(title = runtime_tr("Herb-level overlap (no shared pairs at pair level)", lang),
               xaxis = list(title = ""), yaxis = list(title = runtime_tr("Count", lang)),
               paper_bgcolor = "#ffffff", plot_bgcolor = "#ffffff"))
    }

    d <- pc$cross_all
    if (nrow(d) < 3) return(plotly_empty() %>% layout(title = runtime_tr("Insufficient shared pairs for scatter.", lang),
           paper_bgcolor = "#ffffff", plot_bgcolor = "#ffffff"))

    pal <- c("High on both (consensus)" = "#27ae60",
             "Support dominant" = "#e67e22",
             "Low on both" = "#95a5a6")
    pal[paste(pc$edge_mode, "dominant")] <- "#2980b9"
    d$Quadrant <- vapply(d$Quadrant, runtime_tr, character(1), lang = lang)
    names(pal) <- vapply(names(pal), runtime_tr, character(1), lang = lang)
    edge_title <- runtime_tr("M3 %s Edge Weight", lang, pc$edge_mode)

    plot_ly(d, x = ~Support, y = ~Edge_Weight, color = ~Quadrant, colors = pal,
            type = "scatter", mode = "markers",
            marker = list(size = 8, opacity = 0.7),
            text = ~Label, hoverinfo = "text") %>%
      layout(title = runtime_tr("M2 Support vs %s (%d shared pairs)", lang, edge_title, nrow(d)),
             xaxis = list(title = runtime_tr("M2 Association Rule Support", lang)),
             yaxis = list(title = edge_title),
             paper_bgcolor = "#ffffff", plot_bgcolor = "#ffffff")
  })

  output$dt_pair_overlap <- renderDT({
    lang <- runtime_lang(shared_data)
    r <- m6_res()
    if (is.null(r) || is.null(r$pair_compare)) return(datatable(localized_placeholder("Run comparison", lang), rownames = FALSE))
    pc <- r$pair_compare
    # Summary table
    n_shared <- pc$n_shared
    n_m2_only <- pc$n_m2_only
    n_m3_only <- pc$n_m3_only
    total_m2 <- n_shared + n_m2_only
    total_m3 <- n_shared + n_m3_only
    df <- data.frame(
      Metric = c("Shared Pairs", "M2 Association Rules Only", "M3 Network Edges Only",
                 "Total M2 Pairs", "Total M3 Edges",
                 "Spearman rho", "Pearson r"),
      Value = c(
        n_shared, n_m2_only, n_m3_only,
        total_m2, total_m3,
        if (!is.null(pc$sp_cor)) pc$sp_cor else NA_real_,
        if (!is.null(pc$pr_cor)) pc$pr_cor else NA_real_),
      stringsAsFactors = FALSE
    )
    df$Metric <- vapply(df$Metric, runtime_tr, character(1), lang = lang)
    datatable(localized_columns(df, lang), options = list(dom = "t", pageLength = 10), rownames = FALSE)
  })

  # ===== Dimension 3: Community-Cluster Correspondence =====
  output$nmi_ari_text <- renderText({
    lang <- runtime_lang(shared_data)
    r <- m6_res()
    if (is.null(r)) return(runtime_tr("Run comparison to see NMI/ARI scores.\nBoth M2 clustering and M3 communities must be available.", lang))
    if (!is.null(r$dim3_status) && r$dim3_status != "ok")
      return(paste0("NMI = NA   |   ARI = NA\n\n", r$dim3_status))
    if (is.na(r$nmi) || is.na(r$ari)) return(paste0("NMI = NA   |   ARI = NA\n\n", runtime_tr("Not applicable: metric is undefined for this partition.", lang)))
    if (identical(lang, "zh")) return(paste0(
      "NMI = ", round(r$nmi, 3), "   |   ARI = ", round(r$ari, 3),
      "   |   共有药物：", r$n_common_herbs, "\n\n",
      "NMI 衡量两种划分的信息重合程度，1 表示完全一致。\n",
      "ARI 校正随机一致性，0 表示接近随机一致，1 表示完全一致。\n",
      "这两项指标用于比较聚类与社区结构，不代表临床效果。\n\n",
      "表格每行为一个 M2 聚类，每列为一个 M3 社区；\n",
      "数值表示该聚类中归入相应社区的药物比例（%）。"))
    paste0(
      "NMI = ", round(r$nmi, 3), "   |   ARI = ", round(r$ari, 3),
      "   |   Common herbs: ", r$n_common_herbs, "\n\n",
      "Interpretation:\n",
      "  NMI measures information overlap (0 = no shared information, 1 = identical).\n",
      "  ARI adjusts for chance: 0 = random agreement, 1 = perfect match.\n",
      "  In TCM prescription mining, NMI > 0.3 suggests communities partially\n",
      "  recapitulate clustering structure; NMI > 0.6 implies strong convergence.\n",
      "  Values below 0.2 are expected when the two methods capture fundamentally\n",
      "  different aspects of the data (one frequency-driven, the other structure-driven).\n\n",
      "Table: Each row = one M2 cluster (hierarchical), each column = one M3 community.\n",
      "Values show the % of herbs in each M2 cluster that fall into each M3 community."
    )
  })

  output$dt_cluster_comm_map <- renderDT({
    lang <- runtime_lang(shared_data)
    r <- m6_res()
    if (is.null(r)) return(datatable(localized_placeholder("Run comparison", lang), rownames = FALSE))
    m <- r$cluster_comm_map
    if (!is.null(r$dim3_status) && r$dim3_status != "ok")
      return(datatable(localized_placeholder(paste0(runtime_tr("Skipped: ", lang), r$dim3_status), lang), rownames = FALSE))
    if (is.null(m)) return(datatable(localized_placeholder("Run comparison", lang), rownames = FALSE))
    tbl <- as.data.frame.matrix(table(m$Cluster, m$Community))
    # Convert to row percentages
    row_sums <- rowSums(tbl)
    tbl_pct <- round(tbl / row_sums * 100, 1)
    # Format: "count (pct%)"
    tbl_display <- as.data.frame(matrix(
      paste0(as.matrix(tbl), " (", as.matrix(tbl_pct), "%)"),
      nrow = nrow(tbl), ncol = ncol(tbl),
      dimnames = list(rownames(tbl), colnames(tbl))
    ))
    datatable(tbl_display, options = list(
      pageLength = 15, scrollX = TRUE,
      columnDefs = list(list(targets = "_all", className = "dt-nowrap"))
    ), rownames = TRUE)
  })

  output$plot_sankey <- renderPlotly({
    lang <- runtime_lang(shared_data)
    r <- m6_res(); m <- if (is.null(r)) NULL else r$cluster_comm_map
    if (is.null(m)) return(plotly_empty() %>%
      layout(paper_bgcolor = "#ffffff", plot_bgcolor = "#ffffff"))

    # Build Sankey from cluster -> community flows
    edges <- m %>% count(Cluster, Community, name = "value")
    all_nodes <- unique(c(edges$Cluster, edges$Community))
    nodes_df <- data.frame(name = all_nodes, stringsAsFactors = FALSE)
    nodes_df$id <- seq_len(nrow(nodes_df)) - 1

    edges$source <- nodes_df$id[match(edges$Cluster, nodes_df$name)]
    edges$target <- nodes_df$id[match(edges$Community, nodes_df$name)]
    edges$value  <- edges$value

    plot_ly(type = "sankey", orientation = "h",
            node = list(
              label = nodes_df$name,
              color = ifelse(nodes_df$name %in% unique(m$Cluster), "#2c3e50", "#3498db"),
              pad = 15, thickness = 20,
              line = list(color = "black", width = 0.5)),
            link = list(source = edges$source, target = edges$target,
                        value = edges$value)) %>%
      layout(title = runtime_tr("Cluster -> Community Flow", lang),
             paper_bgcolor = "#ffffff", plot_bgcolor = "#ffffff")
  })

  # ===== Dimension 4: Per-Community Core Consensus =====
  output$dt_core_compare <- renderDT({
    lang <- runtime_lang(shared_data)
    r <- m6_res()
    if (is.null(r)) return(datatable(localized_placeholder("Run comparison", lang), rownames = FALSE))
    cc <- r$core_compare
    if (!is.null(r$dim4_status) && r$dim4_status != "ok")
      return(datatable(localized_placeholder(paste0(runtime_tr("Skipped: ", lang), r$dim4_status), lang), rownames = FALSE))
    if (is.null(cc) || nrow(cc) == 0)
      return(datatable(localized_placeholder("Run comparison to see per-community consensus. Requires both M2 clustering and M3 network results.", lang), rownames = FALSE))
    datatable(cc, options = list(
      pageLength = 15, scrollX = TRUE,
      columnDefs = list(list(targets = "_all", className = "dt-nowrap"))
    ), rownames = FALSE) %>%
      formatRound("Overlap_Pct", 1) %>%
      formatStyle("Overlap_Pct",
        background = styleColorBar(cc$Overlap_Pct / 100, "#d4edda"),
        backgroundSize = "100% 90%", backgroundRepeat = "no-repeat")
  })
}
