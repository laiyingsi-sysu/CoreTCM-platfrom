# ============================================================
# mod_5_mapping.R — Module 5: Formula-Syndrome Association (方证关联分析)
# ============================================================

mod_5_sidebar <- function(id) {
  ns <- NS(id)
  tagList(
    div(class = "param-section",
      h5(icon("info-circle"), "Data Status"),
      uiOutput(ns("data_status"))
    ),
    div(class = "param-section",
      h5(icon("cogs"), "Mapping Settings"),
      h5("Available Data"),
      uiOutput(ns("available_ui"))
    ),
    div(class = "sidebar-action-bar",
      parameter_guide("m5"),
      actionButton(ns("run_m5"), "Run Association Analysis", icon = icon("play"),
                   class = "btn-success btn-block"),
      br(),
      p(class = "text-muted", style = "font-size:11px;",
        "Requires M1 (Clinical Features) and M3 (Network Mining) results.")
    )
  )
}

mod_5_feat_ui <- function(id) {
  ns <- NS(id)
  tagList(
    workflow_guide("m5"),
    fluidRow(
      box(id = ns("fsa_title_box"),
          title = "Formula-Syndrome Association", status = "primary",
          solidHeader = TRUE, width = 12,
          p("Map network mining results to clinical features, and recommend herb formula
             for specific TCM clinical patterns.")
      )
    ),
    fluidRow(
      box(title = "Results", status = "success", solidHeader = TRUE, width = 12,
        uiOutput(ns("m5_not_run")),
        uiOutput(ns("m5_na_warning")),
        tabBox(width = 12,
          tabPanel("Community x Feature",
            h5("Community x Disease Location (病位)"),
            DTOutput(ns("dt_comm_bingwei")),
            br(),
            plotlyOutput(ns("plot_comm_bingwei"), height = "350px"),
            br(),
            h5("Community x Disease Nature (病性)"),
            DTOutput(ns("dt_comm_bingxing")),
            br(),
            plotlyOutput(ns("plot_comm_bingxing"), height = "350px")
          ),
          tabPanel("Community x Syndrome",
            h5("Community x Standardized Syndrome (标准证型)"),
            DTOutput(ns("dt_comm_syndrome")),
            br(),
            plotlyOutput(ns("plot_comm_syndrome"), height = "350px")
          ),
          tabPanel("Core Herbal Combination x Feature",
            DTOutput(ns("dt_core_feature"))
          )
        )
      )
    )
  )
}

mod_5_rec_sidebar <- function(id) {
  ns <- NS(id)
  tagList(
    div(class = "param-section",
      h5(icon("info-circle"), "Data Status"),
      uiOutput(ns("data_status"))
    ),
    div(class = "param-section",
      h5(icon("cogs"), "Step 1: Train Classifier Chain"),
      p(class = "text-muted", style = "font-size:11px;",
        "Herbs sorted by frequency. Each herb model receives previous herbs' predictions as input."),
      sliderInput(ns("m5_min_herb_pct"), "Min Herb Frequency (%)", 0.1, 5, 0.5, 0.1),
      sliderInput(ns("m5_max_chain"), "Max Chain Length", min = 1, max = 20, value = 10, step = 1),
      sliderInput(ns("m5_max_cat_levels"), "Max Categorical Levels", min = 2, max = 30, value = 10, step = 1),
      h6("Covariate Type Assignment"),
      p(class = "text-muted", style = "font-size:11px;",
        "Manually set each extra column as numeric / categorical / exclude. Auto-detected by default — override if wrong."),
      uiOutput(ns("baseline_cols_ui")),
      br(),
      div(style = "display:flex; justify-content:center;",
        actionButton(ns("train_model"), "Train Recommendation Model",
                     icon = icon("cogs"), class = "btn-primary")
      ),
      br(),
      uiOutput(ns("model_status_ui"))
    ),
    div(class = "param-section",
      h5(icon("search"), "Step 2: Target Features"),
      uiOutput(ns("rec_bingwei_select")),
      uiOutput(ns("rec_bingxing_select")),
      h5("Baseline Covariate Values"),
      uiOutput(ns("baseline_values_ui"))
    ),
    div(class = "param-section",
      h5(icon("network-wired"), "Step 3: Network Enhancement Weights"),
      uiOutput(ns("nw_status_ui")),
      p(class = "text-muted", style = "font-size:11px;",
        "Score_h = P_h × (1 + λ·W_h) × (1 + γ·C_h). W_h = α·K + β·D + δ·R."),
      h6("Network Structural Score W_h"),
      selectInput(ns("w_preset"), "Weight Preset", choices = c(
        "Balanced" = "balanced",
        "Core Priority" = "core",
        "Co-occurrence Priority" = "pair"
      ), selected = "balanced"),
      sliderInput(ns("w_alpha"), "α — k-core strength", min = 0, max = 1, value = 0.33, step = 0.01),
      sliderInput(ns("w_beta_share"), "β share of remaining weight", min = 0, max = 1, value = 0.5, step = 0.01),
      uiOutput(ns("weight_sum_warn")),
      h6("Enhancement Coefficients"),
      sliderInput(ns("w_lambda"), "λ — network enhancement factor", min = 0, max = 1, value = 0.5, step = 0.05),
      sliderInput(ns("w_gamma"), "γ — core formula bonus",         min = 0, max = 1, value = 0.3, step = 0.05)
    ),
    div(class = "sidebar-action-bar",
      parameter_guide("m5"),
      actionButton(ns("run_recommend"), "Generate Recommendation",
                   icon = icon("search"), class = "btn-success btn-block"),
      br(),
      downloadButton(ns("dl_rec"), "Download Recommendation (CSV)", class = "btn-sm btn-block")
    )
  )
}

mod_5_rec_ui <- function(id) {
  ns <- NS(id)
  tagList(
    workflow_guide("m5"),
    fluidRow(
      box(title = "Model Performance (AUC per Herb)", status = "success", solidHeader = TRUE, width = 12,
        DTOutput(ns("dt_model_perf"))
      )
    ),
    br(),
    fluidRow(
      box(title = "Recommendation Results", status = "success", solidHeader = TRUE, width = 12,
        tabBox(width = 12,
          tabPanel("Recommended Herbs",
            h5("Network-Enhanced Classifier Chain Recommendation"),
            p(class = "text-muted", style = "font-size:11px;",
              "Ranking = EnhancedScore = P × (1 + λW) × (1 + γC). ",
              "W = α·k-core + β·weighted-degree + δ·pair-strength. ",
              "RankChange > 0 (green) = moved up after network enhancement."),
            DTOutput(ns("dt_recommend")),
            br(),
            plotlyOutput(ns("plot_recommend"), height = "400px")
          )
        )
      )
    )
  )
}

mod_5_mapping_server <- function(input, output, session, shared_data) {
  datatable <- function(data, ...) localized_datatable(data, ..., lang = runtime_lang(shared_data))


  m5_results <- reactiveVal(NULL)

  output$data_status <- renderUI({
    m1_ok <- !is.null(shared_data$module_results$m1)
    m3_ok <- !is.null(shared_data$module_results$m3)
    if (!m1_ok && !m3_ok)
      div(class = "alert alert-warning", "M1 and M3 not run yet.")
    else if (!m1_ok)
      div(class = "alert alert-warning", "M1 (Clinical Features) not run yet.")
    else if (!m3_ok)
      div(class = "alert alert-warning", "M3 (Network Mining) not run yet.")
    else
      div(class = "alert alert-success", "M1 and M3 ready.")
  })

  output$available_ui <- renderUI({
    m1 <- shared_data$module_results$m1
    m3 <- shared_data$module_results$m3
    tagList(
      p(if (!is.null(m1)) span(class = "text-success", icon("check"), runtime_tr("M1: %d syndromes", runtime_lang(shared_data), m1$n_zx))
        else span(class = "text-warning", icon("times"), " M1: not run")),
      p(if (!is.null(m3)) span(class = "text-success", icon("check"), runtime_tr("M3: %d communities", runtime_lang(shared_data), m3$n_comm))
        else span(class = "text-warning", icon("times"), " M3: not run"))
    )
  })

  output$m5_not_run <- renderUI({
    if (is.null(m5_results()))
      div(class = "alert alert-info", "Click 'Run Association Analysis' to compute.")
    else NULL
  })

  output$m5_na_warning <- renderUI({
    r <- m5_results()
    if (is.null(r)) return(NULL)
    n_miss <- r$n_missing
    if (is.null(n_miss) || n_miss == 0) return(NULL)
    div(class = "alert alert-warning",
        icon("exclamation-triangle"),
        paste0("Dataset contains ", n_miss, " NA syndrome rows. ",
               "These were filtered out of syndrome/disease-location/pathogenesis ",
               "distribution stats. Percentages may not sum to 100% — ",
               "(missing) rows show NA scale. Results have selection bias risk."))
  })

  observeEvent(input$run_m5, {
    m1 <- shared_data$module_results$m1
    m3 <- shared_data$module_results$m3
    if (is.null(m1) || is.null(m3)) {
      showNotification("Both M1 and M3 must be run first.", type = "warning"); return()
    }

    df <- shared_data$clean
    col <- shared_data$clean_col
    if (is.null(col) || length(col) == 0 || !col %in% names(df)) {
      fallback <- grep("_std$|_clean_processed_std$", names(df), value = TRUE)
      if (length(fallback) > 0) col <- fallback[1]
      else {
        fallback <- grep("中药|处方|herb|prescription", names(df), ignore.case = TRUE, value = TRUE)
        if (length(fallback) > 0) col <- fallback[1]
      }
    }
    if (is.null(col) || length(col) == 0 || !col %in% names(df)) {
      all_cols <- paste(head(names(df), 10), collapse = ", ")
      showNotification(paste("Cannot identify prescription column. Available columns:", all_cols), type = "error", duration = 10); return()
    }

    feat <- m1$features
    bw_col <- if ("Disease_Location" %in% names(feat)) "Disease_Location" else NULL
    bx_col <- if ("Pathogenesis" %in% names(feat)) "Pathogenesis" else NULL
    syn_col <- if ("Syndrome_Standardized" %in% names(feat)) "Syndrome_Standardized" else NULL
    if (is.null(bw_col) && is.null(bx_col) && is.null(syn_col)) {
      showNotification("No clinical features found in M1.", type = "warning"); return()
    }

    presc_contains <- function(herbs, data, col) {
      all_herbs <- str_split(data[[col]], "\\s+")
      sapply(all_herbs, function(h) any(herbs %in% h))
    }

    memb <- m3$memb; comm_ids <- sort(unique(memb$Community))

    # === 1. Community x Clinical Feature ===
    comm_bw_rows <- list(); comm_bx_rows <- list(); comm_syn_rows <- list()
    for (cid in comm_ids) {
      herbs_c <- memb$Herb[memb$Community == cid]
      idx <- presc_contains(herbs_c, df, col)

      if (!is.null(bw_col)) {
        vals <- unlist(str_split(feat[[bw_col]][idx], "[、，,\\s]+"))
        vals <- vals[nchar(vals) >= 1 & vals != "" & !is.na(vals)]
        tab <- sort(table(vals), decreasing = TRUE)
        for (nm in names(tab)) {
          pct_val <- round(tab[nm] / sum(idx) * 100, 2)
          comm_bw_rows[[length(comm_bw_rows) + 1]] <- data.frame(
            Community = paste0("C", cid),
            Disease_Location = nm, Count = as.integer(tab[nm]),
            Percentage = pct_val, stringsAsFactors = FALSE)
        }
        # Ensure all communities appear in table even if no feature extracted
        if (length(tab) == 0) {
          comm_bw_rows[[length(comm_bw_rows) + 1]] <- data.frame(
            Community = paste0("C", cid),
            Disease_Location = "(none)", Count = 0, Percentage = 0, stringsAsFactors = FALSE)
        }
        # (missing) row: prescriptions in this community with NA/empty bw_col
        raw_bw <- feat[[bw_col]][idx]
        n_miss_bw <- sum(is.na(raw_bw) | raw_bw == "")
        if (n_miss_bw > 0) {
          comm_bw_rows[[length(comm_bw_rows) + 1]] <- data.frame(
            Community = paste0("C", cid),
            Disease_Location = "(missing)", Count = n_miss_bw,
            Percentage = round(n_miss_bw / sum(idx) * 100, 2), stringsAsFactors = FALSE)
        }
      }

      if (!is.null(bx_col)) {
        vals <- unlist(str_split(feat[[bx_col]][idx], "[、，,\\s]+"))
        vals <- vals[nchar(vals) >= 1 & vals != "" & !is.na(vals)]
        tab <- sort(table(vals), decreasing = TRUE)
        for (nm in names(tab)) {
          pct_val <- round(tab[nm] / sum(idx) * 100, 2)
          comm_bx_rows[[length(comm_bx_rows) + 1]] <- data.frame(
            Community = paste0("C", cid),
            Pathogenesis = nm, Count = as.integer(tab[nm]),
            Percentage = pct_val, stringsAsFactors = FALSE)
        }
        if (length(tab) == 0) {
          comm_bx_rows[[length(comm_bx_rows) + 1]] <- data.frame(
            Community = paste0("C", cid),
            Pathogenesis = "(none)", Count = 0, Percentage = 0, stringsAsFactors = FALSE)
        }
        # (missing) row: prescriptions in this community with NA/empty bx_col
        raw_bx <- feat[[bx_col]][idx]
        n_miss_bx <- sum(is.na(raw_bx) | raw_bx == "")
        if (n_miss_bx > 0) {
          comm_bx_rows[[length(comm_bx_rows) + 1]] <- data.frame(
            Community = paste0("C", cid),
            Pathogenesis = "(missing)", Count = n_miss_bx,
            Percentage = round(n_miss_bx / sum(idx) * 100, 2), stringsAsFactors = FALSE)
        }
      }

      if (!is.null(syn_col)) {
        vals <- feat[[syn_col]][idx]
        vals <- vals[nchar(vals) >= 1 & vals != "" & !is.na(vals)]
        tab <- sort(table(vals), decreasing = TRUE)
        for (nm in names(tab)) {
          pct_val <- round(tab[nm] / sum(idx) * 100, 2)
          comm_syn_rows[[length(comm_syn_rows) + 1]] <- data.frame(
            Community = paste0("C", cid),
            Syndrome = nm, Count = as.integer(tab[nm]),
            Percentage = pct_val, stringsAsFactors = FALSE)
        }
        if (length(tab) == 0) {
          comm_syn_rows[[length(comm_syn_rows) + 1]] <- data.frame(
            Community = paste0("C", cid),
            Syndrome = "(none)", Count = 0, Percentage = 0, stringsAsFactors = FALSE)
        }
        # (missing) row: prescriptions in this community with NA/empty syn_col
        raw_syn <- feat[[syn_col]][idx]
        n_miss_syn <- sum(is.na(raw_syn) | raw_syn == "")
        if (n_miss_syn > 0) {
          comm_syn_rows[[length(comm_syn_rows) + 1]] <- data.frame(
            Community = paste0("C", cid),
            Syndrome = "(missing)", Count = n_miss_syn,
            Percentage = round(n_miss_syn / sum(idx) * 100, 2), stringsAsFactors = FALSE)
        }
      }
    }
    comm_bw_df <- if (length(comm_bw_rows) > 0) do.call(rbind, comm_bw_rows) else NULL
    comm_bx_df <- if (length(comm_bx_rows) > 0) do.call(rbind, comm_bx_rows) else NULL
    comm_syn_df <- if (length(comm_syn_rows) > 0) do.call(rbind, comm_syn_rows) else NULL
    if (is.null(comm_bw_df) && is.null(comm_bx_df) && is.null(comm_syn_df)) {
      comm_bw_df <- data.frame(Note = "No clinical features found.", stringsAsFactors = FALSE)
      comm_bx_df <- data.frame(Note = "No clinical features found.", stringsAsFactors = FALSE)
      comm_syn_df <- data.frame(Note = "No clinical features found.", stringsAsFactors = FALSE)
    }

    # === 2. Core Formula x Clinical Feature ===
    cf <- m3$core$core_formula_table; core_feature_rows <- list()
    for (i in seq_len(nrow(cf))) {
      herbs <- unlist(str_split(cf$Core_Formula[i], "\\s+"))
      if (length(herbs) == 0) next
      idx <- presc_contains(herbs, df, col)
      if (sum(idx) == 0) next

      bw_features <- ""
      bw_pct_total <- 0
      if (!is.null(bw_col)) {
        vals <- unlist(str_split(feat[[bw_col]][idx], "[、，,\\s]+"))
        vals <- vals[nchar(vals) >= 1 & vals != "" & !is.na(vals)]
        if (length(vals) > 0) {
          t <- sort(table(vals), decreasing = TRUE)
          pcts <- t / sum(idx) * 100
          cum <- cumsum(pcts)
          keep <- which(cum <= 80 | (cum > 80 & lag(cum, default = 0) < 80))
          bw_features <- paste0(names(t)[keep], " (", round(pcts[keep], 2), "%)",
                                collapse = "; ")
          bw_pct_total <- round(min(cum[length(keep)], 100), 0)
        }
      }

      bx_features <- ""
      bx_pct_total <- 0
      if (!is.null(bx_col)) {
        vals <- unlist(str_split(feat[[bx_col]][idx], "[、，,\\s]+"))
        vals <- vals[nchar(vals) >= 1 & vals != "" & !is.na(vals)]
        if (length(vals) > 0) {
          t <- sort(table(vals), decreasing = TRUE)
          pcts <- t / sum(idx) * 100
          cum <- cumsum(pcts)
          keep <- which(cum <= 80 | (cum > 80 & lag(cum, default = 0) < 80))
          bx_features <- paste0(names(t)[keep], " (", round(pcts[keep], 2), "%)",
                                collapse = "; ")
          bx_pct_total <- round(min(cum[length(keep)], 100), 0)
        }
      }

      core_feature_rows[[length(core_feature_rows) + 1]] <- data.frame(
        Community = paste0("C", cf$Community[i]), Core_Formula = cf$Core_Formula[i],
        N_Presc = sum(idx),
        Disease_Location = bw_features, BW_Coverage = paste0(bw_pct_total, "%"),
        Pathogenesis = bx_features, BX_Coverage = paste0(bx_pct_total, "%"),
        stringsAsFactors = FALSE)
    }
    core_feature_df <- do.call(rbind, core_feature_rows)
    if (is.null(core_feature_df) || nrow(core_feature_df) == 0)
      core_feature_df <- data.frame(Note = "No core formulas to map.", stringsAsFactors = FALSE)

    # === 3. Reverse index for recommendation ===
    sr <- m3$core$all_strength_rank
    herb_strength <- setNames(sr$Strength, sr$Herb)
    rec_index <- list()
    # Disease Location
    if (!is.null(bw_col) && !is.null(comm_bw_df) && !is.null(comm_bw_df$Note)) {
      sub <- comm_bw_df
    } else if (!is.null(comm_bw_df)) {
      sub <- comm_bw_df[!is.na(comm_bw_df$Disease_Location) & comm_bw_df$Disease_Location != "(none)", ]
      if (nrow(sub) > 0) {
        features <- unique(sub$Disease_Location)
        rec_index[["Disease Location"]] <- lapply(features, function(f) {
          rows <- sub[sub$Disease_Location == f, ]
          rows <- rows[order(-rows$Percentage), ]
          rows$Feature <- rows$Disease_Location
          top_comm <- rows$Community[1]
          top_cid <- as.integer(gsub("C", "", top_comm))
          top_herbs <- memb$Herb[memb$Community == top_cid]
          top_herbs_str <- names(sort(herb_strength[top_herbs], decreasing = TRUE))
          cf_row <- cf[cf$Community == top_cid, ]
          top_core <- if (nrow(cf_row) > 0) cf_row$Core_Formula[1] else ""
          list(feature = f, communities = rows, top_community = top_comm,
               top_community_percentage = rows$Percentage[1],
               top_core_formula = top_core, ranked_herbs = top_herbs_str)
        })
        names(rec_index[["Disease Location"]]) <- features
      }
    }
    # Pathogenesis
    if (!is.null(bx_col) && !is.null(comm_bx_df) && !is.null(comm_bx_df$Note)) {
      NULL  # no data
    } else if (!is.null(comm_bx_df)) {
      sub <- comm_bx_df[!is.na(comm_bx_df$Pathogenesis) & comm_bx_df$Pathogenesis != "(none)", ]
      if (nrow(sub) > 0) {
        features <- unique(sub$Pathogenesis)
        rec_index[["Pathogenesis"]] <- lapply(features, function(f) {
          rows <- sub[sub$Pathogenesis == f, ]
          rows <- rows[order(-rows$Percentage), ]
          rows$Feature <- rows$Disease_Location
          top_comm <- rows$Community[1]
          top_cid <- as.integer(gsub("C", "", top_comm))
          top_herbs <- memb$Herb[memb$Community == top_cid]
          top_herbs_str <- names(sort(herb_strength[top_herbs], decreasing = TRUE))
          cf_row <- cf[cf$Community == top_cid, ]
          top_core <- if (nrow(cf_row) > 0) cf_row$Core_Formula[1] else ""
          list(feature = f, communities = rows, top_community = top_comm,
               top_community_percentage = rows$Percentage[1],
               top_core_formula = top_core, ranked_herbs = top_herbs_str)
        })
        names(rec_index[["Pathogenesis"]]) <- features
      }
    }

    # Total NA count across clinical feature columns (for disclaimer banner)
    n_missing_total <- 0
    if (!is.null(syn_col) && syn_col %in% names(feat))
      n_missing_total <- n_missing_total + sum(is.na(feat[[syn_col]]) | feat[[syn_col]] == "")
    if (!is.null(bw_col) && bw_col %in% names(feat))
      n_missing_total <- n_missing_total + sum(is.na(feat[[bw_col]]) | feat[[bw_col]] == "")
    if (!is.null(bx_col) && bx_col %in% names(feat))
      n_missing_total <- n_missing_total + sum(is.na(feat[[bx_col]]) | feat[[bx_col]] == "")

    m5_results(list(
      comm_bingwei  = comm_bw_df,
      comm_bingxing = comm_bx_df,
      comm_syndrome = comm_syn_df,
      core_feature  = core_feature_df,
      rec_index     = rec_index,
      memb          = memb,
      cf            = cf,
      herb_strength = herb_strength,
      # ML-ready data
      features_df   = feat,
      prescriptions = df[[col]],
      raw_data      = df,
      n_missing     = n_missing_total
    ))
    showNotification("Association analysis complete. Ready for model training.", type = "message")
  })

  # ===== Tab 1: Community x Disease Location =====
  output$dt_comm_bingwei <- renderDT({
    lang <- runtime_lang(shared_data)
    r <- m5_results()
    if (is.null(r) || is.null(r$comm_bingwei)) return(datatable(localized_placeholder("Run analysis first.", lang), rownames = FALSE))
    d <- r$comm_bingwei
    if (!is.null(d$Note)) return(datatable(localized_columns(d, lang), rownames = FALSE))
    datatable(localized_columns(d, lang), options = list(pageLength = 15), rownames = FALSE) %>%
      formatRound(runtime_tr("Percentage", lang), 2)
  })

  output$dt_comm_bingxing <- renderDT({
    lang <- runtime_lang(shared_data)
    r <- m5_results()
    if (is.null(r) || is.null(r$comm_bingxing)) return(datatable(localized_placeholder("Run analysis first.", lang), rownames = FALSE))
    d <- r$comm_bingxing
    if (!is.null(d$Note)) return(datatable(localized_columns(d, lang), rownames = FALSE))
    datatable(localized_columns(d, lang), options = list(pageLength = 15), rownames = FALSE) %>%
      formatRound(runtime_tr("Percentage", lang), 2)
  })

  output$plot_comm_bingwei <- renderPlotly({
    lang <- runtime_lang(shared_data)
    r <- m5_results(); d <- if (is.null(r)) NULL else r$comm_bingwei
    if (is.null(r) || is.null(d) || !is.null(d$Note))
      return(plotly_empty() %>% layout(paper_bgcolor = "#ffffff", plot_bgcolor = "#ffffff"))
    d <- d[!is.na(d$Disease_Location) & d$Disease_Location != "(none)", ]
    if (nrow(d) == 0) return(plotly_empty() %>%
      layout(title = runtime_tr("No disease location data", lang), paper_bgcolor = "#ffffff", plot_bgcolor = "#ffffff"))
    mat <- as.data.frame.matrix(xtabs(Percentage ~ Community + Disease_Location, data = d))
    if (nrow(mat) <= 1) return(plotly_empty() %>%
      layout(title = runtime_tr("Insufficient data", lang), paper_bgcolor = "#ffffff", plot_bgcolor = "#ffffff"))
    m <- as.matrix(mat)
    plot_ly(x = colnames(m), y = rownames(m), z = m, type = "heatmap",
            colorscale = list(
              c(0, "#fcfcfc"), c(0.15, "#fee5d9"), c(0.35, "#fcae91"),
              c(0.5, "#fb6a4a"), c(0.7, "#ef3b3b"), c(0.85, "#dc143c"), c(1, "#b71c1c")
            ),
            zmin = 0, zmax = 100, zauto = FALSE,
            colorbar = list(title = list(text = "%", font = list(size = 11))),
            hovertemplate = "%{y} x %{x}<br>%{z:.1f}%<extra></extra>") %>%
      layout(title = runtime_tr("Community x Disease Location (%)", lang), xaxis = list(title = ""), yaxis = list(title = ""),
             paper_bgcolor = "#ffffff", plot_bgcolor = "#ffffff")
  })

  output$dt_comm_syndrome <- renderDT({
    lang <- runtime_lang(shared_data)
    r <- m5_results()
    if (is.null(r) || is.null(r$comm_syndrome)) return(datatable(localized_placeholder("Run analysis first.", lang), rownames = FALSE))
    d <- r$comm_syndrome
    if (!is.null(d$Note)) return(datatable(localized_columns(d, lang), rownames = FALSE))
    datatable(localized_columns(d, lang), options = list(pageLength = 15), rownames = FALSE) %>%
      formatRound(runtime_tr("Percentage", lang), 2)
  })

  output$plot_comm_syndrome <- renderPlotly({
    lang <- runtime_lang(shared_data)
    r <- m5_results(); d <- if (is.null(r)) NULL else r$comm_syndrome
    if (is.null(r) || is.null(d) || !is.null(d$Note))
      return(plotly_empty() %>% layout(paper_bgcolor = "#ffffff", plot_bgcolor = "#ffffff"))
    d <- d[!is.na(d$Syndrome) & d$Syndrome != "(none)", ]
    if (nrow(d) == 0) return(plotly_empty() %>%
      layout(title = runtime_tr("No syndrome data", lang), paper_bgcolor = "#ffffff", plot_bgcolor = "#ffffff"))
    mat <- as.data.frame.matrix(xtabs(Percentage ~ Community + Syndrome, data = d))
    if (nrow(mat) <= 1) return(plotly_empty() %>%
      layout(title = runtime_tr("Insufficient data", lang), paper_bgcolor = "#ffffff", plot_bgcolor = "#ffffff"))
    m <- as.matrix(mat)
    plot_ly(x = colnames(m), y = rownames(m), z = m, type = "heatmap",
            colorscale = list(
              c(0, "#fcfcfc"), c(0.15, "#fee5d9"), c(0.35, "#fcae91"),
              c(0.5, "#fb6a4a"), c(0.7, "#ef3b3b"), c(0.85, "#dc143c"), c(1, "#b71c1c")
            ),
            zmin = 0, zmax = 100, zauto = FALSE,
            colorbar = list(title = list(text = "%", font = list(size = 11))),
            hovertemplate = "%{y} x %{x}<br>%{z:.1f}%<extra></extra>") %>%
      layout(title = runtime_tr("Community x Standardized Syndrome (%)", lang), xaxis = list(title = ""), yaxis = list(title = ""),
             paper_bgcolor = "#ffffff", plot_bgcolor = "#ffffff")
  })

  output$plot_comm_bingxing <- renderPlotly({
    lang <- runtime_lang(shared_data)
    r <- m5_results(); d <- if (is.null(r)) NULL else r$comm_bingxing
    if (is.null(r) || is.null(d) || !is.null(d$Note))
      return(plotly_empty() %>% layout(paper_bgcolor = "#ffffff", plot_bgcolor = "#ffffff"))
    d <- d[!is.na(d$Pathogenesis) & d$Pathogenesis != "(none)", ]
    if (nrow(d) == 0) return(plotly_empty() %>%
      layout(title = runtime_tr("No pathogenesis data", lang), paper_bgcolor = "#ffffff", plot_bgcolor = "#ffffff"))
    mat <- as.data.frame.matrix(xtabs(Percentage ~ Community + Pathogenesis, data = d))
    if (nrow(mat) <= 1) return(plotly_empty() %>%
      layout(title = runtime_tr("Insufficient data", lang), paper_bgcolor = "#ffffff", plot_bgcolor = "#ffffff"))
    m <- as.matrix(mat)
    plot_ly(x = colnames(m), y = rownames(m), z = m, type = "heatmap",
            colorscale = list(
              c(0, "#fcfcfc"), c(0.15, "#fee5d9"), c(0.35, "#fcae91"),
              c(0.5, "#fb6a4a"), c(0.7, "#ef3b3b"), c(0.85, "#dc143c"), c(1, "#b71c1c")
            ),
            zmin = 0, zmax = 100, zauto = FALSE,
            colorbar = list(title = list(text = "%", font = list(size = 11))),
            hovertemplate = "%{y} x %{x}<br>%{z:.1f}%<extra></extra>") %>%
      layout(title = runtime_tr("Community x Pathogenesis (%)", lang), xaxis = list(title = ""), yaxis = list(title = ""),
             paper_bgcolor = "#ffffff", plot_bgcolor = "#ffffff")
  })

  # ===== Tab 2 Output =====
  output$dt_core_feature <- renderDT({
    lang <- runtime_lang(shared_data)
    r <- m5_results()
    if (is.null(r)) return(datatable(localized_placeholder("Run analysis first.", lang), rownames = FALSE))
    d <- r$core_feature
    if (!is.null(d$Note)) return(datatable(localized_columns(d, lang), rownames = FALSE))
    datatable(localized_columns(d, lang), options = list(pageLength = 15), rownames = FALSE)
  })







  # ===== Tab 3: ML-Based Prescription Recommendation =====

  ml_model  <- reactiveVal(NULL)
  ml_status <- reactiveVal("")

  output$baseline_cols_ui <- renderUI({
    r <- m5_results()
    if (is.null(r) || is.null(r$raw_data)) return(p("Run association analysis first."))
    df <- r$raw_data
    # Exclude prescription/herb columns + already-used disease location/pathogenesis
    # (those go through dedicated BW_/BX_ logic). Keep 标准证型 available as covariate.
    exclude <- grep("处方|中药|herb|prescription|_clean|_processed|_std$|病位|病性|病机|治法|症状",
                    names(df), ignore.case = TRUE, value = TRUE)
    avail <- setdiff(names(df), exclude)
    if (length(avail) == 0) return(p("No additional baseline columns available."))
    # Users explicitly choose covariates. No source column is included automatically.
    rows <- lapply(avail, function(col) {
      vals <- df[[col]]
      auto <- if (is.numeric(vals)) "numeric" else "categorical"
      col_id <- gsub("[^A-Za-z0-9_]", "_", col)
      fluidRow(
        column(3, checkboxInput(paste0("covuse_", col_id), col, value = FALSE)),
        column(9,
          radioButtons(paste0("covtype_", col_id),
                       label = NULL,
                       choices = list("Numeric" = "numeric",
                                      "Categorical" = "categorical"),
                       selected = auto, inline = TRUE))
      )
    })
    do.call(tagList, rows)
  })

  # Reactive: collect covariate type decisions into a named list
  cov_types <- reactive({
    r <- m5_results()
    if (is.null(r) || is.null(r$raw_data)) return(list())
    df <- r$raw_data
    exclude <- grep("处方|中药|herb|prescription|_clean|_processed|_std$|病位|病性|病机|治法|症状",
                    names(df), ignore.case = TRUE, value = TRUE)
    avail <- setdiff(names(df), exclude)
    out <- list()
    for (col in avail) {
      col_id <- gsub("[^A-Za-z0-9_]", "_", col)
      if (isTRUE(input[[paste0("covuse_", col_id)]])) {
        v <- input[[paste0("covtype_", col_id)]]
        if (!is.null(v)) out[[col]] <- v
      }
    }
    out
  })

  # Baseline value inputs for prediction (numeric sliders / categorical selects)
  output$baseline_values_ui <- renderUI({
    m <- ml_model()
    if (is.null(m)) return(p("Train the model first."))
    r <- m5_results(); req(r)
    cov <- cov_types()
    if (length(cov) == 0) return(p("No baseline covariates selected in training."))
    df <- r$raw_data
    inputs <- list()
    for (bc in names(cov)) {
      ct <- cov[[bc]]
      if (!bc %in% names(df)) next
      vals <- df[[bc]]
      if (ct == "numeric") {
        vals_num <- suppressWarnings(as.numeric(as.character(vals)))
        if (all(is.na(vals_num))) next
        rng <- diff(range(vals_num, na.rm = TRUE))
        all_int <- all(vals_num[!is.na(vals_num)] == floor(vals_num[!is.na(vals_num)]))
        step_val <- if (all_int) 1 else round(rng / 100, 2)
        default_val <- if (all_int) round(mean(vals_num, na.rm = TRUE)) else round(mean(vals_num, na.rm = TRUE), 1)
        inputs[[length(inputs) + 1]] <- sliderInput(paste0("bl_", bc), bc,
          min = min(vals_num, na.rm = TRUE), max = max(vals_num, na.rm = TRUE),
          value = default_val, step = step_val)
      } else {
        vals <- as.character(vals); vals[is.na(vals)] <- "missing"
        levs <- sort(unique(vals)[1:min(10, length(unique(vals)))])
        inputs[[length(inputs) + 1]] <- selectInput(paste0("bl_", bc), bc,
          choices = levs, selected = levs[1])
      }
    }
    if (length(inputs) == 0) return(p("No baseline features configured."))
    do.call(tagList, inputs)
  })

  output$model_status_ui <- renderUI({
    s <- ml_status()
    m <- ml_model()
    if (s == "") return(NULL)
    if (!is.null(m)) {
      div(class = "alert alert-success",
          strong("Model trained. "), nrow(m$auc), " herbs, Mean AUC = ", round(mean(m$auc$AUC, na.rm = TRUE), 3),
          ", AUC > 0.6: ", sum(m$auc$AUC > 0.6, na.rm = TRUE))
    } else if (s != "") {
      div(class = "alert alert-danger", s)
    }
  })

  observeEvent(input$w_preset, {
    presets <- list(
      balanced = c(alpha = 0.33, beta_share = 0.50),
      core = c(alpha = 0.60, beta_share = 0.625),
      pair = c(alpha = 0.20, beta_share = 0.3125)
    )
    preset <- presets[[input$w_preset]]
    if (!is.null(preset)) {
      updateSliderInput(session, "w_alpha", value = preset["alpha"])
      updateSliderInput(session, "w_beta_share", value = preset["beta_share"])
    }
  }, ignoreInit = TRUE)

  network_weights <- reactive({
    alpha <- input$w_alpha
    beta <- (1 - alpha) * input$w_beta_share
    list(alpha = alpha, beta = beta, delta = 1 - alpha - beta)
  })

  network_weight_df <- reactive({
    m3 <- shared_data$module_results$m3
    if (is.null(m3) || is.null(m3$core) || is.null(m3$edge_data)) return(NULL)
    w <- network_weights()
    tryCatch(
      calculate_network_weight(m3, a = w$alpha, b = w$beta, c_param = w$delta),
      error = function(e) NULL
    )
  })

  output$weight_sum_warn <- renderUI({
    w <- network_weights()
    div(class = "text-success", style = "font-size:11px;",
        icon("check-circle"),
        runtime_tr("α = %.2f, β = %.2f, δ = %.2f (sum = 1.00)", runtime_lang(shared_data),
                w$alpha, w$beta, w$delta))
  })

  output$nw_status_ui <- renderUI({
    m <- ml_model()
    m3 <- shared_data$module_results$m3
    if (is.null(m)) {
      div(class = "alert alert-warning", style = "font-size:11px;",
          icon("exclamation-triangle"), " Model not trained yet.")
    } else if (is.null(m$nw_df) || is.null(m3) || is.null(m3$edge_data)) {
      div(class = "alert alert-danger", style = "font-size:11px;",
          icon("times-circle"),
          " M3 network data missing. W_h = 0 for all herbs. ",
          "EnhancedScore degrades to P × (1 + γC) — network enhancement inactive.")
    } else {
      n_nw <- nrow(m$nw_df)
      div(class = "alert alert-success", style = "font-size:11px;",
          icon("check-circle"),
          runtime_tr("Network weights ready: %d herbs with structural scores.", runtime_lang(shared_data), n_nw))
    }
  })

  observeEvent(input$train_model, {
    r <- m5_results()
    if (is.null(r)) { ml_status("Run association analysis first."); return() }

    ml_status("Training Classifier Chain...")
    showNotification("Training Classifier Chain...", type = "message", duration = 3)

    feat <- r$features_df; presc <- r$prescriptions
    bw_col <- if ("Disease_Location" %in% names(feat)) "Disease_Location" else NULL
    bx_col <- if ("Pathogenesis" %in% names(feat)) "Pathogenesis" else NULL

    all_bw <- character(0); all_bx <- character(0)
    if (!is.null(bw_col)) {
      all_bw <- unique(na.omit(unlist(str_split(feat[[bw_col]], "[、，,\\s]+"))))
      all_bw <- all_bw[nchar(all_bw) >= 1 & all_bw != ""]
    }
    if (!is.null(bx_col)) {
      all_bx <- unique(na.omit(unlist(str_split(feat[[bx_col]], "[、，,\\s]+"))))
      all_bx <- all_bx[nchar(all_bx) >= 1 & all_bx != ""]
    }

    N <- nrow(feat)
    X <- data.frame(row.names = seq_len(N))

    for (bw in all_bw) {
      X[[paste0("BW_", bw)]] <- as.integer(grepl(bw, feat[[bw_col]], fixed = TRUE))
    }
    for (bx in all_bx) {
      X[[paste0("BX_", bx)]] <- as.integer(grepl(bx, feat[[bx_col]], fixed = TRUE))
    }

    cov <- cov_types()
    raw <- r$raw_data
    if (length(cov) > 0) {
      for (bc in names(cov)) {
        if (!bc %in% names(raw)) next
        ct <- cov[[bc]]
        vals <- raw[[bc]]
        if (ct == "numeric") {
          vals <- suppressWarnings(as.numeric(as.character(vals)))
          if (all(is.na(vals))) next
          vals[is.na(vals)] <- mean(vals, na.rm = TRUE)
          X[[paste0("NUM_", bc)]] <- scale(vals)[, 1]
        } else {  # categorical
          vals <- as.character(vals); vals[is.na(vals)] <- "missing"
          tab <- table(vals)
          if (length(tab) <= input$m5_max_cat_levels && length(tab) > 1) {
            for (lev in names(tab)) {
              X[[paste0("CAT_", bc, "_", lev)]] <- as.integer(vals == lev)
            }
          }
        }
      }
    }

    if (ncol(X) == 0) { ml_status("No features available."); return() }

    # Filter herbs by frequency percentage (no upper limit)
    herbs_vec <- unlist(str_split(presc, "\\s+"))
    herbs_vec <- herbs_vec[nchar(herbs_vec) >= 2 & herbs_vec != ""]
    herb_tab <- sort(table(herbs_vec), decreasing = TRUE)
    min_pct <- input$m5_min_herb_pct
    if (is.null(min_pct)) min_pct <- 0.5
    top_herbs <- names(herb_tab[herb_tab >= N * min_pct / 100])

    if (length(top_herbs) < 3) {
      ml_status("Too few herbs. Lower the frequency percentage threshold.")
      return()
    }

    Y <- sapply(top_herbs, function(h) as.integer(grepl(paste0("(^|\\s)", h, "(\\s|$)"), presc)))

    set.seed(42)
    train_idx <- sample(seq_len(N), size = floor(N * 0.8))
    test_idx  <- setdiff(seq_len(N), train_idx)

    base_feats <- names(X)
    auc_table <- data.frame(Herb = top_herbs, AUC = NA_real_, Status = "Not trained", stringsAsFactors = FALSE)
    models <- list()
    chain_names <- character(0)
    X_train_full <- X[train_idx, base_feats, drop = FALSE]
    X_test_full <- X[test_idx, base_feats, drop = FALSE]

    # ===== Classifier Chain =====
    MAX_CHAIN <- input$m5_max_chain
    K_FOLD <- 5
    make_folds <- function(y, k = 5) {
      folds <- integer(length(y))
      pos <- which(y == 1)
      neg <- which(y == 0)
      folds[pos] <- sample(rep(seq_len(k), length.out = length(pos)))
      folds[neg] <- sample(rep(seq_len(k), length.out = length(neg)))
      folds
    }
    fit_glm_safe <- function(train_df) {
      col_vars <- apply(train_df[, -1, drop = FALSE], 2, var, na.rm = TRUE)
      if (all(col_vars == 0 | is.na(col_vars), na.rm = TRUE)) train_df$dummy <- 1
      glm(y ~ ., data = train_df, family = binomial, control = list(maxit = 25))
    }
    withProgress(message = "Training 5-fold OOF Classifier Chain...", value = 0, {
      n_total <- length(top_herbs)
      oof_chain <- data.frame(row.names = train_idx)
      test_chain <- data.frame(row.names = test_idx)
      for (i in seq_along(top_herbs)) {
        incProgress(1 / n_total, detail = paste(i, "/", n_total))
        h <- top_herbs[i]
        y_all <- Y[, i]
        y_train <- y_all[train_idx]
        y_test <- y_all[test_idx]
        n_pos_train <- sum(y_train)
        n_neg_train <- sum(y_train == 0)

        if (n_pos_train < 10 || n_neg_train < 10) {
          auc_table$Status[i] <- "Skipped: fewer than 10 positive or negative training cases"
          next
        }

        X_train <- X_train_full
        X_test  <- X_test_full

        recent_chains <- if (length(chain_names) > MAX_CHAIN)
          tail(chain_names, MAX_CHAIN) else chain_names
        for (cn in recent_chains) {
          X_train[[paste0("chain_", cn)]] <- oof_chain[[cn]]
          X_test[[paste0("chain_", cn)]] <- test_chain[[cn]]
        }

        tryCatch({
          folds <- make_folds(y_train, K_FOLD)
          oof_pred <- rep(NA_real_, length(train_idx))

          for (fold in seq_len(K_FOLD)) {
            val_pos <- which(folds == fold)
            tr_pos <- which(folds != fold)
            y_fold <- y_train[tr_pos]
            if (length(unique(y_fold)) < 2) next

            fold_train <- cbind(data.frame(y = factor(y_fold)), X_train[tr_pos, , drop = FALSE])
            fold_fit <- fit_glm_safe(fold_train)
            fold_new <- X_train[val_pos, setdiff(names(coef(fold_fit)), "(Intercept)"), drop = FALSE]
            oof_pred[val_pos] <- suppressWarnings(predict(fold_fit, newdata = fold_new, type = "response"))
          }

          fill_prob <- mean(y_train)
          oof_pred[is.na(oof_pred)] <- fill_prob

          train_df <- cbind(data.frame(y = factor(y_train)), X_train)
          fit <- fit_glm_safe(train_df)
          models[[h]] <- fit
          chain_names <- c(chain_names, h)
          auc_table$Status[i] <- "Trained (5-fold OOF chain)"

          needed <- setdiff(names(coef(fit)), "(Intercept)")
          missing_cols <- setdiff(needed, names(X_test))
          for (mc in missing_cols) X_test[[mc]] <- 0
          pred <- suppressWarnings(predict(fit, newdata = X_test[, needed, drop = FALSE], type = "response"))

          oof_chain[[h]] <- oof_pred
          test_chain[[h]] <- pred

          pos_test <- sum(y_test == 1)
          neg_test <- sum(y_test == 0)

          if (pos_test > 0 && neg_test > 0 && length(unique(round(pred, 6))) > 1) {
            auc_table$AUC[i] <- suppressWarnings(rank_auc(pred, as.integer(y_test)))
          }
        }, error = function(e) {
          auc_table$Status[i] <<- paste("Failed:", conditionMessage(e))
        })
      }
    })

    auc_table <- auc_table[order(auc_table$Status != "Trained", is.na(auc_table$AUC), -auc_table$AUC), ]
    rownames(auc_table) <- NULL

    herb_comm <- NULL; core_herbs <- character(0)
    nw_df <- NULL
    m3 <- shared_data$module_results$m3
    if (!is.null(m3) && !is.null(m3$memb)) {
      herb_comm <- setNames(m3$memb$Community, m3$memb$Herb)
    }
    if (!is.null(m3) && !is.null(m3$core$core_formula_table)) {
      cf <- m3$core$core_formula_table
      core_herbs <- unique(unlist(str_split(cf$Core_Formula, "\\s+")))
      core_herbs <- core_herbs[nchar(core_herbs) >= 2 & core_herbs != ""]
    }
    # Network structural score W_h = α K + β D + δ R
    if (!is.null(m3) && !is.null(m3$core) && !is.null(m3$edge_data)) {
      tryCatch({
        nw_df <- network_weight_df()
      }, error = function(e) {
        showNotification(paste("Network weight calc failed:", e$message), type = "warning")
        nw_df <- NULL
      })
    }

    ml_model(list(
      auc = auc_table, models = models, chain_order = chain_names,
      top_herbs = top_herbs, all_bw = all_bw, all_bx = all_bx,
      base_feats = base_feats, N = N,
      herb_comm = herb_comm, core_herbs = core_herbs,
      nw_df = nw_df,
      max_chain = MAX_CHAIN,
      train_idx = train_idx, test_idx = test_idx,
      Y = Y, presc_vec = presc
    ))
    ml_status("")
    n_nw <- if (!is.null(nw_df)) nrow(nw_df) else 0
    showNotification(paste("Chain trained:", length(chain_names), "herbs, Mean AUC:",
                           round(mean(auc_table$AUC), 3),
                           "| Network weights:", n_nw, "herbs"), type = "message")
  })

  output$dt_model_perf <- renderDT({
    m <- ml_model()
    if (is.null(m)) return(datatable(localized_placeholder("Train the model to see performance.", runtime_lang(shared_data)), rownames = FALSE))
    d <- m$auc
    d$Status <- vapply(d$Status, runtime_tr, character(1), lang = runtime_lang(shared_data))
    datatable(d, options = list(pageLength = 15), rownames = FALSE) %>%
      formatRound("AUC", 3) %>%
      formatStyle("AUC", backgroundColor = styleInterval(c(0.5, 0.7), c("#f8d7da", "#fff3cd", "#d4edda")))
  })

  output$rec_bingwei_select <- renderUI({
    m <- ml_model()
    if (is.null(m)) return(p("Train the model first."))
    choices <- m$all_bw
    if (length(choices) == 0) return(p("No disease location features available."))
    ns <- session$ns
    selectInput(ns("rec_bingwei"), "Disease Location (病位)", choices = choices,
                multiple = TRUE, selected = choices[1])
  })

  output$rec_bingxing_select <- renderUI({
    m <- ml_model()
    if (is.null(m)) return(p("Train the model first."))
    choices <- m$all_bx
    if (length(choices) == 0) return(p("No pathogenesis features available."))
    ns <- session$ns
    selectInput(ns("rec_bingxing"), "Disease Nature (病性)", choices = choices,
                multiple = TRUE, selected = NULL)
  })

  rec_result <- reactiveVal(NULL)

  observeEvent(input$run_recommend, {
    m <- ml_model()
    if (is.null(m)) { showNotification("Train the model first.", type = "warning"); return() }

    bw_targets <- input$rec_bingwei
    bx_targets <- input$rec_bingxing
    if ((is.null(bw_targets) || length(bw_targets) == 0) &&
        (is.null(bx_targets) || length(bx_targets) == 0)) {
      showNotification("Select at least one target feature.", type = "warning"); return()
    }

    target_cols <- c(
      if (!is.null(bw_targets)) paste0("BW_", bw_targets) else character(0),
      if (!is.null(bx_targets)) paste0("BX_", bx_targets) else character(0)
    )

    new_x <- setNames(rep(0, length(m$base_feats)), m$base_feats)
    for (tc in target_cols) {
      if (tc %in% names(new_x)) new_x[tc] <- 1
    }

    cov <- cov_types()
    if (length(cov) > 0) {
      r <- m5_results(); raw <- r$raw_data
      for (bc in names(cov)) {
        ct <- cov[[bc]]
        bl_input <- input[[paste0("bl_", bc)]]
        if (is.null(bl_input)) next
        if (ct == "numeric") {
          vals <- suppressWarnings(as.numeric(as.character(raw[[bc]])))
          val_mean <- mean(vals, na.rm = TRUE); val_sd <- sd(vals, na.rm = TRUE)
          bl_scaled <- if (val_sd > 0) (as.numeric(bl_input) - val_mean) / val_sd else 0
          col_name <- paste0("NUM_", bc)
          if (col_name %in% names(new_x)) new_x[col_name] <- bl_scaled
        } else {
          col_name <- paste0("CAT_", bc, "_", as.character(bl_input))
          if (col_name %in% names(new_x)) new_x[col_name] <- 1
        }
      }
    }

    new_df <- as.data.frame(t(new_x))

    # ===== Classifier Chain prediction =====
    max_chain <- m$max_chain
    # Network-enhanced recommendation:
    #   Score_h = P_h * (1 + lambda * W_h) * (1 + gamma * C_h)
    #   W_h     = alpha * K_h + beta * D_h + delta * R_h
    lambda  <- input$w_lambda; gamma_w <- input$w_gamma
    # Use cached network weights from current preset or advanced settings
    nw_df <- network_weight_df()
    if (is.null(nw_df)) nw_df <- m$nw_df

    results_rows <- vector("list", length(m$chain_order))
    result_count <- 0
    chain_probs <- numeric(0)
    chain_names <- character(0)

    for (h in m$chain_order) {
      if (!h %in% names(m$models)) next
      fit <- m$models[[h]]

      new_df_pred <- new_df
      # Add only the recent chain columns (matched to training)
      recent <- if (length(chain_names) > max_chain) tail(chain_names, max_chain) else chain_names
      for (j in seq_along(recent)) {
        pi <- which(chain_names == recent[j])
        new_df_pred[[paste0("chain_", recent[j])]] <- chain_probs[pi]
      }

      needed <- setdiff(names(coef(fit)), "(Intercept)")
      missing_cols <- setdiff(needed, names(new_df_pred))
      for (mc in missing_cols) new_df_pred[[mc]] <- 0
      new_df_pred <- new_df_pred[, needed, drop = FALSE]

      prob <- suppressWarnings(predict(fit, newdata = new_df_pred, type = "response"))
      if (is.na(prob)) next
      chain_probs <- c(chain_probs, prob)
      chain_names <- c(chain_names, h)

      comm <- if (!is.null(m$herb_comm) && h %in% names(m$herb_comm))
        paste0("C", m$herb_comm[h]) else "Unknown"
      in_core <- if (h %in% m$core_herbs) "Yes" else "No"
      auc_val <- if (h %in% m$auc$Herb) m$auc$AUC[m$auc$Herb == h] else NA

      # Network structural score W_h (default 0 if herb not in nw_df)
      kcore_n <- 0; wdeg_n <- 0; pair_n <- 0; w_h <- 0
      if (!is.null(nw_df) && h %in% nw_df$Herb) {
        row <- nw_df[nw_df$Herb == h, ]
        kcore_n <- if (!is.null(row$kcore_norm)) row$kcore_norm else 0
        wdeg_n  <- if (!is.null(row$weighted_degree_norm)) row$weighted_degree_norm else 0
        pair_n  <- if (!is.null(row$pair_norm)) row$pair_norm else 0
        w_h     <- if (!is.null(row$NetworkWeight)) row$NetworkWeight else 0
      }
      core_bonus <- if (in_core == "Yes") 1 else 0
      # Enhanced score: P_h * (1 + lambda * W_h) * (1 + gamma * C_h)
      enhanced_score <- prob * (1 + lambda * w_h) * (1 + gamma_w * core_bonus)

      result_count <- result_count + 1
      results_rows[[result_count]] <- data.frame(
        Herb = h, Community = comm, In_Core_Formula = in_core,
        AUC = auc_val, Probability = round(prob, 4),
        K_core = round(kcore_n, 3), Weighted_Degree = round(wdeg_n, 3),
        Pair_Strength = round(pair_n, 3), NetworkWeight = round(w_h, 3),
        Core_Bonus = core_bonus, EnhancedScore = round(enhanced_score, 4),
        stringsAsFactors = FALSE
      )
    }

    results <- if (result_count > 0) do.call(rbind, results_rows[seq_len(result_count)]) else data.frame()
    if (nrow(results) == 0) {
      rec_result(list(error = "No herbs could be predicted.")); return()
    }

    results$OriginalRank  <- rank(-results$Probability,   ties.method = "min")
    results$EnhancedRank  <- rank(-results$EnhancedScore, ties.method = "min")
    results$RankChange    <- results$OriginalRank - results$EnhancedRank
    results <- results[order(-results$EnhancedScore), ]
    rownames(results) <- NULL

    rec_result(list(
      herbs       = results,
      top15       = head(results, 15),
      top_formula = paste(head(results$Herb, min(12, nrow(results))), collapse = ", "),
      lambda = lambda, gamma = gamma_w
    ))

    showNotification(paste("Recommendation:", nrow(results), "herbs."), type = "message")
  })

  output$dt_recommend <- renderDT({
    lang <- runtime_lang(shared_data)
    rec <- rec_result()
    if (is.null(rec)) return(datatable(localized_placeholder("Generate recommendation to see results.", lang), rownames = FALSE))
    if (!is.null(rec$error)) return(datatable(localized_placeholder(rec$error, lang), rownames = FALSE))
    d <- rec$herbs
    d$In_Core_Formula <- vapply(d$In_Core_Formula, runtime_tr, character(1), lang = lang)
    show_cols <- c("Herb", "Community", "In_Core_Formula", "AUC", "Probability",
                   "K_core", "Weighted_Degree", "Pair_Strength", "NetworkWeight",
                   "EnhancedScore", "OriginalRank", "EnhancedRank", "RankChange")
    show_cols <- show_cols[show_cols %in% names(d)]
    datatable(localized_columns(d[, show_cols, drop = FALSE], lang), options = list(pageLength = 15, scrollX = TRUE), rownames = FALSE) %>%
      formatRound(vapply(c("AUC", "Probability", "K_core", "Weighted_Degree",
                           "Pair_Strength", "NetworkWeight", "EnhancedScore"), runtime_tr, character(1), lang = lang), 3) %>%
      formatStyle(runtime_tr("RankChange", lang),
        color = styleEqual(c(-1, 1), c("#dc3545", "#28a745")),
        fontWeight = "bold")
  })

  output$plot_recommend <- renderPlotly({
    lang <- runtime_lang(shared_data)
    rec <- rec_result()
    if (is.null(rec) || !is.null(rec$error))
      return(plotly_empty() %>% layout(title = runtime_tr("Generate recommendation to see chart.", lang),
             paper_bgcolor = "#ffffff", plot_bgcolor = "#ffffff"))
    d <- head(rec$herbs, 20)
    plot_ly(d, x = ~EnhancedScore, y = ~reorder(Herb, EnhancedScore), type = "bar",
            marker = list(color = "#3498db"),
            text = ~paste0(Herb, "<br>", runtime_tr("EnhancedScore", lang), ": ", round(EnhancedScore, 3),
                           "<br>", runtime_tr("Probability", lang), ": ", round(Probability * 100, 1), "%",
                           "<br>W: ", round(NetworkWeight, 2)),
            textposition = "none", hoverinfo = "text") %>%
      layout(title = runtime_tr("Top Recommended Herbs (by Enhanced Score)", lang),
             xaxis = list(title = runtime_tr("Enhanced Score = P × (1 + λW) × (1 + γC)", lang)),
             yaxis = list(title = ""),
             paper_bgcolor = "#ffffff", plot_bgcolor = "#ffffff",
             margin = list(l = 120))
  })


  output$dl_rec <- downloadHandler(
    filename = function() localized_filename("recommendation", "csv", runtime_lang(shared_data)),
    content = function(file) {
      rec <- rec_result(); req(rec, !is.null(rec$herbs))
      write.csv(localized_columns(rec$herbs, runtime_lang(shared_data)), file, row.names = FALSE, fileEncoding = "UTF-8")
    }
  )
}

rank_auc <- function(pred, actual) {
  pos <- which(actual == 1); neg <- which(actual == 0)
  if (length(pos) == 0 || length(neg) == 0) return(NA)
  n_pos <- length(pos); n_neg <- length(neg)
  n_correct <- sum(sapply(pred[pos], function(p) sum(p > pred[neg]))) +
    0.5 * sum(sapply(pred[pos], function(p) sum(p == pred[neg])))
  n_correct / (n_pos * n_neg)
}
