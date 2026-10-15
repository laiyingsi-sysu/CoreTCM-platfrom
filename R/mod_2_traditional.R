# ============================================================
# mod_2_traditional.R — Module 2: Traditional Mining Workbench
# Split into: Descriptive Statistics / Association Rules / Clustering
# All share one server ("m2" namespace)
# ============================================================

# ===== DESCRIPTIVE STATISTICS (描述性统计) =====

mod_2_descriptive_sidebar <- function(id) {
  ns <- NS(id)
  tagList(
    div(class = "param-section",
      h5(icon("chart-bar"), "Frequency Analysis"),
      p(class = "text-muted", style = "font-size:11px;", "Compute herb frequency distribution from cleaned prescriptions."),
      sliderInput(ns("m2_freq_top_n"), "Top-N Herbs", min = 5, max = 100, value = 20)
    ),
    div(class = "sidebar-action-bar",
      parameter_guide("m2"),
      actionButton(ns("run_freq"), "Run Frequency Analysis", icon = icon("play"),
                   class = "btn-primary btn-block"),
      br(),
      uiOutput(ns("status_freq")),
      actionButton(ns("reset_params"), "Reset to Global Defaults", icon = icon("undo"),
                   class = "btn-sm btn-block")
    )
  )
}

mod_2_descriptive_ui <- function(id) {
  ns <- NS(id)
  tagList(
    workflow_guide("m2"),
    fluidRow(
      box(title = "Frequency Analysis Results", status = "success", solidHeader = TRUE, width = 12,
        plotlyOutput(ns("plot_freq"), height = "450px"),
        br(),
        DTOutput(ns("dt_freq")),
        br(),
        downloadButton(ns("dl_freq"), "Download Table (CSV)", class = "btn-sm")
      )
    )
  )
}

# ===== ASSOCIATION RULES (关联规则) =====

mod_2_rules_sidebar <- function(id) {
  ns <- NS(id)
  tagList(
    div(class = "param-section",
      h5(icon("link"), "Association Rules (Apriori)"),
      p(class = "text-muted", style = "font-size:11px;", "Mine herb combination rules from prescriptions."),
      sliderInput(ns("m2_min_freq"), "Min Herb Frequency", min = 1, max = 200, value = 20),
      sliderInput(ns("m2_apriori_support"), "Min Support", min = 0.01, max = 0.50, value = 0.05, step = 0.01),
      sliderInput(ns("m2_apriori_confidence"), "Min Confidence", min = 0.10, max = 1.00, value = 0.30, step = 0.01),
      sliderInput(ns("m2_apriori_lift"), "Min Lift", min = 0.5, max = 5.0, value = 1.0, step = 0.1),
      sliderInput(ns("m2_max_combo_size"), "Max Combo Size", min = 2, max = 6, value = 4),
      sliderInput(ns("m2_max_rule_herbs"), "Max Herbs for Rules", min = 50, max = 400, value = 200, step = 10)
    ),
    div(class = "sidebar-action-bar",
      parameter_guide("m2"),
      actionButton(ns("run_rules"), "Run Association Rules", icon = icon("play"),
                   class = "btn-primary btn-block"),
      br(),
      uiOutput(ns("status_rules")),
      actionButton(ns("reset_params"), "Reset to Global Defaults", icon = icon("undo"),
                   class = "btn-sm btn-block")
    )
  )
}

mod_2_rules_ui <- function(id) {
  ns <- NS(id)
  tagList(
    workflow_guide("m2"),
    fluidRow(
      box(title = "Association Rules Results", status = "success", solidHeader = TRUE, width = 12,
        DTOutput(ns("dt_rules")),
        br(),
        h5("Support vs Confidence"),
        fluidRow(
          column(4, sliderInput(ns("m2_scatter_size"), "Filter by Combo Size",
                    min = 2, max = 2, value = 2, step = 1))
        ),
        plotlyOutput(ns("plot_rules_scatter"), height = "420px"),
        br(), hr(),
        h5("Rule Association Network"),
        fluidRow(
          column(4, sliderInput(ns("m2_net_top_n"), "Show Top-N Rules (by Lift)",
                    min = 10, max = 500, value = 50, step = 10))
        ),
        visNetworkOutput(ns("plot_rules_net"), height = "500px"),
        br(),
        downloadButton(ns("dl_rules"), "Download Rules Table (CSV)", class = "btn-sm")
      )
    )
  )
}

# ===== CLUSTERING (聚类) =====

mod_2_cluster_sidebar <- function(id) {
  ns <- NS(id)
  tagList(
    div(class = "param-section",
      h5(icon("th-large"), "Herb Clustering"),
      p(class = "text-muted", style = "font-size:11px;",
        "Hierarchical (Ward) or K-means clustering of herbs based on co-occurrence patterns."),
      sliderInput(ns("m2_min_freq_clust"), "Min Herb Frequency", min = 1, max = 200, value = 20),
      selectInput(ns("m2_cluster_method"), "Method",
        choices = c("Ward (hierarchical)", "K-means"), selected = "Ward (hierarchical)"),
      conditionalPanel(
        condition = paste0("input['", ns("m2_cluster_method"), "'] == 'K-means'"),
        sliderInput(ns("m2_cluster_k"), "Number of Clusters k (K-means)", min = 2, max = 20, value = 5)
      )
    ),
    div(class = "sidebar-action-bar",
      parameter_guide("m2"),
      actionButton(ns("run_cluster"), "Run Clustering", icon = icon("play"),
                   class = "btn-primary btn-block"),
      br(),
      uiOutput(ns("status_cluster")),
      actionButton(ns("reset_params"), "Reset to Global Defaults", icon = icon("undo"),
                   class = "btn-sm btn-block")
    )
  )
}

mod_2_cluster_ui <- function(id) {
  ns <- NS(id)
  tagList(
    workflow_guide("m2"),
    fluidRow(
      box(title = "Clustering Results", status = "success", solidHeader = TRUE, width = 12,
        conditionalPanel(
          condition = paste0("input['", ns("m2_cluster_method"), "'] == 'K-means'"),
          sliderInput(ns("m2_elbow_max_k"), "Max K for Elbow / Heatmap", min = 3, max = 30, value = 18, step = 1),
          br(),
          plotlyOutput(ns("plot_kguide"), height = "200px"),
          br()
        ),
        conditionalPanel(
          condition = paste0("input['", ns("m2_cluster_method"), "'] == 'Ward (hierarchical)'"),
          fluidRow(
            column(6, sliderInput(ns("ward_k_slider"), "Number of Clusters k (Ward)",
                                  min = 2, max = 20, value = 5, step = 1)),
            column(6, br(), p(class = "text-muted", style = "font-size:11px;",
                              "Adjust k to re-cut the dendrogram. Heatmap & cluster table update live."))
          ),
          plotOutput(ns("plot_ward_heatmap"), height = "600px"),
          br(),
          downloadButton(ns("dl_ward_hmap"), "Download Ward Heatmap (PNG)", class = "btn-sm"),
          br(), br(),
          plotlyOutput(ns("plot_dendrogram"), height = "400px"),
          br()
        ),
        conditionalPanel(
          condition = paste0("input['", ns("m2_cluster_method"), "'] == 'K-means'"),
          plotOutput(ns("plot_kmeans_heatmap"), height = "600px"),
          br(),
          downloadButton(ns("dl_kmeans_hmap"), "Download K-means Heatmap (PNG)", class = "btn-sm"),
          br()
        ),
        selectInput(ns("m2_cluster_sel"), "Select Cluster for Detail View", choices = NULL),
        plotlyOutput(ns("plot_cluster_detail"), height = "350px"),
        br(),
        DTOutput(ns("dt_clusters")),
        br(),
        downloadButton(ns("dl_cluster"), "Download Cluster Table (CSV)", class = "btn-sm")
      )
    )
  )
}

# ===== SERVER (shared across all m2 sub-modules) =====

mod_2_traditional_server <- function(input, output, session, shared_data) {
  datatable <- function(data, ...) localized_datatable(data, ..., lang = runtime_lang(shared_data))


  freq_r   <- reactiveVal(NULL)
  rules_r  <- reactiveVal(NULL)
  cluster_r <- reactiveVal(NULL)

  output$status_freq   <- renderUI({
    if (is.null(freq_r())) div(class = "text-muted", "Not run yet.") else div(class = "text-success", "Done.")
  })
  output$status_rules  <- renderUI({
    r <- rules_r()
    if (is.null(r)) div(class = "text-muted", "Not run yet.")
    else if (!isTRUE(r$ok)) div(class = "text-danger", r$msg)
    else if (isTRUE(r$truncated)) div(class = "text-warning", r$msg)
    else div(class = "text-success", runtime_tr("%d rules, %d after lift filter", runtime_lang(shared_data), r$n_raw, nrow(r$df)))
  })
  output$status_cluster <- renderUI({
    if (is.null(cluster_r())) div(class = "text-muted", "Not run yet.") else div(class = "text-success", "Done.")
  })

  # ===== Reset =====
  observeEvent(input$reset_params, {
    p <- shared_data$active_params
    updateSliderInput(session, "m2_freq_top_n",        value = p$m2_freq_top_n)
    updateSliderInput(session, "m2_min_freq",           value = p$m2_min_freq)
    updateSliderInput(session, "m2_apriori_support",    value = p$m2_apriori_support)
    updateSliderInput(session, "m2_apriori_confidence", value = p$m2_apriori_confidence)
    updateSliderInput(session, "m2_apriori_lift",       value = p$m2_apriori_lift)
    updateSliderInput(session, "m2_max_combo_size",     value = p$m2_max_combo_size)
    updateSliderInput(session, "m2_min_freq_clust",     value = p$m2_min_freq_clust)
    updateSelectInput(session, "m2_cluster_method",      selected = p$m2_cluster_method)
    updateSliderInput(session, "m2_cluster_k",          value = p$m2_cluster_k)
    showNotification("Parameters reset to global defaults", type = "message")
  })

  # ===== Frequency =====
  observeEvent(input$run_freq, {
    req(shared_data$clean, shared_data$clean_col)
    data <- shared_data$clean; col <- shared_data$clean_col
    withProgress(message = "Computing frequency...", {
      f <- get_freq(data, !!sym(col))
      freq_r(list(freq = f, top_n = input$m2_freq_top_n))
      m2_res <- shared_data$module_results$m2
      if (is.null(m2_res)) m2_res <- list()
      m2_res$freq <- list(freq = f, top_n = input$m2_freq_top_n)
      shared_data$module_results$m2 <- m2_res
    })
    showNotification("Frequency analysis complete", type = "message")
  })

  output$plot_freq <- renderPlotly({
    lang <- runtime_lang(shared_data)
    r <- freq_r(); req(r)
    d <- head(r$freq, r$top_n)
    lab <- names(d)[1]
    freq_norm <- (d$频率 - min(d$频率)) / (max(d$频率) - min(d$频率))
    pal <- colorRamp(c("#fcfcfc","#fee5d9","#fcae91","#fb6a4a","#ef3b3b","#dc143c","#b71c1c"))
    bar_colors <- rgb(pal(freq_norm), maxColorValue = 255)
    plot_ly(d, x = ~频数, y = ~reorder(.data[[lab]], 频数), type = "bar",
            marker = list(color = bar_colors,
                          line = list(color = "#a50f15", width = 0.3)),
            text = ~paste0(.data[[lab]], "<br>", runtime_tr("Frequency", lang), ": ", 频数, " (", round(频率 * 100, 2), "%)"),
            hoverinfo = "text") %>%
      layout(title = runtime_tr("Top %d Herbs by Frequency", lang, nrow(d)),
             xaxis = list(title = runtime_tr("Frequency", lang), fixedrange = TRUE, rangemode = "tozero"),
             yaxis = list(title = "", fixedrange = TRUE))
  })

  output$dt_freq <- renderDT({
    r <- freq_r(); req(r)
    d <- r$freq %>% select(中药 = 1, 频数, 频率)
    datatable(d, options = list(pageLength = 20, scrollY = "400px"),
              rownames = FALSE) %>% formatPercentage("频率", 2)
  })

  output$dl_freq <- downloadHandler(
    filename = function() localized_filename("frequency_analysis", "csv", runtime_lang(shared_data)),
    content = function(file) {
      lang <- runtime_lang(shared_data)
      r <- freq_r(); req(r)
      d <- r$freq
      colnames(d)[1] <- "Herb"
      write.csv(localized_columns(d, lang), file, row.names = FALSE, fileEncoding = "UTF-8")
    }
  )

  # ===== Association Rules =====
  observeEvent(input$run_rules, {
    req(shared_data$clean, shared_data$clean_col)
    data <- shared_data$clean; col <- shared_data$clean_col
    showNotification("Mining association rules...", type = "message", duration = 3)

    rx_col <- as.character(data[[col]])
    tx_list <- lapply(str_split(rx_col, "\\s+"), function(x) x[nchar(x) >= 2 & x != ""])
    tx_list <- tx_list[lengths(tx_list) > 0]

    mf <- input$m2_min_freq
    max_rule_herbs <- if (is.null(input$m2_max_rule_herbs)) 200 else input$m2_max_rule_herbs
    herb_counts <- table(unlist(tx_list))
    freq_sorted <- sort(herb_counts[herb_counts >= mf], decreasing = TRUE)
    n_pass <- length(freq_sorted)
    truncated <- n_pass > max_rule_herbs
    keep_herbs <- names(freq_sorted)
    if (truncated) keep_herbs <- names(freq_sorted)[seq_len(max_rule_herbs)]
    tx_list <- lapply(tx_list, function(x) intersect(x, keep_herbs))
    tx_list <- tx_list[lengths(tx_list) >= 2]

    n_tx <- length(tx_list)
    n_items <- length(keep_herbs)

    if (n_tx < 2) {
      rules_r(list(rules = NULL, df = data.frame(), ok = FALSE,
                   msg = paste("Only", n_tx, "valid prescriptions. Need at least 2.")))
      return()
    }

    tx_err <- NULL
    tx <- tryCatch(as(tx_list, "transactions"),
                   error = function(e) { tx_err <<- e$message; NULL })

    if (is.null(tx)) {
      rules_r(list(rules = NULL, df = data.frame(), ok = FALSE,
                   msg = paste("Transaction conversion failed:", tx_err)))
      return()
    }

    ap_err <- NULL
    rules_raw <- tryCatch(
      arules::apriori(tx, parameter = list(supp = input$m2_apriori_support,
                                           conf = input$m2_apriori_confidence,
                                           minlen = 2,
                                           maxlen = input$m2_max_combo_size,
                                           target = "rules"),
                      control = list(verbose = FALSE)),
      error = function(e) { ap_err <<- e$message; NULL }
    )

    if (is.null(rules_raw)) {
      rules_r(list(rules = NULL, df = data.frame(), ok = FALSE,
                   msg = paste("Apriori failed:", ap_err)))
      return()
    }

    n_raw <- length(rules_raw)
    if (n_raw == 0) {
      rules_r(list(rules = NULL, df = data.frame(), ok = FALSE,
                   msg = paste("0 rules found with Support >=", input$m2_apriori_support,
                               "and Confidence >=", input$m2_apriori_confidence,
                               ". Try lowering these thresholds.")))
      return()
    }

    rules_filt <- rules_raw[quality(rules_raw)$lift >= input$m2_apriori_lift]
    n_filt <- length(rules_filt)

    if (n_filt == 0) {
      rules_r(list(rules = NULL, df = data.frame(), ok = FALSE,
                   msg = paste(n_raw, "rules found, but none passed lift filter (>=",
                               input$m2_apriori_lift, "). Try lowering Min Lift.")))
      return()
    }

    rdf <- data.frame(
      LHS = labels(lhs(rules_filt)),
      RHS = labels(rhs(rules_filt)),
      size = size(lhs(rules_filt)) + size(rhs(rules_filt)),
      support = quality(rules_filt)$support,
      confidence = quality(rules_filt)$confidence,
      lift = quality(rules_filt)$lift,
      count = quality(rules_filt)$count,
      stringsAsFactors = FALSE
    )
    ok <- TRUE

    max_size <- max(rdf$size)
    updateSliderInput(session, "m2_scatter_size", max = max_size, value = 2)

    status_msg <- paste(n_raw, "rules,", n_filt, "after lift filter")
    if (truncated) {
      status_msg <- paste0(status_msg, "; ", n_pass, " herbs passed min_freq, truncated to Top ", max_rule_herbs, " by frequency")
      showNotification(paste(n_pass, "herbs passed min_freq; using Top", max_rule_herbs, "by frequency."),
                       type = "warning", duration = 8)
    }
    rules_r(list(rules = rules_filt, df = rdf, ok = ok, msg = status_msg, n_raw = n_raw,
                 n_items = n_items, n_pass = n_pass, max_rule_herbs = max_rule_herbs,
                 truncated = truncated))
    m2_res <- shared_data$module_results$m2
    if (is.null(m2_res)) m2_res <- list()
    m2_res$rules <- list(df = rdf, ok = ok, n_items = n_items, n_pass = n_pass,
                         max_rule_herbs = max_rule_herbs, truncated = truncated)
    shared_data$module_results$m2 <- m2_res
    showNotification(paste(n_filt, "rules found"), type = "message")
  })

  output$dt_rules <- renderDT({
    r <- rules_r(); req(r)
    if (!r$ok) return(datatable(data.frame(Note = r$msg), rownames = FALSE))
    d <- r$df
    sz <- input$m2_scatter_size
    if (!is.null(sz) && "size" %in% names(d)) d <- d[d$size == sz, ]
    datatable(d[, c("LHS","RHS","support","confidence","lift","count")],
              options = list(pageLength = 15), rownames = FALSE) %>%
      formatRound(c("support","confidence","lift"), 3)
  })

  output$plot_rules_scatter <- renderPlotly({
    lang <- runtime_lang(shared_data)
    r <- rules_r(); req(r, r$ok)
    d <- r$df
    sz <- input$m2_scatter_size
    if (!is.null(sz) && "size" %in% names(d)) d <- d[d$size == sz, ]
    if (nrow(d) == 0) return(plotly_empty(type="scatter",mode="none") %>%
      layout(title = runtime_tr("No rules with %d herbs", lang, sz), paper_bgcolor="#fff", plot_bgcolor="#fff"))
    plot_ly(d, x = ~support, y = ~confidence, color = ~lift, type = "scatter", mode = "markers",
            marker = list(size = 8, opacity = 0.7),
            text = ~paste(LHS, "→", RHS, "<br>Lift:", round(lift, 2)), hoverinfo = "text") %>%
      layout(title = runtime_tr("Support vs Confidence (Size = %d)", lang, sz),
             coloraxis = list(colorbar = list(title = "Lift")))
  })

  output$plot_rules_net <- renderVisNetwork({
    r <- rules_r(); req(r, r$ok)
    d <- r$df
    n_show <- if (is.null(input$m2_net_top_n)) 50 else input$m2_net_top_n
    if (nrow(d) > n_show) d <- d[order(-d$lift), ][1:n_show, ]
    nodes <- unique(c(d$LHS, d$RHS))
    visNetwork(
      data.frame(id = nodes, label = nodes, stringsAsFactors = FALSE),
      data.frame(from = d$LHS, to = d$RHS,
                 label = paste0("L:", round(d$lift, 1)), value = d$lift, stringsAsFactors = FALSE)
    ) %>% visEdges(arrows = "to") %>% visOptions(highlightNearest = TRUE)
  })

  output$dl_rules <- downloadHandler(
    filename = function() localized_filename("association_rules", "csv", runtime_lang(shared_data)),
    content = function(file) {
      r <- rules_r(); req(r, r$ok)
      write.csv(localized_columns(r$df, runtime_lang(shared_data)), file, row.names = FALSE, fileEncoding = "UTF-8")
    }
  )

  # ===== Clustering =====
  observeEvent(input$run_cluster, {
    req(shared_data$clean, shared_data$clean_col)
    data <- shared_data$clean; col <- shared_data$clean_col

    withProgress(message = "Clustering + silhouette analysis...", {
      herbs_all <- sort(unique(unlist(str_split(data[[col]], "\\s+"))))
      herbs_all <- herbs_all[nchar(herbs_all) >= 2]

      mat <- matrix(0, nrow = length(herbs_all), ncol = nrow(data))
      rownames(mat) <- herbs_all
      for (i in seq_len(nrow(data))) {
        hx <- unlist(str_split(data[[col]][i], "\\s+"))
        hx <- hx[hx != "" & nchar(hx) >= 2]
        if (length(hx) > 0) mat[hx, i] <- 1
      }

      freq <- rowSums(mat)
      keep_herbs <- names(freq)[freq >= input$m2_min_freq_clust]
      if (length(keep_herbs) < 5) keep_herbs <- names(sort(freq, decreasing = TRUE))[1:min(50, length(freq))]
      if (length(keep_herbs) > 200) keep_herbs <- names(sort(freq, decreasing = TRUE))[1:200]
      mat_sub <- mat[keep_herbs, , drop = FALSE]

      method <- input$m2_cluster_method
      cdf <- data.frame()
      hc <- NULL
      k_guide <- NULL

      if (grepl("Ward", method)) {
        d <- dist(mat_sub, method = "binary")
        hc <- hclust(d, method = "ward.D2")
        cdf <- data.frame(Herb = character(0), Cluster = integer(0), stringsAsFactors = FALSE)
      } else {
        set.seed(42)
        km <- kmeans(mat_sub, centers = input$m2_cluster_k, nstart = 10)
        cdf <- data.frame(Herb = names(km$cluster), Cluster = as.integer(km$cluster))
        n_max <- min(if(is.null(input$m2_elbow_max_k)) 18 else input$m2_elbow_max_k, nrow(mat_sub) - 1)
        if (n_max >= 2) {
          wss_vals <- sapply(2:n_max, function(k) {
            kmeans(mat_sub, centers = k, nstart = 10)$tot.withinss
          })
          if (length(wss_vals) >= 4) {
            d2 <- diff(wss_vals, differences=2)
            best_k <- 2 + which.max(d2)
          } else {
            best_k <- 2
          }
          k_guide <- data.frame(k = 2:n_max, value = wss_vals, type = "elbow",
                                y_label = "Total Within-Cluster Sum of Squares (WSS)",
                                title = "Elbow Method for K-means Clustering",
                                best_k = best_k)
        }
      }
      cluster_r(list(df = cdf, mat = mat_sub, hc = hc, method = method, k_guide = k_guide))
      final_clusters <- if (grepl("Ward", method)) {
        k <- input$ward_k_slider
        if (is.null(k)) k <- 5
        data.frame(Herb = names(cutree(hc, k = k)), Cluster = as.integer(cutree(hc, k = k)),
                   stringsAsFactors = FALSE)
      } else cdf
      final_clusters$Freq <- rowSums(mat_sub)[final_clusters$Herb]
      m2_res <- shared_data$module_results$m2
      if (is.null(m2_res)) m2_res <- list()
      m2_res$cluster <- list(
        df = final_clusters, hc = hc, method = method, mat = mat_sub,
        ward_k = if (grepl("Ward", method)) k else NULL
      )
      shared_data$module_results$m2 <- m2_res
    })
    showNotification("Clustering complete", type = "message")
  })

  ward_cut <- reactive({
    r <- cluster_r()
    req(r, r$hc)
    k <- input$ward_k_slider
    if (is.null(k)) k <- 5
    hc <- r$hc
    data.frame(Herb = names(cutree(hc, k = k)), Cluster = as.integer(cutree(hc, k = k)),
               stringsAsFactors = FALSE)
  })

  observeEvent(input$ward_k_slider, {
    r <- cluster_r()
    if (is.null(r) || is.null(r$hc) || is.null(shared_data$module_results$m2$cluster)) return()
    k <- input$ward_k_slider
    if (is.null(k)) return()
    final_clusters <- data.frame(
      Herb = names(cutree(r$hc, k = k)), Cluster = as.integer(cutree(r$hc, k = k)),
      stringsAsFactors = FALSE
    )
    final_clusters$Freq <- rowSums(r$mat)[final_clusters$Herb]
    shared_data$module_results$m2$cluster$df <- final_clusters
    shared_data$module_results$m2$cluster$ward_k <- k
  }, ignoreInit = TRUE)

  ward_hmap <- reactive({
    r <- cluster_r(); cm <- cluster_members()
    if (is.null(r) || is.null(r$hc) || is.null(cm) || nrow(cm) < 2) return(NULL)
    hc <- r$hc; k <- input$ward_k_slider
    if (is.null(k)) k <- 5
    herbs <- cm$Herb; mat <- r$mat
    clusters <- sort(unique(cm$Cluster)); nc <- length(clusters)
    pm <- matrix(0, nrow=length(herbs), ncol=nc)
    colnames(pm) <- paste0("C", clusters); rownames(pm) <- herbs
    trx <- pmax(rowSums(mat[herbs, , drop=FALSE] > 0), 1)
    for (ci in seq_along(clusters)) {
      ch <- cm$Herb[cm$Cluster == clusters[ci]]
      rx <- colSums(mat[ch, , drop=FALSE]) > 0
      pm[, ci] <- rowSums(mat[herbs, rx, drop=FALSE] > 0) / trx
    }
    ann_row <- data.frame(Cluster=factor(paste0("C", cutree(hc, k=k)[herbs])), row.names=herbs)
    actual_levels <- levels(ann_row$Cluster)
    n_seen <- length(actual_levels)
    cl_pal <- c("#BAB0AC","#FF9DA7","#4B5D67","#D37295","#499894","#8CD17D","#76D1C1",
                "#4C72B0","#EDC948","#4DB6AC","#9575CD","#E15759","#F28E2B","#59A14F","#4E79A7")
    cl_colors <- setNames(rep(cl_pal, length.out = n_seen), actual_levels)
    pheatmap::pheatmap(pm,
      color=colorRampPalette(c("#fcfcfc","#fee5d9","#fcae91","#fb6a4a","#ef3b3b","#dc143c","#b71c1c"))(100),
      cluster_rows=hc, cluster_cols=FALSE,
      annotation_row=setNames(ann_row, runtime_tr("Cluster", runtime_lang(shared_data))),
      annotation_colors=setNames(list(cl_colors[levels(ann_row$Cluster)]), runtime_tr("Cluster", runtime_lang(shared_data))),
      annotation_names_row=FALSE, show_rownames=TRUE, fontsize_row=7, fontsize_col=10,
      main=paste("Ward (k =", k, ")"), angle_col=0, border_color=NA, silent=TRUE)
  })

  output$plot_ward_heatmap <- renderPlot({
    p <- ward_hmap()
    if (is.null(p)) { plot.new(); text(0.5, 0.5, runtime_tr("Run Ward clustering first.", runtime_lang(shared_data)), cex=1.2); return() }
    grid::grid.newpage(); grid::grid.draw(p$gtable)
  })

  output$dl_ward_hmap <- downloadHandler(
    filename = function() {
      k <- input$ward_k_slider; if (is.null(k)) k <- 5
      paste0(runtime_tr("ward_heatmap", runtime_lang(shared_data)), "_k", k, "_", Sys.Date(), ".png")
    },
    content = function(file) {
      p <- ward_hmap(); req(p)
      png(file, width=1200, height=1600, res=150)
      grid::grid.newpage(); grid::grid.draw(p$gtable)
      dev.off()
    }
  )

  output$plot_dendrogram <- renderPlotly({
    lang <- runtime_lang(shared_data)
    r <- cluster_r()
    if (is.null(r) || is.null(r$hc)) return(plotly_empty(type="scatter",mode="none") %>%
      layout(title=runtime_tr("Run Ward clustering to see dendrogram.", lang), paper_bgcolor="#fff", plot_bgcolor="#fff"))
    hc <- r$hc; n <- length(hc$order); nm <- n - 1
    pos <- match(seq_len(n), hc$order)
    xv <- numeric(n + nm); yv <- numeric(n + nm)
    xv[seq_len(n)] <- pos; yv[seq_len(n)] <- 0
    sx <- c(); sy <- c()
    for (i in seq_len(nm)) { ni <- n + i; L <- hc$merge[i,1]; R <- hc$merge[i,2]
      xL <- if (L < 0) xv[-L] else xv[n + L]; xR <- if (R < 0) xv[-R] else xv[n + R]
      yL <- if (L < 0) 0 else yv[n + L]; yR <- if (R < 0) 0 else yv[n + R]
      xv[ni] <- (xL + xR) / 2; yv[ni] <- hc$height[i]
      sx <- c(sx, xL,xL,NA, xL,xR,NA, xR,xR,NA)
      sy <- c(sy, yL,hc$height[i],NA, hc$height[i],hc$height[i],NA, yR,hc$height[i],NA) }
    lbs <- hc$labels[hc$order]; lx <- pos[hc$order]
    plot_ly() %>%
      add_trace(x=sx, y=sy, type="scatter", mode="lines", line=list(color="#2c3e50", width=1),
                hoverinfo="none", showlegend=FALSE) %>%
      add_trace(x=lx, y=rep(-max(hc$height)*0.02, n), type="scatter", mode="text",
                text=lbs, textfont=list(size=9, color="#2c3e50"), textposition="top center",
                hoverinfo="text", hovertext=lbs, showlegend=FALSE) %>%
      layout(title=list(text=runtime_tr("Ward Hierarchical Clustering - %d herbs", lang, n), x=0.5),
             xaxis=list(title="", showticklabels=FALSE, showgrid=FALSE, zeroline=FALSE, range=c(0.5, n+0.5)),
             yaxis=list(title=runtime_tr("Distance", lang), showgrid=TRUE, rangemode="tozero"),
             paper_bgcolor="#fff", plot_bgcolor="#fff", margin=list(t=60, b=60, l=40, r=20),
             showlegend=FALSE) })

  kmeans_hmap <- reactive({
    r <- cluster_r(); cm <- cluster_members()
    if (is.null(r) || is.null(r$df) || nrow(r$df) == 0 || is.null(cm) || nrow(cm) < 2) return(NULL)
    mat <- r$mat; herbs_all <- rownames(mat)
    clusters <- sort(unique(cm$Cluster)); nc <- length(clusters)
    herb_order <- cm[order(cm$Cluster, -cm$Freq), ]
    herbs_all <- herb_order$Herb
    pm <- matrix(0, nrow=length(herbs_all), ncol=nc)
    colnames(pm) <- paste0("C", clusters); rownames(pm) <- herbs_all
    trx <- pmax(rowSums(mat[herbs_all, , drop=FALSE] > 0), 1)
    for (ci in seq_along(clusters)) {
      ch <- cm$Herb[cm$Cluster == clusters[ci]]
      rx <- colSums(mat[ch, , drop=FALSE]) > 0
      pm[, ci] <- rowSums(mat[herbs_all, rx, drop=FALSE] > 0) / trx
    }
    cl_vec <- setNames(paste0("C", cm$Cluster), cm$Herb)
    ann_row <- data.frame(Cluster=factor(cl_vec[herbs_all]), row.names=herbs_all)
    actual_levels <- levels(ann_row$Cluster)
    nn <- length(actual_levels)
    cl_pal <- c("#BAB0AC","#FF9DA7","#4B5D67","#D37295","#499894","#8CD17D","#76D1C1",
                "#4C72B0","#EDC948","#4DB6AC","#9575CD","#E15759","#F28E2B","#59A14F","#4E79A7")
    cl_colors <- setNames(rep(cl_pal, length.out = nn), actual_levels)
    pheatmap::pheatmap(pm,
      color=colorRampPalette(c("#fcfcfc","#fee5d9","#fcae91","#fb6a4a","#ef3b3b","#dc143c","#b71c1c"))(100),
      cluster_rows=FALSE, cluster_cols=FALSE,
      annotation_row=setNames(ann_row, runtime_tr("Cluster", runtime_lang(shared_data))),
      annotation_colors=setNames(list(cl_colors[levels(ann_row$Cluster)]), runtime_tr("Cluster", runtime_lang(shared_data))),
      annotation_names_row=FALSE, show_rownames=TRUE, fontsize_row=7, fontsize_col=10,
      main=paste("K-means (k =", nn, ")"), angle_col=0, border_color=NA, silent=TRUE)
  })

  output$plot_kmeans_heatmap <- renderPlot({
    p <- kmeans_hmap()
    if (is.null(p)) { plot.new(); text(0.5, 0.5, runtime_tr("Run K-means clustering first.", runtime_lang(shared_data)), cex=1.2); return() }
    grid::grid.newpage(); grid::grid.draw(p$gtable)
  })

  output$dl_kmeans_hmap <- downloadHandler(
    filename = function() localized_filename("kmeans_heatmap", "png", runtime_lang(shared_data)),
    content = function(file) {
      p <- kmeans_hmap(); req(p)
      png(file, width=1200, height=1600, res=150)
      grid::grid.newpage(); grid::grid.draw(p$gtable)
      dev.off()
    }
  )

  output$plot_kguide <- renderPlotly({
    lang <- runtime_lang(shared_data)
    r <- cluster_r()
    if (is.null(r) || is.null(r$k_guide) || nrow(r$k_guide) == 0)
      return(plotly_empty(type="scatter",mode="none") %>%
        layout(title=runtime_tr("K selection guide will appear after clustering.", lang), paper_bgcolor="#fff", plot_bgcolor="#fff"))
    kd <- r$k_guide
    bk <- kd$best_k[1]
    plot_ly() %>%
      add_trace(data=kd, x=~k, y=~value, type="scatter", mode="lines+markers",
                line=list(color="#3498db", width=2), marker=list(color="#3498db", size=6),
                name="WSS", showlegend=FALSE) %>%
      add_trace(data=kd[kd$k == bk, ], x=~k, y=~value, type="scatter", mode="markers",
                marker=list(color="#e74c3c", size=12, symbol="circle"),
                name=runtime_tr("Best k = %d", lang, bk), showlegend=FALSE) %>%
      layout(title=runtime_tr(kd$title[1], lang), xaxis=list(title=runtime_tr("Number of Clusters k", lang), dtick=1),
             yaxis=list(title=runtime_tr(kd$y_label[1], lang)), paper_bgcolor="#fff", plot_bgcolor="#fff") })

  cluster_members <- reactive({ r <- cluster_r()
    if (is.null(r)) return(NULL)
    df <- if (grepl("Ward", r$method)) ward_cut() else r$df
    if (is.null(df) || nrow(df)==0) return(NULL)
    freq <- rowSums(r$mat); df$Freq <- freq[df$Herb]
    df[order(df$Cluster, -df$Freq), ] })

  cluster_props <- reactive({ r <- cluster_r(); cm <- cluster_members()
    if (is.null(r) || is.null(cm)) return(NULL)
    mat <- r$mat; herbs <- rownames(mat); cls <- sort(unique(cm$Cluster))
    nc <- length(cls); pm <- matrix(0, nrow=length(herbs), ncol=nc)
    colnames(pm) <- paste0("C", cls); rownames(pm) <- herbs
    trx <- pmax(rowSums(mat > 0), 1)
    for (ci in seq_along(cls)) { ch <- cm$Herb[cm$Cluster == cls[ci]]
      rx <- colSums(mat[ch, , drop=FALSE]) > 0
      pm[, ci] <- rowSums(mat[, rx, drop=FALSE] > 0) / trx }
    pm })

  output$plot_cluster_detail <- renderPlotly({
    lang <- runtime_lang(shared_data)
    cm <- cluster_members(); cr <- cluster_r()
    if (is.null(cm) || is.null(cr)) return(plotly_empty(type="scatter",mode="none") %>%
      layout(title=runtime_tr("Select a cluster above.", lang), paper_bgcolor="#fff", plot_bgcolor="#fff"))
    sel <- input$m2_cluster_sel
    choices <- paste0("C", sort(unique(cm$Cluster)))
    if (is.null(sel) || !(sel %in% choices)) sel <- choices[1]
    cid <- as.integer(gsub("C","",sel))
    dc <- cm[cm$Cluster == cid, ]; if (nrow(dc) > 30) dc <- head(dc, 30)
    total_rx <- ncol(cr$mat); dc$Rate <- round(dc$Freq / total_rx * 100, 2)
    dc <- dc[order(-dc$Freq), ]
    n_dc <- nrow(dc)
    pal <- colorRamp(c("#fcfcfc","#fee5d9","#fcae91","#fb6a4a","#ef3b3b","#dc143c","#b71c1c"))
    t_vals <- if (n_dc > 1) seq(1, 0, length.out = n_dc) else 0.5
    cols <- rgb(pal(t_vals), maxColorValue = 255)
    plot_ly(dc, x=~Freq, y=~reorder(Herb, Freq), type="bar",
            marker=list(color=cols),
            text=~paste0(Herb, "<br>", Freq, " (", Rate, "%)"),
            hoverinfo="text") %>%
      layout(title=list(text=runtime_tr("Top Herbs in %s", lang, sel), x=0.5),
             xaxis=list(title=runtime_tr("Prescription Count", lang), fixedrange=TRUE, rangemode="tozero"),
             yaxis=list(title="", fixedrange=TRUE),
             paper_bgcolor="#fff", plot_bgcolor="#fff") })

  observe({ cm <- cluster_members()
    if (!is.null(cm)) { choices <- paste0("C", sort(unique(cm$Cluster)))
      updateSelectInput(session, "m2_cluster_sel", choices=choices, selected=choices[1]) } })

  output$dt_clusters <- renderDT({ r <- cluster_r(); req(r)
    lang <- runtime_lang(shared_data)
    df <- if (grepl("Ward", r$method)) ward_cut() else r$df
    if (is.null(df) || nrow(df)==0)
      return(datatable(localized_placeholder("Adjust k slider to see clusters.", lang), rownames=FALSE))
    df <- df[order(df$Cluster, df$Herb), ]
    summary <- df %>% group_by(Cluster) %>%
      summarise(Herbs=paste(Herb, collapse=", "), Count=n(), .groups="drop")
    datatable(summary, options=list(pageLength=15, dom="tp"), rownames=FALSE,
              colnames=c("Cluster", "Herbs", "Count")) })

  output$dl_cluster <- downloadHandler(
    filename = function() localized_filename("clustering_result", "csv", runtime_lang(shared_data)),
    content = function(file) {
      cm <- cluster_members(); cr <- cluster_r()
      if (is.null(cm) || is.null(cr)) {
        write.csv(localized_placeholder("Run clustering first.", runtime_lang(shared_data)), file, row.names=FALSE, fileEncoding="UTF-8")
        return()
      }
      total_rx <- ncol(cr$mat)
      cm$Rate <- round(cm$Freq / total_rx * 100, 2)
      cm <- cm[order(cm$Cluster, -cm$Freq), ]
      out <- cm[, c("Cluster", "Herb", "Freq", "Rate")]
      colnames(out) <- c("Cluster", "Herb", "Frequency", "UsageRate")
      write.csv(localized_columns(out, runtime_lang(shared_data)), file, row.names = FALSE, fileEncoding = "UTF-8")
    }
  )
}
