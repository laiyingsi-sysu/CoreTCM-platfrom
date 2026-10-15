# ============================================================
# mod_global_settings.R — Global Settings Panel
# ============================================================

mod_global_settings_ui <- function(id) {
  ns <- NS(id)
  # No tabItem — this is a modal triggered from navbar
  NULL
}

mod_global_settings_modal <- function(id, lang = "zh", params = DEFAULT_PARAMS) {
  ns <- NS(id)

  tagList(
    fileInput(ns("import_json"), runtime_tr("Import Parameters (.json)", lang), accept = ".json"),
    actionButton(ns("reset_global"), runtime_tr("Reset Global Defaults", lang)),
    h4(tr("m0_title", lang), style = "color:#2c3e50; border-bottom:1px solid #eee; padding-bottom:5px;"),
    fluidRow(
      column(4, checkboxInput(ns("g_m0_include_standardized_dosage_export"), tr("m0_include_standardized_dosage_export", lang), value = isTRUE(params$m0_include_standardized_dosage_export))),
      column(4, checkboxInput(ns("g_m0_remove_prefix"), tr("m0_remove_prefix", lang), value = isTRUE(params$m0_remove_prefix))),
      column(4, checkboxInput(ns("g_m0_remove_duplicate"), tr("m0_remove_duplicate", lang), value = isTRUE(params$m0_remove_duplicate)))
    ),

    h4(tr("m2_title", lang), style = "color:#2c3e50; border-bottom:1px solid #eee; padding-bottom:5px; margin-top:15px;"),
    fluidRow(
      column(4, sliderInput(ns("g_m2_freq_top_n"), tr("m2_freq_top_n", lang),
                           min = 5, max = 100, value = 20)),
      column(4, sliderInput(ns("g_m2_apriori_support"), tr("m2_apriori_support", lang),
                           min = 0.01, max = 0.50, value = 0.05, step = 0.01)),
      column(4, sliderInput(ns("g_m2_apriori_confidence"), tr("m2_apriori_confidence", lang),
                           min = 0.10, max = 1.00, value = 0.30, step = 0.01))
    ),
    fluidRow(
      column(4, sliderInput(ns("g_m2_apriori_lift"), tr("m2_apriori_lift", lang),
                           min = 0.5, max = 5.0, value = 1.0, step = 0.1)),
      column(4, sliderInput(ns("g_m2_max_combo_size"), tr("m2_max_combo_size", lang),
                           min = 2, max = 6, value = 4)),
      column(4, sliderInput(ns("g_m2_max_rule_herbs"), tr("m2_max_rule_herbs", lang),
                           min = 50, max = 400, value = 200, step = 10))
    ),
    fluidRow(
      column(4, selectInput(ns("g_m2_cluster_method"), tr("m2_cluster_method", lang),
                           choices = c("Ward (hierarchical)", "K-means"), selected = "Ward (hierarchical)")),
    ),
    fluidRow(
      column(4, sliderInput(ns("g_m2_cluster_k"), tr("m2_cluster_k", lang),
                           min = 2, max = 20, value = 5))
    ),

    h4(tr("m3_title", lang), style = "color:#2c3e50; border-bottom:1px solid #eee; padding-bottom:5px; margin-top:15px;"),
    fluidRow(
      column(4, selectInput(ns("g_m3_edge_weight"), tr("m3_edge_weight", lang),
                           choices = c("PMI", "Frequency", "Hybrid"), selected = "PMI")),
      column(8, sliderInput(ns("g_m3_hybrid_alpha"), tr("m3_hybrid_alpha", lang),
                           min = 0, max = 1, value = 0.5, step = 0.05)),
      column(4, p("Beta is derived as 1 − Alpha."))
    ),
    fluidRow(
      column(4, sliderInput(ns("g_m3_kcore_quantile"), tr("m3_kcore_quantile", lang),
                           min = 0.30, max = 0.90, value = 0.70, step = 0.01)),
      column(4, sliderInput(ns("g_m3_min_freq"), tr("m3_min_freq", lang),
                           min = 1, max = 20, value = 3))
    ),

    h4(tr("m4_title", lang), style = "color:#2c3e50; border-bottom:1px solid #eee; padding-bottom:5px; margin-top:15px;"),
    fluidRow(
      column(4, sliderInput(ns("g_m4_support_quantile"), tr("m4_support_quantile", lang),
                           min = 0.10, max = 0.90, value = 0.50, step = 0.01)),
      column(4, sliderInput(ns("g_m4_coverage_quantile"), tr("m4_coverage_quantile", lang),
                           min = 0.50, max = 0.95, value = 0.75, step = 0.01)),
      column(4, sliderInput(ns("g_m4_specificity_quantile"), tr("m4_specificity_quantile", lang),
                           min = 0.10, max = 0.90, value = 0.50, step = 0.01))
    ),
    fluidRow(
      column(4, sliderInput(ns("g_m4_lift_quantile"), tr("m4_lift_quantile", lang),
                           min = 0.50, max = 0.95, value = 0.75, step = 0.01)),
      column(4, checkboxInput(ns("g_m4_coverage_by_size"), tr("m4_coverage_by_size", lang), value = TRUE))
    ),

    h4(tr("m6_title", lang), style = "color:#2c3e50; border-bottom:1px solid #eee; padding-bottom:5px; margin-top:15px;"),
    fluidRow(
      column(6, sliderInput(ns("g_m6_herb_top_n"), tr("m6_herb_top_n", lang),
                           min = 5, max = 100, value = 20)),
      column(6, sliderInput(ns("g_m6_pair_top_n"), tr("m6_pair_top_n", lang),
                           min = 5, max = 100, value = 20))
    )
  )
}

mod_global_settings_server <- function(input, output, session, shared_data) {

  update_global_controls <- function(params) {
    for (key in names(DEFAULT_PARAMS)) {
      value <- params[[key]]
      if (is.null(value)) next
      id <- paste0("g_", key)
      if (is.logical(value)) updateCheckboxInput(session, id, value = value)
      else if (is.numeric(value)) updateSliderInput(session, id, value = value)
      else updateSelectInput(session, id, selected = value)
    }
  }
  observeEvent(input$reset_global, {
    shared_data$active_params <- DEFAULT_PARAMS
    update_global_controls(DEFAULT_PARAMS)
  })
  observeEvent(input$import_json, {
    req(input$import_json$datapath)
    tryCatch({
      params <- jsonlite::read_json(input$import_json$datapath, simplifyVector = TRUE)
      if (!is.list(params) || is.null(names(params))) stop("Invalid parameter JSON")
      params <- migrate_formula_params(params)
      keys <- intersect(names(params), names(DEFAULT_PARAMS))
      for (key in keys) {
        value <- params[[key]]; default <- DEFAULT_PARAMS[[key]]
        if (length(value) != 1L || is.na(value) ||
            (is.logical(default) && !is.logical(value)) ||
            (is.numeric(default) && (!is.numeric(value) || !is.finite(value))) ||
            (is.character(default) && !is.character(value))) stop("Invalid parameter JSON")
      }
      merged <- utils::modifyList(as.list(shared_data$active_params), params[keys])
      shared_data$active_params <- merged
      update_global_controls(merged)
      showNotification(runtime_tr("Parameters imported", runtime_lang(shared_data)), type = "message")
    }, error = function(e) showNotification(runtime_tr("Invalid parameter JSON", runtime_lang(shared_data)), type = "error"))
  })

  observeEvent(input$apply_global, {
    shared_data$active_params$m0_include_standardized_dosage_export      <- input$g_m0_include_standardized_dosage_export
    shared_data$active_params$m0_remove_prefix      <- input$g_m0_remove_prefix
    shared_data$active_params$m0_remove_duplicate   <- input$g_m0_remove_duplicate
    shared_data$active_params$m2_freq_top_n         <- input$g_m2_freq_top_n
    shared_data$active_params$m2_apriori_support    <- input$g_m2_apriori_support
    shared_data$active_params$m2_apriori_confidence <- input$g_m2_apriori_confidence
    shared_data$active_params$m2_apriori_lift       <- input$g_m2_apriori_lift
    shared_data$active_params$m2_max_combo_size     <- input$g_m2_max_combo_size
    shared_data$active_params$m2_max_rule_herbs     <- input$g_m2_max_rule_herbs
    shared_data$active_params$m2_cluster_method     <- input$g_m2_cluster_method
    shared_data$active_params$m2_cluster_k          <- input$g_m2_cluster_k
    shared_data$active_params$m3_edge_weight        <- input$g_m3_edge_weight
    shared_data$active_params$m3_hybrid_alpha       <- input$g_m3_hybrid_alpha
    shared_data$active_params$m3_hybrid_beta        <- round(1 - input$g_m3_hybrid_alpha, 2)
    shared_data$active_params$m3_kcore_quantile     <- input$g_m3_kcore_quantile
    shared_data$active_params$m3_min_freq           <- input$g_m3_min_freq
    shared_data$active_params$m4_support_quantile      <- input$g_m4_support_quantile
    shared_data$active_params$m4_coverage_quantile     <- input$g_m4_coverage_quantile
    shared_data$active_params$m4_specificity_quantile <- input$g_m4_specificity_quantile
    shared_data$active_params$m4_lift_quantile         <- input$g_m4_lift_quantile
    shared_data$active_params$m4_coverage_by_size      <- input$g_m4_coverage_by_size
    shared_data$active_params$m6_herb_top_n         <- input$g_m6_herb_top_n
    shared_data$active_params$m6_pair_top_n         <- input$g_m6_pair_top_n

    showNotification(tr("global_updated", shared_data$lang), type = "message")
  })

  output$export_json <- downloadHandler(
    filename = function() paste0(runtime_tr("params", runtime_lang(shared_data)), "_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".json"),
    content = function(file) {
      params_list <- migrate_formula_params(as.list(shared_data$active_params))
      # Remove Shiny internal fields
      params_list <- params_list[!grepl("^\\.", names(params_list))]
      write_json(params_list, file, pretty = TRUE, auto_unbox = TRUE)
    }
  )
}
