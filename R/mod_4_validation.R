# ============================================================
# mod_4_validation.R — Module 4: Combination Evaluation (SCCL)
# Support / Coverage / Association Specificity / Lift
# ============================================================

mod_4_sidebar <- function(id) {
  ns <- NS(id)
  tagList(
    div(class = "param-section",
      h5(icon("info-circle"), "Data Status"),
      uiOutput(ns("data_status"))
    ),
    div(class = "param-section",
      h5(icon("sliders-h"), "Threshold Quantiles"),
      sliderInput(ns("m4_support_quantile"), "Support High Quantile",
                  min = 0.10, max = 0.90, value = 0.50, step = 0.01),
      sliderInput(ns("m4_coverage_quantile"), "Coverage High Quantile",
                  min = 0.50, max = 0.95, value = 0.75, step = 0.01),
      sliderInput(ns("m4_specificity_quantile"), "Association Specificity High Quantile",
                  min = 0.10, max = 0.90, value = 0.50, step = 0.01),
      sliderInput(ns("m4_lift_quantile"), "Lift High Quantile",
                  min = 0.50, max = 0.95, value = 0.75, step = 0.01),
      checkboxInput(ns("m4_coverage_by_size"), "Coverage by Combo Size", value = TRUE)
    ),
    div(class = "sidebar-action-bar",
      parameter_guide("m4"),
      actionButton(ns("run_m4"), "Run Evaluation", icon = icon("play"),
                   class = "btn-primary btn-block"),
      br(),
      actionButton(ns("reset_params"), "Reset to Global", icon = icon("undo"),
                   class = "btn-sm btn-block")
    )
  )
}

mod_4_validation_ui <- function(id) {
  ns <- NS(id)
  tagList(
    workflow_guide("m4"),
    fluidRow(
      box(title = "Combination Evaluation (SCCL Framework)", status = "primary",
          solidHeader = TRUE, width = 12,
        p("This module characterizes core herbal combinations identified by the M3 network-mining module using four metrics: Support, Coverage, Association Specificity, and Lift. Based on dynamic quantile thresholds, the combinations are classified into five categories: Core Formula Group, Core Herb Group, Specific Compatibility Group, Noise Combination, and Other. These categories describe their co-occurrence and association profiles within the current dataset."),
        hr(),
        h5("Classification Rules"),
        p("Thresholds are dynamic quantiles (adjustable below). After evaluation, placeholders are replaced with actual values.", style = "font-size:12px; color:#6c757d;"),
        uiOutput(ns("rules_table"))
      )
    ),
    fluidRow(
      box(title = "Output", status = "success", solidHeader = TRUE, width = 12,
        tabBox(width = 12,
          tabPanel("Classification Table",
            DTOutput(ns("dt_classified"))),
          tabPanel("Support-Coverage Scatter",
            plotlyOutput(ns("plot_support_coverage"), height = "550px")),
          tabPanel("Per-Type Summary",
            verbatimTextOutput(ns("type_summary")),
            br(),
            plotlyOutput(ns("plot_metrics_dist"), height = "800px"),
            br(),
            downloadButton(ns("dl_per_type_csv"), "Download Per-Type Summary (CSV)", class = "btn-sm")),
          tabPanel("Custom Combination Characterization / 自定义配伍特征分析",
            fluidRow(
              column(8,
                textInput(ns("custom_combination"), "Herbs / 药物组合",
                          placeholder = "请输入至少两味药，以空格分隔，例如：黄芪 当归 白术"),
                p("Uses exact herb names from cleaned prescriptions. Metrics and thresholds follow current M4 settings.",
                  style = "font-size:12px; color:#6c757d;")),
              column(4,
                br(),
                actionButton(ns("run_custom_combination"), "Characterize Combination / 分析配伍",
                             icon = icon("play"), class = "btn-primary"))
            ),
            br(),
            uiOutput(ns("custom_combination_result")))
        )
      )
    )
  )
}

mod_4_validation_server <- function(input, output, session, shared_data) {
  datatable <- function(data, ...) localized_datatable(data, ..., lang = runtime_lang(shared_data))


  m4_results <- reactiveVal(NULL)
  custom_combination_result <- reactiveVal(NULL)

  output$data_status <- renderUI({
    m3_ok <- !is.null(shared_data$module_results$m3) &&
             !is.null(shared_data$module_results$m3$core) &&
             nrow(shared_data$module_results$m3$core$core_formula_table) > 0
    if (!m3_ok)
      div(class = "alert alert-warning", icon("exclamation-triangle"),
          "M3 results not available. Run Network Mining (M3) first.")
    else
      div(class = "alert alert-success", icon("check-circle"),
          runtime_tr("Ready: %d core formulas from M3.", runtime_lang(shared_data),
                nrow(shared_data$module_results$m3$core$core_formula_table)))
  })

  observeEvent(input$reset_params, {
    p <- shared_data$active_params
    updateSliderInput(session, "m4_support_quantile",      value = p$m4_support_quantile)
    updateSliderInput(session, "m4_coverage_quantile",     value = p$m4_coverage_quantile)
    updateSliderInput(session, "m4_specificity_quantile", value = p$m4_specificity_quantile)
    updateSliderInput(session, "m4_lift_quantile",         value = p$m4_lift_quantile)
    updateCheckboxInput(session, "m4_coverage_by_size",    value = p$m4_coverage_by_size)
    showNotification("Parameters reset to global defaults", type = "message")
  })

  observeEvent(input$run_m4, {
    if (is.null(shared_data$module_results$m3)) {
      m4_results(list(error = "M3 results not available. Run Network Mining first."))
      return()
    }
    if (is.null(shared_data$clean)) {
      m4_results(list(error = "No cleaned data. Run M0 first."))
      return()
    }

    m3 <- shared_data$module_results$m3
    data <- shared_data$clean
    col  <- shared_data$clean_col

    cf <- m3$core$core_formula_table
    if (is.null(cf) || nrow(cf) == 0) {
      m4_results(list(error = "No core formulas available. Run M3 first."))
      return()
    }

    showNotification("Computing metrics...", type = "message", duration = 3)

    val <- validate_core_formula(data, col, cf)
    if (nrow(val) == 0) {
      m4_results(list(error = "Validation produced empty results."))
      return()
    }

    cl <- classify_combinations(val,
      support_q      = input$m4_support_quantile,
      coverage_q     = input$m4_coverage_quantile,
      specificity_q = input$m4_specificity_quantile,
      lift_q         = input$m4_lift_quantile,
      coverage_by_size = input$m4_coverage_by_size)

    # Step 3: per-type summary
    type_summary <- cl %>%
      group_by(Type) %>%
      summarise(
        Count = n(),
        Pct   = round(n() / nrow(cl) * 100, 1),
        Avg_Support      = round(mean(Support), 4),
        Avg_Coverage     = round(mean(Coverage), 4),
        Avg_Association_Specificity = round(mean(Specificity), 4),
        Avg_Lift         = round(mean(Lift), 2),
        .groups = "drop"
      )

    coverage_by_size <- tapply(attr(cl, "coverage_thresh"), cl$ComboSize, function(x) x[1])
    thresholds <- list(
      support = attr(cl, "support_thresh"),
      coverage_by_size = coverage_by_size,
      coverage_global = as.numeric(quantile(val$Coverage, input$m4_coverage_quantile, na.rm = TRUE)),
      specificity = attr(cl, "specificity_thresh"),
      lift = attr(cl, "lift_thresh")
    )
    choices <- sort(unique(cl$Type))
    m4_results(list(
      val   = val,
      cl    = cl,
      types = type_summary,
      choices = choices,
      thresholds = thresholds,
      support_thresh      = thresholds$support,
      coverage_thresh     = attr(cl, "coverage_thresh"),
      specificity_thresh = thresholds$specificity,
      lift_thresh         = thresholds$lift
    ))
    showNotification(paste("Done!", nrow(cl), "combinations evaluated."), type = "message")
  })

  observeEvent(input$run_custom_combination, {
    if (is.null(shared_data$clean)) {
      custom_combination_result(list(error = "No cleaned data. Run M0 first."))
      return()
    }

    custom_input <- stringr::str_trim(input$custom_combination)
    herbs <- stringr::str_split(if (is.null(custom_input)) "" else custom_input, "\\s+")[[1]]
    herbs <- unique(herbs[nzchar(herbs)])
    if (length(herbs) < 2) {
      custom_combination_result(list(error = "Enter at least two herbs separated by spaces."))
      return()
    }

    data <- shared_data$clean
    col <- shared_data$clean_col
    custom_formula <- data.frame(Community = "Custom", Core_Formula = paste(herbs, collapse = " "), stringsAsFactors = FALSE)
    val <- validate_core_formula(data, col, custom_formula)
    r <- m4_results()

    if (!is.null(r) && is.null(r$error) && !is.null(r$thresholds)) {
      cl <- classify_with_thresholds(val, r$thresholds)
      custom_combination_result(list(result = cl[1, ], thresholds = list(
        support = r$thresholds$support,
        coverage = attr(cl, "coverage_thresh")[1],
        specificity = r$thresholds$specificity,
        lift = r$thresholds$lift
      )))
    } else {
      custom_combination_result(list(error = "Run M4 first to establish classification thresholds."))
    }
  })

  output$custom_combination_result <- renderUI({
    x <- custom_combination_result()
    if (is.null(x)) return(div(class = "alert alert-info", "Enter a combination and click Characterize Combination."))
    if (!is.null(x$error)) return(div(class = "alert alert-warning", icon("exclamation-triangle"), x$error))

    d <- x$result
    t <- x$thresholds
    hi_lo <- function(value, threshold) if (value >= threshold) "High" else "Low"
    type_colors <- c("Core Formula Group" = "#d4edda", "Core Herb Group" = "#fff3cd",
                     "Specific Compatibility Group" = "#cce5ff", "Noise Combination" = "#f8d7da", "Other" = "#e9ecef")

    tags$div(
      div(class = "alert", style = paste0("background:", type_colors[[d$Type]], "; border-color:#ced4da;"),
          strong(paste0(runtime_tr("Classification", runtime_lang(shared_data)), " / 配伍类型: ")), runtime_tr(d$Type, runtime_lang(shared_data))),
      tags$table(class = "table table-bordered table-condensed", style = "width:100%;",
        tags$thead(tags$tr(tags$th(runtime_tr("Combination", runtime_lang(shared_data))), tags$th(runtime_tr("Support", runtime_lang(shared_data))), tags$th(runtime_tr("Coverage", runtime_lang(shared_data))),
                           tags$th(runtime_tr("Association Specificity", runtime_lang(shared_data))), tags$th(runtime_tr("Lift", runtime_lang(shared_data))))),
        tags$tbody(tags$tr(
          tags$td(d$Formula),
          tags$td(sprintf("%.4f (%s; threshold %.4f)", d$Support, hi_lo(d$Support, t$support), t$support)),
          tags$td(sprintf("%.4f (%s; threshold %.4f)", d$Coverage, hi_lo(d$Coverage, t$coverage), t$coverage)),
          tags$td(sprintf("%.4f (%s; threshold %.4f)", d$Specificity, hi_lo(d$Specificity, t$specificity), t$specificity)),
          tags$td(sprintf("%.2f (%s; threshold %.2f)", d$Lift, hi_lo(d$Lift, t$lift), t$lift))
        ))
      )
    )
  })

  # ===== Classification rules table =====
  output$rules_table <- renderUI({
    r <- m4_results()
    has_vals <- !is.null(r) && !is.null(r$support_thresh)

    sq <- input$m4_support_quantile
    cq <- input$m4_coverage_quantile
    dq <- input$m4_specificity_quantile
    lq <- input$m4_lift_quantile

    # Helper: build "High (>=X)" or "Low (<X)" with HTML
    mk_hi <- function(label, val) HTML(sprintf("%s (&ge;%s)", label, val))
    mk_lo <- function(label, val) HTML(sprintf("%s (&lt;%s)",  label, val))

    if (has_vals) {
      sup_hi  <- mk_hi("<strong>High</strong>", round(r$support_thresh, 3))
      sup_lo  <- mk_lo("<strong>Low</strong>",  round(r$support_thresh, 3))
      cov_hi  <- mk_hi("<strong>High</strong>", sprintf("P<sub>%.0f</sub>(k)", cq*100))
      cov_lo  <- mk_lo("<strong>Low</strong>",  sprintf("P<sub>%.0f</sub>(k)", cq*100))
      spec_hi <- mk_hi("<strong>High</strong>", round(r$specificity_thresh, 3))
      spec_lo <- mk_lo("<strong>Low</strong>",  round(r$specificity_thresh, 3))
      lift_hi <- mk_hi("<strong>High</strong>", round(r$lift_thresh, 2))
      lift_lo <- mk_lo("<strong>Low</strong>",  round(r$lift_thresh, 2))
    } else {
      sup_hi  <- mk_hi("<strong>High</strong>", sprintf("P<sub>%.0f</sub>", sq*100))
      sup_lo  <- mk_lo("<strong>Low</strong>",  sprintf("P<sub>%.0f</sub>", sq*100))
      cov_hi  <- mk_hi("<strong>High</strong>", sprintf("P<sub>%.0f</sub>(k)", cq*100))
      cov_lo  <- mk_lo("<strong>Low</strong>",  sprintf("P<sub>%.0f</sub>(k)", cq*100))
      spec_hi <- mk_hi("<strong>High</strong>", sprintf("P<sub>%.0f</sub>", dq*100))
      spec_lo <- mk_lo("<strong>Low</strong>",  sprintf("P<sub>%.0f</sub>", dq*100))
      lift_hi <- mk_hi("<strong>High</strong>", sprintf("P<sub>%.0f</sub>", lq*100))
      lift_lo <- mk_lo("<strong>Low</strong>",  sprintf("P<sub>%.0f</sub>", lq*100))
    }

    tags$div(
      tags$table(class = "table table-bordered table-condensed",
        style = "width:100%; margin-bottom:0; font-size:12px;",
        tags$thead(tags$tr(
          tags$th("Combination Type"),
          tags$th("Support"),
          tags$th("Coverage"),
          tags$th("Association_Specificity"),
          tags$th("Lift"),
          tags$th("Clinical Interpretation")
        )),
        tags$tbody(
          tags$tr(style = "background:#d4edda;",
            tags$td(strong("Core Formula Group")),
            tags$td(sup_hi),
            tags$td(cov_hi),
            tags$td(spec_hi),
            tags$td(span(class = "text-muted", "—")),
            tags$td(span(style = "font-size:11px;", "High Support, Coverage & Association Specificity. Main prescription framework."))
          ),
          tags$tr(style = "background:#fff3cd;",
            tags$td(strong("Core Herb Group")),
            tags$td(sup_hi),
            tags$td(cov_hi),
            tags$td(spec_lo),
            tags$td(lift_hi),
            tags$td(span(style = "font-size:11px;", "High frequency, wide coverage, low fixity. Core herbs with specific syndrome associations."))
          ),
          tags$tr(style = "background:#cce5ff;",
            tags$td(strong("Specific Compatibility Group")),
            tags$td(sup_lo),
            tags$td(cov_lo),
            tags$td(spec_hi),
            tags$td(lift_hi),
            tags$td(span(style = "font-size:11px;", "Low frequency but high Lift & Association Specificity. Specific syndrome compatibility."))
          ),
          tags$tr(style = "background:#f8d7da;",
            tags$td(strong("Noise Combination")),
            tags$td(sup_lo),
            tags$td(cov_lo),
            tags$td(spec_lo),
            tags$td(lift_lo),
            tags$td(span(style = "font-size:11px;", "Low on all metrics. Limited clinical value."))
          ),
          tags$tr(style = "background:#e9ecef;",
            tags$td(strong("Other")),
            tags$td(span(class = "text-muted", "Does not meet rules above")),
            tags$td(span(class = "text-muted", "Does not meet rules above")),
            tags$td(span(class = "text-muted", "Does not meet rules above")),
            tags$td(span(class = "text-muted", "Does not meet rules above")),
            tags$td(span(style = "font-size:11px;", "Mixed indicator profile. No explicit combination type assigned."))
          )
        )
      )
    )
  })

  output$dt_classified <- renderDT({
    lang <- runtime_lang(shared_data)
    r <- m4_results()
    if (is.null(r) || !is.null(r$error))
      return(datatable(localized_placeholder(if (!is.null(r$error)) r$error else "Run evaluation to see results.", lang), rownames = FALSE))

    # Read attributes from original full data (subset drops them)
    full <- r$cl
    st <- attr(full, "support_thresh")      # scalar
    ct <- attr(full, "coverage_thresh")     # vector, one per row
    dt <- attr(full, "specificity_thresh")   # scalar
    lt <- attr(full, "lift_thresh")         # scalar

    d <- full[, c("Formula","ComboSize","Support","Coverage","Specificity","Lift","Type")]
    # Convert to numeric defensively (sub-setting may coerce)
    sv <- as.numeric(d$Support)
    cv_col <- as.numeric(d$Coverage)
    sp <- as.numeric(d$Specificity)
    lf <- as.numeric(d$Lift)

    lo <- function(x, t) if (x >= t) ">=" else "<"

    if (!is.null(st) && !is.null(ct) && !is.null(dt) && !is.null(lt)) {
      n <- nrow(d)
      for (i in seq_len(n)) {
        cti <- if (length(ct) > 1) ct[i] else ct
        d$Support[i]      <- sprintf("%.3f (%s%.3f)", sv[i],  lo(sv[i], st), st)
        d$Coverage[i]     <- sprintf("%.3f (%s%.3f)", cv_col[i], lo(cv_col[i], cti), cti)
        d$Specificity[i]  <- sprintf("%.3f (%s%.3f)", sp[i],  lo(sp[i], dt), dt)
        d$Lift[i]         <- sprintf("%.2f (%s%.2f)", lf[i],   lo(lf[i], lt), lt)
      }
    }

    d$Type <- vapply(d$Type, runtime_tr, character(1), lang = lang)
    datatable(d, options = list(pageLength = 15, scrollX = TRUE,
                                 columnDefs = list(list(className = "dt-left", targets = "_all"))),
              rownames = FALSE) %>%
      formatStyle("Type",
        backgroundColor = styleEqual(
          vapply(c("Core Formula Group","Core Herb Group","Specific Compatibility Group","Noise Combination","Other"), runtime_tr, character(1), lang = lang),
          c("#d4edda","#fff3cd","#cce5ff","#f8d7da","#e9ecef")))
  })

  output$plot_support_coverage <- renderPlotly({
    lang <- runtime_lang(shared_data)
    r <- m4_results()
    if (is.null(r) || !is.null(r$error)) return(plotly_empty() %>%
      layout(paper_bgcolor = "#ffffff", plot_bgcolor = "#ffffff",
             title = runtime_tr("Run evaluation to see results.", lang)))
    d <- r$cl
    pal <- c("Core Formula Group"="#27ae60","Core Herb Group"="#e67e22",
             "Specific Compatibility Group"="#2980b9","Noise Combination"="#95a5a6","Other"="#6c757d")
    d$Type <- vapply(d$Type, runtime_tr, character(1), lang = lang)
    names(pal) <- vapply(names(pal), runtime_tr, character(1), lang = lang)
    plot_ly(d, x = ~Support, y = ~Coverage, color = ~Type, colors = pal,
            type = "scatter", mode = "markers",
            marker = list(size = 10, opacity = 0.7),
            text = ~paste(Formula, "<br>", runtime_tr("Lift", lang), ":", round(Lift, 2)),
            hoverinfo = "text") %>%
      layout(title = runtime_tr("Support vs Coverage (color by Type)", lang),
             xaxis = list(title = runtime_tr("Support", lang)), yaxis = list(title = runtime_tr("Coverage", lang)),
             margin = list(t = 40),
             paper_bgcolor = "#ffffff", plot_bgcolor = "#ffffff")
  })

  output$type_summary <- renderText({
    lang <- runtime_lang(shared_data)
    r <- m4_results()
    if (is.null(r) || !is.null(r$error)) return(runtime_tr("Run evaluation to see summary.", lang))
    paste0(runtime_tr("Total combinations: ", lang), nrow(r$cl), "\n",
           runtime_tr("Core Formula Group", lang), ":      ", sum(r$cl$Type == "Core Formula Group"), "\n",
           runtime_tr("Core Herb Group", lang), ":       ", sum(r$cl$Type == "Core Herb Group"), "\n",
           runtime_tr("Specific Compatibility Group", lang), ":  ", sum(r$cl$Type == "Specific Compatibility Group"), "\n",
           runtime_tr("Noise Combination", lang), ":       ", sum(r$cl$Type == "Noise Combination"), "\n",
           runtime_tr("Other", lang), ":                   ", sum(r$cl$Type == "Other"), "\n\n",
           runtime_tr("Thresholds used:", lang), "\n",
           runtime_tr("Support:      > ", lang), round(attr(r$cl, "support_thresh"), 3), "\n",
           runtime_tr("Coverage:     by-size (", lang), input$m4_coverage_quantile, runtime_tr(" quantile)", lang), "\n",
           runtime_tr("Association Specificity: > ", lang), round(attr(r$cl, "specificity_thresh"), 3), "\n",
           runtime_tr("Lift:         > ", lang), round(attr(r$cl, "lift_thresh"), 2))
  })

  output$plot_metrics_dist <- renderPlotly({
    lang <- runtime_lang(shared_data)
    r <- m4_results()
    if (is.null(r) || !is.null(r$error)) return(plotly_empty() %>%
      layout(paper_bgcolor = "#ffffff", plot_bgcolor = "#ffffff"))
    d <- r$cl
    pal <- c("Core Formula Group"="#27ae60","Core Herb Group"="#e67e22",
             "Specific Compatibility Group"="#2980b9","Noise Combination"="#95a5a6","Other"="#6c757d")
    d$Type <- vapply(d$Type, runtime_tr, character(1), lang = lang)
    names(pal) <- vapply(names(pal), runtime_tr, character(1), lang = lang)

    p_support <- plot_ly(d, x = ~Type, y = ~Support, color = ~Type, colors = pal,
      type = "box", showlegend = FALSE) %>%
      layout(yaxis = list(title = runtime_tr("Support", lang)), xaxis = list(title = ""),
             paper_bgcolor = "#ffffff", plot_bgcolor = "#ffffff",
             margin = list(l = 50, r = 15, t = 10, b = 40))

    p_coverage <- plot_ly(d, x = ~Type, y = ~Coverage, color = ~Type, colors = pal,
      type = "box", showlegend = FALSE) %>%
      layout(yaxis = list(title = runtime_tr("Coverage", lang)), xaxis = list(title = ""),
             paper_bgcolor = "#ffffff", plot_bgcolor = "#ffffff",
             margin = list(l = 50, r = 15, t = 10, b = 40))

    p_specificity <- plot_ly(d, x = ~Type, y = ~Specificity, color = ~Type, colors = pal,
      type = "box", showlegend = FALSE) %>%
      layout(yaxis = list(title = runtime_tr("Association Specificity", lang)), xaxis = list(title = ""),
             paper_bgcolor = "#ffffff", plot_bgcolor = "#ffffff",
             margin = list(l = 50, r = 15, t = 10, b = 40))

    p_lift <- plot_ly(d, x = ~Type, y = ~Lift, color = ~Type, colors = pal,
      type = "box", showlegend = TRUE) %>%
      layout(yaxis = list(title = runtime_tr("Lift", lang)), xaxis = list(title = ""),
             paper_bgcolor = "#ffffff", plot_bgcolor = "#ffffff",
             margin = list(l = 50, r = 15, t = 10, b = 40))

    subplot(p_support, p_coverage, p_specificity, p_lift,
            nrows = 2, shareX = TRUE, titleY = TRUE,
            margin = 0.08, widths = c(0.5, 0.5)) %>%
      layout(title = runtime_tr("Metric Distributions by Type", lang),
             margin = list(t = 60),
             paper_bgcolor = "#ffffff", plot_bgcolor = "#ffffff") %>%
      config(displayModeBar = TRUE,
             modeBarButtonsToRemove = list("zoom2d","pan2d","select2d","lasso2d","zoomIn2d","zoomOut2d","autoScale2d","resetScale2d","hoverClosestCartesian","hoverCompareCartesian"),
             toImageButtonOptions = list(format = "png", filename = "per_type_metrics", height = 800, width = 1000))
  })

  output$dl_per_type_csv <- downloadHandler(
    filename = function() localized_filename("per_type_summary", "csv", runtime_lang(shared_data)),
    content = function(file) {
      r <- m4_results(); req(r, !is.null(r$types))
      write.csv(localized_columns(r$types, runtime_lang(shared_data)), file, row.names = FALSE, fileEncoding = "UTF-8")
    }
  )
}
