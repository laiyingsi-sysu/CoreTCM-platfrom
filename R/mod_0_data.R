# ============================================================
# mod_0_data.R — Module 0: Data Management
# ============================================================

mod_0_sidebar <- function(id) {
  ns <- NS(id)
  tagList(
    div(class = "param-section",
      h5(icon("upload"), "Step 1: Load Data"),
      data_requirements(),
      downloadButton(ns("dl_example_formula"), "Download formula-only example", class = "btn-sm btn-block"),
      downloadButton(ns("dl_example_clinical"), "Download formula-clinical example", class = "btn-sm btn-block"),
      downloadButton(ns("dl_mapping_templates"), "Download mapping templates", class = "btn-sm btn-block"),
      br(),
      fileInput(ns("csv_upload"), "Upload Prescription CSV", accept = ".csv"),
      actionButton(ns("btn_read"), "Read File", icon = icon("file-import"),
                   class = "btn-primary btn-sm"),
      br(), br(),
      textOutput(ns("file_info"))
    ),
    div(class = "param-section",
      h5(icon("columns"), "Step 2: Column Selection"),
      selectInput(ns("prescription_col"), "Prescription Column", choices = "(upload first)"),
      uiOutput(ns("status_box"))
    ),
    div(class = "param-section",
      h5(icon("filter"), "Step 3: Cleaning Parameters"),
      checkboxInput(ns("include_standardized_dosage_export"), "Include Standardized Formula with Dosage in Export", TRUE),
      p(class = "text-muted", style = "font-size:11px; margin-top:-8px;",
        "Herb names are standardized while the recorded dosage expressions are retained. This field is included for export only and is not used in downstream analyses."),
      checkboxInput(ns("remove_duplicate"), "Remove Exact Duplicate Formula Records", TRUE),
      hr(),
      checkboxInput(ns("remove_prefix"), "Remove Processing Prefixes", TRUE),
      div(style = "margin-top:8px; padding:10px 12px; background:#f8f9fa; border-left:3px solid #3c8dbc; border-radius:3px;",
        p(style = "font-weight:600; font-size:12px; color:#495057; margin:0 0 8px 0;",
          icon("tags"), " Prefix Settings"),
        textInput(ns("builtin_prefixes"), "Built-in Prefixes", width = "100%",
                  value = paste(DEFAULT_PREFIXES, collapse = ",")),
        p(style = "color:#6c757d; font-size:11px; margin:-8px 0 12px 0;",
          "Comma-separated, matched by length (longer first)"),
        textInput(ns("extra_prefixes"), "Additional Prefixes", width = "100%",
                  value = "制,焦,干,鲜,麸,燀,煨,后下,先煎"),
        p(style = "color:#6c757d; font-size:11px; margin:-8px 0 0 0;",
          "Extra processing prefixes in your data, comma-separated")
      )
    ),
    div(class = "param-section",
      h5(icon("book"), "Dictionaries"),
      checkboxInput(ns("use_builtin_dict"), "Use built-in dictionary (auto-loaded)", TRUE),
      uiOutput(ns("dict_status")),
      fluidRow(
        column(6, textInput(ns("alias_col"), "Alias column", "原始名称")),
        column(6, textInput(ns("std_col"),  "Standard column", "标准名称"))
      ),
      fileInput(ns("custom_dict"), "Upload Custom Herb-Name Mapping Table (.xlsx)", accept = ".xlsx")
    ),
    div(class = "sidebar-action-bar",
      parameter_guide("m0"),
      actionButton(ns("reset_params"), "Reset to Global Defaults", icon = icon("undo"), class = "btn-sm btn-block"),
      br(),
      actionButton(ns("run_cleaning"), "Run Cleaning Pipeline",
                   icon = icon("play"), class = "btn-success btn-block")
    )
  )
}

mod_0_data_ui <- function(id) {
  ns <- NS(id)
  tagList(
    workflow_guide("m0"),
    fluidRow(
      valueBoxOutput(ns("vb_presc"), width = 4),
      valueBoxOutput(ns("vb_herbs"), width = 4)
    ),
    br(),
    downloadButton(ns("dl_clean"), "Download Standardized CSV (Single Sheet)", class = "btn-primary"),
    downloadButton(ns("dl_xlsx"), "Download Full Results (Multi-Sheet Excel)", class = "btn-success"),
    br(), br(),
    tabBox(title = "Results", width = 12,
      tabPanel("Data Preview", DTOutput(ns("dt_preview"))),
      tabPanel("Missing Values", DTOutput(ns("dt_missing"))),
      tabPanel("Herb Summary", DTOutput(ns("dt_herb_summary"))),
      tabPanel("Unmatched Herbs", DTOutput(ns("dt_unmatched"))),
      tabPanel("Unparsed Formula Items", DTOutput(ns("dt_parsing_warnings"))),
      tabPanel("Before / After", DTOutput(ns("dt_compare"))),
      tabPanel("Log", verbatimTextOutput(ns("txt_log")))
    )
  )
}

mod_0_data_server <- function(input, output, session, shared_data) {
  datatable <- function(data, ...) localized_datatable(data, ..., lang = runtime_lang(shared_data))


  output$dl_example_formula <- downloadHandler(
    filename = function() localized_filename("example_formula_only", "csv", runtime_lang(shared_data)),
    content = function(file) file.copy(file.path("data", "example_formula_only.csv"), file, overwrite = TRUE)
  )
  output$dl_example_clinical <- downloadHandler(
    filename = function() localized_filename("example_formula_clinical", "csv", runtime_lang(shared_data)),
    content = function(file) file.copy(file.path("data", "example_formula_clinical.csv"), file, overwrite = TRUE)
  )
  output$dl_mapping_templates <- downloadHandler(
    filename = function() localized_filename("mapping_templates", "zip", runtime_lang(shared_data)),
    content = function(file) {
      files <- list.files("data", pattern = "^template_.*\\.csv$", full.names = TRUE)
      old <- getwd(); on.exit(setwd(old), add = TRUE)
      setwd("data")
      utils::zip(file, basename(files))
    }
  )

  raw_df   <- reactiveVal(NULL)
  log_text <- reactiveVal("")

  add_log <- function(msg) {
    old <- isolate(log_text())
    log_text(paste0(old, if (nchar(old) > 0) "\n" else "", msg))
  }

  # Auto-detect alias/standard column names from a dict data.frame
  auto_detect_dict_cols <- function(d) {
    if (is.null(d) || ncol(d) < 2) return(invisible())
    cn <- names(d)
    alias <- cn[grep("alias|原始|别名|原名|原方", cn, ignore.case = TRUE)[1]]
    std   <- cn[grep("standard|标准|正名|规范",   cn, ignore.case = TRUE)[1]]
    if (is.na(alias)) alias <- cn[1]
    if (is.na(std))   std   <- cn[2]
    updateTextInput(session, "alias_col", value = alias)
    updateTextInput(session, "std_col",   value = std)
  }

  observeEvent(input$reset_params, {
    p <- shared_data$active_params
    updateCheckboxInput(session, "include_standardized_dosage_export", value = p$m0_include_standardized_dosage_export)
    updateCheckboxInput(session, "remove_prefix", value = p$m0_remove_prefix)
    updateCheckboxInput(session, "remove_duplicate", value = p$m0_remove_duplicate)
    updateTextInput(session, "extra_prefixes", value = p$m0_extra_prefixes)
    showNotification(runtime_tr("Parameters reset to global defaults", runtime_lang(shared_data)), type = "message")
  })

  # ===== 0. Toggle prefix inputs via raw JS =====
  observe({
    frozen <- if (isTRUE(input$remove_prefix)) "false" else "true"
    shinyjs::runjs(sprintf(
      "var b=document.getElementById('%s'); if(b){b.disabled=%s; b.style.opacity=%s;}
       var e=document.getElementById('%s'); if(e){e.disabled=%s; e.style.opacity=%s;}",
      session$ns("builtin_prefixes"), frozen, if (frozen == "true") "0.5" else "1",
      session$ns("extra_prefixes"),   frozen, if (frozen == "true") "0.5" else "1"
    ))
  })

  # ===== 1. Auto-load dictionaries =====
  observe({
    if (isTRUE(input$use_builtin_dict)) {
      f1 <- file.path("data", "中药标准化字典.xlsx")
      if (file.exists(f1) && is.null(shared_data$dict)) {
        shared_data$dict <- tryCatch(read_excel(f1), error = function(e) NULL)
        if (!is.null(shared_data$dict)) {
          shared_data$dict_source <- "builtin"
          add_log("[OK] Standardization dict loaded (built-in)")
          auto_detect_dict_cols(shared_data$dict)
        }
      }
    }
    f2 <- file.path("data", "药物归经、五味、功效属性.xlsx")
    if (file.exists(f2) && is.null(shared_data$herb_attr)) {
      shared_data$herb_attr <- tryCatch(read_excel(f2), error = function(e) NULL)
      if (!is.null(shared_data$herb_attr)) add_log("[OK] Herb properties loaded")
    }
  })

  # ===== 1b. Built-in dict toggle =====
  observeEvent(input$use_builtin_dict, {
    src <- shared_data$dict_source
    if (isTRUE(input$use_builtin_dict)) {
      # Re-check: load built-in only if no custom dict active
      if (is.null(src) || !identical(src, "custom")) {
        f1 <- file.path("data", "中药标准化字典.xlsx")
        if (file.exists(f1)) {
          shared_data$dict <- tryCatch(read_excel(f1), error = function(e) NULL)
          if (!is.null(shared_data$dict)) {
            shared_data$dict_source <- "builtin"
            add_log("[OK] Built-in dict loaded")
            auto_detect_dict_cols(shared_data$dict)
          }
        }
      } else {
        add_log("[INFO] Custom dict in use — built-in toggle has no effect")
      }
    } else {
      # Uncheck: null out built-in dict; leave custom dict alone
      if (identical(src, "builtin")) {
        shared_data$dict <- NULL
        shared_data$dict_source <- NULL
        add_log("[INFO] Built-in dict disabled — standardize will skip alias mapping (basic dedup only)")
      } else if (identical(src, "custom")) {
        add_log("[INFO] Custom dict in use — toggle off has no effect on custom dict")
      }
    }
  }, ignoreNULL = FALSE)

  output$dict_status <- renderUI({
    d <- shared_data$dict
    src <- shared_data$dict_source
    tagList(
      if (!is.null(d)) {
        label <- switch(src, builtin = "Built-in dict", custom = "Custom dict", "Dict")
        span(class="text-success", runtime_tr("%s loaded: %d entries", runtime_lang(shared_data), runtime_tr(label, runtime_lang(shared_data)), nrow(d)))
      } else {
        span(class="text-warning", "Standardization dict: NOT LOADED — enable built-in or upload one below")
      }
    )
  })

  # ===== 2. Custom dict =====
  observeEvent(input$custom_dict, {
    req(input$custom_dict)
    d <- tryCatch(read_excel(input$custom_dict$datapath), error = function(e) NULL)
    if (!is.null(d)) {
      shared_data$dict <- d
      shared_data$dict_source <- "custom"
      auto_detect_dict_cols(d)
      showNotification("Custom dict loaded", type = "message")
    } else {
      showNotification("Failed to read custom dict file", type = "error")
    }
  })

  # ===== 3. File info display =====
  output$file_info <- renderText({
    f <- input$csv_upload
    if (is.null(f)) "No file selected."
    else paste("File:", f$name, "(", format(f$size, big.mark = ","), "bytes )")
  })

  output$status_box <- renderUI({
    df <- raw_df()
    if (is.null(df))
      return(div(class = "alert alert-info", icon("info-circle"), "No data loaded."))
    div(class = "alert alert-success", icon("check-circle"),
        paste("Loaded:", nrow(df), "rows x", ncol(df), "cols"))
  })

  # ===== 4. CSV reader — try GB18030, GBK, UTF-8, then native =====
  read_csv_gb <- function(filepath) {
    for (enc in c("UTF-8", "GB18030", "GBK", "unknown")) {
      df <- suppressWarnings(tryCatch(
        read.csv(filepath, stringsAsFactors = FALSE, fileEncoding = enc, check.names = FALSE),
        error = function(e) NULL
      ))
      if (!is.null(df) && is.data.frame(df) && nrow(df) > 0 && ncol(df) >= 1) {
        # Skip if header got mangled into a single column (encoding mismatch)
        if (anyNA(names(df)) || any(!nzchar(names(df)))) next
        for (j in seq_len(ncol(df))) {
          if (is.character(df[[j]])) df[[j]] <- enc2utf8(df[[j]])
        }
        colnames(df) <- enc2utf8(colnames(df))
        return(df)
      }
    }
    NULL
  }

  # ===== 5. Shared: process loaded dataframe =====
  process_df <- function(df, label) {
    add_log(paste("[OK]", label, ":", nrow(df), "rows x", ncol(df), "cols"))
    raw_df(df)
    shared_data$raw <- df
    cols <- names(df)
    add_log(paste("Columns:", paste(head(cols, 10), collapse = ", ")))
    rx <- grep("处方|中药|herb|prescription|formula|方剂", cols, ignore.case = TRUE, value = TRUE)
    rx_sel <- if (length(rx) > 0) rx[1] else cols[1]
    updateSelectInput(session, "prescription_col", choices = cols, selected = rx_sel)
    add_log(paste("[OK] Rx:", rx_sel))
    showNotification(paste("Loaded:", nrow(df), "rows,", ncol(df), "cols"), type = "message")
  }

  # ===== 6. Read Uploaded File =====
  observeEvent(input$btn_read, {
    f <- input$csv_upload
    if (is.null(f)) {
      showNotification("Upload a CSV file first", type = "warning")
      return()
    }
    add_log(paste("--- Reading:", f$name, "---"))
    df <- read_csv_gb(f$datapath)
    if (is.null(df)) {
      add_log("[ERROR] Could not parse CSV")
      showNotification("Could not read file. Try a UTF-8 encoded CSV.", type = "error")
      return()
    }
    process_df(df, f$name)
  })

  # ===== 7. Cleaning pipeline =====
  observeEvent(input$run_cleaning, {
    req(raw_df(), input$prescription_col)
    data <- raw_df()
    col  <- input$prescription_col
    add_log(paste("--- Cleaning:", col, "(", nrow(data), "rows ) ---"))
    tryCatch(withProgress(message = "Cleaning pipeline...", value = 0, {
      incProgress(0.3, detail = runtime_tr("Standardizing herb names and retaining dosage", runtime_lang(shared_data)))
      builtin <- strsplit(input$builtin_prefixes, "[,，、\\s]+", perl = TRUE)[[1]]
      extra <- strsplit(input$extra_prefixes, "[,，、\\s]+", perl = TRUE)[[1]]
      all_pfx <- unique(c(builtin, extra))
      dict <- shared_data$dict
      has_dict <- !is.null(dict) && nrow(dict) > 0
      res <- standardize_prescriptions(
        data, col,
        dict_data         = if (has_dict) dict else NULL,
        alias_col         = input$alias_col,
        standard_col      = input$std_col,
        remove_duplicate  = input$remove_duplicate,
        remove_processing = isTRUE(input$remove_prefix),
        processing_prefixes = all_pfx
      )
      data <- res$data; col <- res$new_col
      shared_data$unmatched <- res$unmatched
      shared_data$raw_formula_col <- res$raw_formula_col
      shared_data$dose_preserved_col <- res$dose_preserved_col
      shared_data$parsing_warnings <- res$parsing_warnings
      add_log("Dosage-preserving standardization completed")
      if (nrow(res$parsing_warnings)) add_log(paste("Unparsed Formula Items:", nrow(res$parsing_warnings)))
      if (!has_dict) {
        add_log("  [INFO] No dict loaded — basic dedup + sort only (alias mapping skipped).")
      } else if (length(res$unmatched) > 0) {
        add_log(paste("  Unmatched herbs (", length(res$unmatched), "): ",
                      paste(res$unmatched, collapse = ", "), sep = ""))
      } else {
        add_log("  All herbs matched in dictionary")
      }
      if (res$dup_removed > 0) {
        add_log(paste("  Duplicates removed:", res$dup_removed, "prescriptions"))
      } else {
        add_log("  No duplicate prescriptions found")
      }
    }), error = function(e) {
      showNotification(conditionMessage(e), type = 'error')
      add_log(paste('[ERROR]', conditionMessage(e)))
    })
    if (!exists('res', inherits = FALSE)) return()
    shared_data$clean     <- data
    shared_data$clean_col <- col
    herbs_vec <- unlist(str_split(data[[col]], "\\s+"))
    herbs_vec <- herbs_vec[nchar(herbs_vec) >= 2 & herbs_vec != "" & !is.na(herbs_vec)]
    herb_summary <- as.data.frame(sort(table(herbs_vec), decreasing = TRUE))
    names(herb_summary) <- c("Herb", "Frequency")
    herb_summary$UsageRate <- round(herb_summary$Frequency / nrow(data) * 100, 1)
    um_list <- shared_data$unmatched
    herb_summary$Standardized <- if (length(um_list) > 0) !(herb_summary$Herb %in% um_list) else TRUE
    shared_data$herb_summary <- herb_summary
    add_log(paste("  Unique herbs:", nrow(herb_summary)))
    add_log(paste("[DONE]", nrow(data), "prescriptions"))
    showNotification("Cleaning complete!", type = "message")
  })

  # ===== 8. Outputs =====
  output$txt_log <- renderText({
    l <- log_text()
    lang <- runtime_lang(shared_data)
    l <- gsub("Dosage-preserving standardization completed", runtime_tr("Dosage-preserving standardization completed", lang), l, fixed = TRUE)
    l <- gsub("Unparsed Formula Items", runtime_tr("Unparsed Formula Items", lang), l, fixed = TRUE)
    if (nchar(l) == 0) "Log will appear here..." else l
  })

  output$dt_preview <- renderDT({
    lang <- runtime_lang(shared_data)
    df <- raw_df()
    if (is.null(df)) return(datatable(localized_placeholder("Load data first", lang, "Tip"), rownames = FALSE))
    if (!is.null(shared_data$clean)) df <- shared_data$clean
    n <- min(nrow(df), 50)
    datatable(df[1:n, , drop = FALSE],
              options = list(scrollX = TRUE, pageLength = 10, autoWidth = FALSE,
                             columnDefs = list(list(targets = "_all", className = "dt-nowrap"))),
              rownames = FALSE)
  })

  output$dt_compare <- renderDT({
    lang <- runtime_lang(shared_data)
    df <- shared_data$clean; cl <- shared_data$clean_col; rx <- shared_data$raw_formula_col
    if (is.null(df) || is.null(cl))
      return(datatable(localized_placeholder("Run the cleaning pipeline to see comparison.", lang, "Tip"), rownames = FALSE))
    raw_col <- rx
    if (!raw_col %in% names(df)) raw_col <- names(df)[1]
    n <- min(20, nrow(df))
    preview <- df[seq_len(n), c(raw_col, shared_data$dose_preserved_col, cl), drop = FALSE]
    names(preview) <- vapply(c("Original Formula", "Standardized Formula with Dosage",
                               "Standardized Formula"), runtime_tr, character(1), lang = lang)
    datatable(preview, options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE)
  })

  output$dt_herb_summary <- renderDT({
    lang <- runtime_lang(shared_data)
    hs <- shared_data$herb_summary
    if (is.null(hs)) return(datatable(localized_placeholder("Run cleaning first", lang, "Tip"), rownames = FALSE))
    display_hs <- hs
    names(display_hs) <- vapply(names(display_hs), runtime_tr, character(1), lang = lang)
    datatable(display_hs, options = list(pageLength = 15, scrollY = "400px"),
              rownames = FALSE, class = "compact") %>%
      formatRound(runtime_tr("UsageRate", lang), 1) %>%
      formatStyle(runtime_tr("Standardized", lang),
        backgroundColor = styleEqual(c(TRUE, FALSE), c("#d4edda", "#f8d7da")))
  })

  output$dt_unmatched <- renderDT({
    lang <- runtime_lang(shared_data)
    um <- shared_data$unmatched
    if (is.null(um) || length(um) == 0)
      return(datatable(localized_placeholder("All herbs matched. No unstandardized herbs found.", lang), rownames = FALSE))
    datatable(data.frame(stats::setNames(list(um), runtime_tr("Herb", lang)), check.names = FALSE),
              options = list(pageLength = 15), rownames = FALSE, class = "compact")
  })

  output$dt_parsing_warnings <- renderDT({
    lang <- runtime_lang(shared_data)
    warnings <- shared_data$parsing_warnings
    if (is.null(warnings) || !nrow(warnings))
      return(datatable(localized_placeholder("No parsing warnings", lang), rownames = FALSE))
    warnings$Reason <- vapply(warnings$Reason, runtime_tr, character(1), lang = lang)
    datatable(localized_columns(warnings, lang), rownames = FALSE, options = list(scrollX = TRUE))
  })

  output$dt_missing <- renderDT({
    lang <- runtime_lang(shared_data)
    df <- shared_data$clean
    if (is.null(df)) return(datatable(localized_placeholder("Run cleaning pipeline first", lang, "Tip"), rownames = FALSE))
    miss <- data.frame(
      Column = names(df),
      Missing_Count = sapply(names(df), function(cn) sum(is.na(df[[cn]]) | df[[cn]] == "")),
      stringsAsFactors = FALSE, check.names = FALSE
    )
    miss$Missing_Rate_pct <- round(miss$Missing_Count / nrow(df) * 100, 2)
    names(miss) <- vapply(names(miss), runtime_tr, character(1), lang = lang)
    datatable(miss, options = list(pageLength = 15), rownames = FALSE, class = "compact") %>%
      formatStyle(runtime_tr("Missing_Rate_pct", lang),
        backgroundColor = styleInterval(c(0, 5, 20), c("#d4edda", "#fff3cd", "#f8d7da", "#f5c6cb")))
  })

  output$vb_presc <- renderValueBox({
    valueBox(if (is.null(shared_data$clean)) "—" else format(nrow(shared_data$clean), big.mark=","),
             "Prescriptions", icon = icon("file-prescription"), color = "blue")
  })
  output$vb_herbs <- renderValueBox({
    cl <- shared_data$clean_col; df <- shared_data$clean
    if (is.null(df) || is.null(cl)) return(valueBox("—", "Unique Herbs", icon = icon("leaf"), color = "green"))
    h <- unlist(str_split(df[[cl]], "\\s+"))
    h <- unique(h[nchar(h) >= 2 & h != "" & !is.na(h)])
    valueBox(format(length(h), big.mark=","), "Unique Herbs", icon = icon("leaf"), color = "green")
  })

  export_clean_data <- function() {
    req(shared_data$clean)
    formula_export_data(shared_data$clean, shared_data$dose_preserved_col,
                        input$include_standardized_dosage_export)
  }

  output$dl_clean <- downloadHandler(
    filename = function() localized_filename("standardized", "csv", runtime_lang(shared_data)),
    content = function(file) write.csv(export_clean_data(), file, fileEncoding = "GB18030", row.names = FALSE)
  )

  output$dl_xlsx <- downloadHandler(
    filename = function() localized_filename("cleaning_results", "xlsx", runtime_lang(shared_data)),
    content = function(file) {
      lang <- runtime_lang(shared_data)
      req(shared_data$clean)
      wb <- createWorkbook()
      standardized_sheet <- runtime_tr("Standardized Data", lang)
      addWorksheet(wb, standardized_sheet)
      writeData(wb, standardized_sheet, export_clean_data())
      if (!is.null(shared_data$herb_summary)) {
        herb_summary_sheet <- runtime_tr("Herb Summary", lang)
        addWorksheet(wb, herb_summary_sheet)
        writeData(wb, herb_summary_sheet, localized_columns(shared_data$herb_summary, lang))
      }
      if (length(shared_data$unmatched) > 0) {
        unmatched_sheet <- runtime_tr("Unmatched Herbs", lang)
        addWorksheet(wb, unmatched_sheet)
        writeData(wb, unmatched_sheet,
                  data.frame(stats::setNames(list(shared_data$unmatched), runtime_tr("Herb", lang)), check.names = FALSE))
      }
      if (!is.null(shared_data$parsing_warnings) && nrow(shared_data$parsing_warnings)) {
        warning_sheet <- runtime_tr("Unparsed Formula Items", lang)
        addWorksheet(wb, warning_sheet)
        writeData(wb, warning_sheet, localized_columns(shared_data$parsing_warnings, lang))
      }
      saveWorkbook(wb, file, overwrite = TRUE)
    }
  )
}
