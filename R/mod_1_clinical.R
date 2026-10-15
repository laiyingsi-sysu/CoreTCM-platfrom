 # ============================================================
# mod_1_clinical.R — Module 1: Clinical Feature Extraction
# ============================================================

mod_1_sidebar <- function(id) {
  ns <- NS(id)
  tagList(
    div(class = "param-section",
      h5(icon("info-circle"), "Data Status"),
      uiOutput(ns("data_status"))
    ),
    div(class = "param-section",
      h5(icon("search"), "Extraction Settings"),
      h5("Detected Clinical Columns"),
      uiOutput(ns("detected_cols_ui")),
      hr(),
      h5("Syndrome Standardization (证型标准化)"),
      p(class = "text-muted", style = "font-size:11px;",
        "Upload a 2-column mapping table (original → standard) to normalize
         syndrome labels. CSV or Excel. First column = original, second = standard."),
      fileInput(ns("zx_map_file"), "Upload Syndrome Mapping Table (.csv / .xlsx)", accept = c(".csv",".xlsx")),
      uiOutput(ns("zx_map_status")),
      hr(),
      h5("Disease Location / Disease Nature Extraction (病位/病性提取)"),
      radioButtons(ns("bwbx_source"), "Extraction Method",
        choices = list(
          "Use data columns (if available)" = "data",
          "Lookup mapping table" = "table",
          "String matching (built-in dict)" = "match"
        ),
        selected = "data"
      ),
      conditionalPanel(
        condition = paste0("input['", ns("bwbx_source"), "'] == 'data'"),
        h6("Manual Column Override (optional)"),
        p(class = "text-muted", style = "font-size:11px;",
          "Auto-detect picks columns by name pattern. Override here if detection fails or wrong column picked."),
        uiOutput(ns("manual_bw_col_ui")),
        uiOutput(ns("manual_bx_col_ui"))
      ),
      p(class = "text-muted", style = "font-size:11px;",
        "Upload a 3-column table (标准证型, 病位, 病性) for direct lookup.
         Select 'Lookup mapping table' to use it; otherwise string matching with built-in dict."),
      fileInput(ns("zxbx_map_file"), "Upload BW/BX Mapping Table (.csv / .xlsx)", accept = c(".csv",".xlsx")),
      uiOutput(ns("zxbx_map_status"))
    ),
    div(class = "sidebar-action-bar",
      parameter_guide("m1"),
      actionButton(ns("run_m1"), "Run Feature Extraction", icon = icon("play"),
                   class = "btn-success btn-block")
    )
  )
}

mod_1_clinical_ui <- function(id) {
  ns <- NS(id)
  tagList(
    workflow_guide("m1"),
    fluidRow(
      box(title = "Clinical Feature Extraction & Standardization", status = "primary",
          solidHeader = TRUE, width = 12,
          p("Auto-detect clinical columns, standardize syndrome labels, and extract
             disease location (病位) / pathogenesis (病性) via longest-match dictionary.")
      )
    ),
    fluidRow(
      box(title = "Extraction Results", status = "success", solidHeader = TRUE, width = 12,
        uiOutput(ns("m1_not_run")),
        h5(icon("terminal"), "Run Log"),
        verbatimTextOutput(ns("run_log_text")),
        br(),
        downloadButton(ns("dl_processed"), "Download Processed Data (CSV)",
                       class = "btn-sm btn-success"),
        br(), hr(),
        tabBox(width = 12,
          tabPanel("Syndrome → BW/BX",
            h5("Syndrome → Disease Location & Disease Nature (证型拆分结果)"),
            DTOutput(ns("dt_zhengxing_map"))
          ),
          tabPanel("Syndrome Standardization",
            h5("Syndrome Label Standardization (证型标准化前后对比)"),
            p(class = "text-muted", style = "font-size:11px;",
              "Only shown when a syndrome mapping table was uploaded and used."),
            DTOutput(ns("dt_zx_std"))
          ),
          tabPanel("Clinical Feature Summary",
            h5("Clinical Feature Standardization Summary (临床特征标准化汇总)"),
            DTOutput(ns("dt_feature_summary"))
          ),
          tabPanel("Unmatched Clinical Features",
            h5("Unmatched Syndrome / Disease Location / Disease Nature (未匹配临床特征)"),
            DTOutput(ns("dt_unmatched_clinical"))
          )
        )
      )
    )
  )
}

mod_1_clinical_server <- function(input, output, session, shared_data) {
  datatable <- function(data, ...) localized_datatable(data, ..., lang = runtime_lang(shared_data))


  ns <- session$ns
  m1_results <- reactiveVal(NULL)
  user_zx_map    <- reactiveVal(NULL)
  user_zxbx_map  <- reactiveVal(NULL)
  run_log <- reactiveVal("")  # persistent run log (like M0)

  add_log <- function(msg) {
    old <- isolate(run_log())
    run_log(paste0(old, if (nchar(old) > 0) "\n" else "", msg))
  }

  output$data_status <- renderUI({
    if (is.null(shared_data$clean)) {
      div(class = "alert alert-warning", "No standardized data. Run M0 first.")
    } else {
      div(class = "alert alert-success",
          runtime_tr("Ready: %d prescriptions, %d columns.", runtime_lang(shared_data), nrow(shared_data$clean), ncol(shared_data$clean)))
    }
  })

  clinical_cols <- reactive({
    df <- shared_data$clean
    if (is.null(df)) return(NULL)
    cols <- names(df)
    detected <- list()
    if (any(grepl("证型|Syndrome|syndrome", cols, ignore.case = TRUE)))
      detected$zhengxing <- grep("证型|Syndrome|syndrome", cols, ignore.case = TRUE, value = TRUE)[1]
    if (any(grepl("病位|Disease.?Location|disease.?location", cols, ignore.case = TRUE)))
      detected$bingwei <- grep("病位|Disease.?Location|disease.?location", cols, ignore.case = TRUE, value = TRUE)[1]
    if (any(grepl("病性|病机|Pathogenesis|Disease.?Nature", cols, ignore.case = TRUE)))
      detected$bingxing <- grep("病性|病机|Pathogenesis|Disease.?Nature", cols, ignore.case = TRUE, value = TRUE)[1]
    if (any(grepl("症状|Symptom|symptom", cols, ignore.case = TRUE)))
      detected$zhengzhuang <- grep("症状|Symptom|symptom", cols, ignore.case = TRUE, value = TRUE)[1]
    if (any(grepl("治法|Treatment|treatment", cols, ignore.case = TRUE)))
      detected$zhifa <- grep("治法|Treatment|treatment", cols, ignore.case = TRUE, value = TRUE)[1]
    return(detected)
  })

  output$detected_cols_ui <- renderUI({
    cc <- clinical_cols()
    df <- shared_data$clean
    if (is.null(df) || length(cc) == 0)
      return(p("No clinical columns detected in the dataset."))
    tagList(
      if (!is.null(cc$zhengxing)) p(icon("check"), " Syndrome (证型): ", code(cc$zhengxing)) else p(icon("times"), " Syndrome (证型): not found"),
      if (!is.null(cc$bingwei))   p(icon("check"), " Disease Location (病位): ", code(cc$bingwei)) else p(icon("times"), " Disease Location (病位): not found — will extract from syndrome"),
      if (!is.null(cc$bingxing))  p(icon("check"), " Disease Nature (病性): ", code(cc$bingxing)) else p(icon("times"), " Disease Nature (病性): not found — will extract from syndrome"),
      if (!is.null(cc$zhengzhuang)) p(icon("check"), " Symptoms (症状): ", code(cc$zhengzhuang)) else p(),
      if (!is.null(cc$zhifa))     p(icon("check"), " Treatment (治法): ", code(cc$zhifa)) else p()
    )
  })

  # ===== Manual column selection (override auto-detect) =====
  all_cols <- reactive({
    df <- shared_data$clean
    if (is.null(df)) return(character(0))
    names(df)
  })

  output$manual_bw_col_ui <- renderUI({
    cols <- all_cols()
    if (length(cols) == 0) return(NULL)
    cc <- clinical_cols()
    sel <- if (!is.null(cc$bingwei)) cc$bingwei else ""
    sel <- if (!is.null(input$manual_bw_col) && input$manual_bw_col %in% cols) input$manual_bw_col else sel
    selectInput(ns("manual_bw_col"), "Disease Location Column (病位)",
                choices = c("(auto-detect)" = "", cols), selected = sel)
  })

  output$manual_bx_col_ui <- renderUI({
    cols <- all_cols()
    if (length(cols) == 0) return(NULL)
    cc <- clinical_cols()
    sel <- if (!is.null(cc$bingxing)) cc$bingxing else ""
    sel <- if (!is.null(input$manual_bx_col) && input$manual_bx_col %in% cols) input$manual_bx_col else sel
    selectInput(ns("manual_bx_col"), "Disease Nature Column (病性)",
                choices = c("(auto-detect)" = "", cols), selected = sel)
  })

  output$m1_not_run <- renderUI({
    if (is.null(m1_results()))
      div(class = "alert alert-info", "Click 'Run Feature Extraction' to analyze.")
    else NULL
  })

  output$run_log_text <- renderText({
    value <- run_log()
    if (!identical(runtime_lang(shared_data), "zh")) return(value)
    patterns <- c(
      "^=== M1 Clinical Feature Extraction ===$" = "=== M1 临床特征提取 ===",
      "^=== M1 Complete ===$" = "=== M1 处理完成 ===",
      "^\\[INFO\\] Data: ([0-9]+) rows x ([0-9]+) cols$" = "[信息] 数据：\\1 行，\\2 列",
      "^\\[INFO\\] Detected columns: syndrome = " = "[信息] 识别字段：证型 = ",
      "^--- Step 1: Syndrome Standardization ---$" = "--- 步骤 1：证型标准化 ---",
      "^--- Step 2: BW/BX Extraction ---$" = "--- 步骤 2：病位与病性提取 ---",
      "^\\[INFO\\] Syndrome column: (.*) \\| NA/empty: ([0-9]+) rows$" = "[信息] 证型列：\\1 | 缺失或空值：\\2 行",
      "^\\[INFO\\] No syndrome mapping table — skip standardization$" = "[信息] 未上传证型映射表，保留原始证型",
      "^\\[INFO\\] Extraction method: data$" = "[信息] 提取方式：使用已有数据列",
      "^\\[INFO\\] Extraction method: " = "[信息] 提取方式：",
      "^\\[OK\\] Used data columns: " = "[完成] 已读取数据列：",
      "^\\[DONE\\] Features: ([0-9]+) cols x ([0-9]+) rows$" = "[完成] 临床特征：\\1 列，\\2 行",
      "^\\[DONE\\] Disease_Location: +([0-9]+) non-empty$" = "[完成] 病位：\\1 条非空记录",
      "^\\[DONE\\] Pathogenesis: +([0-9]+) non-empty$" = "[完成] 病性：\\1 条非空记录"
    )
    lines <- strsplit(value, "\n", fixed = TRUE)[[1]]
    for (pattern in names(patterns)) lines <- sub(pattern, patterns[[pattern]], lines)
    paste(lines, collapse = "\n")
  })

  # ===== Download handler — processed data (original + extracted features) =====
  output$dl_processed <- downloadHandler(
    filename = function() localized_filename("clinical_processed", "csv", runtime_lang(shared_data)),
    content = function(file) {
      r <- m1_results()
      if (is.null(r) || is.null(r$features)) {
        showNotification("No data to download. Run M1 first.", type = "warning")
        return()
      }
      raw <- shared_data$clean
      feats <- r$features
      # Merge original data with extracted feature columns
      out <- cbind(raw, feats[, setdiff(names(feats), "row_id"), drop = FALSE])
      write.csv(localized_columns(out, runtime_lang(shared_data)), file, row.names = FALSE, fileEncoding = "UTF-8")
    }
  )

  observeEvent(input$zx_map_file, {
    req(input$zx_map_file)
    fp <- input$zx_map_file$datapath; fn <- input$zx_map_file$name
    df <- NULL
    if (grepl("\\.csv$", fn, ignore.case = TRUE)) {
      for (enc in c("UTF-8", "GB18030", "unknown")) {
        df <- tryCatch(read.csv(fp, stringsAsFactors = FALSE, fileEncoding = enc, check.names = FALSE),
                       error = function(e) NULL)
        if (!is.null(df) && is.data.frame(df) && ncol(df) >= 2 &&
            any(grepl("证型", names(df), ignore.case = TRUE))) {
          for (j in seq_len(ncol(df))) {
            if (is.character(df[[j]])) df[[j]] <- enc2utf8(df[[j]])
          }
          colnames(df) <- enc2utf8(colnames(df))
          break
        }
        df <- NULL
      }
    } else {
      df <- tryCatch(read_excel(fp), error = function(e) NULL)
    }
    if (!is.null(df) && ncol(df) >= 2) {
      names(df)[1:2] <- c("原始证型", "标准证型")
      user_zx_map(df)
      showNotification(paste("Syndrome mapping loaded:", nrow(df), "rows"), type = "message")
    } else {
      showNotification("Failed to read mapping file.", type = "error")
    }
  })

  output$zx_map_status <- renderUI({
    d <- user_zx_map()
    if (is.null(d)) return(p(class = "text-muted", "No mapping table uploaded."))
    p(class = "text-success", paste("Loaded:", nrow(d), "syndrome mappings"))
  })

  observeEvent(input$zxbx_map_file, {
    req(input$zxbx_map_file)
    fp <- input$zxbx_map_file$datapath; fn <- input$zxbx_map_file$name
    df <- NULL
    if (grepl("\\.csv$", fn, ignore.case = TRUE)) {
      # Try UTF-8 first (most common), then GB18030, then native
      for (enc in c("UTF-8", "GB18030", "unknown")) {
        df <- tryCatch(read.csv(fp, stringsAsFactors = FALSE, fileEncoding = enc, check.names = FALSE),
                       error = function(e) NULL)
        if (!is.null(df) && is.data.frame(df) && ncol(df) >= 3 &&
            any(grepl("病位|病性|标准证型|证型", names(df), ignore.case = TRUE))) {
          for (j in seq_len(ncol(df))) {
            if (is.character(df[[j]])) df[[j]] <- enc2utf8(df[[j]])
          }
          colnames(df) <- enc2utf8(colnames(df))
          break
        }
        df <- NULL
      }
    } else {
      df <- tryCatch(read_excel(fp), error = function(e) NULL)
    }
    if (!is.null(df) && ncol(df) >= 3) {
      zx_col <- grep("标准证型|证型", names(df), ignore.case = TRUE, value = TRUE)[1]
      bw_col <- grep("病位", names(df), ignore.case = TRUE, value = TRUE)[1]
      bx_col <- grep("病性", names(df), ignore.case = TRUE, value = TRUE)[1]
      if (is.na(zx_col)) zx_col <- names(df)[1]
      if (is.na(bw_col)) bw_col <- names(df)[2]
      if (is.na(bx_col)) bx_col <- names(df)[3]
      mapped <- data.frame(
        标准证型 = as.character(df[[zx_col]]),
        病位 = as.character(df[[bw_col]]),
        病性 = as.character(df[[bx_col]]),
        stringsAsFactors = FALSE
      )
      mapped <- mapped[!is.na(mapped$标准证型) & mapped$标准证型 != "" & nchar(mapped$标准证型) >= 2, ]
      user_zxbx_map(mapped)
      showNotification(paste("BW/BX mapping table loaded:", nrow(mapped), "entries"), type = "message")
    } else {
      showNotification("Failed to read mapping table or fewer than 3 columns.", type = "error")
    }
  })

  output$zxbx_map_status <- renderUI({
    d <- user_zxbx_map()
    if (is.null(d)) return(p(class = "text-muted", "No mapping table uploaded. String matching will be used if selected."))
    p(class = "text-success", paste("BW/BX mapping table loaded:", nrow(d), "entries"))
  })

  # ===== M1 implementation (called from run_m1 or force_continue) =====
  run_m1_impl <- function() {
    req(shared_data$clean)
    df <- shared_data$clean
    cc <- clinical_cols()
    run_log("")  # clear previous log

    add_log("=== M1 Clinical Feature Extraction ===")
    add_log(paste("[INFO] Data:", nrow(df), "rows x", ncol(df), "cols"))
    add_log(paste("[INFO] Detected columns: syndrome =", ifelse(is.null(cc$zhengxing), "NA", cc$zhengxing),
                  "| 病位 =", ifelse(is.null(cc$bingwei), "NA", cc$bingwei),
                  "| 病性 =", ifelse(is.null(cc$bingxing), "NA", cc$bingxing)))

    std_map <- user_zx_map()
    zxbx_map <- user_zxbx_map()
    zx_map <- data.frame()
    zx_std_table <- data.frame()
    unmatched_clinical <- data.frame(Feature = character(0), Value = character(0), stringsAsFactors = FALSE)
    features <- data.frame(row_id = seq_len(nrow(df)), stringsAsFactors = FALSE)

    if (!is.null(cc$zhengxing)) {
      zx_col <- cc$zhengxing
      zx_values <- as.character(df[[zx_col]])
      zx_values[is.na(zx_values)] <- ""
      zx_original_values <- zx_values
      n_na <- sum(zx_values == "")
      add_log(paste("--- Step 1: Syndrome Standardization ---"))
      add_log(paste("[INFO] Syndrome column:", zx_col, "| NA/empty:", n_na, "rows"))

      if (!is.null(std_map) && nrow(std_map) > 0) {
        add_log(paste("[INFO] Mapping table loaded:", nrow(std_map), "entries"))
        lookup <- setNames(std_map$标准证型, std_map$原始证型)
        used <- character(0)
        for (orig in names(lookup)) {
          hit_idx <- which(zx_values == orig)
          if (length(hit_idx) > 0) {
            zx_values[hit_idx] <- lookup[orig]
            used <- c(used, orig)
          }
        }
        if (length(used) > 0) {
          add_log(paste("[OK] Standardized:", length(used), "unique syndromes mapped"))
          zx_std_table <- data.frame(
            Original = used, Standardized = lookup[used], stringsAsFactors = FALSE)
        } else {
          add_log("[WARN] No syndromes matched the mapping table keys")
        }
        unmatched_zx <- unique(zx_original_values[!(zx_original_values %in% names(lookup)) & nzchar(zx_original_values)])
        if (length(unmatched_zx) > 0) {
          unmatched_clinical <- rbind(unmatched_clinical,
            data.frame(Feature = "证型", Value = unmatched_zx, stringsAsFactors = FALSE))
          add_log(paste("[WARN] Unmatched syndromes:", length(unmatched_zx), "unique"))
        } else {
          add_log("[OK] All syndromes matched in standardization mapping table")
        }
      } else {
        add_log("[INFO] No syndrome mapping table — skip standardization")
      }
      features$Syndrome_Standardized <- zx_values
    } else {
      zx_values <- rep("", nrow(df))
      zx_original_values <- zx_values
      add_log("[INFO] No syndrome column detected")
    }

    add_log(paste("--- Step 2: BW/BX Extraction ---"))
    src <- input$bwbx_source
    add_log(paste("[INFO] Extraction method:", src))

    manual_bw <- input$manual_bw_col
    manual_bx <- input$manual_bx_col
    if (!is.null(manual_bw) && nchar(manual_bw) > 0) cc$bingwei <- manual_bw
    if (!is.null(manual_bx) && nchar(manual_bx) > 0) cc$bingxing <- manual_bx

    if (src == "data" && (!is.null(cc$bingwei) || !is.null(cc$bingxing))) {
      if (!is.null(cc$bingwei)) {
        bw_values <- as.character(df[[cc$bingwei]])
        bw_values[is.na(bw_values)] <- ""
        features$Disease_Location <- bw_values
      }
      if (!is.null(cc$bingxing)) {
        bx_values <- as.character(df[[cc$bingxing]])
        bx_values[is.na(bx_values)] <- ""
        features$Pathogenesis <- bx_values
      }
      add_log(paste("[OK] Used data columns: 病位 =", cc$bingwei, "| 病性 =", cc$bingxing))
      if (is.null(cc$zhengxing)) add_log("[INFO] No syndrome column; extracted BW/BX directly from data columns")

    } else if (is.null(cc$zhengxing)) {
      add_log("[WARN] Lookup mapping table and string matching require a syndrome column; BW/BX extraction skipped")

    } else if (src == "table" && !is.null(zxbx_map) && nrow(zxbx_map) > 0) {
      lookup_bw <- setNames(zxbx_map$病位, zxbx_map$标准证型)
      lookup_bx <- setNames(zxbx_map$病性, zxbx_map$标准证型)
      bw_vec <- ifelse(nzchar(zx_values), lookup_bw[zx_values], "")
      bx_vec <- ifelse(nzchar(zx_values), lookup_bx[zx_values], "")
      bw_vec[is.na(bw_vec)] <- ""
      bx_vec[is.na(bx_vec)] <- ""
      features$Disease_Location <- bw_vec
      features$Pathogenesis     <- bx_vec
      miss_idx <- nzchar(zx_values) & is.na(lookup_bw[zx_values])
      miss_unique <- unique(zx_values[miss_idx])
      if (length(miss_unique) > 0) {
        unmatched_clinical <- rbind(unmatched_clinical,
          data.frame(Feature = "病位/病性映射", Value = miss_unique, stringsAsFactors = FALSE))
        add_log(paste("[WARN] Unmatched syndromes:", sum(miss_idx), "rows,",
                      length(miss_unique), "unique:"))
        for (u in head(miss_unique, 10)) add_log(paste("       -", u))
        if (length(miss_unique) > 10) add_log(paste("       ... and", length(miss_unique) - 10, "more"))
      } else {
        add_log("[OK] All standardized syndromes matched in mapping table")
      }

    } else {
      add_log("[INFO] Using string matching with built-in dictionary")
      dict <- BUILD_BINGWEI_DICT()
      zx_split <- str_split(zx_values, "[,，、；;]")
      all_zx <- unique(unlist(zx_split))
      all_zx <- str_trim(all_zx)
      all_zx <- all_zx[nchar(all_zx) >= 2 & all_zx != ""]
      if (length(all_zx) > 0) {
        zx_map <- extract_bingwei_bingxing(all_zx, dict = dict)
        zx_map <- zx_map[zx_map$病位 != "" | zx_map$病性 != "", ]
        add_log(paste("[OK] Dictionary matched:", nrow(zx_map), "syndromes"))
      }
      bingwei_vec <- character(nrow(df))
      bingxing_vec <- character(nrow(df))
      for (i in seq_len(nrow(df))) {
        zx_i <- str_trim(zx_split[[i]])
        zx_i <- zx_i[nchar(zx_i) >= 2]
        if (length(zx_i) > 0) {
          match_rows <- zx_map[zx_map$证型 %in% zx_i, ]
          if (nrow(match_rows) > 0) {
            bingwei_vec[i] <- paste(unique(unlist(str_split(match_rows$病位, "、"))), collapse = "、")
            bingxing_vec[i] <- paste(unique(unlist(str_split(match_rows$病性, "、"))), collapse = "、")
          }
        }
      }
      features$Disease_Location <- bingwei_vec
      features$Pathogenesis <- bingxing_vec
      unmatched_match <- unique(all_zx[!(all_zx %in% zx_map$证型)])
      if (length(unmatched_match) > 0) {
        unmatched_clinical <- rbind(unmatched_clinical,
          data.frame(Feature = "病位/病性内置词典", Value = unmatched_match, stringsAsFactors = FALSE))
        add_log(paste("[WARN] Built-in dictionary unmatched:", length(unmatched_match), "unique syndromes"))
      } else if (length(all_zx) > 0) {
        add_log("[OK] All syndromes matched in built-in disease location/pathogenesis dictionary")
      }
    }

    m1_results(list(
      zx_map      = zx_map,
      features    = features,
      n_zx        = length(unique(zx_values[nzchar(zx_values)])),
      cc          = cc,
      has_std     = !is.null(std_map) && nrow(std_map) > 0,
      n_std       = if (!is.null(std_map)) nrow(std_map) else 0,
      zx_std_table = zx_std_table,
      unmatched_clinical = unmatched_clinical,
      feature_summary = {
        syndrome_summary <- data.frame(
          Feature = "证型",
          Value = unique(zx_values[nzchar(zx_values)]),
          Standardized = unique(zx_values[nzchar(zx_values)]) %in% unname(if (!is.null(std_map)) setNames(std_map$标准证型, std_map$原始证型) else character(0)),
          stringsAsFactors = FALSE
        )
        bw_summary <- if ("Disease_Location" %in% names(features)) data.frame(
          Feature = "病位", Value = unique(features$Disease_Location[nzchar(features$Disease_Location)]),
          Standardized = input$bwbx_source != "data", stringsAsFactors = FALSE) else data.frame()
        bx_summary <- if ("Pathogenesis" %in% names(features)) data.frame(
          Feature = "病性", Value = unique(features$Pathogenesis[nzchar(features$Pathogenesis)]),
          Standardized = input$bwbx_source != "data", stringsAsFactors = FALSE) else data.frame()
        rbind(syndrome_summary, bw_summary, bx_summary)
      },
      bwbx_source = input$bwbx_source
    ))
    shared_data$module_results$m1 <- m1_results()
    add_log("=== M1 Complete ===")
    add_log(paste("[DONE] Features:", ncol(features), "cols x", nrow(features), "rows"))
    if ("Disease_Location" %in% names(features))
      add_log(paste("[DONE] Disease_Location: ", sum(nzchar(features$Disease_Location)), "non-empty"))
    if ("Pathogenesis" %in% names(features))
      add_log(paste("[DONE] Pathogenesis: ", sum(nzchar(features$Pathogenesis)), "non-empty"))
    showNotification("Clinical feature extraction complete. See Run Log for details.", type = "message")
  }

  # ===== M1 entry: NA check + modalDialog (A2 / ②C) =====
  observeEvent(input$run_m1, {
    req(shared_data$clean)
    df <- shared_data$clean
    cc <- clinical_cols()

    na_rows <- list()
    check_col <- function(col_name) {
      if (!is.null(col_name) && col_name %in% names(df)) {
        n_na <- sum(is.na(df[[col_name]]) | df[[col_name]] == "")
        if (n_na > 0) {
          na_rows[[length(na_rows) + 1]] <<- data.frame(
            Column = col_name,
            Missing_Count = n_na,
            Missing_Rate_pct = round(n_na / nrow(df) * 100, 2),
            stringsAsFactors = FALSE)
        }
      }
    }
    check_col(cc$zhengxing)
    check_col(cc$bingwei)
    check_col(cc$bingxing)

    if (length(na_rows) > 0) {
      stat_df <- do.call(rbind, na_rows)
      total_na <- sum(stat_df$Missing_Count)
      showModal(modalDialog(
        title = tagList(icon("exclamation-triangle"), "Missing Values Detected"),
        p(strong(paste("Detected", sum(stat_df$Missing_Count), "NA rows across",
                       nrow(stat_df), "clinical column(s)."))),
        p("NA rows will not participate in syndrome / disease-location / pathogenesis
          distribution stats in M5. M5 results may have selection bias risk."),
        p(class = "text-muted",
          "Suggested: handle missing values in M0 first (delete rows / upload inference mapping)."),
        DTOutput(ns("na_check_tbl")),
        footer = tagList(
          modalButton("Abort"),
          actionButton(ns("force_continue"), "Continue (acknowledge risk)",
                       icon = icon("exclamation"), class = "btn-warning")
        ),
        easyClose = FALSE
      ))
      output$na_check_tbl <- renderDT({
        datatable(stat_df, options = list(dom = "t", pageLength = -1),
                  rownames = FALSE, class = "compact")
      })
      return(invisible())
    }
    run_m1_impl()
  })

  observeEvent(input$force_continue, {
    removeModal()
    run_m1_impl()
  })

  output$dt_zhengxing_map <- renderDT({
    lang <- runtime_lang(shared_data)
    r <- m1_results()
    if (is.null(r)) return(datatable(localized_placeholder("Run extraction to see results.", lang), rownames = FALSE))
    if (nrow(r$zx_map) == 0) {
      msg <- if (identical(r$bwbx_source, "data")) "Clinical features were read directly from data columns; syndrome decomposition was not required." else "No syndrome (证型) column detected in the dataset."
      return(datatable(localized_placeholder(msg, lang), rownames = FALSE))
    }
    datatable(r$zx_map, options = list(pageLength = 15), rownames = FALSE)
  })

  output$dt_feature_summary <- renderDT({
    lang <- runtime_lang(shared_data)
    r <- m1_results()
    if (is.null(r) || nrow(r$feature_summary) == 0)
      return(datatable(localized_placeholder("Run extraction to see clinical feature summary.", lang), rownames = FALSE))
    datatable(r$feature_summary, options = list(pageLength = 15, scrollY = "400px"),
              rownames = FALSE, class = "compact") %>%
      formatStyle("Standardized",
        backgroundColor = styleEqual(c(TRUE, FALSE), c("#d4edda", "#f8d7da")))
  })

  output$dt_unmatched_clinical <- renderDT({
    lang <- runtime_lang(shared_data)
    r <- m1_results()
    if (is.null(r) || nrow(r$unmatched_clinical) == 0)
      return(datatable(localized_placeholder("All clinical features matched. No unmatched syndromes, disease locations, or pathogenesis values found.", lang), rownames = FALSE))
    datatable(r$unmatched_clinical, options = list(pageLength = 15),
              rownames = FALSE, class = "compact")
  })

  output$dt_zx_std <- renderDT({
    lang <- runtime_lang(shared_data)
    r <- m1_results()
    if (is.null(r) || !r$has_std || nrow(r$zx_std_table) == 0)
      return(datatable(localized_placeholder("No syndrome mapping table was uploaded, or no mappings were applied. Upload a mapping table to see standardization results.", lang), rownames = FALSE))
    datatable(r$zx_std_table, options = list(pageLength = 15),
              colnames = c("Original (原始证型)", "Standardized (标准证型)"), rownames = FALSE)
  })
}
