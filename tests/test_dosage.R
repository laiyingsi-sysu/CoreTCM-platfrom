# Run from the project root: Rscript tests/test_dosage.R
Sys.setlocale("LC_CTYPE", "Chinese (Simplified)_China.utf8")
for (file in c("global.R", "app.R", list.files("R", pattern = "[.]R$", full.names = TRUE)))
  parse(file, encoding = "UTF-8")
source("R/utils_data_cleaning.R", encoding = "UTF-8")
source("R/utils_i18n.R", encoding = "UTF-8")
check_i18n_keys()
check <- function(ok) stopifnot(isTRUE(ok))
dict <- data.frame(alias = c("黄芪", "白术", "茯苓", "北芪"), standard_name = c("黄芪", "白术", "茯苓", "黄芪"))
run <- function(x, ...) standardize_prescriptions(data.frame(formula = x), "formula", ...)
x <- "生黄芪30g、白术 10克；茯苓（15g）"
r <- run(x, dict_data = dict, remove_processing = TRUE, processing_prefixes = "生")
check(identical(r$data$formula, x))
check(identical(r$data$formula_std_with_dosage, "黄芪30g; 白术10克; 茯苓15g"))
check(setequal(strsplit(r$data$formula_std, " ")[[1]], c("黄芪", "白术", "茯苓")))
doses <- c("15g", "15 g", "15克", "0.5g", "0.5 g", "10-15g", "10–15g", "10~15g", "各10g", "适量", "少许",
           paste0("5", c("mg", "g", "G", "克", "两", "钱", "ml", "mL", "ML", "片", "袋", "包", "丸", "瓶", "条")))
for (dose in doses) {
  a <- run(paste0("北芪 ", dose), dict_data = dict)
  check(identical(a$data$formula_std_with_dosage, paste0("黄芪", dose)))
  check(identical(a$data$formula_std, "黄芪"))
}
check(run("黄芪（15g）")$data$formula_std_with_dosage == "黄芪15g")
check(run("黄芪( 15 g )")$data$formula_std_with_dosage == "黄芪15 g")
check(run("黄芪、白术10g")$data$formula_std_with_dosage == "黄芪; 白术10g")
check(run("黄芪15g\n白术10g  茯苓\t甘草")$data$formula_std_with_dosage == "黄芪15g; 白术10g; 茯苓; 甘草")
check(run("黄芪15g;黄芪30g")$data$formula_std_with_dosage == "黄芪15g; 黄芪30g")
check(run("黄芪15g;黄芪30g")$data$formula_std == "黄芪")
check(run("生黄芪15g", remove_processing = FALSE)$data$formula_std_with_dosage == "生黄芪15g")
a <- run("未知药0.5 g", dict_data = dict)
check(identical(a$unmatched, "未知药")); check(a$data$formula_std_with_dosage == "未知药0.5 g")
a <- run(c("黄芪15g; 白术10g", "黄芪30g; 白术10g", "生黄芪15g、白术10g"),
         remove_processing = TRUE, processing_prefixes = "生")
check(nrow(a$data) == 2 && a$dup_removed == 1)
check(nrow(run(c("黄芪 白术", "白术 黄芪"))$data) == 2)
check(nrow(run(c("黄芪15g", "黄芪15g"), remove_duplicate = FALSE)$data) == 2)
a <- run(c("黄芪15xyz;白术10g", "黄芪15xyz;白术10g", NA, ""))
check(nrow(a$parsing_warnings) == 2 && nrow(a$data) == 4)
check(a$data$formula_std[1] == "白术")
check(grepl("黄芪15xyz", a$data$formula_std_with_dosage[1], fixed = TRUE))
check(nrow(run("黄芪十五克")$parsing_warnings) == 1)
check(nrow(run(character())$data) == 0)
bad <- try(standardize_prescriptions(data.frame(formula = "黄芪", formula_std = "source"), "formula"), silent = TRUE)
check(inherits(bad, "try-error"))
for (include in c(TRUE, FALSE)) {
  exported <- formula_export_data(r$data, r$dose_preserved_col, include)
  check(all(c("formula", "formula_std") %in% names(exported)))
  check(("formula_std_with_dosage" %in% names(exported)) == include)
  check(identical(exported$formula_std, r$data$formula_std))
  csv <- tempfile(fileext = ".csv"); xlsx <- tempfile(fileext = ".xlsx")
  write.csv(exported, csv, row.names = FALSE, fileEncoding = "GB18030")
  check(identical(names(read.csv(csv, fileEncoding = "GB18030")), names(exported)))
  openxlsx::write.xlsx(list(Data = exported, Summary = data.frame(Herb = "黄芪")), xlsx)
  check(identical(names(openxlsx::read.xlsx(xlsx)), names(exported)))
  unlink(c(csv, xlsx))
}
check(identical(migrate_formula_params(list(m0_remove_dosage = TRUE)), list(m0_include_standardized_dosage_export = FALSE)))
check(isTRUE(migrate_formula_params(list(m0_remove_dosage = FALSE))$m0_include_standardized_dosage_export))
check(isTRUE(migrate_formula_params(list(m0_remove_dosage = TRUE, m0_include_standardized_dosage_export = TRUE))$m0_include_standardized_dosage_export))
for (key in c("Original Formula", "Standardized Formula with Dosage", "Standardized Formula",
              "Include Standardized Formula with Dosage in Export", "Unparsed Formula Items")) {
  check(runtime_tr(key, "zh") != key); check(runtime_tr(key, "en") == key)
  check(ui_tr(key, "zh") != key)
}
cat("PASS: syntax, parsing, mapping, dosage, deduplication, review, exports, migration and translations\n")

library(shinydashboard); library(readxl); library(shiny); library(stringr); library(DT); library(dplyr); library(openxlsx)
source("R/mod_0_data.R", encoding = "UTF-8")
shared <- reactiveValues(lang = "zh", dict = dict, dict_source = "custom", active_params = list(), clean = NULL)
shiny::testServer(function(input, output, session) { shared_data <- shared; eval(body(mod_0_data_server)) }, {
  session$setInputs(use_builtin_dict = FALSE, prescription_col = "formula", remove_prefix = TRUE,
                    builtin_prefixes = "生", extra_prefixes = "", alias_col = "alias", std_col = "standard_name",
                    remove_duplicate = TRUE, include_standardized_dosage_export = TRUE)
  raw_df(data.frame(formula = c("生黄芪15g、白术10g", "黄芪30g;白术10g"), clinical = c("A", "B")))
  session$setInputs(run_cleaning = 1)
  check(shared_data$clean_col == "formula_std")
  check(shared_data$dose_preserved_col == "formula_std_with_dosage")
  check(shared_data$raw_formula_col == "formula")
  check(nrow(shared_data$clean) == 2)
  before <- shared_data$clean
  check("formula_std_with_dosage" %in% names(export_clean_data()))
  session$setInputs(include_standardized_dosage_export = FALSE)
  check(!"formula_std_with_dosage" %in% names(export_clean_data()))
  check("formula" %in% names(export_clean_data()))
  check(identical(before, shared_data$clean))
  check(!is.null(output$dt_compare))
})
cat("PASS: Shiny M0 pipeline, shared state, preview and live export toggle\n")

# Verify real download handlers, settings import/reset/export and UI generation.
source("global.R", encoding = "UTF-8")
source("R/mod_global_settings.R", encoding = "UTF-8")
shared <- reactiveValues(lang = "zh", dict = dict, dict_source = "custom", active_params = DEFAULT_PARAMS)
shiny::testServer(function(input, output, session) { shared_data <- shared; eval(body(mod_0_data_server)) }, {
  session$setInputs(use_builtin_dict = FALSE, prescription_col = "formula", remove_prefix = TRUE,
    builtin_prefixes = "生", extra_prefixes = "", alias_col = "alias", std_col = "standard_name",
    remove_duplicate = TRUE, include_standardized_dosage_export = TRUE)
  raw_df(data.frame(formula = c("生黄芪15g、白术10g", "黄芪30g;白术10g")))
  session$setInputs(run_cleaning = 1)
  for (include in c(TRUE, FALSE)) {
    session$setInputs(include_standardized_dosage_export = include)
    csv_data <- read.csv(output$dl_clean, fileEncoding = "GB18030", check.names = FALSE)
    excel_data <- openxlsx::read.xlsx(output$dl_xlsx, sheet = 1)
    check(("formula_std_with_dosage" %in% names(csv_data)) == include)
    check(identical(names(csv_data), names(excel_data)))
    check(identical(csv_data$formula, shared_data$clean$formula))
  }
})
shared <- reactiveValues(lang = "zh", active_params = DEFAULT_PARAMS)
shiny::testServer(function(input, output, session) { shared_data <- shared; eval(body(mod_global_settings_server)) }, {
  json <- tempfile(fileext = ".json")
  jsonlite::write_json(list(m0_remove_dosage = TRUE), json, auto_unbox = TRUE)
  session$setInputs(import_json = list(datapath = json, name = "legacy.json", size = file.size(json)))
  check(identical(shared_data$active_params$m0_include_standardized_dosage_export, FALSE))
  exported <- jsonlite::read_json(output$export_json, simplifyVector = TRUE)
  check(is.null(exported$m0_remove_dosage))
  check(identical(exported$m0_include_standardized_dosage_export, FALSE))
  session$setInputs(reset_global = 1)
  check(identical(shared_data$active_params, DEFAULT_PARAMS))
  values <- setNames(DEFAULT_PARAMS, paste0("g_", names(DEFAULT_PARAMS)))
  values$g_m0_include_standardized_dosage_export <- FALSE
  do.call(session$setInputs, values)
  session$setInputs(apply_global = 1)
  check(identical(shared_data$active_params$m0_include_standardized_dosage_export, FALSE))
  unlink(json)
})
for (lang in c("zh", "en")) {
  html <- as.character(mod_global_settings_modal("settings", lang, DEFAULT_PARAMS))
  check(grepl("g_m0_include_standardized_dosage_export", html, fixed = TRUE))
  check(!grepl("g_m0_remove_dosage", html, fixed = TRUE))
}

cat("PASS: actual CSV/Excel downloads; global import, legacy migration, reset, save, JSON export and bilingual settings\n")
