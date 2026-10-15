# Run from the application directory: Rscript tests/test_i18n.R
source("R/utils_i18n.R", encoding = "UTF-8")
check_i18n_keys()
stopifnot(ui_tr("Missing translation key", "zh") == "Missing translation key")
stopifnot(runtime_tr("Missing translation key", "zh") == "Missing translation key")
stopifnot(ui_tr("Network-Based Mining Workbench", "zh") == "基于社区结构的核心配伍挖掘")
stopifnot(ui_tr("Network-Based Mining Workbench", "en") == "Network-Based Mining Workbench")
stopifnot(runtime_tr("Network weights ready: %d herbs with structural scores.", "zh", 12) ==
  "网络权重已就绪：12 味药物具有结构得分。")
raw <- data.frame(Herb = c("Frequency", "当归"), stringsAsFactors = FALSE)
translated <- localized_columns(raw, "zh")
stopifnot(identical(translated[[1]], raw[[1]]), names(translated) == "中药")
stopifnot(!anyDuplicated(names(I18N_RUNTIME_ZH)), length(I18N_UI$zh) > 500)
cat("i18n dictionary, fallback, formatting and data-preservation tests passed\n")
