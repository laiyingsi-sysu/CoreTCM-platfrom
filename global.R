# ============================================================
# TCM Prescription Mining Platform — global.R
# Shared constants, package loading, default parameters
# ============================================================

# ===== PACKAGES (graceful loading with diagnostics) =====
pkg_status <- list()

load_pkg <- function(name) {
  ok <- require(name, character.only = TRUE, quietly = TRUE)
  pkg_status[[name]] <<- ok
  if (!ok) message("[WARN] Package not available: ", name)
  invisible(ok)
}

# Core (must work)
load_pkg("shiny")
load_pkg("shinydashboard")
load_pkg("shinythemes")
load_pkg("shinyjs")
load_pkg("DT")
load_pkg("dplyr")
load_pkg("tidyr")
load_pkg("stringr")
load_pkg("purrr")
load_pkg("readxl")

# Optional (app runs with degraded features if missing)
load_pkg("igraph")
load_pkg("visNetwork")
load_pkg("plotly")
load_pkg("wordcloud2")
load_pkg("arules")
load_pkg("arulesViz")
load_pkg("fmsb")
load_pkg("openxlsx")
load_pkg("jsonlite")
load_pkg("rmarkdown")
load_pkg("ggraph")
load_pkg("ggplot2")
load_pkg("pheatmap")

# ===== CONSTANTS =====

DEFAULT_PARAMS <- list(
  m0_include_standardized_dosage_export      = TRUE,
  m0_remove_prefix      = TRUE,
  m0_remove_duplicate   = TRUE,
  m0_extra_prefixes     = "制,焦,干,鲜,麸,燀,煨,后下,先煎",
  m2_freq_top_n         = 20,
  m2_min_freq           = 20,
  m2_min_freq_clust     = 20,
  m2_apriori_support    = 0.05,
  m2_apriori_confidence = 0.30,
  m2_apriori_lift       = 1.0,
  m2_max_combo_size     = 4,
  m2_max_rule_herbs     = 200,
  m2_cluster_method     = "Ward (hierarchical)",
  m2_cluster_k          = 5,
  m3_edge_weight        = "PMI",
  m3_hybrid_alpha       = 0.5,
  m3_hybrid_beta        = 0.5,
  m3_kcore_quantile     = 0.70,
  m3_min_freq           = 3,
  m3_min_nodes          = 3,
  m4_support_quantile      = 0.50,
  m4_coverage_quantile     = 0.75,
  m4_specificity_quantile = 0.50,
  m4_lift_quantile         = 0.75,
  m4_coverage_by_size      = TRUE,
  m6_herb_top_n         = 20,
  m6_pair_top_n         = 20,
  random_seed           = 42
)

RADAR_LEVELS <- list(
  sx = c("寒", "凉", "平", "温", "热"),
  ww = c("淡", "甘", "辛", "苦", "酸", "涩", "咸"),
  gj = c("肝", "心", "脾", "肺", "肾", "心包", "胆", "胃", "小肠", "大肠", "膀胱", "三焦")
)

DEFAULT_PREFIXES <- c(
  "炙", "生", "炒", "醋", "酒", "盐", "蜜",
  "麸炒", "清炒", "土炒", "炒炭", "煅", "蒸", "漂"
)

I18N <- list(
  zh = list(
    nav_data_cleaning = "数据预处理与标准化",
    nav_core_herbs = "中药方剂挖掘",
    nav_validation = "核心配伍特征分析",
    nav_fsa = "方-证关联分析",
    sub_data_management = "数据管理与中药名称标准化",
    sub_clinical_features = "临床特征提取与标准化",
    sub_descriptive = "描述性分析",
    sub_network = "基于社区结构的核心配伍挖掘",
    sub_rules = "关联规则挖掘",
    sub_cluster = "聚类分析",
    sub_compare = "多方法结果比较",
    sub_feature_association = "临床特征关联分析",
    sub_prescription_recommendation = "处方推荐",
    global_settings = "全局参数设置",
    export_json = "导出 JSON",
    apply = "应用",
    language = "语言",
    m0_title = "M0 — 数据管理与中药名称标准化",
    m0_include_standardized_dosage_export = "导出时包含保留剂量的标准化方剂字段",
    m0_remove_prefix = "去除炮制/修饰前缀",
    m0_remove_duplicate = "删除完全重复的方剂记录",
    m1_title = "M1 — 临床特征提取与标准化",
    m1_fuzzy_threshold = "模糊匹配阈值",
    m1_ai_assist = "AI 辅助识别",
    m2_title = "M2 — 中药方剂挖掘",
    m2_freq_top_n = "频次分析 Top-N",
    m2_apriori_support = "Apriori 最小支持度",
    m2_apriori_confidence = "Apriori 最小置信度",
    m2_apriori_lift = "Apriori 最小提升度",
    m2_max_combo_size = "最大组合药味数",
    m2_max_rule_herbs = "关联规则最大药物数",
    m2_cluster_method = "聚类方法",
    m2_cluster_k = "聚类数量 k",
    m3_title = "M3 — 基于社区结构的核心配伍挖掘",
    m3_edge_weight = "边权重类型",
    m3_hybrid_alpha = "混合权重 α（PMI 权重）",
    m3_hybrid_beta = "Hybrid Beta（频次权重）",
    m3_kcore_quantile = "核心收缩分位数（强度剪枝阈值）",
    m3_min_freq = "最小共现频次",
    m4_title = "M4 — 核心配伍特征分析",
    m4_support_quantile = "支持度高值分位数",
    m4_coverage_quantile = "覆盖度高值分位数",
    m4_specificity_quantile = "关联特异性高值分位数",
    m4_lift_quantile = "提升度高值分位数",
    m4_coverage_by_size = "按药味数分别计算覆盖度阈值",
    m6_title = "M6 — 多方法结果比较",
    m6_herb_top_n = "药物比较 Top-N",
    m6_pair_top_n = "药对比较 Top-N",
    global_updated = "全局参数已更新。点击各模块 Reset 以应用。"
  ),
  en = list(
    nav_data_cleaning = "Data Preprocessing and Standardization",
    nav_core_herbs = "TCM Herbal Formula Mining",
    nav_validation = "Core Combination Characterization",
    nav_fsa = "Formula-Syndrome Association Analysis",
    sub_data_management = "Data Management and Herbal Name Standardization",
    sub_clinical_features = "Clinical Feature Extraction and Standardization",
    sub_descriptive = "Descriptive Analysis",
    sub_network = "Community-based Core Herbal Combination Mining",
    sub_rules = "Association Rule Mining",
    sub_cluster = "Cluster Analysis",
    sub_compare = "Cross-method Comparison",
    sub_feature_association = "Clinical Feature Association Analysis",
    sub_prescription_recommendation = "Prescription Recommendation",
    global_settings = "Global Parameter Settings",
    export_json = "Export JSON",
    apply = "Apply",
    language = "Language",
    m0_title = "M0 — Data Management and Herbal Name Standardization",
    m0_include_standardized_dosage_export = "Include Standardized Formula with Dosage in Export",
    m0_remove_prefix = "Remove Prefixes",
    m0_remove_duplicate = "Remove Exact Duplicate Formula Records",
    m1_title = "M1 — Clinical Feature Extraction and Standardization",
    m1_fuzzy_threshold = "Fuzzy Match Threshold",
    m1_ai_assist = "AI-Assisted Recognition",
    m2_title = "M2 — TCM Herbal Formula Mining",
    m2_freq_top_n = "Frequency Top-N",
    m2_apriori_support = "Apriori Min Support",
    m2_apriori_confidence = "Apriori Min Confidence",
    m2_apriori_lift = "Apriori Min Lift",
    m2_max_combo_size = "Max Combo Size",
    m2_max_rule_herbs = "Max Herbs for Rules",
    m2_cluster_method = "Clustering Method",
    m2_cluster_k = "Number of Clusters k",
    m3_title = "M3 — Community-based Core Herbal Combination Mining",
    m3_edge_weight = "Edge Weight Type",
    m3_hybrid_alpha = "Hybrid Alpha (PMI weight)",
    m3_hybrid_beta = "Hybrid Beta (Frequency weight)",
    m3_kcore_quantile = "Core Shrink Quantile (strength pruning threshold)",
    m3_min_freq = "Min Co-occurrence Freq",
    m4_title = "M4 — Core Combination Characterization",
    m4_support_quantile = "Support High Quantile",
    m4_coverage_quantile = "Coverage High Quantile",
    m4_specificity_quantile = "Specificity High Quantile",
    m4_lift_quantile = "Lift High Quantile",
    m4_coverage_by_size = "Coverage by Combo Size",
    m6_title = "M6 — Cross-method Comparison",
    m6_herb_top_n = "Herb Comparison Top-N",
    m6_pair_top_n = "Pair Comparison Top-N",
    global_updated = "Global parameters updated. Click 'Reset' in each module to apply."
  )
)

tr <- function(key, lang = "zh") {
  if (is.null(lang) || !lang %in% names(I18N)) lang <- "zh"
  val <- I18N[[lang]][[key]]
  if (is.null(val)) key else val
}

options(encoding = "UTF-8")
