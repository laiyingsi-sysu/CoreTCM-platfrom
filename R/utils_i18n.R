# ============================================================
# utils_i18n.R — CoreTCM UI text, workflow and parameter guidance
# ============================================================

I18N_UI <- list(
  # Canonical keys are current English UI strings. zh holds Chinese UI translations.
  zh = c(
    "This module characterizes core herbal combinations identified by the M3 network-mining module using four metrics: Support, Coverage, Association Specificity, and Lift. Based on dynamic quantile thresholds, the combinations are classified into five categories: Core Formula Group, Core Herb Group, Specific Compatibility Group, Noise Combination, and Other. These categories describe their co-occurrence and association profiles within the current dataset." = "本模块基于支持度、覆盖度、关联特异性和提升度四项指标，对模块3网络挖掘识别的核心配伍进行特征分析。依据动态分位数阈值，核心配伍分为核心配伍组、核心药群、专属性配伍组、噪声组合和其他五类，用于描述其在当前数据集中的共现与关联特征。",
    "Include Standardized Formula with Dosage in Export" = "导出时包含保留剂量的标准化方剂字段",
    "Herb names are standardized while the recorded dosage expressions are retained. This field is included for export only and is not used in downstream analyses." = "对中药名称进行标准化并保留原始剂量表达。该字段仅用于导出，不参与任何后续分析。",
    "Remove Exact Duplicate Formula Records" = "删除完全重复的方剂记录",
    "Upload Custom Herb-Name Mapping Table (.xlsx)" = "上传自定义中药名称映射表（.xlsx）",
    "Original Formula" = "原始处方", "Standardized Formula with Dosage" = "保留剂量的标准化方剂", "Standardized Formula" = "无剂量标准化方剂",
    "Dosage-preserving standardization completed" = "保留剂量的标准化处理已完成",
    "Unparsed Formula Items" = "未完整解析的方剂条目",
    "No parsing warnings" = "没有需要复核的解析条目", "Review formula item" = "请复核此处方条目",
    "Row" = "原始行号", "Item" = "原始条目", "Reason" = "原因",
    "Import Parameters (.json)" = "导入参数（.json）", "Reset Global Defaults" = "恢复全局初始默认值",
    "Parameters imported" = "参数已导入", "Invalid parameter JSON" = "参数 JSON 无效",
    "Standardizing herb names and retaining dosage" = "正在标准化药名并保留剂量",
    "Remove Processing Prefixes" = "去除炮制前缀",
    "Purpose: clean and standardize formulas. Original formulas are retained. The export option adds standardized formulas with recorded dosage; analysis always uses dosage-free names. Exact duplicates compare standardized names, dosage and original order. Review unparsed items before analysis." = "清洗并标准化处方，始终保留原始处方。导出选项控制是否增加保留原始剂量的标准化字段；分析始终使用无剂量药名。完全重复记录按标准化药名、剂量和原始顺序判断。分析前请复核未完整解析的条目。",
    "Output: original formulas, standardized formulas with dosage, dosage-free analysis formulas, herb frequency and parsing review." = "输出：原始处方、保留剂量的标准化方剂、无剂量分析方剂、药物频次及解析复核清单。",

    "Data Status" = "数据状态", "Parameters" = "参数", "Results" = "结果",
    "Step 1: Load Data" = "步骤 1：加载数据", "Step 2: Column Selection" = "步骤 2：选择列", "Step 3: Cleaning Parameters" = "步骤 3：清洗参数",
    "Upload Prescription CSV" = "上传处方 CSV", "Read File" = "读取文件", "Prescription Column" = "处方列", "Dictionaries" = "字典",
    "Data requirements" = "数据要求", "Download formula-only example" = "下载仅含方剂示例", "Download formula-clinical example" = "下载方剂与临床字段示例", "Download mapping templates" = "下载映射模板",
    "Reset to Global Defaults" = "重置为全局默认值", "Reset to Global" = "重置为全局默认值", "Run Full Pipeline" = "运行完整流程", "Run Cleaning Pipeline" = "运行清洗流程",
    "Run Feature Extraction" = "运行特征提取", "Run Frequency Analysis" = "运行频次分析", "Run Association Rules" = "运行关联规则", "Run Clustering" = "运行聚类分析", "Run Evaluation" = "运行特征分析", "Run Association Analysis" = "运行关联分析", "Run Comparison" = "运行比较", "Generate Recommendation" = "生成候选药物排序", "Save as Global Defaults" = "保存为全局默认值",
    "Workflow guide" = "流程指引", "Parameter guidance" = "参数说明", "Frequency Analysis" = "频次分析", "Association Rules" = "关联规则", "Herb Clustering" = "中药聚类", "Network Construction" = "网络构建", "Community Detection" = "社区发现", "Herb Property Dictionary" = "中药属性字典", "Comparison Center" = "多方法结果比较",
    "Core Formula" = "核心配伍", "Core Formulas" = "核心配伍", "Core Formula Comparison" = "核心配伍比较", "Combination Evaluation" = "核心配伍特征分析", "Formula-Syndrome Association" = "方-证关联分析", "Pathogenesis" = "病性", "Disease Location" = "病位", "Disease Nature" = "病性",
    "Download Table (CSV)" = "下载表格（CSV）", "Download Rules Table (CSV)" = "下载规则表（CSV）", "Download Cluster Table (CSV)" = "下载聚类表（CSV）", "Download Core Formulas (CSV)" = "下载核心配伍（CSV）", "Download Recommendation (CSV)" = "下载候选药物排序（CSV）",
    "Download Standardized CSV (Single Sheet)" = "下载标准化数据（CSV）", "Download Full Results (Multi-Sheet Excel)" = "下载完整结果（多工作表 Excel）", "Data Preview" = "数据预览", "Missing Values" = "缺失值", "Herb Summary" = "药物汇总", "Unmatched Herbs" = "未匹配药物", "Before / After" = "清洗前后对比", "Log" = "日志",
    "Extraction Settings" = "提取设置", "Detected Clinical Columns" = "识别到的临床列", "Extraction Results" = "提取结果", "Run Log" = "运行日志", "Download Processed Data (CSV)" = "下载处理后数据（CSV）",
    "Association Rules (Apriori)" = "关联规则（Apriori）", "Min Herb Frequency" = "最小药物频次", "Min Support" = "最小支持度", "Min Confidence" = "最小置信度", "Min Lift" = "最小 Lift", "Max Combo Size" = "最大组合药味数", "Max Herbs for Rules" = "关联规则最大候选药数",
    "Edge Weight Type" = "边权重类型", "Min Co-occurrence" = "最小共现频次", "Core Shrink Quantile (strength pruning threshold)" = "核心收缩分位数（强度剪枝阈值）", "Threshold Quantiles" = "阈值分位数", "Support High Quantile" = "Support 高值分位数", "Coverage High Quantile" = "Coverage 高值分位数", "Association Specificity High Quantile" = "关联特异性高值分位数", "Lift High Quantile" = "Lift 高值分位数",
    "Mapping Settings" = "关联设置", "Available Data" = "可用数据", "Recommendation Results" = "候选药物排序结果", "Model Performance (AUC per Herb)" = "模型表现（每味药 AUC）"
  )
)

ui_tr <- function(key, lang = "zh", ...) {
  if (identical(lang, "en")) return(key)
  value <- unname(I18N_UI$zh[key])
  if (length(value) == 0L || is.na(value)) key else value
}

format_ui_tr <- function(key, lang = "zh", ...) sprintf(ui_tr(key, lang), ...)

# Static Shiny controls retain state during language changes. Client-side text replacement
# changes labels only; input IDs, values, uploads, selections, and results stay intact.
i18n_client_script <- function() {
  tagList(
    tags$script(HTML(paste0("window.coreTCMTranslations = ",
      jsonlite::toJSON(as.list(I18N_UI$zh), auto_unbox = TRUE), ";"))),
    tags$script(src = "i18n.js")
  )
}

workflow_guide <- function(stage) {
  content <- list(
    m0 = list("1. Data management and herbal name standardization", "Upload CSV; select formula column; clean and map names.", "Needs one non-empty formula column; one record per formula recommended.", "Output: original formulas, standardized formulas with dosage, dosage-free analysis formulas, herb frequency and parsing review.", "Next: M2 (optional) or M3; use M1 first when clinical fields exist."),
    m1 = list("2. Clinical feature extraction and standardization (optional)", "Confirm detected columns or override; upload mappings if needed; run extraction.", "Needs M0. Clinical fields are optional unless using M5.", "Output: standardized syndrome, disease location, disease nature, unmatched values.", "Next: M2/M3. Skip for formula-only data."),
    m2 = list("3. Conventional formula mining (optional/comparison)", "Run frequency, rules, and clustering independently.", "Needs M0 standardized formulas.", "Output: high-frequency herbs, local association rules, clusters.", "Next: M3; run M6 after M2 and M3."),
    m3 = list("4. Community-based core herbal combination mining", "Set edge weight, minimum co-occurrence, pruning quantile; run pipeline.", "Needs M0 standardized formulas.", "Output: co-occurrence network, Louvain communities, core herbal combinations.", "Next: M4; M5 if M1 exists; M6 if M2 exists."),
    m4 = list("5. Core herbal combination characterization", "Set empirical quantiles; run characterization.", "Needs M3 core herbal combinations.", "Output: metrics, thresholds, groups, charts.", "Next: revisit M3 for sensitivity; use M5 when clinical fields exist."),
    m5 = list("6. Formula-syndrome association and recommendation", "Run association first; train model then generate candidate herb ranking.", "Association needs M1 + M3. Formula-only data are not applicable, not an error.", "Output: descriptive associations and exploratory candidate rankings; not causal or clinical prescribing advice.", "Next: export parameters and results for research reporting."),
    m6 = list("Optional. Cross-method comparison", "Run comparison for available M2 and M3 results.", "Pairs need M2 rules + M3; structure/core comparisons need M2 clusters + M3.", "Output: pair, structure, and core-combination differences.", "Next: return to M2/M3 for interpretation or sensitivity checks.")
  )[[stage]]
  if (is.null(content)) return(tags$div())
  tags$details(class = "workflow-guide", open = "open",
    tags$summary(icon("route"), " Workflow guide"),
    tags$p(tags$b(content[[1]])),
    tags$ul(tags$li(tags$b("Action: "), content[[2]]), tags$li(tags$b("Input / prerequisite: "), content[[3]]), tags$li(content[[4]]), tags$li(content[[5]])))
}

parameter_guide <- function(module) {
  text <- switch(module,
    m0 = "Purpose: clean and standardize formulas. Original formulas are retained. The export option adds standardized formulas with recorded dosage; analysis always uses dosage-free names. Exact duplicates compare standardized names, dosage and original order. Review unparsed items before analysis.",
    m1 = "Purpose: standardize clinical fields. Auto detection uses column-name patterns; manually override wrong detection. Syndrome mapping: original → standard. Disease mapping: standard syndrome, disease location, disease nature. Source priority follows selected method; inspect unmatched values.",
    m2 = "Top-N only limits display/export. Support is formula proportion; confidence is conditional association; Lift > 1 exceeds independent expectation. Higher thresholds reduce rules. Frequency filtering stabilizes clusters. Ward k cuts dendrogram; choose K-means k using elbow plot, heatmap, and interpretability.",
    m3 = "PMI reduces broad high-frequency co-occurrence bias and is default. Frequency uses counts. Hybrid combines both: β is derived as 1 − α. Higher minimum co-occurrence sparsifies network. Higher pruning quantile produces smaller cores. Property dictionary changes attribute charts only.",
    m4 = "Thresholds are empirical quantiles within current combinations, not clinical cutoffs. Support = occurrence proportion; Coverage = prescription coverage; association specificity = concentration; Lift = observed versus independent expectation. Coverage by size reduces combination-size effect and falls back to global threshold for fewer than three combinations.",
    m5 = "Classifier-chain candidates need sufficient herb frequency; longer chains increase complexity. Covariates must be available at prediction time, never post-treatment. Network weights sum to 1; λ=γ=0 gives base ranking. Enhancement changes ranking only; no validated performance or clinical-prescribing claim.",
    m6 = "Top-N controls display only. NMI and ARI are structural-consistency references, not clinical-effectiveness measures.", "")
  tags$details(class = "parameter-guide", tags$summary(icon("info-circle"), " Parameter guidance"), tags$p(text))
}

data_requirements <- function() {
  tags$details(class = "data-requirements", open = "open",
    tags$summary(icon("clipboard-list"), " Data requirements"),
    tags$ul(tags$li("CSV: UTF-8 recommended; GB18030 is also supported."),
            tags$li("One row per prescription; formula field cannot be empty."),
            tags$li("Dosage is not used in analysis. Clinical fields are optional.")),
    tags$p("Downloads contain interface field names only. Raw herb and clinical values are never translated."))
}

check_i18n_keys <- function() {
  if (anyDuplicated(names(I18N_UI$zh))) stop("Duplicate UI translation keys")
  invisible(TRUE)
}

# Server-rendered text is created after the DOM translator runs, so translate it on
# the R side as well. Keys are English source strings; raw dataset values never pass
# through this helper.
I18N_RUNTIME_ZH <- c(
    "Include Standardized Formula with Dosage in Export" = "导出时包含保留剂量的标准化方剂字段",
    "Herb names are standardized while the recorded dosage expressions are retained. This field is included for export only and is not used in downstream analyses." = "对中药名称进行标准化并保留原始剂量表达。该字段仅用于导出，不参与任何后续分析。",
    "Remove Exact Duplicate Formula Records" = "删除完全重复的方剂记录",
    "Upload Custom Herb-Name Mapping Table (.xlsx)" = "上传自定义中药名称映射表（.xlsx）",
    "Original Formula" = "原始处方", "Standardized Formula with Dosage" = "保留剂量的标准化方剂", "Standardized Formula" = "无剂量标准化方剂",
    "Dosage-preserving standardization completed" = "保留剂量的标准化处理已完成",
    "Unparsed Formula Items" = "未完整解析的方剂条目",
    "No parsing warnings" = "没有需要复核的解析条目", "Review formula item" = "请复核此处方条目",
    "Row" = "原始行号", "Item" = "原始条目", "Reason" = "原因",
    "Import Parameters (.json)" = "导入参数（.json）", "Reset Global Defaults" = "恢复全局初始默认值",
    "Parameters imported" = "参数已导入", "Invalid parameter JSON" = "参数 JSON 无效",
    "Standardizing herb names and retaining dosage" = "正在标准化药名并保留剂量",
    "Remove Processing Prefixes" = "去除炮制前缀",
    "Purpose: clean and standardize formulas. Original formulas are retained. The export option adds standardized formulas with recorded dosage; analysis always uses dosage-free names. Exact duplicates compare standardized names, dosage and original order. Review unparsed items before analysis." = "清洗并标准化处方，始终保留原始处方。导出选项控制是否增加保留原始剂量的标准化字段；分析始终使用无剂量药名。完全重复记录按标准化药名、剂量和原始顺序判断。分析前请复核未完整解析的条目。",
    "Output: original formulas, standardized formulas with dosage, dosage-free analysis formulas, herb frequency and parsing review." = "输出：原始处方、保留剂量的标准化方剂、无剂量分析方剂、药物频次及解析复核清单。",

  "Tip" = "提示", "Note" = "说明", "Herb" = "中药", "Frequency" = "频次", "UsageRate" = "使用率", "Standardized" = "已标准化",
  "Raw" = "原始处方", "Cleaned" = "清洗后处方", "Column" = "列名", "Missing_Count" = "缺失数", "Missing_Rate_pct" = "缺失率（%）",
  "Load data first" = "请先加载数据", "Run cleaning first" = "请先运行清洗", "Run cleaning pipeline first" = "请先运行清洗流程",
  "Run the cleaning pipeline to see comparison." = "请运行清洗流程后查看对比。", "All herbs matched. No unstandardized herbs found." = "所有中药均已匹配，未发现未标准化中药。",
  "Run extraction to see results." = "请运行特征提取后查看结果。", "Run extraction to see clinical feature summary." = "请运行特征提取后查看临床特征汇总。",
  "All clinical features matched. No unmatched syndromes, disease locations, or pathogenesis values found." = "所有临床特征均已匹配，未发现未匹配证型、病位或病性。",
  "No syndrome (证型) column detected in the dataset." = "数据中未识别到证型列。", "No syndrome mapping table was uploaded, or no mappings were applied. Upload a mapping table to see standardization results." = "未上传证型映射表，或未应用映射。上传映射表后可查看标准化结果。",
  "Run analysis first." = "请先运行关联分析。", "Train the model to see performance." = "请先训练模型后查看性能。", "Generate recommendation to see results." = "请先生成候选药物排序。",
  "Run evaluation to see results." = "请先运行特征分析后查看结果。", "Run comparison" = "请先运行比较。", "No data" = "暂无数据", "No core formulas" = "暂无核心配伍", "No property dict" = "未加载属性字典", "Adjust k slider to see clusters." = "请调整 k 值滑块后查看聚类。",
  "Community" = "社区", "Core Formula" = "核心配伍", "Strength" = "强度",  "Parameters reset to global defaults" = "参数已重置为全局默认值", "Parameters reset" = "参数已重置", "Cleaning complete!" = "清洗完成！", "Frequency analysis complete" = "频次分析完成", "Clustering complete" = "聚类完成", "Comparison complete!" = "比较完成！",
  "No results to download. Run pipeline first." = "暂无可下载结果，请先运行流程。", "Select at least one community." = "请至少选择一个社区。",
  "Standardized Data" = "标准化数据", "Herb Summary" = "中药汇总", "Unmatched Herbs" = "未匹配中药",
  "standardized" = "标准化数据", "cleaning_results" = "清洗结果", "clinical_processed" = "临床特征处理结果", "frequency_analysis" = "频次分析", "association_rules" = "关联规则", "clustering_result" = "聚类结果", "ward_heatmap" = "Ward聚类热图", "kmeans_heatmap" = "K均值聚类热图", "core_formulas" = "核心配伍", "tcm_communities" = "中药社区结果", "per_type_summary" = "分型汇总", "recommendation" = "候选药物排序", "params" = "参数",
  "Prescriptions" = "处方数", "Unique Herbs" = "中药种类数", "Done." = "已完成。", "Not run yet." = "尚未运行。",
  "Frequency Analysis Results" = "频次分析结果", "Association Rules Results" = "关联规则结果", "Clustering Results" = "聚类结果",
  "Top %d Herbs by Frequency" = "频次最高的 %d 味中药", "Frequency" = "频次", "Support vs Confidence (Size = %d)" = "支持度与置信度（组合大小 = %d）", "No rules with %d herbs" = "没有包含 %d 味中药的规则",
  "Run Ward clustering to see dendrogram." = "请运行 Ward 聚类后查看树状图。", "K selection guide will appear after clustering." = "运行聚类后将显示 K 值选择指引。", "Top Herbs in %s" = "%s 中频次最高的中药",
  "Nodes: %d  |  Edges: %d  |  Density: %.4f  |  Avg Degree: %.1f" = "节点：%d  |  边：%d  |  密度：%.4f  |  平均度：%.1f", "Communities: " = "社区数：", "Modularity: " = "模块度：",
  "Click Run Full Pipeline to start." = "请点击“运行完整流程”开始。", "Error:" = "错误：", "No communities found." = "未发现社区。", "Community Herb List" = "社区中药列表", " herbs" = " 味中药", "Core Formula: " = "核心配伍：", "Medicinal properties" = "药性", "Medicinal flavors" = "五味", "Meridian tropism" = "归经", "Strength: " = "强度：",
  "Run evaluation to see results." = "请运行特征分析后查看结果。", "Support vs Coverage (color by Type)" = "支持度与覆盖度（按类型着色）", "Support" = "支持度", "Coverage" = "覆盖度", "Association Specificity" = "关联特异性", "Metric Distributions by Type" = "各类型指标分布",
  "Run analysis first." = "请先运行关联分析。", "No disease location data" = "暂无病位数据", "No syndrome data" = "暂无证型数据", "No pathogenesis data" = "暂无病性数据", "Insufficient data" = "数据不足", "Community x Disease Location (%)" = "社区与病位（%）", "Community x Standardized Syndrome (%)" = "社区与标准证型（%）", "Community x Pathogenesis (%)" = "社区与病性（%）", "Generate recommendation to see chart." = "请生成候选药物排序后查看图表。", "Top Recommended Herbs (by Enhanced Score)" = "推荐中药（按增强评分）",
  "Cannot compare at pair level." = "无法进行药对层面比较。", "Insufficient shared pairs for scatter." = "共享药对不足，无法绘制散点图。", "Herb-level overlap (no shared pairs at pair level)" = "中药层面重叠（药对层面无共享药对）", "Count" = "数量", "Cluster -> Community Flow" = "聚类到社区的流向",
  "example_formula_only" = "仅方剂示例", "example_formula_clinical" = "方剂临床字段示例", "mapping_templates" = "映射模板", "herb_table" = "中药表",
  "Disease_Location" = "病位", "Pathogenesis" = "病性", "Syndrome" = "证型", "Percentage" = "百分比", "Core_Formula" = "核心配伍", "N_Presc" = "处方数",
  "Medicinal properties" = "药性", "Medicinal flavors" = "五味", "Meridian tropism" = "归经",
  "Metric Distributions by Type" = "各类型指标分布", "Total combinations: " = "组合总数：", "Thresholds used:" = "使用阈值：", "Support:      > " = "  支持度：> ", "Coverage:     by-size (" = "  覆盖度：按组合大小（", " quantile)" = " 分位数）", "Association Specificity: > " = "  关联特异性：> ", "Lift:         > " = "  提升度：> ", "Run evaluation to see summary." = "请运行特征分析后查看汇总。",
  "Core Formula Group" = "核心配伍组", "Core Herb Group" = "核心药群", "Specific Compatibility Group" = "专属性配伍组", "Noise Combination" = "噪声组合", "Other" = "其他", "Combination" = "配伍组合", "Classification" = "分型", "High" = "高", "Low" = "低", "threshold" = "阈值",
  "Train the model to see performance." = "请先训练模型后查看性能。", "In_Core_Formula" = "是否核心配伍", "AUC" = "AUC", "Probability" = "预测概率", "K_core" = "K核", "Weighted_Degree" = "加权度", "Pair_Strength" = "药对强度", "NetworkWeight" = "网络权重", "EnhancedScore" = "增强评分", "OriginalRank" = "原始排名", "EnhancedRank" = "增强排名", "RankChange" = "排名变化",
  "Shared Pairs" = "共享药对", "M2 Association Rules Only" = "仅 M2 关联规则", "M3 Network Edges Only" = "仅 M3 网络边", "Total M2 Pairs" = "M2 药对总数", "Total M3 Edges" = "M3 网络边总数", "Spearman rho" = "Spearman rho", "Pearson r" = "Pearson r", "Skipped: " = "已跳过：", "Run comparison to see NMI/ARI scores.\nBoth M2 clustering and M3 communities must be available." = "请运行比较后查看 NMI/ARI 分数。\n需要同时具备 M2 聚类与 M3 社区结果。", "Not applicable: metric is undefined for this partition." = "不适用：该分区的指标未定义。", "Run comparison to see per-community consensus. Requires both M2 clustering and M3 network results." = "请运行比较后查看各社区共识。需要同时具备 M2 聚类与 M3 网络结果。"
)

runtime_tr <- function(key, lang = "zh", ...) {
  value <- if (identical(lang, "zh")) unname(I18N_RUNTIME_ZH[key]) else NULL
  if (is.null(value) || is.na(value)) value <- key
  if (length(list(...))) sprintf(value, ...) else value
}

runtime_lang <- function(shared_data) {
  lang <- shared_data$lang
  if (is.null(lang) || !lang %in% c("zh", "en")) "zh" else lang
}

localized_filename <- function(stem, extension, lang = "zh") {
  paste0(runtime_tr(stem, lang), "_", Sys.Date(), ".", extension)
}

localized_placeholder <- function(message, lang = "zh", column = "Note") {
  data.frame(stats::setNames(list(runtime_tr(message, lang)), runtime_tr(column, lang)), check.names = FALSE)
}

# Apply translations only to generated field labels. Values, including raw herb and clinical data, remain unchanged.
localized_columns <- function(data, lang = "zh") {
  out <- data
  names(out) <- vapply(names(out), runtime_tr, character(1), lang = lang)
  out
}

# Local module wrappers establish a reactive language dependency for DT output.
# Translate headings/options only, never records or submitted input values.
localized_datatable <- function(data, ..., lang = "zh") {
  args <- list(...)
  if (is.data.frame(data) || is.matrix(data)) {
    headings <- colnames(data)
    if (is.null(args$colnames) && !is.null(headings))
      args$colnames <- unname(vapply(headings, runtime_tr, character(1), lang = lang))
  }
  if (identical(lang, "zh")) {
    language <- list(
      emptyTable = "表中暂无数据", zeroRecords = "未找到匹配记录",
      info = "显示第 _START_ 至 _END_ 条，共 _TOTAL_ 条记录",
      infoEmpty = "暂无记录", infoFiltered = "（从 _MAX_ 条记录中筛选）",
      lengthMenu = "每页显示 _MENU_ 条记录", search = "搜索：",
      loadingRecords = "正在加载…", processing = "正在处理…",
      aria = list(sortAscending = "：点击按升序排列", sortDescending = "：点击按降序排列"),
      paginate = list(first = "首页", last = "末页", previous = "上一页", `next` = "下一页")
    )
    if (is.null(args$options)) args$options <- list()
    args$options$language <- language
  }
  do.call(DT::datatable, c(list(data = data), args))
}

# One dictionary for static controls, generated labels and exported headings.
# The manuscript supplies terminology only; analysis values are never rewritten.
i18n_extra <- unlist(jsonlite::fromJSON("www/i18n-zh.json"), use.names = TRUE)
I18N_RUNTIME_ZH <- I18N_RUNTIME_ZH[!duplicated(names(I18N_RUNTIME_ZH))]
I18N_UI$zh <- c(I18N_UI$zh, I18N_RUNTIME_ZH[setdiff(names(I18N_RUNTIME_ZH), names(I18N_UI$zh))])
I18N_UI$zh[names(i18n_extra)] <- i18n_extra
I18N_RUNTIME_ZH <- I18N_UI$zh
