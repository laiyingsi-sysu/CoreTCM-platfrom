# Reproducible populated outputs from real module event handlers, independent of WebSocket lifetime.
Sys.setlocale("LC_ALL", "Chinese_China.utf8")
options(encoding = "UTF-8")
for (expr in parse("tests/visual_app.R", encoding = "UTF-8")) {
  if (is.call(expr) && identical(expr[[1]], as.name("<-")) && identical(expr[[2]], as.name("original_m0_sidebar"))) break
  eval(expr)
}
report_dir <- "tests/visual-report"
dir.create(report_dir, recursive = TRUE, showWarnings = FALSE)
shared <- reactiveValues(lang = "zh", dict = NULL, herb_attr = NULL,
  clean = NULL, active_params = DEFAULT_PARAMS, module_results = list())
widgets <- list()
capture_plot <- function(serialized, name, title) {
  payload <- jsonlite::fromJSON(serialized, simplifyVector = FALSE)
  widget <- htmlwidgets::createWidget("plotly", payload$x, height = 500,
    package = "plotly", dependencies = plotly::plotly_build(plotly::plot_ly(x = 1, y = 1))$dependencies)
  widgets[[name]] <<- list(title = title, widget = widget)
}
shiny::testServer(function(input, output, session) {
  shared_data <- shared; eval(body(mod_0_data_server))
}, {
  session$setInputs(use_builtin_dict = TRUE, prescription_col = "formula", remove_prefix = TRUE,
    builtin_prefixes = paste(DEFAULT_PREFIXES, collapse = ","), extra_prefixes = "",
    alias_col = "原始名称", std_col = "标准名称", remove_duplicate = TRUE,
    include_standardized_dosage_export = TRUE)
  raw_df(fixture)
  session$setInputs(run_cleaning = 1)
  stopifnot(nrow(shared$clean) == 180)
})
shiny::testServer(function(input, output, session) {
  shared_data <- shared; eval(body(mod_1_clinical_server))
}, {
  session$setInputs(bwbx_source = "data")
  run_m1_impl()
  stopifnot(m1_results()$n_zx == 3)
})
shiny::testServer(function(input, output, session) {
  shared_data <- shared; eval(body(mod_2_traditional_server))
}, {
  do.call(session$setInputs, DEFAULT_PARAMS[grepl("^m2_", names(DEFAULT_PARAMS))])
  session$setInputs(ward_k_slider = 5, m2_scatter_size = 2, m2_net_top_n = 50,
                   m2_elbow_max_k = 18, m2_cluster_sel = "C1")
  session$setInputs(run_freq = 1, run_rules = 1, run_cluster = 1)
  stopifnot(isTRUE(rules_r()$ok), !is.null(cluster_r()$hc))
})
shiny::testServer(function(input, output, session) {
  shared_data <- shared; eval(body(mod_3_network_server))
}, {
  do.call(session$setInputs, DEFAULT_PARAMS[grepl("^m3_", names(DEFAULT_PARAMS))])
  session$setInputs(run_m3 = 1)
  stopifnot(is.null(pipe_res()$error), pipe_res()$n_comm == 3)
})
shiny::testServer(function(input, output, session) {
  shared_data <- shared; eval(body(mod_4_validation_server))
}, {
  do.call(session$setInputs, DEFAULT_PARAMS[grepl("^m4_", names(DEFAULT_PARAMS))])
  session$setInputs(run_m4 = 1)
  stopifnot(!is.null(output$dt_classified))
  capture_plot(output$plot_support_coverage, "characterization", "配伍支持度与覆盖度")
  capture_plot(output$plot_metrics_dist, "distributions", "各类型指标分布")
})
shiny::testServer(function(input, output, session) {
  shared_data <- shared; eval(body(mod_6_comparison_server))
}, {
  session$setInputs(m6_pair_top_n = 20, m6_herb_top_n = 20, run_m6 = 1)
  stopifnot(m6_res()$pair_compare$n_shared > 0, !is.null(m6_res()$cluster_comm_map))
  capture_plot(output$plot_pair_scatter, "pairs_zh", "中文：药对比较")
  capture_plot(output$plot_sankey, "flow_zh", "中文：聚类与社区流向")
  stopifnot(!is.null(output$dt_pair_overlap), !is.null(output$dt_core_compare), !is.null(output$dt_cluster_comm_map))
  shared$lang <- "en"; session$flushReact()
  capture_plot(output$plot_pair_scatter, "pairs_en", "English: pair comparison")
  capture_plot(output$plot_sankey, "flow_en", "English: cluster-community flow")
  shared$lang <- "zh"
})
shiny::testServer(function(input, output, session) {
  shared_data <- shared; eval(body(mod_5_mapping_server))
}, {
  session$setInputs(m5_min_herb_pct = 0.5, m5_max_chain = 10, m5_max_cat_levels = 10,
    w_alpha = 0.33, w_beta_share = 0.5, w_lambda = 0.5, w_gamma = 0.3, run_m5 = 1)
  stopifnot(!is.null(m5_results()))
  session$setInputs(train_model = 1)
  stopifnot(!is.null(ml_model()), length(ml_model()$models) > 0,
            all(c("肝", "脾", "肺") %in% ml_model()$all_bw), "热" %in% ml_model()$all_bx)
  session$setInputs(rec_bingwei = "脾", rec_bingxing = "气虚", run_recommend = 1)
  stopifnot(is.null(rec_result()$error), nrow(rec_result()$herbs) > 0)
  capture_plot(output$plot_recommend, "recommend_zh", "中文：候选药物排序")
  stopifnot(!is.null(output$dt_recommend), !is.null(output$dt_model_perf))
  write.csv(rec_result()$herbs, file.path(report_dir, "recommendation.csv"), row.names = FALSE)
  write.csv(ml_model()$auc, file.path(report_dir, "model_performance.csv"), row.names = FALSE)
  shared$lang <- "en"; session$flushReact()
  capture_plot(output$plot_recommend, "recommend_en", "English: candidate ranking")
})
for (name in names(widgets)) {
  htmlwidgets::saveWidget(widgets[[name]]$widget, file.path(report_dir, paste0(name, ".html")),
    selfcontained = FALSE, libdir = "lib", title = widgets[[name]]$title)
}
saveRDS(shiny::isolate(reactiveValuesToList(shared)), file.path(report_dir, "analysis_state.rds"))
htmltools::save_html(htmltools::tags$html(lang = "zh",
  htmltools::tags$head(htmltools::tags$meta(charset = "utf-8"), htmltools::tags$title("CoreTCM 图表核查")),
  htmltools::tags$body(style = "font-family: sans-serif; max-width: 900px; margin: 40px auto; line-height: 1.8",
    htmltools::tags$h1("CoreTCM 分析后图表核查"),
    htmltools::tags$p("180 条合成处方，21 味药物，3 个药物社区。仅用于界面与流程验证。"),
    htmltools::tags$p("图表由真实模块的分析函数生成；分析全流程与中英文结果渲染均已验证。"),
    htmltools::tags$ul(lapply(names(widgets), function(name)
      htmltools::tags$li(htmltools::tags$a(href = paste0(name, ".html"), widgets[[name]]$title))))
  )), file.path(report_dir, "index.html"))
cat("Full pipeline and populated bilingual visual reports passed.\n")
