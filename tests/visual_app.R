# Local visual QA only. Run from the app directory with Rscript tests/visual_app.R.
# This entry point adds a fixture loader to M0; app.R and the upload path are unchanged.
Sys.setlocale("LC_ALL", "Chinese_China.utf8")
options(encoding = "UTF-8")
source("app.R", encoding = "UTF-8")
shiny::addResourcePath("qa-assets", normalizePath("www"))
qa_assets <- function(x) {
  if (inherits(x, "shiny.tag")) {
    if (identical(x$attribs$src, "i18n.js")) x$attribs$src <- "qa-assets/i18n.js"
    if (identical(x$attribs$href, "custom.css?v=4")) x$attribs$href <- "qa-assets/custom.css?v=4"
    x$children <- lapply(x$children, qa_assets)
  } else if (is.list(x)) x[] <- lapply(x, qa_assets)
  x
}
ui <- qa_assets(ui)

set.seed(20260923)
groups <- list(
  c("黄芪", "白术", "茯苓", "党参", "山药", "甘草", "陈皮"),
  c("当归", "白芍", "川芎", "熟地黄", "阿胶", "枸杞子", "丹参"),
  c("金银花", "连翘", "黄芩", "栀子", "蒲公英", "板蓝根", "鱼腥草")
)
fixture <- do.call(rbind, lapply(seq_len(180), function(i) {
  g <- (i - 1L) %% 3L + 1L
  herbs <- sample(groups[[g]], if (i %% 4L == 0L) 7 else 6)
  if (i %% 7L == 0L) herbs <- c(herbs, sample(groups[[g %% 3L + 1L]], 1))
  data.frame(record_id = sprintf("SYNTHETIC_QA_%03d", i),
    formula = paste0(herbs, sprintf(" %.2fg", 5 + i / 100), collapse = "、"),
    syndrome = c("脾气虚", "血虚", "热毒炽盛")[[g]],
    disease_location = c("脾", "肝", "肺")[[g]],
    disease_nature = c("气虚", "血虚", "热")[[g]],
    age = 20 + i %% 50, stringsAsFactors = FALSE)
}))
fixture_path <- tempfile(fileext = ".csv")
write.csv(fixture, fixture_path, row.names = FALSE, fileEncoding = "UTF-8")

original_m0_sidebar <- mod_0_sidebar
mod_0_sidebar <- function(id) tagList(
  div(class = "param-section", tags$p("视觉核查专用：180 条合成数据，无真实病例"),
      actionButton(NS(id)("qa_load_fixture"), "加载核查数据")),
  original_m0_sidebar(id)
)
loader <- quote(observeEvent(input$qa_load_fixture, {
  process_df(read_csv_gb(fixture_path), "Synthetic visual QA fixture")
}))
body(mod_0_data_server) <- as.call(c(as.list(body(mod_0_data_server)), list(loader)))
shiny::runApp(shinyApp(ui, server), port = 3877, launch.browser = FALSE)

