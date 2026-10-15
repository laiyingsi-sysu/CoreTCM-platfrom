# Run from app directory. Covers real defects found in populated visual QA.
Sys.setlocale("LC_ALL", "Chinese_China.utf8")
options(encoding = "UTF-8")
source("app.R", encoding = "UTF-8")
shared <- shiny::reactiveValues(
  lang = "zh", clean_col = "formula", active_params = list(),
  clean = data.frame(formula = c("黄芪 白术", "当归 白芍", "黄芪 白术"),
    syndrome = c("气虚", "血虚", "气虚"), disease_location = c("脾", "肝", "脾"),
    disease_nature = c("虚", "虚", "热"), stringsAsFactors = FALSE),
  module_results = list())
shiny::testServer(function(input, output, session) {
  shared_data <- shared
  eval(body(mod_1_clinical_server))
}, {
  session$setInputs(bwbx_source = "data")
  stopifnot(clinical_cols()$bingxing == "disease_nature")
  run_m1_impl()
  stopifnot(m1_results()$n_zx == 2L,
            identical(m1_results()$features$Disease_Location, c("脾", "肝", "脾")),
            identical(m1_results()$features$Pathogenesis, c("虚", "虚", "热")))
})
shiny::isolate({
  shared$module_results$m3 <- list(n_comm = 1,
    memb = data.frame(Herb = c("黄芪", "白术"), Community = 1),
    core = list(core_formula_table = data.frame(Community = 1, Core_Formula = "黄芪 白术")))
})
shiny::testServer(function(input, output, session) {
  shared_data <- shared
  eval(body(mod_5_mapping_server))
}, {
  session$setInputs(run_m5 = 1)
  r <- m5_results()
  stopifnot(!is.null(r))
  # Inspect all result frames: one-character clinical terms must survive.
  frames <- r[vapply(r, is.data.frame, logical(1))]
  cells <- unlist(lapply(frames, function(d) unlist(d, use.names = FALSE)), use.names = FALSE)
  stopifnot("脾" %in% cells, "热" %in% cells, "虚" %in% cells)
})
cat("Clinical column detection, syndrome counts and single-character feature regressions passed\n")
