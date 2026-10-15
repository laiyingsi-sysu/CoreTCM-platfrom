# Run from the repository root; no package installation or data changes.
if (.Platform$OS.type == "windows") Sys.setlocale("LC_CTYPE", ".UTF-8")
cat(R.version.string, "\nWorking directory: ", getwd(), "\n", sep = "")
expected <- c("app.R", "global.R", "R/utils_helpers.R", "www/custom.css",
              "www/i18n.js", "www/i18n-zh.json",
              "data/中药标准化字典.xlsx", "data/药物归经、五味、功效属性.xlsx",
              "data/example_formula_only.csv", "data/example_formula_clinical.csv",
              "examples/synthetic/edema_sim_sample_1000.csv")
absent <- expected[!file.exists(expected)]
if (length(absent)) stop("Missing files or wrong working directory: ", paste(absent, collapse = ", "))
source_text <- readLines("global.R", encoding = "UTF-8", warn = FALSE)
calls <- grep('^load_pkg[(]"', source_text, value = TRUE)
packages <- vapply(strsplit(calls, '"', fixed = TRUE), function(x) x[[2]], character(1))
packages <- unique(c(packages, "htmltools", "htmlwidgets"))
loaded <- vapply(packages, requireNamespace, logical(1), quietly = TRUE)
versions <- vapply(packages, function(p) {
  if (loaded[[p]]) as.character(utils::packageVersion(p)) else "MISSING"
}, character(1))
print(data.frame(Package = packages, Version = versions), row.names = FALSE)
if (any(!loaded)) stop("Run scripts/install_dependencies.R to install missing packages.")
files <- c("app.R", "global.R", list.files("R", pattern = "[.]R$", full.names = TRUE),
           list.files("scripts", pattern = "[.]R$", full.names = TRUE),
           list.files("tests", pattern = "[.]R$", full.names = TRUE),
           "examples/synthetic/generate_edema_data.R")
for (file in files) parse(file, encoding = "UTF-8")
cat("Required files, dependencies and R syntax checks passed.\n")
