# Run from the repository root. Installs only missing packages.
options(repos = c(CRAN = "https://cloud.r-project.org"))
core <- c("shiny", "shinydashboard", "shinythemes", "shinyjs", "DT",
          "dplyr", "tidyr", "stringr", "purrr", "readxl")
analysis <- c("igraph", "visNetwork", "plotly", "wordcloud2", "arules",
              "arulesViz", "fmsb", "openxlsx", "jsonlite", "rmarkdown",
              "ggraph", "ggplot2", "pheatmap", "htmltools", "htmlwidgets")
packages <- unique(c(core, analysis))
available <- function(x) vapply(x, requireNamespace, logical(1), quietly = TRUE)
missing <- packages[!available(packages)]
if (length(missing)) install.packages(missing, dependencies = NA)
missing <- packages[!available(packages)]
if (length(missing)) {
  stop("Packages still missing: ", paste(missing, collapse = ", "),
       ". Check installation messages and compatibility with your R version.")
}
cat("All application and regression-test dependencies are available.\n")
