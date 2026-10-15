# Run from the repository root, including when the shell has a legacy locale.
if (.Platform$OS.type == "windows") Sys.setlocale("LC_CTYPE", ".UTF-8")
options(encoding = "UTF-8")
if (!file.exists("app.R")) stop("Run this script from the repository root.")
shiny::runApp(".", launch.browser = interactive())
