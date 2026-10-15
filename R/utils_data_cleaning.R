# ============================================================
# utils_data_cleaning.R — Prescription text cleaning pipeline
# ============================================================

clean_prescription <- function(data, column_name) {

  new_col <- paste0(column_name, "_clean")
  data[[new_col]] <- data[[column_name]]

  # Remove newlines
  data[[new_col]] <- str_replace_all(data[[new_col]], "\n", " ")

  # Unify delimiters
  data[[new_col]] <- str_replace_all(data[[new_col]], "[,，、;；]", " ")

  # Remove bracketed content
  data[[new_col]] <- str_replace_all(data[[new_col]], "[（(][^）)]*[）)]", " ")

  # Remove dosage + unit
  data[[new_col]] <- str_replace_all(
    data[[new_col]],
    "\\d+\\.?\\d*\\s*(mg|g|G|克|两|钱|袋|条|包|丸|片|瓶|ml|ML)", " "
  )

  # Remove residual units
  data[[new_col]] <- str_replace_all(
    data[[new_col]],
    "\\b(mg|g|G|克|两|钱|袋|条|包|丸|片|瓶|ml|ML)\\b", " "
  )

  # Remove English letters
  data[[new_col]] <- str_replace_all(data[[new_col]], "[A-Za-z]+", " ")

  # Remove residual numbers
  data[[new_col]] <- str_replace_all(data[[new_col]], "\\d+\\.?\\d*", " ")

  # Squish extra spaces
  data[[new_col]] <- str_squish(data[[new_col]])

  return(data)
}

process_prescription <- function(data, column_name,
                                  remove_processing = TRUE,
                                  extra_prefix = NULL) {

  default_prefix <- c(
    "炙", "生", "炒", "醋", "酒", "盐", "蜜",
    "麸炒", "清炒", "土炒", "炒炭", "煅", "蒸", "漂"
  )

  if (!is.null(extra_prefix)) {
    processing_prefix <- unique(c(default_prefix, extra_prefix))
  } else {
    processing_prefix <- default_prefix
  }

  processing_prefix <- processing_prefix[
    order(nchar(processing_prefix), decreasing = TRUE)
  ]
  pattern <- paste0("^(", paste(processing_prefix, collapse = "|"), ")")

  new_col <- paste0(column_name, "_processed")

  data[[new_col]] <- sapply(
    data[[column_name]],
    function(text) {
      if (is.na(text) || text == "") return(NA)
      herbs <- unlist(str_split(text, " "))
      herbs <- herbs[herbs != ""]
      if (remove_processing) {
        herbs <- str_replace(herbs, pattern, "")
      }
      paste(herbs, collapse = " ")
    }
  )

  return(data)
}

# Parse once: dosage text is kept out of herb-name normalization and analysis.
parse_formula_items_with_dosage <- function(text) {
  empty <- data.frame(raw_item = character(), raw_herb_name = character(),
                      raw_dosage = character(), warning = character())
  if (is.na(text) || !nzchar(trimws(text))) return(empty)
  number <- "(?:[0-9]+(?:\\.[0-9]+)?|\\.[0-9]+)"
  dose <- paste0("(?:各\\s*)?", number,
                 "(?:\\s*[-–~～]\\s*", number, ")?\\s*",
                 "(?:mg|ml|g|毫克|千克|公斤|克|两|钱|片|袋|条|包|丸|瓶|毫升|升)")
  dose <- paste0("(?:", dose, "|各?适量|各?少许)")
  # Protect whitespace inside dosage expressions before splitting formula items.
  protected <- stringr::str_replace_all(text,
    stringr::regex(paste0("[（(]\\s*", dose, "\\s*[）)]|", dose), ignore_case = TRUE),
    function(x) gsub("[[:space:]]", "\uE000", x))
  protected <- gsub("(?<=[\\p{Han}])[^\\S\\r\\n]+(?=[（(各0-9.]|适量|少许)", "", protected, perl = TRUE)
  parts <- unlist(strsplit(protected, "[,，、;；\\r\\n[:space:]]+", perl = TRUE))
  parts <- parts[nzchar(parts)]
  if (!length(parts)) return(empty)
  rows <- lapply(parts, function(part) {
    raw <- gsub("\uE000", " ", part, fixed = TRUE)
    match <- stringr::str_match(raw, stringr::regex(
      paste0("^([\\p{Han}]+?)\\s*(?:[（(]\\s*(", dose, ")\\s*[）)]|(", dose, "))$"),
      ignore_case = TRUE))
    if (!is.na(match[1, 1])) {
      herb <- match[1, 2]
      dosage <- if (!is.na(match[1, 3])) match[1, 3] else match[1, 4]
      warning <- ""
    } else if (grepl("^[\\p{Han}]+$", raw, perl = TRUE) &&
               !grepl("[一二三四五六七八九十半]+(克|两|钱)$|适量$|少许$", raw, perl = TRUE)) {
      herb <- raw; dosage <- ""; warning <- ""
    } else {
      herb <- ""; dosage <- ""; warning <- "Review formula item"
    }
    data.frame(raw_item = raw, raw_herb_name = herb,
               raw_dosage = trimws(dosage), warning = warning)
  })
  do.call(rbind, rows)
}

standardize_prescriptions <- function(data, column_name, dict_data = NULL,
                                     alias_col = "alias", standard_col = "standard_name",
                                     remove_duplicate = TRUE, remove_processing = FALSE,
                                     processing_prefixes = character()) {
  new_col <- paste0(column_name, "_std")
  dose_col <- paste0(column_name, "_std_with_dosage")
  if (any(c(new_col, dose_col) %in% names(data)))
    stop("Derived formula columns already exist; rename them before cleaning.")
  lookup <- NULL
  if (!is.null(dict_data)) {
    if (!all(c(alias_col, standard_col) %in% names(dict_data)))
      stop("Herb mapping columns not found")
    lookup <- setNames(trimws(as.character(dict_data[[standard_col]])),
                       trimws(as.character(dict_data[[alias_col]])))
  }
  prefixes <- processing_prefixes[!is.na(processing_prefixes) & nzchar(processing_prefixes)]
  prefixes <- prefixes[order(nchar(prefixes), decreasing = TRUE)]
  unmatched <- character()
  warnings <- data.frame(Row = integer(), Item = character(), Reason = character())
  dose_values <- analysis_values <- rep(NA_character_, nrow(data))
  for (i in seq_len(nrow(data))) {
    items <- parse_formula_items_with_dosage(as.character(data[[column_name]][i]))
    if (!nrow(items)) next
    standardized <- character(); rendered <- character()
    for (j in seq_len(nrow(items))) {
      herb <- items$raw_herb_name[j]
      if (nzchar(herb) && isTRUE(remove_processing)) {
        hits <- prefixes[startsWith(herb, prefixes)]
        if (length(hits)) herb <- substring(herb, nchar(hits[1]) + 1L)
      }
      if (nzchar(herb) && !is.null(lookup)) {
        mapped <- unname(lookup[herb])
        if (is.na(mapped) || !nzchar(mapped)) {
          unmatched <- c(unmatched, herb)
        } else herb <- mapped
      } else if (nzchar(herb)) unmatched <- c(unmatched, herb)
      # Invalid mapping targets and unparsed items remain reviewable, never analysis tokens.
      if (!nzchar(herb) || !grepl("^[\\p{Han}]+$", herb, perl = TRUE)) {
        rendered <- c(rendered, items$raw_item[j])
        warnings <- rbind(warnings, data.frame(Row = i, Item = items$raw_item[j],
                                              Reason = "Review formula item"))
      } else {
        standardized <- c(standardized, herb)
        rendered <- c(rendered, paste0(herb, items$raw_dosage[j]))
      }
    }
    dose_values[i] <- paste(rendered, collapse = "; ")
    if (length(standardized)) analysis_values[i] <- paste(sort(unique(standardized)), collapse = " ")
  }
  data[[dose_col]] <- dose_values
  data[[new_col]] <- analysis_values
  # Incomplete or empty records are retained for review, never collapsed accidentally.
  duplicate <- duplicated(dose_values) & !is.na(dose_values) & nzchar(dose_values)
  duplicate[warnings$Row] <- FALSE
  if (!isTRUE(remove_duplicate)) duplicate[] <- FALSE
  list(data = data[!duplicate, , drop = FALSE], unmatched = sort(unique(unmatched)),
       dup_removed = sum(duplicate), new_col = new_col, dose_preserved_col = dose_col,
       raw_formula_col = column_name, parsing_warnings = warnings)
}

formula_export_data <- function(data, dose_preserved_col, include_standardized_dosage_export = TRUE) {
  if (!isTRUE(include_standardized_dosage_export) && !is.null(dose_preserved_col))
    data <- data[, setdiff(names(data), dose_preserved_col), drop = FALSE]
  data
}

migrate_formula_params <- function(params) {
  if (is.null(params$m0_include_standardized_dosage_export) && !is.null(params$m0_remove_dosage))
    params$m0_include_standardized_dosage_export <- !isTRUE(params$m0_remove_dosage)
  params$m0_remove_dosage <- NULL
  params$remove_dosage <- NULL
  params$g_m0_remove_dosage <- NULL
  params
}
