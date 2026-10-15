# ============================================================
# utils_helpers.R — General helper functions
# ============================================================

normalize <- function(x) {
  if (max(x) == min(x)) {
    return(rep(0, length(x)))
  } else {
    return((x - min(x)) / (max(x) - min(x)))
  }
}

safe_split <- function(text, pattern = "\\s+") {
  if (is.na(text) || text == "") return(character(0))
  parts <- unlist(str_split(text, pattern))
  parts[parts != ""]
}

get_freq <- function(data, prescription_col) {
  total_n <- nrow(data)
  data %>%
    mutate({{prescription_col}} := str_squish({{prescription_col}})) %>%
    separate_rows({{prescription_col}}, sep = "\\s+") %>%
    filter({{prescription_col}} != "") %>%
    distinct(row_number(), {{prescription_col}}, .keep_all = TRUE) %>%
    count({{prescription_col}}, name = "频数") %>%
    mutate(
      频率 = 频数 / total_n,
      `频数（频率）` = paste0(频数, "（", round(频率 * 100, 1), "%）")
    ) %>%
    arrange(desc(频数))
}
