# ============================================================
# utils_validation.R — SCCL validation framework
# Support / Coverage / Specificity / Lift
# ============================================================

validate_core_formula <- function(data, prescription_col, core_formula_table) {

  prescriptions <- data[[prescription_col]]
  prescriptions <- str_split(prescriptions, "\\s+")
  total_n <- length(prescriptions)
  N <- total_n

  # Compute per-herb frequencies for Lift denominator
  all_herbs <- unlist(prescriptions)
  all_herbs <- all_herbs[nchar(all_herbs) >= 2 & !is.na(all_herbs) & all_herbs != ""]
  herb_freq <- table(all_herbs) / N

  result <- data.frame()

  for (i in 1:nrow(core_formula_table)) {
    comm <- core_formula_table$Community[i]
    formula <- core_formula_table$Core_Formula[i]
    herbs <- unlist(str_split(formula, "\\s+"))
    n_herbs <- length(herbs)
    if (n_herbs == 0) next

    full_match <- 0; any_match <- 0
    for (p in prescriptions) {
      if (all(herbs %in% p)) full_match <- full_match + 1
      if (any(herbs %in% p)) any_match <- any_match + 1
    }

    support      <- full_match / total_n
    coverage     <- any_match / total_n
    specificity <- ifelse(any_match > 0, full_match / any_match, 0)

    # Lift = P(all together) / product of individual P(herb_i)
    prod_indiv <- prod(sapply(herbs, function(h) {
      f <- herb_freq[h]; if (is.na(f) || f == 0) 1/N else f
    }))
    lift <- ifelse(prod_indiv > 0, support / prod_indiv, 0)

    result <- rbind(result, data.frame(
      Community    = comm,
      Formula      = formula,
      ComboSize    = n_herbs,
      Support      = round(support, 4),
      Coverage     = round(coverage, 4),
      Specificity = round(specificity, 4),
      Lift         = round(lift, 2),
      stringsAsFactors = FALSE
    ))
  }

  return(result)
}

classify_combinations <- function(validation_df,
                                   support_q      = 0.50,
                                   coverage_q     = 0.75,
                                   specificity_q = 0.50,
                                   lift_q         = 0.75,
                                   coverage_by_size = TRUE) {

  df <- validation_df

  # ---- quantile thresholds ----
  support_thresh      <- quantile(df$Support, support_q, na.rm = TRUE)
  specificity_thresh <- quantile(df$Specificity, specificity_q, na.rm = TRUE)
  lift_thresh         <- quantile(df$Lift, lift_q, na.rm = TRUE)

  # Coverage threshold: per-size or global
  if (coverage_by_size) {
    coverage_thresh <- sapply(df$ComboSize, function(sz) {
      vals <- df$Coverage[df$ComboSize == sz]
      if (length(vals) < 3) quantile(df$Coverage, coverage_q, na.rm = TRUE)
      else quantile(vals, coverage_q, na.rm = TRUE)
    })
  } else {
    coverage_thresh <- rep(quantile(df$Coverage, coverage_q, na.rm = TRUE), nrow(df))
  }

  # ---- binary flags ----
  df$Support_High      <- df$Support      >= support_thresh
  df$Coverage_High     <- df$Coverage     >= coverage_thresh
  df$Specificity_High <- df$Specificity >= specificity_thresh
  df$Lift_High         <- df$Lift         >= lift_thresh

  # ---- classification (5 types) ----
  # Core Formula Group (核心配伍组): S↑ C↑ Sp↑ -> core framework, high stability
  # Core Herb Group (核心药群): S↑ C↑ Sp↓ L↑ -> core herbs, wide coverage, low fixity, specific association
  # Specific Compatibility Group (专属性配伍组): S↓ C↓ Sp↑ L↑ -> specific syndrome, tight bond
  # Noise Combination (噪声组合): everything low
  # Other (其他): mixed indicator profile
  df$Type <- dplyr::case_when(
    df$Support_High & df$Coverage_High & df$Specificity_High ~ "Core Formula Group",
    df$Support_High & df$Coverage_High & df$Lift_High & !df$Specificity_High ~ "Core Herb Group",
    !df$Support_High & !df$Coverage_High & df$Specificity_High & df$Lift_High ~ "Specific Compatibility Group",
    !df$Support_High & !df$Coverage_High & !df$Specificity_High & !df$Lift_High ~ "Noise Combination",
    TRUE ~ "Other"
  )

  # Attach thresholds as attributes for reporting
  attr(df, "support_thresh")      <- support_thresh
  attr(df, "coverage_thresh")     <- coverage_thresh
  attr(df, "specificity_thresh") <- specificity_thresh
  attr(df, "lift_thresh")         <- lift_thresh

  return(df)
}

classify_with_thresholds <- function(validation_df, thresholds) {
  df <- validation_df
  coverage_thresh <- unname(thresholds$coverage_by_size[as.character(df$ComboSize)])
  coverage_thresh[is.na(coverage_thresh)] <- thresholds$coverage_global

  df$Support_High <- df$Support >= thresholds$support
  df$Coverage_High <- df$Coverage >= coverage_thresh
  df$Specificity_High <- df$Specificity >= thresholds$specificity
  df$Lift_High <- df$Lift >= thresholds$lift
  df$Type <- dplyr::case_when(
    df$Support_High & df$Coverage_High & df$Specificity_High ~ "Core Formula Group",
    df$Support_High & df$Coverage_High & df$Lift_High & !df$Specificity_High ~ "Core Herb Group",
    !df$Support_High & !df$Coverage_High & df$Specificity_High & df$Lift_High ~ "Specific Compatibility Group",
    !df$Support_High & !df$Coverage_High & !df$Specificity_High & !df$Lift_High ~ "Noise Combination",
    TRUE ~ "Other"
  )

  attr(df, "coverage_thresh") <- coverage_thresh
  return(df)
}
