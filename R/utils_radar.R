# ============================================================
# utils_radar.R — Radar charts for herb property analysis
# ============================================================

compute_property_count <- function(herbs, herb_attr_df) {

  df <- herb_attr_df %>% filter(药物名称 %in% herbs)
  df <- df %>% separate_rows(归经, sep = "、")

  sx <- df %>% count(四性)
  ww <- df %>% count(五味)
  gj <- df %>% count(归经)

  return(list(四性 = sx, 五味 = ww, 归经 = gj))
}

prepare_single_radar <- function(count_df, levels_vec, total_n) {
  vec <- setNames(rep(0, length(levels_vec)), levels_vec)
  vec[count_df[[1]]] <- count_df$n / total_n
  return(vec)
}

plot_radar_single <- function(radar_vec, title = "") {
  df <- as.data.frame(t(radar_vec))
  df <- rbind(rep(1, ncol(df)), rep(0, ncol(df)), df)
  colnames(df) <- names(radar_vec)

  radarchart(df, axistype = 1, pcol = 1, plwd = 2, title = title)
}

plot_radar_fmsb <- function(values, title, color) {
  dat <- rbind(rep(1, length(values)), rep(0, length(values)), values)
  dat <- as.data.frame(dat)
  colnames(dat) <- names(values)

  radarchart(
    dat, axistype = 1, seg = 4,
    pcol = color, plwd = 1, plty = 1, pty = 20,
    pfcol = adjustcolor(color, alpha.f = 0.12),
    cglcol = "#D0D7E2", cglty = 1, cglwd = 0.8,
    axislabcol = "#4D4D4D",
    caxislabels = c("0%", "25%", "50%", "75%", "100%"),
    vlcex = 1, calcex = 0.8,
    title = title, centerzero = TRUE
  )
}
