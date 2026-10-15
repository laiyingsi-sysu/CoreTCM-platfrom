# ============================================================
# utils_zhengsu.R — Syndrome element extraction (证素提取)
# Longest-match algorithm for extracting 病位/病性 from 证型
# ============================================================

# Build a lookup dictionary of known disease locations and pathogenesis
BUILD_BINGWEI_DICT <- function() {
  list(
    病位 = c("肺", "脾", "肾", "肝", "心", "三焦", "膀胱", "胃", "大肠",
            "小肠", "胆", "心包", "胞宫", "经络", "筋骨", "肌肤", "清窍",
            "脑", "髓海", "咽喉", "胸膈", "胁肋"),
    病性 = c("气虚", "阳虚", "阴虚", "血虚", "精亏", "津亏",
            "水湿", "水停", "水饮", "水肿", "湿", "湿热", "痰湿",
            "痰热", "痰浊", "寒湿", "寒凝", "热", "火", "火毒",
            "血瘀", "瘀血", "瘀热", "瘀毒", "气滞", "气逆", "气陷",
            "风", "风热", "风寒", "风湿", "风痰", "风燥",
            "燥", "食积", "虫积", "毒", "浊毒")
  )
}

extract_bingwei_bingxing <- function(zhengxing_vec, dict = NULL) {
  if (is.null(dict)) dict <- BUILD_BINGWEI_DICT()

  bingwei_all <- dict$病位[order(nchar(dict$病位), decreasing = TRUE)]
  bingxing_all <- dict$病性[order(nchar(dict$病性), decreasing = TRUE)]

  result <- data.frame(
    证型 = zhengxing_vec,
    病位 = character(length(zhengxing_vec)),
    病性 = character(length(zhengxing_vec)),
    stringsAsFactors = FALSE
  )

  for (i in seq_along(zhengxing_vec)) {
    zx <- zhengxing_vec[i]
    if (is.na(zx) || zx == "") next

    # Longest-match for 病位
    found_bw <- c()
    remaining <- zx
    for (bw in bingwei_all) {
      if (str_detect(remaining, fixed(bw))) {
        found_bw <- c(found_bw, bw)
        remaining <- str_replace_all(remaining, fixed(bw), "")
      }
    }
    result$病位[i] <- paste(sort(unique(found_bw)), collapse = "、")

    # Longest-match for 病性
    found_bx <- c()
    remaining <- zx
    for (bx in bingxing_all) {
      if (str_detect(remaining, fixed(bx))) {
        found_bx <- c(found_bx, bx)
        remaining <- str_replace_all(remaining, fixed(bx), "")
      }
    }
    result$病性[i] <- paste(sort(unique(found_bx)), collapse = "、")
  }

  return(result)
}
