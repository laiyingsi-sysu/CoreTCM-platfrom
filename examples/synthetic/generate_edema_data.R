# 水肿真实世界混合方案数据生成器 — 平台测试版 (20,000 方)
# 以经典水肿方为社区种子 + 模拟扰动
# 输出字段: patient_id, herb_medicine, 证型, 标准证型, 病位, 病性, dosage, dosage_form, 年龄, 性别
# 附带生成 syndrome_mapping.csv (证型标准化) + syndrome_bingwei_bingxing.csv (证型→病位/病性)
# 以及 edema_sim_sample_1000.csv（主数据随机抽样 1,000 条）
set.seed(20260814)

N <- 20000L

# 7 个经典水肿方 = 7 个社区（核心配伍 + 证候）
formulas <- list(
  真武汤     = list(herbs = c("附子","白芍","白术","茯苓","生姜"),     syndrome = "阳虚水泛证"),
  五苓散     = list(herbs = c("泽泻","茯苓","猪苓","白术","桂枝"),     syndrome = "膀胱蓄水证"),
  实脾饮     = list(herbs = c("附子","干姜","白术","茯苓","木瓜","厚朴","草果","木香","槟榔","甘草","生姜","大枣"), syndrome = "脾阳虚水停证"),
  防己黄芪汤 = list(herbs = c("防己","黄芪","白术","甘草"),             syndrome = "气虚湿盛证"),
  苓桂术甘汤 = list(herbs = c("茯苓","桂枝","白术","甘草"),             syndrome = "脾阳虚饮停证"),
  猪苓汤     = list(herbs = c("猪苓","茯苓","泽泻","阿胶","滑石"),     syndrome = "水热互结证"),
  越婢汤     = list(herbs = c("麻黄","石膏","生姜","甘草","大枣"),     syndrome = "风水相搏证")
)
fnames <- names(formulas)

# 各药常规剂量范围 g（参考《中国药典》2020）
dose_range <- list(
  附子=c(3,10), 白芍=c(6,15), 白术=c(6,12), 茯苓=c(9,15), 生姜=c(3,9),
  泽泻=c(6,12), 猪苓=c(6,12), 桂枝=c(3,10), 干姜=c(3,9), 木瓜=c(6,12),
  厚朴=c(3,9), 草果=c(3,6), 木香=c(3,6), 槟榔=c(3,9), 甘草=c(3,6),
  大枣=c(6,12), 防己=c(6,12), 黄芪=c(9,30), 阿胶=c(3,9), 滑石=c(9,15),
  麻黄=c(3,9), 石膏=c(15,30)
)

# 加减药池（水肿常用药，用于扰动）
extra_herb_groups <- list(
  利水渗湿 = c("大腹皮","车前子","冬瓜皮","薏苡仁","赤小豆","桑白皮"),
  活血化瘀 = c("泽兰","益母草","丹参","川芎","桃仁","红花"),
  理气 = c("陈皮"),
  补气健脾 = c("党参","山药")
)
extra_herbs <- unlist(extra_herb_groups, use.names = FALSE)
extra_dose  <- list(陈皮=c(3,9), 大腹皮=c(6,12), 车前子=c(9,15), 冬瓜皮=c(15,30), 薏苡仁=c(15,30), 赤小豆=c(15,30), 桑白皮=c(9,15), 泽兰=c(9,15), 益母草=c(9,15), 党参=c(9,15), 山药=c(15,30), 丹参=c(9,15), 川芎=c(3,9), 桃仁=c(6,9), 红花=c(3,9))
all_dose <- c(dose_range, extra_dose)

# 噪声药（低概率混入，模拟录入误差或兼证）
noise_herbs <- c("当归","柴胡","黄芩","栀子","连翘")
noise_dose  <- list(当归=c(6,12), 柴胡=c(3,9), 黄芩=c(6,12), 栀子=c(3,9), 连翘=c(6,12))
all_dose2 <- c(all_dose, noise_dose)

# 原始证型变体池（测试 M1 标准化映射）
# 每个标准证型对应多个原始写法变体（去后缀/加空格/加前缀/简写）
syndrome_variants <- list(
  阳虚水泛证   = c("阳虚水泛", "阳虚水泛证", "阳虚 水泛证", "证型:阳虚水泛证", "阳虚水泛 症"),
  膀胱蓄水证   = c("膀胱蓄水", "膀胱蓄水证", "膀胱 蓄水证", "证型:膀胱蓄水证"),
  脾阳虚水停证 = c("脾阳虚水停", "脾阳虚水停证", "脾阳虚 水停证", "证型:脾阳虚水停证"),
  气虚湿盛证   = c("气虚湿盛", "气虚湿盛证", "气虚 湿盛证", "证型:气虚湿盛证"),
  脾阳虚饮停证 = c("脾阳虚饮停", "脾阳虚饮停证", "脾阳虚 饮停证", "证型:脾阳虚饮停证"),
  水热互结证   = c("水热互结", "水热互结证", "水热 互结证", "证型:水热互结证"),
  风水相搏证   = c("风水相搏", "风水相搏证", "风水 相搏证", "证型:风水相搏证")
)

# 标准证型 → (病位, 病性) 拆分映射（标准化之后的拆分步骤）
# token 对齐平台 utils_zhengsu BUILD_BINGWEI_DICT
zhengxing_meta <- list(
  阳虚水泛证   = list(bingwei = "肾、心",   bingxing = "阳虚、水停"),
  膀胱蓄水证   = list(bingwei = "膀胱",     bingxing = "水停"),
  脾阳虚水停证 = list(bingwei = "脾",       bingxing = "阳虚、水停"),
  气虚湿盛证   = list(bingwei = "脾",       bingxing = "气虚、湿"),
  脾阳虚饮停证 = list(bingwei = "脾",       bingxing = "阳虚、水饮"),
  水热互结证   = list(bingwei = "膀胱",     bingxing = "水停、热"),
  风水相搏证   = list(bingwei = "肺、脾",   bingxing = "风、水停")
)

dosage_forms <- c(rep("汤剂", 8), "散剂", "丸剂")
genders <- c("男", "女")

demographic_profile <- function(syndrome) {
  if (syndrome %in% c("阳虚水泛证", "脾阳虚水停证", "脾阳虚饮停证")) {
    list(age = c(55L, 75L), female_prob = 0.5, extra_bias = "补气健脾")
  } else if (syndrome == "气虚湿盛证") {
    list(age = c(45L, 70L), female_prob = 0.55, extra_bias = "补气健脾")
  } else {
    list(age = c(25L, 55L), female_prob = 0.5, extra_bias = "利水渗湿")
  }
}

sample_extra_herbs <- function(n_add, profile, gender) {
  if (n_add == 0) return(character(0))
  weights <- rep(1, length(extra_herbs))
  names(weights) <- extra_herbs
  weights[extra_herb_groups[[profile$extra_bias]]] <- 1.3
  if (gender == "女") weights[extra_herb_groups[["活血化瘀"]]] <- weights[extra_herb_groups[["活血化瘀"]]] * 1.1
  sample(extra_herbs, n_add, replace = FALSE, prob = weights)
}

# 剂量批量计算函数（向量化 runif）
dose_vec <- function(herbs) {
  r <- all_dose2[herbs]
  miss <- lengths(r) == 0
  if (any(miss)) r[miss] <- list(c(3, 9))
  m <- matrix(unlist(r), nrow = length(herbs), byrow = TRUE)
  round(runif(nrow(m), m[, 1], m[, 2]))
}

gen_one <- function() {
  main <- fnames[sample.int(length(fnames), 1)]
  base_herbs <- formulas[[main]]$herbs
  syn_std <- formulas[[main]]$syndrome

  if (runif(1) < 0.3) {
    aux <- fnames[sample.int(length(fnames) - 1, 1)]
    if (aux == main) aux <- fnames[length(fnames)]
    base_herbs <- unique(c(base_herbs, formulas[[aux]]$herbs))
  }

  profile <- demographic_profile(syn_std)
  gender <- if (runif(1) < profile$female_prob) "女" else "男"
  age <- sample(profile$age[1]:profile$age[2], 1)

  n_add <- sample.int(4L, 1) - 1L
  if (n_add > 0) base_herbs <- c(base_herbs, sample_extra_herbs(n_add, profile, gender))

  if (runif(1) < 0.05) base_herbs <- c(base_herbs, sample(noise_herbs, 1))

  dup_keep <- runif(1) < 0.05
  if (!dup_keep) base_herbs <- unique(base_herbs)

  # ⑥ 8% 概率证候置 NA（模拟不规范录入缺失）
  if (runif(1) < 0.08) {
    syn_raw <- NA_character_
    syn_std <- NA_character_
    bw <- NA_character_
    bx <- NA_character_
  } else {
    # 原始证型 = 随机变体（测试 M1 标准化）
    syn_raw <- sample(syndrome_variants[[syn_std]], 1)
    # 病位/病性 = 标准证型拆分（标准化之后的步骤）
    meta <- zhengxing_meta[[syn_std]]
    bw <- meta$bingwei
    bx <- meta$bingxing
  }

  doses <- dose_vec(base_herbs)

  list(
    patient_id    = sprintf("P%06d", sample.int(999999, 1)),
    herb_medicine = paste(base_herbs, collapse = ";"),
    syndrome      = syn_raw,      # 原始证型（含变体）
    syndrome_std  = syn_std,      # 标准化证型
    bingwei       = bw,           # 病位（标准证型拆分）
    bingxing      = bx,           # 病性（标准证型拆分）
    dosage        = paste(doses, collapse = ";"),
    dosage_form   = sample(dosage_forms, 1),
    age           = age,
    gender        = gender
  )
}

# 预分配 + for 循环填入
patient_id    <- character(N)
herb_medicine <- character(N)
syndrome      <- character(N)
syndrome_std  <- character(N)
bingwei       <- character(N)
bingxing      <- character(N)
dosage        <- character(N)
dosage_form   <- character(N)
age           <- integer(N)
gender        <- character(N)

for (i in seq_len(N)) {
  rec <- gen_one()
  patient_id[i]    <- rec$patient_id
  herb_medicine[i] <- rec$herb_medicine
  syndrome[i]      <- rec$syndrome
  syndrome_std[i]  <- rec$syndrome_std
  bingwei[i]       <- rec$bingwei
  bingxing[i]      <- rec$bingxing
  dosage[i]        <- rec$dosage
  dosage_form[i]   <- rec$dosage_form
  age[i]           <- rec$age
  gender[i]        <- rec$gender
  if (i %% 50000 == 0) cat("进度:", i, "/", N, "\n")
}

df <- data.frame(
  patient_id = patient_id, herb_medicine = herb_medicine,
  证型 = syndrome, 标准证型 = syndrome_std,
  病位 = bingwei, 病性 = bingxing,
  dosage = dosage, dosage_form = dosage_form,
  年龄 = age, 性别 = gender,
  stringsAsFactors = FALSE, check.names = FALSE
)

out_path <- "edema_sim_data.csv"
write.csv(df, out_path, row.names = FALSE, fileEncoding = "UTF-8")

sample_n <- min(1000L, nrow(df))
sample_df <- df[sample.int(nrow(df), sample_n), , drop = FALSE]
write.csv(sample_df, "edema_sim_sample_1000.csv", row.names = FALSE, fileEncoding = "UTF-8")

# 生成证型标准化映射表（供 M1 上传用）
mapping_rows <- list()
for (std in names(syndrome_variants)) {
  for (raw in syndrome_variants[[std]]) {
    if (raw != std) {
      mapping_rows[[length(mapping_rows) + 1]] <- data.frame(
        原始证型 = raw, 标准证型 = std, stringsAsFactors = FALSE)
    }
  }
}
mapping_df <- do.call(rbind, mapping_rows)
write.csv(mapping_df, "syndrome_mapping.csv", row.names = FALSE, fileEncoding = "UTF-8")

# 生成证型→病位/病性标准映射表（供 M1 Upload B 上传用）
meta_df <- data.frame(
  标准证型 = names(zhengxing_meta),
  病位 = sapply(zhengxing_meta, function(x) x$bingwei),
  病性 = sapply(zhengxing_meta, function(x) x$bingxing),
  stringsAsFactors = FALSE, row.names = NULL, check.names = FALSE
)
write.csv(meta_df, "syndrome_bingwei_bingxing.csv", row.names = FALSE, fileEncoding = "UTF-8")

cat("生成处方数:", nrow(df), "\n")
cat("字段:", paste(colnames(df), collapse = ", "), "\n")
cat("证型标准化映射数:", nrow(mapping_df), "\n")
cat("证型→病位/病性映射数:", nrow(meta_df), "\n")
cat("证型分布:\n"); print(table(df$证型, useNA = "ifany"))
cat("标准证型分布:\n"); print(table(df$标准证型, useNA = "ifany"))
cat("病位分布:\n"); print(table(df$病位, useNA = "ifany"))
cat("病性分布:\n"); print(table(df$病性, useNA = "ifany"))
cat("剂型分布:\n"); print(table(df$dosage_form))
idx <- sample.int(N, min(5000, N))
nh <- sapply(strsplit(df$herb_medicine[idx], ";"), length)
cat("药味数(min/median/max, 抽样5000):", min(nh), "/", median(nh), "/", max(nh), "\n")
cat("输出文件:", normalizePath(out_path, winslash = "/"), "\n")
cat("1000 条抽样文件:", normalizePath("edema_sim_sample_1000.csv", winslash = "/"), "\n")
cat("证型标准化映射表:", normalizePath("syndrome_mapping.csv", winslash = "/"), "\n")
cat("证型→病位/病性映射表:", normalizePath("syndrome_bingwei_bingxing.csv", winslash = "/"), "\n")
