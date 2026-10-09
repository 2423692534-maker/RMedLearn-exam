# ============================================================
# RMedLearn 教师端：根据 x1.csv ~ x10.csv 自动生成标准答案
# 当前试卷：9 题，代码 70 分 + 填空 30 分 = 100 分
# ============================================================

.rml_prepare_standard <- function(dat) {
  needed <- c("id", "age", "sex", "huji", "height_cm", "weight_kg",
              "smoking", "insomnia", "tg", "sugar", "health")
  miss <- setdiff(needed, names(dat))
  if (length(miss) > 0L) stop("数据缺少变量：", paste(miss, collapse = ", "), call. = FALSE)

  data1 <- dat
  keep <- !is.na(data1$age) & data1$age >= 40 & data1$age <= 75
  data2 <- data1[keep, , drop = FALSE]

  data2$bmi <- data2$weight_kg / (data2$height_cm / 100)^2
  data2$bmi1 <- ifelse(is.na(data2$bmi), NA_integer_,
                       ifelse(data2$bmi < 18, 1L,
                              ifelse(data2$bmi < 24, 2L,
                                     ifelse(data2$bmi < 28, 3L, 4L))))
  data2$tg_cat <- ifelse(is.na(data2$tg), NA_integer_, ifelse(data2$tg >= 2.3, 1L, 0L))

  data2$sex <- stats::relevel(factor(data2$sex), ref = "Female")
  data2$huji <- factor(data2$huji)
  data2$bmi1 <- stats::relevel(factor(data2$bmi1), ref = "2")
  data2$smoking <- factor(data2$smoking)
  data2$insomnia <- factor(data2$insomnia)
  data2$tg_cat <- factor(data2$tg_cat)
  list(data1 = data1, data2 = data2)
}

make_exam_key <- function(dataset_dir = "datasets", output = "exam_key_teacher.rds",
                          exam_id = "RMedLearn_Teacher_Exam_2026_v1") {
  all_keys <- vector("list", 10L)
  for (dataset_id in 1:10) {
    path <- file.path(dataset_dir, paste0("x", dataset_id, ".csv"))
    if (!file.exists(path)) stop("找不到数据集：", path, call. = FALSE)
    dat <- utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
    std <- .rml_prepare_standard(dat)
    data2 <- std$data2

    sex_test <- stats::t.test(health ~ sex, data = data2)
    bmi_aov <- stats::aov(health ~ bmi1, data = data2)
    bmi_p <- summary(bmi_aov)[[1]][["Pr(>F)"]][1]

    fit <- stats::glm(
      tg_cat ~ insomnia + sex + huji + bmi1 + smoking + sugar,
      family = stats::binomial(), data = data2
    )
    sm <- summary(fit)$coefficients
    sex_row <- grep("^sexMale$", rownames(sm), value = TRUE)[1]
    if (is.na(sex_row) || !nzchar(sex_row)) stop("无法找到 sexMale 回归系数。", call. = FALSE)

    answers <- list(
      q03_fill1 = list(value = nrow(data2), type = "integer", tolerance = 0, points = 2.5),
      q03_fill2 = list(value = ncol(std$data1), type = "integer", tolerance = 0, points = 2.5),
      q05_fill1 = list(value = mean(as.integer(as.character(data2$tg_cat)) == 1L, na.rm = TRUE), type = "numeric", tolerance = 0.05, points = 5),
      q08_fill1 = list(value = unname(sex_test$p.value), type = "numeric", tolerance = 0.05, points = 5),
      q08_fill2 = list(value = unname(bmi_p), type = "numeric", tolerance = 0.05, points = 5),
      q09_fill1 = list(value = unname(exp(sm[sex_row, "Estimate"])), type = "numeric", tolerance = 0.05, points = 5),
      q09_fill2 = list(value = unname(sm["sugar", "Pr(>|z|)"]), type = "numeric", tolerance = 0.05, points = 5)
    )
    all_keys[[dataset_id]] <- list(dataset_id = dataset_id, answers = answers)
  }
  names(all_keys) <- as.character(1:10)
  key <- list(exam_id = exam_id, created_at = Sys.time(), datasets = all_keys)
  saveRDS(key, output)
  message("Created exam key: ", normalizePath(output, winslash = "/", mustWork = FALSE))
  invisible(key)
}
