# ============================================================
# RMedLearn 教师端：当前 9 题试卷单份自动评分
#
# 总分：
#   代码 70 分
#   填空 30 分
#   总分 100 分
#
# 原则：
# 1. 优先运行学生代码并检查生成对象/模型是否正确。
# 2. 代码结构仅作为无法直接从结果判断时的辅助。
# 3. 接受多种等价正确写法；被自动扣分且学生确有作答的
#    评分点会在批量结果中进入“人工复核”。
# 4. 填空：
#    - 行数/列数必须精确；
#    - 其余数值题允许绝对误差 ±0.05。
# ============================================================


# ---------- 通用工具 ----------

.rml_norm_code <- function(code) {
  if (
    is.null(code) ||
    length(code) == 0L
  ) {
    return("")
  }

  x <- paste(
    code,
    collapse = "\n"
  )

  lines <- strsplit(
    x,
    "\n",
    fixed = TRUE
  )[[1]]

  lines <- sub(
    "#.*$",
    "",
    lines
  )

  tolower(
    trimws(
      paste(
        lines,
        collapse = "\n"
      )
    )
  )
}


.rml_has <- function(
  code,
  pattern
) {
  grepl(
    pattern,
    .rml_norm_code(code),
    perl = TRUE
  )
}


.rml_add <- function(
  detail,
  qid,
  item,
  points,
  earned,
  note
) {
  rbind(
    detail,
    data.frame(
      question_id = qid,
      item = item,
      points = as.numeric(points),
      earned = as.numeric(earned),
      note = as.character(note),
      stringsAsFactors = FALSE
    )
  )
}


.rml_eval <- function(
  code,
  env
) {
  out <- list(
    ok = FALSE,
    error = NULL,
    results = list()
  )

  code <- paste(
    code,
    collapse = "\n"
  )

  if (
    !nzchar(
      trimws(
        .rml_norm_code(code)
      )
    )
  ) {
    out$error <- "未作答"
    return(out)
  }

  exprs <- tryCatch(
    parse(
      text = code
    ),
    error = function(e) e
  )

  if (inherits(
    exprs,
    "error"
  )) {
    out$error <- paste0(
      "代码无法解析：",
      conditionMessage(exprs)
    )

    return(out)
  }

  results <- list()

  # 学生绘图输出静默到临时 PDF，避免批改时占用 Plots/Zoom
  plot_file <- tempfile(
    pattern = "rmedlearn_grade_",
    fileext = ".pdf"
  )

  plot_device <- NULL

  try(
    {
      grDevices::pdf(
        plot_file
      )

      plot_device <- grDevices::dev.cur()
    },
    silent = TRUE
  )

  on.exit(
    {
      if (!is.null(plot_device)) {
        devices <- grDevices::dev.list()

        if (
          !is.null(devices) &&
          plot_device %in% devices
        ) {
          try(
            grDevices::dev.off(
              plot_device
            ),
            silent = TRUE
          )
        }
      }

      unlink(
        plot_file
      )
    },
    add = TRUE
  )

  for (i in seq_along(exprs)) {
    one <- tryCatch(
      eval(
        exprs[[i]],
        envir = env
      ),
      error = function(e) e
    )

    results[[i]] <- one

    if (inherits(
      one,
      "error"
    )) {
      out$error <- paste0(
        "运行错误：",
        conditionMessage(one)
      )

      out$results <- results
      return(out)
    }
  }

  out$ok <- TRUE
  out$results <- results
  out
}


.rml_same_ids <- function(
  x,
  y
) {
  is.data.frame(x) &&
    is.data.frame(y) &&
    "id" %in% names(x) &&
    "id" %in% names(y) &&
    identical(
      sort(
        as.character(
          x$id
        )
      ),
      sort(
        as.character(
          y$id
        )
      )
    )
}


.rml_get_question <- function(
  record,
  qid
) {
  idx <- match(
    qid,
    vapply(
      record$questions,
      function(q) {
        as.character(
          q$question_id
        )
      },
      character(1)
    )
  )

  if (is.na(idx)) {
    return(NULL)
  }

  record$questions[[idx]]
}


.rml_get_code <- function(
  record,
  qid,
  label = NULL
) {
  q <- .rml_get_question(
    record,
    qid
  )

  if (
    is.null(q) ||
    length(
      q$code_answers
    ) == 0L
  ) {
    return("")
  }

  if (
    !is.null(label) &&
    label %in%
      names(
        q$code_answers
      )
  ) {
    return(
      paste(
        as.character(
          q$code_answers[[label]]
        ),
        collapse = "\n"
      )
    )
  }

  paste(
    unlist(
      q$code_answers,
      use.names = FALSE
    ),
    collapse = "\n"
  )
}


.rml_collect_class <- function(
  env,
  ev,
  class_name
) {
  out <- list()

  env_names <- ls(
    env,
    all.names = TRUE
  )

  if (length(env_names) > 0L) {
    for (nm in env_names) {
      obj <- tryCatch(
        get(
          nm,
          envir = env,
          inherits = FALSE
        ),
        error = function(e) NULL
      )

      if (
        !is.null(obj) &&
        inherits(
          obj,
          class_name
        )
      ) {
        out[[length(out) + 1L]] <- obj
      }
    }
  }

  if (
    !is.null(ev$results) &&
    length(ev$results) > 0L
  ) {
    for (obj in ev$results) {
      if (
        !is.null(obj) &&
        inherits(
          obj,
          class_name
        )
      ) {
        out[[length(out) + 1L]] <- obj
      }
    }
  }

  out
}


.rml_formula_matches <- function(
  obj,
  response,
  predictors
) {
  f <- tryCatch(
    stats::formula(obj),
    error = function(e) NULL
  )

  if (is.null(f)) {
    return(FALSE)
  }

  vars <- all.vars(f)

  if (
    length(vars) < 2L ||
    !identical(
      vars[1],
      response
    )
  ) {
    return(FALSE)
  }

  setequal(
    vars[-1],
    predictors
  )
}


.rml_prepare_standard <- function(dat) {
  data1 <- dat
  keep <- !is.na(data1$age) & data1$age >= 40 & data1$age <= 75
  data2_base <- data1[keep, , drop = FALSE]

  data2_raw <- data2_base
  data2_raw$bmi <- data2_raw$weight_kg / (data2_raw$height_cm / 100)^2
  data2_raw$bmi1 <- ifelse(is.na(data2_raw$bmi), NA_integer_,
                           ifelse(data2_raw$bmi < 18, 1L,
                                  ifelse(data2_raw$bmi < 24, 2L,
                                         ifelse(data2_raw$bmi < 28, 3L, 4L))))
  data2_raw$tg_cat <- ifelse(is.na(data2_raw$tg), NA_integer_, ifelse(data2_raw$tg >= 2.3, 1L, 0L))

  data2_factor <- data2_raw
  for (v in c("sex", "huji", "bmi1", "smoking", "insomnia", "tg_cat")) {
    data2_factor[[v]] <- factor(data2_factor[[v]])
  }

  data2 <- data2_factor
  data2$sex <- stats::relevel(data2$sex, ref = "Female")
  data2$bmi1 <- stats::relevel(data2$bmi1, ref = "2")

  list(data1 = data1, data2_base = data2_base, data2_raw = data2_raw,
       data2_factor = data2_factor, data2 = data2)
}


# ---------- 填空评分 ----------

.rml_grade_fills <- function(
  record,
  key_dataset
) {
  rows <- list()
  k <- 1L

  for (q in record$questions) {
    if (
      length(
        q$fill_answers
      ) == 0L
    ) {
      next
    }

    for (
      answer_id in
        names(
          q$fill_answers
        )
    ) {
      spec <- key_dataset$answers[[answer_id]]

      if (is.null(spec)) {
        next
      }

      submitted <- trimws(
        as.character(
          q$fill_answers[[answer_id]]
        )
      )

      expected <- as.numeric(
        spec$value
      )

      tolerance <- as.numeric(
        spec$tolerance
      )

      type <- as.character(
        spec$type
      )

      points <- if (
        !is.null(spec$points)
      ) {
        as.numeric(
          spec$points
        )
      } else {
        0
      }

      value <- suppressWarnings(
        as.numeric(
          submitted
        )
      )

      correct <- FALSE
      note <- ""

      if (!nzchar(submitted)) {
        note <- "未作答"
      } else if (is.na(value)) {
        note <- "不是数值"
      } else if (
        identical(
          type,
          "integer"
        )
      ) {
        correct <- isTRUE(
          all.equal(
            value,
            expected,
            tolerance = 0
          )
        )

        note <- if (correct) {
          "正确"
        } else {
          paste0(
            "标准答案：",
            expected
          )
        }
      } else {
        correct <- abs(
          value -
            expected
        ) <=
          tolerance +
            1e-12

        note <- if (correct) {
          "在允许误差内"
        } else {
          paste0(
            "标准答案：",
            signif(
              expected,
              8
            ),
            "；允许误差 ±",
            tolerance
          )
        }
      }

      rows[[k]] <- data.frame(
        question_id = as.character(
          q$question_id
        ),
        answer_id = answer_id,
        expected = expected,
        submitted = submitted,
        correct = correct,
        tolerance = tolerance,
        points = points,
        earned = if (correct) points else 0,
        note = note,
        stringsAsFactors = FALSE
      )

      k <- k + 1L
    }
  }

  if (length(rows) == 0L) {
    return(
      data.frame()
    )
  }

  do.call(
    rbind,
    rows
  )
}


# ---------- 主评分函数 ----------

grade_one_submission <- function(
  result,
  dataset_dir = "datasets",
  key_path = "exam_key_teacher.rds"
) {
  record <- if (
    !is.null(
      result$record
    )
  ) {
    result$record
  } else {
    result
  }

  dataset_id <- suppressWarnings(
    as.integer(
      record$dataset_id
    )
  )

  if (
    is.na(dataset_id) ||
    !(dataset_id %in% 1:10)
  ) {
    stop(
      "提交记录中的 dataset_id 无效。",
      call. = FALSE
    )
  }

  data_path <- file.path(
    dataset_dir,
    paste0(
      "x",
      dataset_id,
      ".csv"
    )
  )

  if (!file.exists(data_path)) {
    stop(
      "找不到学生对应数据集：",
      data_path,
      call. = FALSE
    )
  }

  dat <- utils::read.csv(
    data_path,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  std <- .rml_prepare_standard(
    dat
  )

  if (!file.exists(key_path)) {
    stop(
      "找不到标准答案文件：",
      key_path,
      call. = FALSE
    )
  }

  key <- readRDS(
    key_path
  )

  if (
    !is.null(
      key$exam_id
    ) &&
    !is.null(
      record$exam_id
    ) &&
    !identical(
      as.character(
        key$exam_id
      ),
      as.character(
        record$exam_id
      )
    )
  ) {
    stop(
      paste0(
        "标准答案 exam_id 与提交记录不一致：",
        key$exam_id,
        " vs ",
        record$exam_id
      ),
      call. = FALSE
    )
  }

  key_dataset <- key$datasets[[dataset_id]]

  if (is.null(key_dataset)) {
    stop(
      "标准答案中缺少数据集 X",
      dataset_id,
      "。",
      call. = FALSE
    )
  }

  detail <- data.frame()


  # ==========================================================
  # Q01 工作空间设置与 R 包加载：10 分
  # ==========================================================
  wd_code <- .rml_get_code(record, "Q01", "q01_workdir")
  pkg_code <- .rml_get_code(record, "Q01", "q01_import")
  wd_ok <- .rml_has(wd_code, "\\bsetwd\\s*\\(")
  foreign_ok <- (.rml_has(pkg_code, "\\bforeign\\b") && (.rml_has(pkg_code, "\\blibrary\\s*\\(") || .rml_has(pkg_code, "\\brequire\\s*\\(")))
  dplyr_ok <- (.rml_has(pkg_code, "\\bdplyr\\b") && (.rml_has(pkg_code, "\\blibrary\\s*\\(") || .rml_has(pkg_code, "\\brequire\\s*\\(")))
  detail <- .rml_add(detail, "Q01", "使用 setwd() 设置考试工作空间", 4, if (wd_ok) 4 else 0, if (wd_ok) "正确" else "未检测到 setwd()")
  detail <- .rml_add(detail, "Q01", "加载 foreign 包", 3, if (foreign_ok) 3 else 0, if (foreign_ok) "正确" else "未检测到 foreign 包加载")
  detail <- .rml_add(detail, "Q01", "加载 dplyr 包", 3, if (dplyr_ok) 3 else 0, if (dplyr_ok) "正确" else "未检测到 dplyr 包加载")

  # ==========================================================
  # Q02 导入数据：10 分
  # ==========================================================
  code <- .rml_get_code(record, "Q02")
  expected_file <- paste0("x", dataset_id, ".csv")
  import_ok <- .rml_has(code, "\\bdata1\\s*(?:<-|=)\\s*(?:utils::)?read\\.csv\\s*\\(") &&
    .rml_has(code, gsub("\\.", "\\\\.", expected_file))
  verified <- isTRUE(record$dataset_verified)
  detail <- .rml_add(detail, "Q02", paste0("正确导入 ", expected_file, " 并命名为 data1"), 7, if (import_ok) 7 else 0, if (import_ok) "正确" else "未识别到正确数据导入")
  detail <- .rml_add(detail, "Q02", "导入本次随机分配的数据集", 3, if (verified) 3 else 0, if (verified) "数据文件 SHA-256 校验通过" else "数据文件校验未通过")

  # ==========================================================
  # Q03 年龄筛选并建立 data2：5 分
  # ==========================================================
  code <- .rml_get_code(record, "Q03")
  env <- new.env(parent = globalenv()); env$data1 <- std$data1
  ev <- .rml_eval(code, env)
  data2_ok <- ev$ok && exists("data2", envir = env, inherits = FALSE) && .rml_same_ids(env$data2, std$data2_base)
  detail <- .rml_add(detail, "Q03", "按 age >= 40 且 age <= 75 正确建立 data2", 5, if (data2_ok) 5 else 0, if (data2_ok) "data2 样本 ID 与标准结果一致" else if (!ev$ok) ev$error else "data2 与标准结果不一致")

  # ==========================================================
  # Q04 bmi 与 bmi1：10 分
  # ==========================================================
  code <- .rml_get_code(record, "Q04")
  env <- new.env(parent = globalenv()); env$data2 <- std$data2_base
  ev <- .rml_eval(code, env)
  bmi_ok <- ev$ok && "bmi" %in% names(env$data2) && isTRUE(all.equal(as.numeric(env$data2$bmi), as.numeric(std$data2_raw$bmi), tolerance = 1e-6, check.attributes = FALSE))
  bmi1_ok <- FALSE
  if (ev$ok && "bmi1" %in% names(env$data2)) {
    observed <- suppressWarnings(as.integer(as.character(env$data2$bmi1)))
    expected <- suppressWarnings(as.integer(as.character(std$data2_raw$bmi1)))
    bmi1_ok <- identical(observed, expected)
  }
  detail <- .rml_add(detail, "Q04", "正确计算 bmi", 5, if (bmi_ok) 5 else 0, if (bmi_ok) "正确" else if (!ev$ok) ev$error else "bmi 结果不正确")
  detail <- .rml_add(detail, "Q04", "正确建立 bmi1 四分类并处理界值", 5, if (bmi1_ok) 5 else 0, if (bmi1_ok) "正确" else "bmi1 分类不正确")

  # ==========================================================
  # Q05 tg_cat：5 分
  # ==========================================================
  code <- .rml_get_code(record, "Q05")
  env <- new.env(parent = globalenv()); env$data2 <- std$data2_raw[, setdiff(names(std$data2_raw), "tg_cat"), drop = FALSE]
  ev <- .rml_eval(code, env)
  tg_ok <- FALSE
  if (ev$ok && "tg_cat" %in% names(env$data2)) {
    observed <- suppressWarnings(as.integer(as.character(env$data2$tg_cat)))
    expected <- suppressWarnings(as.integer(as.character(std$data2_raw$tg_cat)))
    tg_ok <- identical(observed, expected)
  }
  detail <- .rml_add(detail, "Q05", "按 tg >= 2.3 正确建立 tg_cat", 5, if (tg_ok) 5 else 0, if (tg_ok) "正确" else if (!ev$ok) ev$error else "tg_cat 结果不正确")

  # ==========================================================
  # Q06 六个变量因子化：10 分
  # ==========================================================
  code <- .rml_get_code(record, "Q06")
  env <- new.env(parent = globalenv()); env$data2 <- std$data2_raw
  ev <- .rml_eval(code, env)
  factor_vars <- c("sex", "huji", "bmi1", "smoking", "insomnia", "tg_cat")
  each_points <- 10 / length(factor_vars)
  for (v in factor_vars) {
    ok <- ev$ok && is.data.frame(env$data2) && v %in% names(env$data2) && is.factor(env$data2[[v]])
    detail <- .rml_add(detail, "Q06", paste0(v, " 因子化"), each_points, if (ok) each_points else 0, if (ok) "正确" else "未满足要求")
  }

  # ==========================================================
  # Q07 sex 与 bmi1 参考组：10 分
  # ==========================================================
  code <- .rml_get_code(record, "Q07")
  env <- new.env(parent = globalenv()); env$data2 <- std$data2_factor
  ev <- .rml_eval(code, env)
  bmi_ref_ok <- ev$ok && is.factor(env$data2$bmi1) && identical(levels(env$data2$bmi1)[1], "2")
  sex_ref_ok <- ev$ok && is.factor(env$data2$sex) && identical(levels(env$data2$sex)[1], "Female")
  detail <- .rml_add(detail, "Q07", "bmi1 以第2组为参考组", 5, if (bmi_ref_ok) 5 else 0, if (bmi_ref_ok) "正确" else if (!ev$ok) ev$error else "参考组不正确")
  detail <- .rml_add(detail, "Q07", "sex 以 Female 为参考组", 5, if (sex_ref_ok) 5 else 0, if (sex_ref_ok) "正确" else if (!ev$ok) ev$error else "参考组不正确")

  # ==========================================================
  # Q08 差异比较：5 分
  # ==========================================================
  code <- .rml_get_code(record, "Q08")
  env <- new.env(parent = globalenv()); env$data2 <- std$data2
  ev <- .rml_eval(code, env)
  has_health <- .rml_has(code, "\\bhealth\\b")
  any_two <- function(v) ev$ok && has_health && .rml_has(code, paste0("\\b", v, "\\b")) && (.rml_has(code, "\\bt\\.test\\s*\\(") || .rml_has(code, "\\baov\\s*\\(") || .rml_has(code, "\\boneway\\.test\\s*\\(") || .rml_has(code, "\\banova\\s*\\("))
  sex_ok <- any_two("sex")
  bmi_ok2 <- ev$ok && has_health && .rml_has(code, "\\bbmi1\\b") && (.rml_has(code, "\\baov\\s*\\(") || .rml_has(code, "\\boneway\\.test\\s*\\(") || .rml_has(code, "\\banova\\s*\\("))
  insomnia_ok <- any_two("insomnia")
  detail <- .rml_add(detail, "Q08", "比较不同 sex 的 health", 2, if (sex_ok) 2 else 0, if (sex_ok) "正确" else "未识别到有效比较")
  detail <- .rml_add(detail, "Q08", "比较不同 bmi1 的 health", 2, if (bmi_ok2) 2 else 0, if (bmi_ok2) "正确" else "未识别到有效比较")
  detail <- .rml_add(detail, "Q08", "比较不同 insomnia 的 health", 1, if (insomnia_ok) 1 else 0, if (insomnia_ok) "正确" else "未识别到有效比较")

  # ==========================================================
  # Q09 logistic 回归：5 分
  # ==========================================================
  code <- .rml_get_code(record, "Q09")
  env <- new.env(parent = globalenv()); env$data2 <- std$data2
  ev <- .rml_eval(code, env)
  models <- .rml_collect_class(env, ev, "glm")
  predictors <- c("insomnia", "sex", "huji", "bmi1", "smoking", "sugar")
  model_ok <- any(vapply(models, function(x) {
    .rml_formula_matches(x, response = "tg_cat", predictors = predictors) && !is.null(x$family) && identical(x$family$family, "binomial")
  }, logical(1)))
  if (!model_ok) {
    term_ok <- all(vapply(c("tg_cat", predictors), function(v) .rml_has(code, paste0("\\b", v, "\\b")), logical(1)))
    model_ok <- ev$ok && .rml_has(code, "\\bglm\\s*\\(") && .rml_has(code, "\\bbinomial\\b") && term_ok
  }
  summary_ok <- ev$ok && .rml_has(code, "\\bsummary\\s*\\(")
  or_ok <- ev$ok && .rml_has(code, "\\bexp\\s*\\(")
  detail <- .rml_add(detail, "Q09", "正确建立指定 logistic 回归模型", 3, if (model_ok) 3 else 0, if (model_ok) "正确" else if (!ev$ok) ev$error else "模型不符合题意")
  detail <- .rml_add(detail, "Q09", "使用 summary() 输出模型结果", 1, if (summary_ok) 1 else 0, if (summary_ok) "正确" else "未检测到 summary()")
  detail <- .rml_add(detail, "Q09", "输出 OR 值", 1, if (or_ok) 1 else 0, if (or_ok) "正确" else "未检测到 exp() 等 OR 输出代码")


  # ---------- 填空评分 ----------
  fill_detail <- .rml_grade_fills(
    record,
    key_dataset
  )

  # ---------- 汇总 ----------
  code_score <- sum(
    detail$earned,
    na.rm = TRUE
  )

  fill_score <- if (
    nrow(fill_detail) > 0L
  ) {
    sum(
      fill_detail$earned,
      na.rm = TRUE
    )
  } else {
    0
  }

  qids <- sprintf(
    "Q%02d",
    1:9
  )

  question_scores <- do.call(
    rbind,
    lapply(
      qids,
      function(qid) {
        cr <- detail[
          detail$question_id == qid,
          ,
          drop = FALSE
        ]

        fr <- if (
          nrow(fill_detail) > 0L
        ) {
          fill_detail[
            fill_detail$question_id == qid,
            ,
            drop = FALSE
          ]
        } else {
          data.frame()
        }

        q <- .rml_get_question(
          record,
          qid
        )

        code_earned <- sum(
          cr$earned,
          na.rm = TRUE
        )

        code_max <- sum(
          cr$points,
          na.rm = TRUE
        )

        fill_earned <- if (
          nrow(fr) > 0L
        ) {
          sum(
            fr$earned,
            na.rm = TRUE
          )
        } else {
          0
        }

        fill_max <- if (
          nrow(fr) > 0L
        ) {
          sum(
            fr$points,
            na.rm = TRUE
          )
        } else {
          0
        }

        data.frame(
          question_id = qid,
          title = if (
            is.null(q)
          ) {
            ""
          } else {
            as.character(
              q$title
            )
          },
          code_score = round(
            code_earned,
            2
          ),
          code_max = round(
            code_max,
            2
          ),
          fill_score = round(
            fill_earned,
            2
          ),
          fill_max = round(
            fill_max,
            2
          ),
          total_score = round(
            code_earned +
              fill_earned,
            2
          ),
          total_max = round(
            code_max +
              fill_max,
            2
          ),
          stringsAsFactors = FALSE
        )
      }
    )
  )

  summary_df <- data.frame(
    class = if (
      is.null(
        record$class
      )
    ) {
      ""
    } else {
      as.character(
        record$class
      )
    },
    student_id = as.character(
      record$student_id
    ),
    name = if (
      is.null(
        record$name
      )
    ) {
      ""
    } else {
      as.character(
        record$name
      )
    },
    dataset_id = dataset_id,
    code_score = round(
      code_score,
      2
    ),
    code_max = 70,
    fill_score = round(
      fill_score,
      2
    ),
    fill_max = 30,
    total_score = round(
      code_score +
        fill_score,
      2
    ),
    total_max = 100,
    stringsAsFactors = FALSE
  )

  list(
    summary = summary_df,
    question_scores = question_scores,
    code_detail = detail,
    fill_detail = fill_detail
  )
}
