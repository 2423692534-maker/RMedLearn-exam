# ============================================================
# RMedLearn 教师端：通用单个考试提交码解码工具
# 只要 RMedLearn 提交码底层格式不变，新试卷仍可直接使用本文件。
# ============================================================

decode_one_submission <- function(
  submission_code = NULL,
  save_dir = NULL,
  verbose = TRUE
) {
  if (!requireNamespace("RMedLearn", quietly = TRUE)) {
    stop(
      "未安装 RMedLearn。请先安装当前版本的 RMedLearn 包。",
      call. = FALSE
    )
  }

  # 如果没有手动传入提交码，在 Windows 上默认从剪贴板读取
  if (is.null(submission_code)) {
    if (.Platform$OS.type == "windows") {
      submission_code <- paste(
        utils::readClipboard(),
        collapse = ""
      )
    } else {
      stop(
        "当前系统不是 Windows，请把 submission_code 作为参数传入。",
        call. = FALSE
      )
    }
  }

  # 清理复制时可能混入的换行和空格
  submission_code <- paste0(
    strsplit(
      paste(submission_code, collapse = ""),
      "[[:space:]]+"
    )[[1]],
    collapse = ""
  )

  if (!nzchar(submission_code)) {
    stop(
      "没有读取到提交码。请先复制完整提交码到剪贴板。",
      call. = FALSE
    )
  }

  # 调用 RMedLearn 包内部解码函数
  decode_fun <- getFromNamespace(
    "decode_exam_submission",
    "RMedLearn"
  )

  record <- tryCatch(
    decode_fun(submission_code),
    error = function(e) {
      stop(
        paste0(
          "提交码解码失败：",
          conditionMessage(e),
          "\n请确认提交码完整、未被截断或修改。"
        ),
        call. = FALSE
      )
    }
  )

  if (verbose) {
    cat("\n================ 解码成功 ================\n")
    cat("班级：", if (is.null(record$class)) "" else record$class, "\n", sep = "")
    cat("学号：", record$student_id, "\n", sep = "")
    cat("姓名：", if (is.null(record$name)) "" else record$name, "\n", sep = "")
    cat("后台数据集编号：", record$dataset_id, "\n", sep = "")
    cat("考试编号：", record$exam_id, "\n", sep = "")
    cat("开始时间：", record$started_at, "\n", sep = "")
    cat("完成时间：", record$completed_at, "\n", sep = "")
    cat("用时（分钟）：", record$elapsed_minutes, "\n", sep = "")
    cat("评分状态：", record$grading_status, "\n", sep = "")
    cat("题目数量：", length(record$questions), "\n", sep = "")
    cat("==========================================\n\n")
  }

  # 整理成便于教师检查的长表
  rows <- list()
  k <- 1L

  for (q in record$questions) {
    # 代码答案
    if (length(q$code_answers) > 0L) {
      for (nm in names(q$code_answers)) {
        validation <- if (
          !is.null(q$code_validation) &&
          !is.null(q$code_validation[[nm]])
        ) {
          q$code_validation[[nm]]
        } else {
          list()
        }

        rows[[k]] <- data.frame(
          student_id = record$student_id,
          dataset_id = record$dataset_id,
          question_id = q$question_id,
          question_title = q$title,
          answer_type = "code",
          answer_id = nm,
          answer = as.character(q$code_answers[[nm]]),
          ran = if (is.null(validation$ran)) NA else isTRUE(validation$ran),
          validated = if (is.null(validation$validated)) NA else isTRUE(validation$validated),
          validation_passed = if (is.null(validation$validation_passed)) NA else isTRUE(validation$validation_passed),
          validation_status = if (is.null(validation$status)) "" else as.character(validation$status),
          validation_text = if (is.null(validation$status_text)) "" else as.character(validation$status_text),
          current_matches_last_run = if (is.null(validation$current_matches_last_run)) NA else isTRUE(validation$current_matches_last_run),
          last_run_at = if (is.null(validation$ran_at)) "" else as.character(validation$ran_at),
          run_error = if (is.null(validation$error)) "" else as.character(validation$error),
          dataset_verified = if (is.null(validation$dataset_verified)) NA else validation$dataset_verified,
          stringsAsFactors = FALSE
        )
        k <- k + 1L
      }
    }

    # 填空答案
    if (length(q$fill_answers) > 0L) {
      for (nm in names(q$fill_answers)) {
        rows[[k]] <- data.frame(
          student_id = record$student_id,
          dataset_id = record$dataset_id,
          question_id = q$question_id,
          question_title = q$title,
          answer_type = "fill",
          answer_id = nm,
          answer = as.character(q$fill_answers[[nm]]),
          ran = NA,
          validated = NA,
          validation_passed = NA,
          validation_status = "",
          validation_text = "",
          current_matches_last_run = NA,
          last_run_at = "",
          run_error = "",
          dataset_verified = NA,
          stringsAsFactors = FALSE
        )
        k <- k + 1L
      }
    }
  }

  answers_long <- if (length(rows) > 0L) {
    do.call(rbind, rows)
  } else {
    data.frame()
  }

  # 可选：保存教师端解码结果
  if (!is.null(save_dir)) {
    dir.create(
      save_dir,
      recursive = TRUE,
      showWarnings = FALSE
    )

    prefix <- paste0(
      "submission_",
      record$student_id,
      "_dataset",
      record$dataset_id
    )

    saveRDS(
      record,
      file.path(
        save_dir,
        paste0(prefix, ".rds")
      )
    )

    utils::write.csv(
      answers_long,
      file.path(
        save_dir,
        paste0(prefix, "_answers.csv")
      ),
      row.names = FALSE,
      fileEncoding = "UTF-8"
    )

    if (verbose) {
      cat(
        "已保存教师端记录到：\n",
        normalizePath(save_dir, winslash = "/", mustWork = FALSE),
        "\n\n",
        sep = ""
      )
    }
  }

  invisible(
    list(
      record = record,
      answers = answers_long
    )
  )
}


# ============================================================
# 推荐用法 1：从 Windows 剪贴板直接读取
# ============================================================
#
# 1. 在考试完成页复制“完整考试提交码”
# 2. 回到 RStudio
# 3. source() 本文件
# 4. 运行：
#
# result <- decode_one_submission()
#
# 查看概要：
# result$record
#
# 查看学生所有代码和填空：
# result$answers
#
# 例如：
# View(result$answers)
#
#
# ============================================================
# 推荐用法 2：同时保存到教师文件夹
# ============================================================
#
# result <- decode_one_submission(
#   save_dir = "teacher_results"
# )
#
#
# ============================================================
# 推荐用法 3：手动把提交码传给函数
# ============================================================
#
# result <- decode_one_submission(
#   submission_code = "这里粘贴完整提交码"
# )
#
# 注意：
# RMedLearn 当前的“提交码”不是加密文件，而是
# JSON -> 校验值 -> gzip -> base64。
# 因此这里 technically 是“解码 + 完整性校验”，不是密码学意义上的解密。
# ============================================================
