# ============================================================
# RMedLearn 教师端：通用批量解码 + 批量评分 + 多 Sheet Excel 导出
# 文件名建议：batch_grade_submissions.R
#
# 输入文件（Excel/CSV）必须一人一行。
# 正式推荐收集表头（中文）：
#   班级 | 学号 | 姓名 | 提交码
#
# 真正必需字段只有：
#   学号
#   提交码
#
# 姓名、班级均可为空；同时继续兼容英文表头。
# submitted_at / 提交时间仍兼容读取，但正式模板不再要求。
#
# 推荐用法：
#   source("01_通用教师工具/batch_grade_submissions.R")
#
#   batch_grade(
#     input_file = "submissions.xlsx",
#     output_file = "RMedLearn_考试成绩.xlsx"
#   )
#
# 说明：
# - 不修改原始 submissions.xlsx / csv
# - 单个学生解码或评分失败，不会中断全班
# - 后台 dataset_id 只用于教师评分与核验
# - 本文件不写死题目数量和每题满分
# - 具体题目评分由 grader_script 指向的 grade_one_submission.R 决定
# - 只要新评分器仍返回 summary / question_scores / fill_detail / code_detail，本文件可继续复用
# ============================================================


# ---------- 默认路径 ----------
RML_DEFAULT_GRADER <- "grade_one_submission.R"


# ---------- 通用工具 ----------
.rml_batch_clean_code <- function(x) {
  if (is.null(x) || length(x) == 0L || is.na(x)) {
    return("")
  }

  gsub(
    "[[:space:]]+",
    "",
    as.character(x)
  )
}


.rml_batch_trim <- function(x) {
  if (is.null(x) || length(x) == 0L || is.na(x)) {
    return("")
  }

  trimws(as.character(x))
}


.rml_batch_blank <- function(x) {
  !nzchar(
    .rml_batch_trim(x)
  )
}


.rml_batch_question_answered <- function(q) {
  code_vals <- character(0)

  if (length(q$code_answers) > 0L) {
    code_vals <- vapply(
      q$code_answers,
      function(x) {
        paste(
          as.character(x),
          collapse = "\n"
        )
      },
      character(1)
    )
  }

  fill_vals <- character(0)

  if (length(q$fill_answers) > 0L) {
    fill_vals <- vapply(
      q$fill_answers,
      function(x) {
        .rml_batch_trim(x)
      },
      character(1)
    )
  }

  any(nzchar(trimws(code_vals))) ||
    any(nzchar(fill_vals))
}


.rml_batch_question_code <- function(record, question_id) {
  idx <- match(
    question_id,
    vapply(
      record$questions,
      function(q) q$question_id,
      character(1)
    )
  )

  if (is.na(idx)) {
    return("")
  }

  q <- record$questions[[idx]]

  if (length(q$code_answers) == 0L) {
    return("")
  }

  paste(
    unlist(
      q$code_answers,
      use.names = FALSE
    ),
    collapse = "\n"
  )
}


.rml_batch_read_input <- function(input_file) {
  if (!file.exists(input_file)) {
    stop(
      paste0(
        "找不到学生提交文件：\n",
        input_file
      ),
      call. = FALSE
    )
  }

  ext <- tolower(
    tools::file_ext(input_file)
  )

  if (identical(ext, "csv")) {
    out <- utils::read.csv(
      input_file,
      stringsAsFactors = FALSE,
      check.names = FALSE,
      fileEncoding = "UTF-8"
    )
  } else if (ext %in% c("xlsx", "xlsm")) {
    if (!requireNamespace("openxlsx", quietly = TRUE)) {
      stop(
        paste0(
          "教师端批量评分需要 openxlsx 包。\n",
          "请先运行：install.packages(\"openxlsx\")"
        ),
        call. = FALSE
      )
    }

    out <- openxlsx::read.xlsx(
      input_file,
      sheet = 1,
      colNames = TRUE,
      check.names = FALSE
    )
  } else {
    stop(
      "输入文件仅支持 .xlsx / .xlsm / .csv。",
      call. = FALSE
    )
  }

  # ----------------------------------------------------------
  # 输入表兼容规则
  # 只强制要求：学号 + 提交码
  # 姓名、班级、提交时间均为可选
  # 同时兼容中英文表头
  # ----------------------------------------------------------
  names(out) <- trimws(
    names(out)
  )

  aliases <- list(
    student_id = c(
      "student_id",
      "学号"
    ),
    student_name = c(
      "student_name",
      "姓名"
    ),
    class = c(
      "class",
      "班级"
    ),
    submission_code = c(
      "submission_code",
      "提交码",
      "考试提交码"
    ),
    submitted_at = c(
      "submitted_at",
      "提交时间"
    )
  )

  find_col <- function(candidates) {
    nms <- names(out)

    # 英文表头大小写不敏感；中文直接匹配
    hit <- match(
      tolower(candidates),
      tolower(nms)
    )

    hit <- hit[
      !is.na(hit)
    ]

    if (length(hit) == 0L) {
      return(NA_character_)
    }

    nms[
      hit[1]
    ]
  }

  mapped <- vapply(
    aliases,
    find_col,
    character(1)
  )

  required_internal <- c(
    "student_id",
    "submission_code"
  )

  missing_required <- required_internal[
    is.na(
      mapped[
        required_internal
      ]
    )
  ]

  if (length(missing_required) > 0L) {
    label_map <- c(
      student_id = "student_id（或“学号”）",
      submission_code = "submission_code（或“提交码/考试提交码”）"
    )

    stop(
      paste0(
        "提交表缺少必需字段：",
        paste(
          label_map[
            missing_required
          ],
          collapse = "；"
        ),
        "\n当前版本只强制要求“学号 + 提交码”两列。"
      ),
      call. = FALSE
    )
  }

  # 将识别到的列复制到内部统一字段名
  out$student_id <- as.character(
    out[[
      mapped[["student_id"]]
    ]]
  )

  out$submission_code <- as.character(
    out[[
      mapped[["submission_code"]]
    ]]
  )

  # 可选字段：不存在时自动补空列
  optional_internal <- c(
    "student_name",
    "class",
    "submitted_at"
  )

  for (nm in optional_internal) {
    src_name <- mapped[[nm]]

    if (is.na(src_name)) {
      out[[nm]] <- rep(
        "",
        nrow(out)
      )
    } else {
      out[[nm]] <- as.character(
        out[[src_name]]
      )
    }
  }

  out$student_id <- as.character(
    out$student_id
  )

  out$submission_code <- as.character(
    out$submission_code
  )

  out$submission_row <- seq_len(
    nrow(out)
  )

  out
}


.rml_batch_empty_df <- function(cols) {
  out <- as.data.frame(
    setNames(
      replicate(
        length(cols),
        character(0),
        simplify = FALSE
      ),
      cols
    ),
    stringsAsFactors = FALSE
  )

  out
}


.rml_batch_bind <- function(items) {
  items <- Filter(
    function(x) {
      is.data.frame(x) &&
        nrow(x) > 0L
    },
    items
  )

  if (length(items) == 0L) {
    return(data.frame())
  }

  all_names <- unique(
    unlist(
      lapply(
        items,
        names
      )
    )
  )

  items <- lapply(
    items,
    function(x) {
      miss <- setdiff(
        all_names,
        names(x)
      )

      for (nm in miss) {
        x[[nm]] <- NA
      }

      x[
        ,
        all_names,
        drop = FALSE
      ]
    }
  )

  do.call(
    rbind,
    items
  )
}


# ---------- 工作簿样式 ----------
.rml_batch_excel_styles <- function() {
  list(
    title = openxlsx::createStyle(
      fontSize = 15,
      textDecoration = "bold",
      fontColour = "#2B4057"
    ),

    header = openxlsx::createStyle(
      fontSize = 10,
      textDecoration = "bold",
      fontColour = "#FFFFFF",
      fgFill = "#40566D",
      halign = "center",
      valign = "center",
      border = "Bottom",
      borderColour = "#D9DEE4"
    ),

    subheader = openxlsx::createStyle(
      fontSize = 10,
      textDecoration = "bold",
      fontColour = "#2B4057",
      fgFill = "#EEF3F7",
      halign = "left",
      valign = "center",
      border = "Bottom",
      borderColour = "#D9DEE4"
    ),

    body = openxlsx::createStyle(
      fontSize = 10,
      valign = "top"
    ),

    wrap = openxlsx::createStyle(
      fontSize = 10,
      valign = "top",
      wrapText = TRUE
    ),

    number2 = openxlsx::createStyle(
      numFmt = "0.00"
    ),

    pct2 = openxlsx::createStyle(
      numFmt = "0.00%"
    ),

    warning = openxlsx::createStyle(
      fgFill = "#FFF4E5",
      fontColour = "#7A4A00"
    ),

    error = openxlsx::createStyle(
      fgFill = "#FDECEC",
      fontColour = "#9D2D2D"
    )
  )
}


.rml_batch_write_table <- function(
  wb,
  sheet,
  data,
  start_row = 1,
  start_col = 1,
  title = NULL,
  styles = NULL,
  table_name = NULL
) {
  if (is.null(styles)) {
    styles <- .rml_batch_excel_styles()
  }

  row <- start_row

  if (!is.null(title)) {
    openxlsx::writeData(
      wb,
      sheet,
      title,
      startRow = row,
      startCol = start_col
    )

    openxlsx::addStyle(
      wb,
      sheet,
      styles$title,
      rows = row,
      cols = start_col,
      gridExpand = TRUE
    )

    row <- row + 2L
  }

  if (ncol(data) == 0L) {
    openxlsx::writeData(
      wb,
      sheet,
      "无记录",
      startRow = row,
      startCol = start_col
    )

    return(
      invisible(
        list(
          header_row = row,
          end_row = row
        )
      )
    )
  }

  if (nrow(data) == 0L) {
    openxlsx::writeData(
      wb,
      sheet,
      as.data.frame(
        setNames(
          as.list(
            rep(
              NA_character_,
              ncol(data)
            )
          ),
          names(data)
        ),
        stringsAsFactors = FALSE
      ),
      startRow = row,
      startCol = start_col,
      colNames = TRUE
    )

    end_row <- row + 1L
  } else {
    openxlsx::writeData(
      wb,
      sheet,
      data,
      startRow = row,
      startCol = start_col,
      colNames = TRUE,
      keepNA = TRUE,
      na.string = ""
    )

    end_row <- row + nrow(data)
  }

  openxlsx::addStyle(
    wb,
    sheet,
    styles$header,
    rows = row,
    cols = start_col:(
      start_col +
        ncol(data) -
        1L
    ),
    gridExpand = TRUE
  )

  if (nrow(data) > 0L) {
    openxlsx::addStyle(
      wb,
      sheet,
      styles$body,
      rows = (
        row +
          1L
      ):end_row,
      cols = start_col:(
        start_col +
          ncol(data) -
          1L
      ),
      gridExpand = TRUE
    )
  }

  openxlsx::freezePane(
    wb,
    sheet,
    firstActiveRow = row + 1L,
    firstActiveCol = start_col
  )

  if (
    !is.null(table_name) &&
      nrow(data) > 0L
  ) {
    openxlsx::addFilter(
      wb,
      sheet,
      row = row,
      cols = start_col:(
        start_col +
          ncol(data) -
          1L
      )
    )
  }

  invisible(
    list(
      header_row = row,
      end_row = end_row
    )
  )
}


.rml_batch_set_widths <- function(
  wb,
  sheet,
  data
) {
  if (ncol(data) == 0L) {
    return(invisible(NULL))
  }

  widths <- vapply(
    names(data),
    function(nm) {
      if (
        nm %in% c(
          "submission_code",
          "answer",
          "student_code",
          "note",
          "issue",
          "suggestion",
          "question_title",
          "title",
          "学生代码",
          "学生原始作答",
          "系统判定说明",
          "问题说明",
          "复核建议",
          "题目"
        )
      ) {
        35
      } else if (
        nm %in% c(
          "student_name",
          "class",
          "question_id",
          "answer_id",
          "answer_type",
          "status",
          "decode_status",
          "grading_status",
          "manual_review",
          "type",
          "姓名",
          "班级",
          "题号",
          "填空编号",
          "答案编号",
          "作答类型",
          "作答状态",
          "解码状态",
          "评分状态",
          "是否人工复核",
          "类型"
        )
      ) {
        16
      } else if (
        grepl(
          "time|date|submitted|started|completed",
          nm,
          ignore.case = TRUE
        )
      ) {
        21
      } else {
        13
      }
    },
    numeric(1)
  )

  openxlsx::setColWidths(
    wb,
    sheet,
    cols = seq_along(widths),
    widths = widths
  )

  invisible(widths)
}



# ---------- 输出表中文化 ----------
.rml_batch_cn_summary <- function(x) {
  map <- c(
    submission_row = "提交序号",
    student_id = "提交表学号",
    student_name = "姓名",
    class = "班级",
    decoded_student_id = "提交码学号",
    dataset_id = "数据集编号",
    exam_id = "考试编号",
    started_at = "考试开始时间",
    completed_at = "考试完成时间",
    exam_submitted_at = "考试记录生成时间",
    form_submitted_at = "平台提交时间",
    elapsed_minutes = "答题时长（分钟）",
    code_score = "代码得分",
    code_max = "代码满分",
    fill_score = "填空得分",
    fill_max = "填空满分",
    total_score = "总分",
    total_max = "总满分",
    unanswered_questions = "未作答题数",
    manual_review_count = "人工复核项数",
    duplicate_count = "该学号提交次数",
    decode_status = "解码状态",
    grading_status = "评分状态"
  )

  idx <- match(names(x), names(map))
  hit <- !is.na(idx)
  names(x)[hit] <- unname(map[idx[hit]])

  x
}


.rml_batch_cn_questions <- function(x) {
  nms <- names(x)

  fixed <- c(
    submission_row = "提交序号",
    student_id = "提交表学号",
    decoded_student_id = "提交码学号",
    student_name = "姓名",
    class = "班级",
    dataset_id = "数据集编号",
    code_total = "代码总分",
    fill_total = "填空总分",
    total_score = "总分"
  )

  idx <- match(nms, names(fixed))
  hit <- !is.na(idx)
  nms[hit] <- unname(fixed[idx[hit]])

  nms <- sub(
    "^(Q[0-9]{2})_code$",
    "\\1_代码",
    nms
  )

  nms <- sub(
    "^(Q[0-9]{2})_fill$",
    "\\1_填空",
    nms
  )

  nms <- sub(
    "^(Q[0-9]{2})_total$",
    "\\1_总分",
    nms
  )

  names(x) <- nms
  x
}


.rml_batch_cn_fill <- function(x) {
  map <- c(
    submission_row = "提交序号",
    student_id = "提交表学号",
    decoded_student_id = "提交码学号",
    student_name = "姓名",
    class = "班级",
    dataset_id = "数据集编号",
    question_id = "题号",
    answer_id = "填空编号",
    expected = "标准答案",
    submitted = "学生答案",
    correct = "是否正确",
    answer_status = "作答状态",
    points = "满分",
    earned = "得分"
  )

  idx <- match(names(x), names(map))
  hit <- !is.na(idx)
  names(x)[hit] <- unname(map[idx[hit]])

  if ("是否正确" %in% names(x)) {
    x[["是否正确"]] <- vapply(
      seq_len(nrow(x)),
      function(i) {
        if (
          "作答状态" %in% names(x) &&
          identical(
            as.character(x[["作答状态"]][i]),
            "未作答"
          )
        ) {
          return("")
        }

        if (isTRUE(x[["是否正确"]][i])) {
          "是"
        } else {
          "否"
        }
      },
      character(1)
    )
  }

  x
}


.rml_batch_cn_code <- function(x) {
  map <- c(
    submission_row = "提交序号",
    student_id = "提交表学号",
    decoded_student_id = "提交码学号",
    student_name = "姓名",
    class = "班级",
    dataset_id = "数据集编号",
    question_id = "题号",
    item = "评分点",
    points = "满分",
    earned = "自动得分",
    note = "系统判定说明",
    student_code = "学生代码",
    manual_review = "是否人工复核"
  )

  idx <- match(names(x), names(map))
  hit <- !is.na(idx)
  names(x)[hit] <- unname(map[idx[hit]])

  if ("是否人工复核" %in% names(x)) {
    x[["是否人工复核"]] <- ifelse(
      x[["是否人工复核"]] %in% TRUE,
      "是",
      "否"
    )
  }

  x
}


.rml_batch_cn_raw <- function(x) {
  map <- c(
    submission_row = "提交序号",
    student_id = "提交表学号",
    decoded_student_id = "提交码学号",
    student_name = "姓名",
    class = "班级",
    dataset_id = "数据集编号",
    question_id = "题号",
    question_title = "题目",
    answer_type = "作答类型",
    answer_id = "答案编号",
    answer = "学生原始作答"
  )

  idx <- match(names(x), names(map))
  hit <- !is.na(idx)
  names(x)[hit] <- unname(map[idx[hit]])

  if ("作答类型" %in% names(x)) {
    x[["作答类型"]] <- ifelse(
      x[["作答类型"]] == "code",
      "代码",
      ifelse(
        x[["作答类型"]] == "fill",
        "填空",
        x[["作答类型"]]
      )
    )
  }

  x
}


.rml_batch_cn_issues <- function(x) {
  map <- c(
    submission_row = "提交序号",
    student_id = "提交表学号",
    student_name = "姓名",
    class = "班级",
    type = "类型",
    question_id = "题号",
    issue = "问题说明",
    suggestion = "复核建议",
    manual_review = "是否人工复核"
  )

  idx <- match(names(x), names(map))
  hit <- !is.na(idx)
  names(x)[hit] <- unname(map[idx[hit]])

  if ("是否人工复核" %in% names(x)) {
    x[["是否人工复核"]] <- ifelse(
      x[["是否人工复核"]] %in% TRUE,
      "是",
      "否"
    )
  }

  x
}


.rml_batch_cn_question_stats <- function(x) {
  map <- c(
    question_id = "题号",
    title = "题目",
    max_score = "满分",
    mean_score = "平均分",
    sd_score = "标准差",
    median_score = "中位数",
    score_rate = "得分率",
    full_score_n = "满分人数",
    zero_score_n = "零分人数"
  )

  idx <- match(names(x), names(map))
  hit <- !is.na(idx)
  names(x)[hit] <- unname(map[idx[hit]])

  x
}


# ---------- 主函数 ----------
batch_grade <- function(
  input_file,
  output_file = NULL,
  grader_script = RML_DEFAULT_GRADER,
  dataset_dir = "datasets",
  key_path = "exam_key_teacher.rds",
  expected_exam_id = NULL,
  pass_score = 60
) {
  if (!requireNamespace("RMedLearn", quietly = TRUE)) {
    stop(
      "请先安装当前版本的 RMedLearn。",
      call. = FALSE
    )
  }

  if (!requireNamespace("openxlsx", quietly = TRUE)) {
    stop(
      paste0(
        "批量评分与 Excel 导出需要 openxlsx。\n",
        "请先运行：install.packages(\"openxlsx\")"
      ),
      call. = FALSE
    )
  }

  if (!file.exists(grader_script)) {
    stop(
      paste0(
        "找不到单份评分器：\n",
        grader_script
      ),
      call. = FALSE
    )
  }

  if (!dir.exists(dataset_dir)) {
    stop(
      paste0(
        "找不到考试数据集文件夹：\n",
        dataset_dir
      ),
      call. = FALSE
    )
  }

  if (!file.exists(key_path)) {
    stop(
      paste0(
        "找不到标准答案文件：\n",
        key_path,
        "\n请先运行 make_exam_key.R 生成 exam_key_teacher.rds。"
      ),
      call. = FALSE
    )
  }

  source(
    grader_script,
    encoding = "UTF-8"
  )

  if (!exists(
    "grade_one_submission",
    mode = "function"
  )) {
    stop(
      "grade_one_submission.R 中未找到 grade_one_submission()。",
      call. = FALSE
    )
  }

  submissions <- .rml_batch_read_input(
    input_file
  )

  if (is.null(output_file)) {
    stem <- tools::file_path_sans_ext(
      basename(input_file)
    )

    output_file <- file.path(
      dirname(input_file),
      paste0(
        stem,
        "_results.xlsx"
      )
    )
  }

  duplicate_counts <- table(
    submissions$student_id
  )

  decode_fun <- getFromNamespace(
    "decode_exam_submission",
    "RMedLearn"
  )

  summary_rows <- list()
  question_rows <- list()
  fill_rows <- list()
  code_rows <- list()
  raw_rows <- list()
  issue_rows <- list()
  question_meta_rows <- list()

  for (i in seq_len(
    nrow(submissions)
  )) {
    row <- submissions[
      i,
      ,
      drop = FALSE
    ]

    input_student_id <- .rml_batch_trim(
      row$student_id
    )

    code <- .rml_batch_clean_code(
      row$submission_code
    )

    dup_n <- unname(
      duplicate_counts[
        input_student_id
      ]
    )

    if (length(dup_n) == 0L ||
        is.na(dup_n)) {
      dup_n <- 0L
    }

    base_meta <- list(
      submission_row = i,
      student_id = input_student_id,
      student_name = .rml_batch_trim(
        row$student_name
      ),
      class = .rml_batch_trim(
        row$class
      ),
      form_submitted_at = .rml_batch_trim(
        row$submitted_at
      ),
      duplicate_count = as.integer(
        dup_n
      )
    )

    if (!nzchar(code)) {
      summary_rows[[length(
        summary_rows
      ) + 1L]] <- data.frame(
        submission_row = i,
        student_id = input_student_id,
        student_name = base_meta$student_name,
        class = base_meta$class,
        decoded_student_id = "",
        dataset_id = NA_integer_,
        exam_id = "",
        started_at = "",
        completed_at = "",
        exam_submitted_at = "",
        form_submitted_at = base_meta$form_submitted_at,
        elapsed_minutes = NA_real_,
        code_score = NA_real_,
        code_max = 70,
        fill_score = NA_real_,
        fill_max = 30,
        total_score = NA_real_,
        total_max = 100,
        unanswered_questions = NA_integer_,
        manual_review_count = 1L,
        duplicate_count = base_meta$duplicate_count,
        decode_status = "FAILED",
        grading_status = "未评分",
        stringsAsFactors = FALSE
      )

      issue_rows[[length(
        issue_rows
      ) + 1L]] <- data.frame(
        submission_row = i,
        student_id = input_student_id,
        student_name = base_meta$student_name,
        class = base_meta$class,
        type = "提交码异常",
        question_id = "",
        issue = "submission_code 为空",
        suggestion = "联系学生核对原始提交记录。",
        manual_review = TRUE,
        stringsAsFactors = FALSE
      )

      next
    }

    record <- tryCatch(
      decode_fun(code),
      error = function(e) e
    )

    if (inherits(
      record,
      "error"
    )) {
      summary_rows[[length(
        summary_rows
      ) + 1L]] <- data.frame(
        submission_row = i,
        student_id = input_student_id,
        student_name = base_meta$student_name,
        class = base_meta$class,
        decoded_student_id = "",
        dataset_id = NA_integer_,
        exam_id = "",
        started_at = "",
        completed_at = "",
        exam_submitted_at = "",
        form_submitted_at = base_meta$form_submitted_at,
        elapsed_minutes = NA_real_,
        code_score = NA_real_,
        code_max = 70,
        fill_score = NA_real_,
        fill_max = 30,
        total_score = NA_real_,
        total_max = 100,
        unanswered_questions = NA_integer_,
        manual_review_count = 1L,
        duplicate_count = base_meta$duplicate_count,
        decode_status = "FAILED",
        grading_status = "未评分",
        stringsAsFactors = FALSE
      )

      issue_rows[[length(
        issue_rows
      ) + 1L]] <- data.frame(
        submission_row = i,
        student_id = input_student_id,
        student_name = base_meta$student_name,
        class = base_meta$class,
        type = "解码失败",
        question_id = "",
        issue = conditionMessage(record),
        suggestion = "检查提交码是否完整、是否被修改或截断。",
        manual_review = TRUE,
        stringsAsFactors = FALSE
      )

      next
    }

    decoded_student_id <- as.character(
      record$student_id
    )

    dataset_id <- suppressWarnings(
      as.integer(
        record$dataset_id
      )
    )

    row_issues <- list()

    if (!identical(
      input_student_id,
      decoded_student_id
    )) {
      row_issues[[length(
        row_issues
      ) + 1L]] <- data.frame(
        submission_row = i,
        student_id = input_student_id,
        student_name = base_meta$student_name,
        class = base_meta$class,
        type = "学号不一致",
        question_id = "",
        issue = paste0(
          "提交表学号=",
          input_student_id,
          "；提交码内学号=",
          decoded_student_id
        ),
        suggestion = "核对学生身份和提交记录；评分以提交码内记录为基础。",
        manual_review = TRUE,
        stringsAsFactors = FALSE
      )
    }

    if (
      is.na(dataset_id) ||
        !(dataset_id %in% 1:10)
    ) {
      row_issues[[length(
        row_issues
      ) + 1L]] <- data.frame(
        submission_row = i,
        student_id = input_student_id,
        student_name = base_meta$student_name,
        class = base_meta$class,
        type = "数据集编号异常",
        question_id = "",
        issue = paste0(
          "dataset_id=",
          record$dataset_id
        ),
        suggestion = "停止自动确认成绩，人工核验提交记录。",
        manual_review = TRUE,
        stringsAsFactors = FALSE
      )
    }

    if (
      !is.null(expected_exam_id) &&
        nzchar(expected_exam_id) &&
        !identical(
          as.character(
            record$exam_id
          ),
          as.character(
            expected_exam_id
          )
        )
    ) {
      row_issues[[length(
        row_issues
      ) + 1L]] <- data.frame(
        submission_row = i,
        student_id = input_student_id,
        student_name = base_meta$student_name,
        class = base_meta$class,
        type = "试卷编号不一致",
        question_id = "",
        issue = paste0(
          "提交码 exam_id=",
          record$exam_id,
          "；期望 exam_id=",
          expected_exam_id
        ),
        suggestion = "核对学生是否提交了错误考试的提交码。",
        manual_review = TRUE,
        stringsAsFactors = FALSE
      )
    }

    if (base_meta$duplicate_count > 1L) {
      row_issues[[length(
        row_issues
      ) + 1L]] <- data.frame(
        submission_row = i,
        student_id = input_student_id,
        student_name = base_meta$student_name,
        class = base_meta$class,
        type = "重复提交",
        question_id = "",
        issue = paste0(
          "提交表中该 student_id 共出现 ",
          base_meta$duplicate_count,
          " 次。"
        ),
        suggestion = "人工决定保留哪一次提交；当前结果保留全部记录，不自动覆盖。",
        manual_review = TRUE,
        stringsAsFactors = FALSE
      )
    }

    # 原始作答：无论后续评分是否成功都先保留
    for (q in record$questions) {
      qid <- q$question_id

      if (length(
        q$code_answers
      ) > 0L) {
        for (nm in names(
          q$code_answers
        )) {
          raw_rows[[length(
            raw_rows
          ) + 1L]] <- data.frame(
            submission_row = i,
            student_id = input_student_id,
            decoded_student_id = decoded_student_id,
            student_name = base_meta$student_name,
            class = base_meta$class,
            dataset_id = dataset_id,
            question_id = qid,
            question_title = q$title,
            answer_type = "code",
            answer_id = nm,
            answer = paste(
              as.character(
                q$code_answers[[nm]]
              ),
              collapse = "\n"
            ),
            stringsAsFactors = FALSE
          )
        }
      }

      if (length(
        q$fill_answers
      ) > 0L) {
        for (nm in names(
          q$fill_answers
        )) {
          raw_rows[[length(
            raw_rows
          ) + 1L]] <- data.frame(
            submission_row = i,
            student_id = input_student_id,
            decoded_student_id = decoded_student_id,
            student_name = base_meta$student_name,
            class = base_meta$class,
            dataset_id = dataset_id,
            question_id = qid,
            question_title = q$title,
            answer_type = "fill",
            answer_id = nm,
            answer = .rml_batch_trim(
              q$fill_answers[[nm]]
            ),
            stringsAsFactors = FALSE
          )
        }
      }
    }

    grade <- tryCatch(
      grade_one_submission(
        record,
        dataset_dir = dataset_dir,
        key_path = key_path
      ),
      error = function(e) e
    )

    if (inherits(
      grade,
      "error"
    )) {
      issue_rows <- c(
        issue_rows,
        row_issues,
        list(
          data.frame(
            submission_row = i,
            student_id = input_student_id,
            student_name = base_meta$student_name,
            class = base_meta$class,
            type = "评分失败",
            question_id = "",
            issue = conditionMessage(
              grade
            ),
            suggestion = "保留原始答卷并人工复核评分器错误。",
            manual_review = TRUE,
            stringsAsFactors = FALSE
          )
        )
      )

      summary_rows[[length(
        summary_rows
      ) + 1L]] <- data.frame(
        submission_row = i,
        student_id = input_student_id,
        student_name = base_meta$student_name,
        class = base_meta$class,
        decoded_student_id = decoded_student_id,
        dataset_id = dataset_id,
        exam_id = as.character(
          record$exam_id
        ),
        started_at = as.character(
          record$started_at
        ),
        completed_at = as.character(
          record$completed_at
        ),
        exam_submitted_at = as.character(
          record$submitted_at
        ),
        form_submitted_at = base_meta$form_submitted_at,
        elapsed_minutes = suppressWarnings(
          as.numeric(
            record$elapsed_minutes
          )
        ),
        code_score = NA_real_,
        code_max = 70,
        fill_score = NA_real_,
        fill_max = 30,
        total_score = NA_real_,
        total_max = 100,
        unanswered_questions = sum(
          !vapply(
            record$questions,
            .rml_batch_question_answered,
            logical(1)
          )
        ),
        manual_review_count = length(
          row_issues
        ) + 1L,
        duplicate_count = base_meta$duplicate_count,
        decode_status = "OK",
        grading_status = "评分失败",
        stringsAsFactors = FALSE
      )

      next
    }

    # 填空明细
    f <- grade$fill_detail

    # 区分“未作答”和“答错”
    f$answer_status <- vapply(
      seq_len(nrow(f)),
      function(k) {
        submitted_value <- if (
          "submitted" %in% names(f)
        ) {
          .rml_batch_trim(f$submitted[k])
        } else {
          ""
        }

        if (!nzchar(submitted_value)) {
          return("未作答")
        }

        is_correct <- if (
          "correct" %in% names(f)
        ) {
          isTRUE(f$correct[k])
        } else {
          FALSE
        }

        if (is_correct) {
          "正确"
        } else {
          "错误"
        }
      },
      character(1)
    )

    f$submission_row <- i
    f$student_id <- input_student_id
    f$decoded_student_id <- decoded_student_id
    f$student_name <- base_meta$student_name
    f$class <- base_meta$class
    f$dataset_id <- dataset_id

    fill_rows[[length(
      fill_rows
    ) + 1L]] <- f[
      ,
      c(
        "submission_row",
        "student_id",
        "decoded_student_id",
        "student_name",
        "class",
        "dataset_id",
        setdiff(
          names(f),
          c(
            "submission_row",
            "student_id",
            "decoded_student_id",
            "student_name",
            "class",
            "dataset_id"
          )
        )
      ),
      drop = FALSE
    ]

    # 代码明细 + 原始学生代码 + 人工复核标记
    cdet <- grade$code_detail

    cdet$student_code <- vapply(
      cdet$question_id,
      function(qid) {
        .rml_batch_question_code(
          record,
          qid
        )
      },
      character(1)
    )

    cdet$manual_review <- (
      cdet$earned <
        cdet$points
    ) &
      nzchar(
        trimws(
          cdet$student_code
        )
      )

    cdet$submission_row <- i
    cdet$student_id <- input_student_id
    cdet$decoded_student_id <- decoded_student_id
    cdet$student_name <- base_meta$student_name
    cdet$class <- base_meta$class
    cdet$dataset_id <- dataset_id

    code_rows[[length(
      code_rows
    ) + 1L]] <- cdet[
      ,
      c(
        "submission_row",
        "student_id",
        "decoded_student_id",
        "student_name",
        "class",
        "dataset_id",
        "question_id",
        "item",
        "points",
        "earned",
        "note",
        "student_code",
        "manual_review"
      ),
      drop = FALSE
    ]

    # 被扣分且学生确实写过代码的评分点，进入人工复核 Sheet
    review_rows <- cdet[
      cdet$manual_review,
      ,
      drop = FALSE
    ]

    if (nrow(
      review_rows
    ) > 0L) {
      for (j in seq_len(
        nrow(review_rows)
      )) {
        row_issues[[length(
          row_issues
        ) + 1L]] <- data.frame(
          submission_row = i,
          student_id = input_student_id,
          student_name = base_meta$student_name,
          class = base_meta$class,
          type = "代码人工复核",
          question_id = review_rows$question_id[
            j
          ],
          issue = paste0(
            review_rows$item[
              j
            ],
            "：自动得分 ",
            review_rows$earned[
              j
            ],
            "/",
            review_rows$points[
              j
            ],
            "；",
            review_rows$note[
              j
            ]
          ),
          suggestion = "检查学生是否使用了等价但评分器尚未识别的正确写法。",
          manual_review = TRUE,
          stringsAsFactors = FALSE
        )
      }
    }

    issue_rows <- c(
      issue_rows,
      row_issues
    )

    # 分题得分：转为宽表，一名学生一行
    qs <- grade$question_scores

    if (
      is.data.frame(qs) &&
      nrow(qs) > 0L
    ) {
      question_meta_rows[[length(question_meta_rows) + 1L]] <- qs[
        ,
        intersect(
          c(
            "question_id",
            "title",
            "code_max",
            "fill_max",
            "total_max"
          ),
          names(qs)
        ),
        drop = FALSE
      ]
    }

    wide <- data.frame(
      submission_row = i,
      student_id = input_student_id,
      decoded_student_id = decoded_student_id,
      student_name = base_meta$student_name,
      class = base_meta$class,
      dataset_id = dataset_id,
      stringsAsFactors = FALSE
    )

    for (j in seq_len(
      nrow(qs)
    )) {
      qid <- qs$question_id[
        j
      ]

      wide[[
        paste0(
          qid,
          "_code"
        )
      ]] <- qs$code_score[
        j
      ]

      wide[[
        paste0(
          qid,
          "_fill"
        )
      ]] <- qs$fill_score[
        j
      ]

      wide[[
        paste0(
          qid,
          "_total"
        )
      ]] <- qs$total_score[
        j
      ]
    }

    wide$code_total <- grade$summary$code_score
    wide$fill_total <- grade$summary$fill_score
    wide$total_score <- grade$summary$total_score

    question_rows[[length(
      question_rows
    ) + 1L]] <- wide

    unanswered <- sum(
      !vapply(
        record$questions,
        .rml_batch_question_answered,
        logical(1)
      )
    )

    manual_n <- length(
      Filter(
        function(x) {
          is.data.frame(x) &&
            nrow(x) > 0L
        },
        row_issues
      )
    )

    summary_rows[[length(
      summary_rows
    ) + 1L]] <- data.frame(
      submission_row = i,
      student_id = input_student_id,
      student_name = base_meta$student_name,
      class = base_meta$class,
      decoded_student_id = decoded_student_id,
      dataset_id = dataset_id,
      exam_id = as.character(
        record$exam_id
      ),
      started_at = as.character(
        record$started_at
      ),
      completed_at = as.character(
        record$completed_at
      ),
      exam_submitted_at = as.character(
        record$submitted_at
      ),
      form_submitted_at = base_meta$form_submitted_at,
      elapsed_minutes = suppressWarnings(
        as.numeric(
          record$elapsed_minutes
        )
      ),
      code_score = grade$summary$code_score,
      code_max = grade$summary$code_max,
      fill_score = grade$summary$fill_score,
      fill_max = grade$summary$fill_max,
      total_score = grade$summary$total_score,
      total_max = grade$summary$total_max,
      unanswered_questions = unanswered,
      manual_review_count = manual_n,
      duplicate_count = base_meta$duplicate_count,
      decode_status = "OK",
      grading_status = if (
        manual_n > 0L
      ) {
        "自动评分完成-待复核"
      } else {
        "自动评分完成"
      },
      stringsAsFactors = FALSE
    )
  }

  summary_df <- .rml_batch_bind(
    summary_rows
  )

  question_df <- .rml_batch_bind(
    question_rows
  )

  fill_df <- .rml_batch_bind(
    fill_rows
  )

  code_df <- .rml_batch_bind(
    code_rows
  )

  raw_df <- .rml_batch_bind(
    raw_rows
  )

  issue_df <- .rml_batch_bind(
    issue_rows
  )

  question_meta_df <- .rml_batch_bind(
    question_meta_rows
  )

  if (
    is.data.frame(question_meta_df) &&
    nrow(question_meta_df) > 0L &&
    "question_id" %in% names(question_meta_df)
  ) {
    question_meta_df <- question_meta_df[
      !duplicated(
        question_meta_df$question_id
      ),
      ,
      drop = FALSE
    ]
  }

  # ---------- 考试统计 ----------
  valid <- summary_df[
    !is.na(
      summary_df$total_score
    ),
    ,
    drop = FALSE
  ]

  total_submissions <- nrow(
    submissions
  )

  unique_students <- length(
    unique(
      submissions$student_id[
        nzchar(
          trimws(
            submissions$student_id
          )
        )
      ]
    )
  )

  decoded_success <- if (
    nrow(summary_df) > 0L
  ) {
    sum(
      summary_df$decode_status == "OK",
      na.rm = TRUE
    )
  } else {
    0L
  }

  graded_success <- nrow(
    valid
  )

  duplicate_students <- sum(
    duplicate_counts > 1L
  )

  mean_total <- if (
    nrow(valid) > 0L
  ) {
    mean(
      valid$total_score,
      na.rm = TRUE
    )
  } else {
    NA_real_
  }

  sd_total <- if (
    nrow(valid) > 1L
  ) {
    stats::sd(
      valid$total_score,
      na.rm = TRUE
    )
  } else {
    NA_real_
  }

  median_total <- if (
    nrow(valid) > 0L
  ) {
    stats::median(
      valid$total_score,
      na.rm = TRUE
    )
  } else {
    NA_real_
  }

  min_total <- if (
    nrow(valid) > 0L
  ) {
    min(
      valid$total_score,
      na.rm = TRUE
    )
  } else {
    NA_real_
  }

  max_total <- if (
    nrow(valid) > 0L
  ) {
    max(
      valid$total_score,
      na.rm = TRUE
    )
  } else {
    NA_real_
  }

  pass_rate <- if (
    nrow(valid) > 0L
  ) {
    mean(
      valid$total_score >=
        pass_score,
      na.rm = TRUE
    )
  } else {
    NA_real_
  }

  mean_code <- if (
    nrow(valid) > 0L
  ) {
    mean(
      valid$code_score,
      na.rm = TRUE
    )
  } else {
    NA_real_
  }

  mean_fill <- if (
    nrow(valid) > 0L
  ) {
    mean(
      valid$fill_score,
      na.rm = TRUE
    )
  } else {
    NA_real_
  }

  mean_elapsed <- if (
    nrow(valid) > 0L
  ) {
    mean(
      valid$elapsed_minutes,
      na.rm = TRUE
    )
  } else {
    NA_real_
  }

  overview_stats <- data.frame(
    指标 = c(
      "原始提交记录数",
      "唯一学号数",
      "解码成功数",
      "评分成功数",
      "存在重复提交的学号数",
      "平均总分",
      "总分标准差",
      "总分中位数",
      "最低分",
      "最高分",
      paste0(
        "及格率（≥",
        pass_score,
        "）"
      ),
      "平均代码分",
      "平均填空分",
      "平均答题时长（分钟）"
    ),
    数值 = c(
      total_submissions,
      unique_students,
      decoded_success,
      graded_success,
      duplicate_students,
      round(
        mean_total,
        2
      ),
      round(
        sd_total,
        2
      ),
      round(
        median_total,
        2
      ),
      round(
        min_total,
        2
      ),
      round(
        max_total,
        2
      ),
      pass_rate,
      round(
        mean_code,
        2
      ),
      round(
        mean_fill,
        2
      ),
      round(
        mean_elapsed,
        2
      )
    ),
    stringsAsFactors = FALSE
  )

  question_stats <- data.frame()

  if (
    nrow(question_df) > 0L &&
    nrow(question_meta_df) > 0L
  ) {
    qids <- as.character(
      question_meta_df$question_id
    )

    stats_rows <- lapply(
      qids,
      function(qid) {
        col <- paste0(
          qid,
          "_total"
        )

        if (
          !(col %in% names(question_df))
        ) {
          return(NULL)
        }

        vals <- suppressWarnings(
          as.numeric(
            question_df[[col]]
          )
        )

        pos <- match(
          qid,
          question_meta_df$question_id
        )

        max_score <- suppressWarnings(
          as.numeric(
            question_meta_df$total_max[
              pos
            ]
          )
        )

        if (
          is.na(max_score) ||
          max_score <= 0
        ) {
          return(NULL)
        }

        title <- if (
          "title" %in%
            names(
              question_meta_df
            )
        ) {
          as.character(
            question_meta_df$title[
              pos
            ]
          )
        } else {
          ""
        }

        data.frame(
          question_id = qid,
          title = title,
          max_score = max_score,
          mean_score = round(
            mean(
              vals,
              na.rm = TRUE
            ),
            2
          ),
          sd_score = round(
            stats::sd(
              vals,
              na.rm = TRUE
            ),
            2
          ),
          median_score = round(
            stats::median(
              vals,
              na.rm = TRUE
            ),
            2
          ),
          score_rate = round(
            mean(
              vals,
              na.rm = TRUE
            ) /
              max_score,
            4
          ),
          full_score_n = sum(
            vals == max_score,
            na.rm = TRUE
          ),
          zero_score_n = sum(
            vals == 0,
            na.rm = TRUE
          ),
          stringsAsFactors = FALSE
        )
      }
    )

    stats_rows <- Filter(
      Negate(is.null),
      stats_rows
    )

    if (
      length(stats_rows) > 0L
    ) {
      question_stats <- do.call(
        rbind,
        stats_rows
      )
    }
  }

  # ---------- 导出 Excel ----------
  # 内部评分对象继续使用英文变量名；
  # 这里只生成用于 Excel 展示的中文表头版本。
  summary_out <- .rml_batch_cn_summary(summary_df)
  question_out <- .rml_batch_cn_questions(question_df)
  fill_out <- .rml_batch_cn_fill(fill_df)
  code_out <- .rml_batch_cn_code(code_df)
  raw_out <- .rml_batch_cn_raw(raw_df)
  issue_out <- .rml_batch_cn_issues(issue_df)
  question_stats_out <- .rml_batch_cn_question_stats(question_stats)

  wb <- openxlsx::createWorkbook(
    creator = "RMedLearn"
  )

  styles <- .rml_batch_excel_styles()

  sheets <- c(
    "班级总成绩",
    "分题得分",
    "填空评分明细",
    "代码评分明细",
    "学生原始作答",
    "异常与人工复核",
    "考试统计"
  )

  for (s in sheets) {
    openxlsx::addWorksheet(
      wb,
      s,
      gridLines = TRUE
    )
  }

  .rml_batch_write_table(
    wb,
    "班级总成绩",
    summary_out,
    title = "RMedLearn 班级总成绩",
    styles = styles
  )
  .rml_batch_set_widths(
    wb,
    "班级总成绩",
    summary_out
  )

  .rml_batch_write_table(
    wb,
    "分题得分",
    question_out,
    title = "RMedLearn 分题得分",
    styles = styles
  )
  .rml_batch_set_widths(
    wb,
    "分题得分",
    question_out
  )

  .rml_batch_write_table(
    wb,
    "填空评分明细",
    fill_out,
    title = "RMedLearn 填空评分明细",
    styles = styles
  )
  .rml_batch_set_widths(
    wb,
    "填空评分明细",
    fill_out
  )

  .rml_batch_write_table(
    wb,
    "代码评分明细",
    code_out,
    title = "RMedLearn 代码评分明细",
    styles = styles
  )
  .rml_batch_set_widths(
    wb,
    "代码评分明细",
    code_out
  )

  if (
    nrow(code_df) > 0L &&
      "学生代码" %in%
        names(code_out)
  ) {
    code_col <- match(
      "学生代码",
      names(code_out)
    )

    openxlsx::addStyle(
      wb,
      "代码评分明细",
      styles$wrap,
      rows = 4:(
        3 +
          nrow(code_out)
      ),
      cols = code_col,
      gridExpand = TRUE,
      stack = TRUE
    )
  }

  .rml_batch_write_table(
    wb,
    "学生原始作答",
    raw_out,
    title = "RMedLearn 学生原始作答",
    styles = styles
  )
  .rml_batch_set_widths(
    wb,
    "学生原始作答",
    raw_out
  )

  if (
    nrow(raw_df) > 0L &&
      "学生原始作答" %in%
        names(raw_out)
  ) {
    answer_col <- match(
      "学生原始作答",
      names(raw_out)
    )

    openxlsx::addStyle(
      wb,
      "学生原始作答",
      styles$wrap,
      rows = 4:(
        3 +
          nrow(raw_out)
      ),
      cols = answer_col,
      gridExpand = TRUE,
      stack = TRUE
    )
  }

  .rml_batch_write_table(
    wb,
    "异常与人工复核",
    issue_out,
    title = "RMedLearn 异常与人工复核",
    styles = styles
  )
  .rml_batch_set_widths(
    wb,
    "异常与人工复核",
    issue_out
  )

  if (nrow(issue_out) > 0L) {
    openxlsx::addStyle(
      wb,
      "异常与人工复核",
      styles$warning,
      rows = 4:(
        3 +
          nrow(issue_out)
      ),
      cols = seq_len(
        ncol(issue_out)
      ),
      gridExpand = TRUE,
      stack = TRUE
    )
  }

  # 考试统计：同一 Sheet 分两块
  openxlsx::writeData(
    wb,
    "考试统计",
    "RMedLearn 考试统计",
    startRow = 1,
    startCol = 1
  )
  openxlsx::addStyle(
    wb,
    "考试统计",
    styles$title,
    rows = 1,
    cols = 1
  )

  openxlsx::writeData(
    wb,
    "考试统计",
    "总体统计",
    startRow = 3,
    startCol = 1
  )
  openxlsx::addStyle(
    wb,
    "考试统计",
    styles$subheader,
    rows = 3,
    cols = 1:2,
    gridExpand = TRUE
  )

  openxlsx::writeData(
    wb,
    "考试统计",
    overview_stats,
    startRow = 4,
    startCol = 1,
    colNames = TRUE
  )
  openxlsx::addStyle(
    wb,
    "考试统计",
    styles$header,
    rows = 4,
    cols = 1:2,
    gridExpand = TRUE
  )

  q_start <- 7 +
    nrow(
      overview_stats
    )

  openxlsx::writeData(
    wb,
    "考试统计",
    "分题统计",
    startRow = q_start,
    startCol = 1
  )
  openxlsx::addStyle(
    wb,
    "考试统计",
    styles$subheader,
    rows = q_start,
    cols = 1:max(
      2,
      ncol(
        question_stats
      )
    ),
    gridExpand = TRUE
  )

  if (ncol(
    question_stats
  ) > 0L) {
    openxlsx::writeData(
      wb,
      "考试统计",
      question_stats_out,
      startRow = q_start + 1L,
      startCol = 1,
      colNames = TRUE
    )

    openxlsx::addStyle(
      wb,
      "考试统计",
      styles$header,
      rows = q_start + 1L,
      cols = seq_len(
        ncol(question_stats_out)
      ),
      gridExpand = TRUE
    )
  }

  openxlsx::setColWidths(
    wb,
    "考试统计",
    cols = 1:8,
    widths = c(
      24,
      16,
      14,
      14,
      14,
      14,
      14,
      14
    )
  )

  # 百分比格式
  pass_row <- which(
    grepl(
      "^及格率",
      overview_stats$指标
    )
  )

  if (length(pass_row) == 1L) {
    openxlsx::addStyle(
      wb,
      "考试统计",
      styles$pct2,
      rows = 4L +
        pass_row,
      cols = 2,
      gridExpand = TRUE,
      stack = TRUE
    )
  }

  if (
    nrow(question_stats) > 0L &&
      "得分率" %in%
        names(question_stats_out)
  ) {
    rate_col <- match(
      "得分率",
      names(question_stats_out)
    )

    openxlsx::addStyle(
      wb,
      "考试统计",
      styles$pct2,
      rows = (
        q_start +
          2L
      ):(
        q_start +
          1L +
          nrow(question_stats_out)
      ),
      cols = rate_col,
      gridExpand = TRUE,
      stack = TRUE
    )
  }

  # ----------------------------------------------------------
  # 班级总成绩：异常项醒目标记
  # 1) 人工复核项数 > 0        -> 黄色
  # 2) 该学号提交次数 > 1      -> 黄色
  # 3) 解码状态 != OK          -> 红色
  # ----------------------------------------------------------
  if (nrow(summary_out) > 0L) {

    if ("人工复核项数" %in% names(summary_out)) {
      review_col <- match(
        "人工复核项数",
        names(summary_out)
      )

      review_rows_idx <- which(
        suppressWarnings(
          as.numeric(summary_out[["人工复核项数"]])
        ) > 0
      )

      if (length(review_rows_idx) > 0L) {
        openxlsx::addStyle(
          wb,
          "班级总成绩",
          styles$warning,
          rows = review_rows_idx + 3L,
          cols = review_col,
          gridExpand = TRUE,
          stack = TRUE
        )
      }
    }

    if ("该学号提交次数" %in% names(summary_out)) {
      duplicate_col <- match(
        "该学号提交次数",
        names(summary_out)
      )

      duplicate_rows_idx <- which(
        suppressWarnings(
          as.numeric(summary_out[["该学号提交次数"]])
        ) > 1
      )

      if (length(duplicate_rows_idx) > 0L) {
        openxlsx::addStyle(
          wb,
          "班级总成绩",
          styles$warning,
          rows = duplicate_rows_idx + 3L,
          cols = duplicate_col,
          gridExpand = TRUE,
          stack = TRUE
        )
      }
    }

    if ("解码状态" %in% names(summary_out)) {
      decode_col <- match(
        "解码状态",
        names(summary_out)
      )

      decode_rows_idx <- which(
        is.na(summary_out[["解码状态"]]) |
          toupper(trimws(as.character(
            summary_out[["解码状态"]]
          ))) != "OK"
      )

      if (length(decode_rows_idx) > 0L) {
        openxlsx::addStyle(
          wb,
          "班级总成绩",
          styles$error,
          rows = decode_rows_idx + 3L,
          cols = decode_col,
          gridExpand = TRUE,
          stack = TRUE
        )
      }
    }
  }

  dir.create(
    dirname(output_file),
    recursive = TRUE,
    showWarnings = FALSE
  )

  openxlsx::saveWorkbook(
    wb,
    output_file,
    overwrite = TRUE
  )

  cat(
    "\n================ 批量评分完成 ================\n",
    "原始提交数：",
    total_submissions,
    "\n解码成功：",
    decoded_success,
    "\n评分成功：",
    graded_success,
    "\n输出文件：\n",
    normalizePath(
      output_file,
      winslash = "/",
      mustWork = FALSE
    ),
    "\n==============================================\n\n",
    sep = ""
  )

  invisible(
    list(
      summary = summary_df,
      question_scores = question_df,
      fill_detail = fill_df,
      code_detail = code_df,
      raw_answers = raw_df,
      issues = issue_df,
      overview_stats = overview_stats,
      question_stats = question_stats,
      output_file = output_file
    )
  )
}
