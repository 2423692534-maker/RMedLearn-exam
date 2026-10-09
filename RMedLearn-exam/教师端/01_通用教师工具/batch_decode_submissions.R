# ============================================================
# RMedLearn 教师端：通用批量解码（只导出学生答题记录，不评分）
#
# 输入：
#   submissions.xlsx / submissions.csv
#   推荐表头：班级、学号、姓名、提交码
#
# 输出 Excel：
#   1. 考生信息
#   2. 学生答题记录
#   3. 解码异常
#
# 特点：
# - 不需要 exam_key_teacher.rds
# - 不需要 grade_one_submission.R
# - 不需要 datasets/
# - 不依赖当前试卷的评分规则
# - 只要提交码仍由兼容的 RMedLearn 提交机制生成，就可继续使用
# ============================================================


.rml_decode_trim <- function(x) {
  if (is.null(x) || length(x) == 0L) {
    return("")
  }
  trimws(as.character(x[[1]]))
}


.rml_decode_clean_code <- function(x) {
  x <- paste(
    as.character(x),
    collapse = ""
  )

  paste0(
    strsplit(
      x,
      "[[:space:]]+"
    )[[1]],
    collapse = ""
  )
}


.rml_decode_find_col <- function(nms, candidates) {
  low <- tolower(trimws(nms))

  for (cand in candidates) {
    hit <- which(
      low == tolower(cand)
    )

    if (length(hit) > 0L) {
      return(nms[hit[1]])
    }
  }

  NULL
}


.rml_decode_read_input <- function(path) {
  if (!file.exists(path)) {
    stop(
      paste0(
        "找不到学生提交表：\n",
        path
      ),
      call. = FALSE
    )
  }

  ext <- tolower(
    tools::file_ext(path)
  )

  if (ext %in% c("xlsx", "xlsm")) {
    if (!requireNamespace("openxlsx", quietly = TRUE)) {
      stop(
        "读取 Excel 需要 openxlsx，请先运行：install.packages(\"openxlsx\")",
        call. = FALSE
      )
    }

    x <- openxlsx::read.xlsx(
      path,
      check.names = FALSE
    )
  } else if (ext == "csv") {
    x <- utils::read.csv(
      path,
      stringsAsFactors = FALSE,
      check.names = FALSE,
      fileEncoding = "UTF-8-BOM"
    )
  } else {
    stop(
      "提交表仅支持 .xlsx、.xlsm 或 .csv。",
      call. = FALSE
    )
  }

  if (nrow(x) == 0L) {
    stop(
      "学生提交表中没有记录。",
      call. = FALSE
    )
  }

  class_col <- .rml_decode_find_col(
    names(x),
    c("班级", "class")
  )

  sid_col <- .rml_decode_find_col(
    names(x),
    c("学号", "student_id", "student id")
  )

  name_col <- .rml_decode_find_col(
    names(x),
    c("姓名", "name", "student_name", "student name")
  )

  code_col <- .rml_decode_find_col(
    names(x),
    c("提交码", "submission_code", "submission code")
  )

  if (is.null(sid_col)) {
    stop(
      "提交表中没有找到“学号”列。",
      call. = FALSE
    )
  }

  if (is.null(code_col)) {
    stop(
      "提交表中没有找到“提交码”列。",
      call. = FALSE
    )
  }

  out <- data.frame(
    submission_row = seq_len(nrow(x)),
    class = if (is.null(class_col)) "" else as.character(x[[class_col]]),
    student_id = as.character(x[[sid_col]]),
    student_name = if (is.null(name_col)) "" else as.character(x[[name_col]]),
    submission_code = as.character(x[[code_col]]),
    stringsAsFactors = FALSE
  )

  out$class[is.na(out$class)] <- ""
  out$student_id[is.na(out$student_id)] <- ""
  out$student_name[is.na(out$student_name)] <- ""
  out$submission_code[is.na(out$submission_code)] <- ""

  out
}


.rml_decode_bind <- function(items) {
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


.rml_decode_make_styles <- function() {
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
      valign = "center"
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


.rml_decode_write_sheet <- function(
  wb,
  sheet,
  data,
  title,
  styles
) {
  openxlsx::writeData(
    wb,
    sheet,
    title,
    startRow = 1,
    startCol = 1
  )

  openxlsx::addStyle(
    wb,
    sheet,
    styles$title,
    rows = 1,
    cols = 1
  )

  if (ncol(data) == 0L) {
    openxlsx::writeData(
      wb,
      sheet,
      "无记录",
      startRow = 3,
      startCol = 1
    )
    return(invisible(NULL))
  }

  openxlsx::writeData(
    wb,
    sheet,
    data,
    startRow = 3,
    startCol = 1,
    colNames = TRUE
  )

  openxlsx::addStyle(
    wb,
    sheet,
    styles$header,
    rows = 3,
    cols = seq_len(ncol(data)),
    gridExpand = TRUE
  )

  if (nrow(data) > 0L) {
    openxlsx::addStyle(
      wb,
      sheet,
      styles$body,
      rows = 4:(3 + nrow(data)),
      cols = seq_len(ncol(data)),
      gridExpand = TRUE
    )
  }

  openxlsx::freezePane(
    wb,
    sheet,
    firstActiveRow = 4
  )

  openxlsx::setColWidths(
    wb,
    sheet,
    cols = seq_len(ncol(data)),
    widths = "auto"
  )
}


batch_decode_submissions <- function(
  input_file,
  output_file = "学生答题记录.xlsx",
  expected_exam_id = NULL
) {
  if (!requireNamespace("RMedLearn", quietly = TRUE)) {
    stop(
      "请先安装当前版本的 RMedLearn。",
      call. = FALSE
    )
  }

  if (!requireNamespace("openxlsx", quietly = TRUE)) {
    stop(
      "导出 Excel 需要 openxlsx，请先运行：install.packages(\"openxlsx\")",
      call. = FALSE
    )
  }

  submissions <- .rml_decode_read_input(
    input_file
  )

  duplicate_counts <- table(
    submissions$student_id
  )

  decode_fun <- getFromNamespace(
    "decode_exam_submission",
    "RMedLearn"
  )

  info_rows <- list()
  answer_rows <- list()
  issue_rows <- list()

  for (i in seq_len(nrow(submissions))) {
    one <- submissions[
      i,
      ,
      drop = FALSE
    ]

    input_class <- .rml_decode_trim(
      one$class
    )

    input_sid <- .rml_decode_trim(
      one$student_id
    )

    input_name <- .rml_decode_trim(
      one$student_name
    )

    code <- .rml_decode_clean_code(
      one$submission_code
    )

    dup_n <- unname(
      duplicate_counts[
        input_sid
      ]
    )

    if (
      length(dup_n) == 0L ||
      is.na(dup_n)
    ) {
      dup_n <- 0L
    }

    if (!nzchar(code)) {
      info_rows[[length(info_rows) + 1L]] <- data.frame(
        提交序号 = i,
        班级 = input_class,
        提交表学号 = input_sid,
        姓名 = input_name,
        提交码学号 = "",
        数据编号 = "",
        考试编号 = "",
        开始时间 = "",
        完成时间 = "",
        用时分钟 = NA_real_,
        题目数量 = NA_integer_,
        该学号提交次数 = as.integer(dup_n),
        解码状态 = "FAIL",
        stringsAsFactors = FALSE,
        check.names = FALSE
      )

      issue_rows[[length(issue_rows) + 1L]] <- data.frame(
        提交序号 = i,
        班级 = input_class,
        学号 = input_sid,
        姓名 = input_name,
        类型 = "解码失败",
        问题说明 = "提交码为空。",
        stringsAsFactors = FALSE,
        check.names = FALSE
      )

      next
    }

    record <- tryCatch(
      decode_fun(
        code
      ),
      error = function(e) e
    )

    if (inherits(record, "error")) {
      info_rows[[length(info_rows) + 1L]] <- data.frame(
        提交序号 = i,
        班级 = input_class,
        提交表学号 = input_sid,
        姓名 = input_name,
        提交码学号 = "",
        数据编号 = "",
        考试编号 = "",
        开始时间 = "",
        完成时间 = "",
        用时分钟 = NA_real_,
        题目数量 = NA_integer_,
        该学号提交次数 = as.integer(dup_n),
        解码状态 = "FAIL",
        stringsAsFactors = FALSE,
        check.names = FALSE
      )

      issue_rows[[length(issue_rows) + 1L]] <- data.frame(
        提交序号 = i,
        班级 = input_class,
        学号 = input_sid,
        姓名 = input_name,
        类型 = "解码失败",
        问题说明 = conditionMessage(
          record
        ),
        stringsAsFactors = FALSE,
        check.names = FALSE
      )

      next
    }

    decoded_sid <- if (
      is.null(record$student_id)
    ) {
      ""
    } else {
      as.character(record$student_id)
    }

    decoded_class <- if (
      !is.null(record$class) &&
      nzchar(
        trimws(
          as.character(record$class)
        )
      )
    ) {
      as.character(record$class)
    } else {
      input_class
    }

    decoded_name <- if (
      !is.null(record$name) &&
      nzchar(
        trimws(
          as.character(record$name)
        )
      )
    ) {
      as.character(record$name)
    } else {
      input_name
    }

    exam_id <- if (
      is.null(record$exam_id)
    ) {
      ""
    } else {
      as.character(record$exam_id)
    }

    dataset_id <- if (
      is.null(record$dataset_id)
    ) {
      ""
    } else {
      paste0(
        "X",
        as.character(record$dataset_id)
      )
    }

    info_rows[[length(info_rows) + 1L]] <- data.frame(
      提交序号 = i,
      班级 = decoded_class,
      提交表学号 = input_sid,
      姓名 = decoded_name,
      提交码学号 = decoded_sid,
      数据编号 = dataset_id,
      考试编号 = exam_id,
      开始时间 = if (is.null(record$started_at)) "" else as.character(record$started_at),
      完成时间 = if (is.null(record$completed_at)) "" else as.character(record$completed_at),
      用时分钟 = if (is.null(record$elapsed_minutes)) NA_real_ else suppressWarnings(as.numeric(record$elapsed_minutes)),
      题目数量 = if (is.null(record$questions)) 0L else length(record$questions),
      提交码结构版本 = if (is.null(record$schema_version)) NA_integer_ else suppressWarnings(as.integer(record$schema_version)),
      数据集校验通过 = if (is.null(record$dataset_verified)) NA else isTRUE(record$dataset_verified),
      该学号提交次数 = as.integer(dup_n),
      解码状态 = "OK",
      stringsAsFactors = FALSE,
      check.names = FALSE
    )

    if (
      nzchar(input_sid) &&
      nzchar(decoded_sid) &&
      !identical(
        input_sid,
        decoded_sid
      )
    ) {
      issue_rows[[length(issue_rows) + 1L]] <- data.frame(
        提交序号 = i,
        班级 = decoded_class,
        学号 = input_sid,
        姓名 = decoded_name,
        类型 = "学号不一致",
        问题说明 = paste0(
          "提交表学号为 ",
          input_sid,
          "；提交码内学号为 ",
          decoded_sid,
          "。"
        ),
        stringsAsFactors = FALSE,
        check.names = FALSE
      )
    }

    if (dup_n > 1L) {
      issue_rows[[length(issue_rows) + 1L]] <- data.frame(
        提交序号 = i,
        班级 = decoded_class,
        学号 = input_sid,
        姓名 = decoded_name,
        类型 = "重复提交",
        问题说明 = paste0(
          "该学号在提交表中共出现 ",
          dup_n,
          " 次。"
        ),
        stringsAsFactors = FALSE,
        check.names = FALSE
      )
    }

    if (
      !is.null(expected_exam_id) &&
      nzchar(
        as.character(
          expected_exam_id
        )
      ) &&
      !identical(
        exam_id,
        as.character(
          expected_exam_id
        )
      )
    ) {
      issue_rows[[length(issue_rows) + 1L]] <- data.frame(
        提交序号 = i,
        班级 = decoded_class,
        学号 = input_sid,
        姓名 = decoded_name,
        类型 = "考试编号不一致",
        问题说明 = paste0(
          "提交码考试编号为 ",
          exam_id,
          "；当前期望考试编号为 ",
          expected_exam_id,
          "。"
        ),
        stringsAsFactors = FALSE,
        check.names = FALSE
      )
    }

    if (
      !is.null(record$questions) &&
      length(record$questions) > 0L
    ) {
      for (q in record$questions) {
        qid <- if (
          is.null(q$question_id)
        ) {
          ""
        } else {
          as.character(q$question_id)
        }

        qtitle <- if (
          is.null(q$title)
        ) {
          ""
        } else {
          as.character(q$title)
        }

        if (
          !is.null(q$code_answers) &&
          length(q$code_answers) > 0L
        ) {
          for (nm in names(q$code_answers)) {
            validation <- if (
              !is.null(q$code_validation) &&
              !is.null(q$code_validation[[nm]])
            ) {
              q$code_validation[[nm]]
            } else {
              list()
            }

            answer_rows[[length(answer_rows) + 1L]] <- data.frame(
              提交序号 = i,
              班级 = decoded_class,
              学号 = decoded_sid,
              姓名 = decoded_name,
              数据编号 = dataset_id,
              考试编号 = exam_id,
              题号 = qid,
              题目 = qtitle,
              作答类型 = "代码",
              答案编号 = nm,
              学生原始作答 = paste(
                as.character(
                  q$code_answers[[nm]]
                ),
                collapse = "\n"
              ),
              是否运行 = if (is.null(validation$ran)) NA else isTRUE(validation$ran),
              当前代码已校验 = if (is.null(validation$validated)) NA else isTRUE(validation$validated),
              运行校验通过 = if (is.null(validation$validation_passed)) NA else isTRUE(validation$validation_passed),
              校验状态 = if (is.null(validation$status)) "" else as.character(validation$status),
              校验说明 = if (is.null(validation$status_text)) "" else as.character(validation$status_text),
              当前代码与最后运行版本一致 = if (is.null(validation$current_matches_last_run)) NA else isTRUE(validation$current_matches_last_run),
              最后运行时间 = if (is.null(validation$ran_at)) "" else as.character(validation$ran_at),
              运行错误信息 = if (is.null(validation$error)) "" else as.character(validation$error),
              数据集校验通过 = if (is.null(validation$dataset_verified)) NA else validation$dataset_verified,
              stringsAsFactors = FALSE,
              check.names = FALSE
            )
          }
        }

        if (
          !is.null(q$fill_answers) &&
          length(q$fill_answers) > 0L
        ) {
          for (nm in names(q$fill_answers)) {
            answer_rows[[length(answer_rows) + 1L]] <- data.frame(
              提交序号 = i,
              班级 = decoded_class,
              学号 = decoded_sid,
              姓名 = decoded_name,
              数据编号 = dataset_id,
              考试编号 = exam_id,
              题号 = qid,
              题目 = qtitle,
              作答类型 = "填空",
              答案编号 = nm,
              学生原始作答 = paste(
                as.character(
                  q$fill_answers[[nm]]
                ),
                collapse = "\n"
              ),
              是否运行 = NA,
              当前代码已校验 = NA,
              运行校验通过 = NA,
              校验状态 = "",
              校验说明 = "",
              当前代码与最后运行版本一致 = NA,
              最后运行时间 = "",
              运行错误信息 = "",
              数据集校验通过 = NA,
              stringsAsFactors = FALSE,
              check.names = FALSE
            )
          }
        }
      }
    }
  }

  info_df <- .rml_decode_bind(
    info_rows
  )

  answer_df <- .rml_decode_bind(
    answer_rows
  )

  issue_df <- .rml_decode_bind(
    issue_rows
  )

  wb <- openxlsx::createWorkbook(
    creator = "RMedLearn"
  )

  styles <- .rml_decode_make_styles()

  openxlsx::addWorksheet(
    wb,
    "考生信息"
  )

  openxlsx::addWorksheet(
    wb,
    "学生答题记录"
  )

  openxlsx::addWorksheet(
    wb,
    "解码异常"
  )

  .rml_decode_write_sheet(
    wb,
    "考生信息",
    info_df,
    "RMedLearn 考生信息",
    styles
  )

  .rml_decode_write_sheet(
    wb,
    "学生答题记录",
    answer_df,
    "RMedLearn 学生原始答题记录",
    styles
  )

  .rml_decode_write_sheet(
    wb,
    "解码异常",
    issue_df,
    "RMedLearn 解码异常",
    styles
  )

  if (
    nrow(answer_df) > 0L &&
    "学生原始作答" %in%
      names(answer_df)
  ) {
    answer_col <- match(
      "学生原始作答",
      names(answer_df)
    )

    openxlsx::setColWidths(
      wb,
      "学生答题记录",
      cols = answer_col,
      widths = 60
    )

    openxlsx::addStyle(
      wb,
      "学生答题记录",
      styles$wrap,
      rows = 4:(3 + nrow(answer_df)),
      cols = answer_col,
      gridExpand = TRUE,
      stack = TRUE
    )
  }

  if (
    nrow(info_df) > 0L &&
    "解码状态" %in%
      names(info_df)
  ) {
    bad <- which(
      info_df[["解码状态"]] != "OK"
    )

    if (length(bad) > 0L) {
      openxlsx::addStyle(
        wb,
        "考生信息",
        styles$error,
        rows = bad + 3L,
        cols = match(
          "解码状态",
          names(info_df)
        ),
        gridExpand = TRUE,
        stack = TRUE
      )
    }
  }

  if (nrow(issue_df) > 0L) {
    openxlsx::addStyle(
      wb,
      "解码异常",
      styles$warning,
      rows = 4:(3 + nrow(issue_df)),
      cols = seq_len(ncol(issue_df)),
      gridExpand = TRUE,
      stack = TRUE
    )
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

  message(
    "已生成学生答题记录：",
    normalizePath(
      output_file,
      winslash = "/",
      mustWork = FALSE
    )
  )

  invisible(
    list(
      students = info_df,
      answers = answer_df,
      issues = issue_df,
      output_file = output_file
    )
  )
}
