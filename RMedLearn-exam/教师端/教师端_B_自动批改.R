# ============================================================
# 教师端路径 B：自动解码 + 自动批改
#
# 输入：运行时弹窗选择学生提交表（.xlsx/.xlsm/.csv）
# 输出：RMedLearn_考试成绩.xlsx
# 依赖：02_当前试卷 中的标准答案、评分器和数据集
# ============================================================

# ---------- 自动定位本脚本所在文件夹 ----------
.rml_script_dir <- function() {
  frames <- sys.frames()

  if (length(frames) > 0L) {
    for (i in rev(seq_along(frames))) {
      ofile <- frames[[i]]$ofile

      if (
        !is.null(ofile) &&
        length(ofile) == 1L &&
        nzchar(ofile)
      ) {
        return(
          dirname(
            normalizePath(
              ofile,
              winslash = "/",
              mustWork = TRUE
            )
          )
        )
      }
    }
  }

  args <- commandArgs(
    trailingOnly = FALSE
  )

  file_arg <- grep(
    "^--file=",
    args,
    value = TRUE
  )

  if (length(file_arg) > 0L) {
    script_file <- sub(
      "^--file=",
      "",
      file_arg[1]
    )

    return(
      dirname(
        normalizePath(
          script_file,
          winslash = "/",
          mustWork = TRUE
        )
      )
    )
  }

  normalizePath(
    getwd(),
    winslash = "/",
    mustWork = TRUE
  )
}

demo_dir <- .rml_script_dir()

setwd(
  demo_dir
)

cat(
  "教师端目录：",
  demo_dir,
  "\n\n",
  sep = ""
)


# ---------- 选择学生提交表 ----------
cat(
  "请选择学生提交表（支持 .xlsx / .xlsm / .csv）...\n"
)

submission_file <- tryCatch(
  file.choose(),
  error = function(e) {
    stop(
      "未选择学生提交表，程序已停止。",
      call. = FALSE
    )
  }
)

submission_file <- normalizePath(
  submission_file,
  winslash = "/",
  mustWork = TRUE
)

input_ext <- tolower(
  tools::file_ext(
    submission_file
  )
)

if (!input_ext %in% c(
  "xlsx",
  "xlsm",
  "csv"
)) {
  stop(
    paste0(
      "选择的文件格式不支持：.",
      input_ext,
      "\n请重新运行并选择 .xlsx、.xlsm 或 .csv 文件。"
    ),
    call. = FALSE
  )
}

cat(
  "已选择提交表：",
  submission_file,
  "\n\n",
  sep = ""
)

# 如果标准答案尚未生成，则自动生成一次
key_file <- "02_当前试卷/exam_key_teacher.rds"

if (!file.exists(key_file)) {
  source(
    "02_当前试卷/make_exam_key.R",
    encoding = "UTF-8"
  )

  make_exam_key(
    dataset_dir = "02_当前试卷/datasets",
    output = key_file
  )
}

source(
  "01_通用教师工具/batch_grade_submissions.R",
  encoding = "UTF-8"
)

grade_result <- batch_grade(
  input_file = submission_file,
  output_file = "RMedLearn_考试成绩.xlsx",
  grader_script = "02_当前试卷/grade_one_submission.R",
  dataset_dir = "02_当前试卷/datasets",
  key_path = key_file,
  expected_exam_id = "RMedLearn_Teacher_Exam_2026_v1"
)

cat(
  "\n完成：已生成 RMedLearn_考试成绩.xlsx\n"
)
