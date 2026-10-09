# ============================================================
# 教师端路径 A：仅导出学生答题记录
#
# 不评分，不需要标准答案，不依赖 02_当前试卷 的评分系统。
# 输入：运行时弹窗选择学生提交表（.xlsx/.xlsm/.csv）
# 输出：学生答题记录.xlsx
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

source(
  "01_通用教师工具/batch_decode_submissions.R",
  encoding = "UTF-8"
)

decode_result <- batch_decode_submissions(
  input_file = submission_file,
  output_file = "学生答题记录.xlsx"
)

cat(
  "\n完成：已生成 学生答题记录.xlsx\n"
)
