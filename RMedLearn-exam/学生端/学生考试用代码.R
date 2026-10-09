# ============================================================
# R语言医学数据分析考试：学生用代码
# 请先将本文件所在文件夹设置为 RStudio 工作空间，再从上到下运行。
# ============================================================

# 1. 安装缺失的依赖包
pkgs <- c(
  "learnr", "shiny", "rmarkdown", "digest", "jsonlite",
  "base64enc", "survival", "openxlsx", "foreign", "dplyr"
)
need <- pkgs[!vapply(pkgs, requireNamespace, quietly = TRUE, FUN.VALUE = logical(1))]
if (length(need) > 0) install.packages(need)

# 2. 安装 / 更新 RMedLearn
install.packages(
  "RMedLearn_0.2.0.zip",
  repos = NULL,
  type = "win.binary"
)
# 3. 直接启动考试
RMedLearn::run_exam(
  paper = "exam_paper.json"
)
