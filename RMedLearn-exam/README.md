# RMedLearn Exam

R 语言医学数据分析考试工具，包含学生考试端、答题记录解码工具和教师自动批改工具。

本上传版本于 2026-10-09 整理，适合存放在 **GitHub 私有仓库（Private）**。目录保留当前试卷、教师标准答案和评分材料；向学生发放时，仅发放 `学生端` 文件夹。

## 功能

- 学生在本机 RStudio 中启动 learnr / Shiny 考试页面。
- 随附当前试卷和 `x1.csv` 至 `x10.csv` 数据文件。
- 当前试卷共 9 题、100 分，其中代码 70 分、填空 30 分。
- 教师可以解码提交记录，或批量解码并自动评分。
- 提供提交信息收集模板和当前试卷评分说明。

## 运行环境

- Windows。
- R 和 RStudio；随附 `RMedLearn_0.2.0.zip` 为 Windows 二进制安装包，构建时使用 R 4.5.2，建议使用匹配的 R 版本。
- 首次安装依赖包需要联网。
- 依赖包：`learnr`、`shiny`、`rmarkdown`、`digest`、`jsonlite`、`base64enc`、`survival`、`openxlsx`、`foreign`、`dplyr`。

当前学生启动脚本采用 `type = "win.binary"` 安装随附包。macOS 或 Linux 使用者需要另外准备源码安装包，并调整安装方式。

## 目录

```text
RMedLearn-exam/
├── README.md
├── UPLOAD_GUIDE.md
├── .gitignore
├── README_使用说明.txt
├── 学生端/
│   ├── 学生考试用代码.R
│   ├── RMedLearn_0.2.0.zip
│   ├── exam_paper.json
│   └── x1.csv ... x10.csv
└── 教师端/
    ├── 教师端_A_仅导出答题记录.R
    ├── 教师端_B_自动批改.R
    ├── RMedLearn_0.2.0.zip
    ├── 01_通用教师工具/
    │   ├── batch_decode_submissions.R
    │   ├── batch_grade_submissions.R
    │   ├── decode_one_submission.R
    │   ├── submission_collection_template.csv
    │   └── submission_collection_template.xlsx
    └── 02_当前试卷/
        ├── exam_paper_teacher.json
        ├── exam_key_teacher.rds
        ├── make_exam_key.R
        ├── grade_one_submission.R
        ├── teacher_key_preview.csv
        ├── teacher_reference_code.R
        ├── 评分规则说明.md
        └── datasets/
            └── x1.csv ... x10.csv
```

## 学生使用

1. 获取完整的 `学生端` 文件夹，保留其中所有文件。
2. 在 RStudio 中打开 `学生考试用代码.R`。
3. 将 RStudio 工作目录设置为该 `学生端` 文件夹。可以使用菜单 **Session → Set Working Directory → To Source File Location**。
4. 从上到下运行脚本。脚本会安装缺少的依赖、安装随附的 RMedLearn 包，并启动考试：

```r
RMedLearn::run_exam(paper = "exam_paper.json")
```

5. 按考试页面提示作答，并按教师要求提交答题结果。

## 教师使用

首次使用时，将 RStudio 工作目录设置为 `教师端`，在控制台运行以下安装步骤：

```r
pkgs <- c(
  "learnr", "shiny", "rmarkdown", "digest", "jsonlite",
  "base64enc", "survival", "openxlsx", "foreign", "dplyr"
)
need <- pkgs[!vapply(pkgs, requireNamespace, quietly = TRUE,
                    FUN.VALUE = logical(1))]
if (length(need) > 0) install.packages(need)
install.packages("RMedLearn_0.2.0.zip", repos = NULL, type = "win.binary")
```

使用 `01_通用教师工具` 中的收集模板整理学生提交结果，然后选择以下一种方式：

### A：仅导出答题记录

```r
source("教师端_A_仅导出答题记录.R", encoding = "UTF-8")
```

在弹窗中选择提交表（`.xlsx`、`.xlsm` 或 `.csv`），结果写入 `教师端/学生答题记录.xlsx`。

### B：自动批改

```r
source("教师端_B_自动批改.R", encoding = "UTF-8")
```

在弹窗中选择提交表，结果写入 `教师端/RMedLearn_考试成绩.xlsx`。脚本使用 `02_当前试卷` 中的评分器、标准答案和数据集；如果标准答案文件缺失，会自动生成。

评分细节见 [评分规则说明](教师端/02_当前试卷/评分规则说明.md)。更换试卷或数据集时，需要同步更新标准答案、评分器及试卷标识。

## GitHub 上传与共享

具体上传步骤见 [UPLOAD_GUIDE.md](UPLOAD_GUIDE.md)。

- 保留中文文件名和中文考试界面即可；仓库名建议使用 `RMedLearn-exam`。
- 本版本包含教师答案和当前试卷，请使用 **Private** 仓库。
- 公开发布前，应另行整理只包含通用程序、可公开数据和示例试卷的版本，并检查包内考试材料。
- 上传 GitHub 后，使用者仍需下载文件并在本机运行。GitHub Pages 无法直接运行本系统的 R / Shiny 服务。

本目录提供使用脚本、试卷、数据、教师工具和 Windows 安装包，尚未包含可重建整个 RMedLearn 包的完整原始源码。需要继续开发 RMedLearn 包或准备跨平台安装包时，应补充原始包工程。

## 本次整理

原有程序、试卷、数据和安装包保持原内容，仅新增仓库说明、上传指南和忽略规则。

上传版本未包含以下已有记录文件：

- `学生端/56_56.txt`
- `教师端/学生答题记录.xlsx`
- `教师端/RMedLearn_考试成绩.xlsx`

提交收集模板继续保留。今后产生的学生提交、成绩和答题记录应保存在本地；使用 Git 上传时，`.gitignore` 可排除其中列出的文件和目录。
