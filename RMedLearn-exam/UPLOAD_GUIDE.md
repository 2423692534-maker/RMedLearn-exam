# 上传到 GitHub

本文件对应 **RMedLearn Exam 私有仓库上传版本**。

## 1. 解压

将 `RMedLearn-exam-GitHub-private.zip` 解压，得到 `RMedLearn-exam` 文件夹。

打开该文件夹后，应能看到 `README.md`、`UPLOAD_GUIDE.md`、`.gitignore`、`学生端` 和 `教师端`。

## 2. 创建私有仓库

1. 登录 GitHub，点击右上角 **“＋” → “New repository”**。
2. 仓库名填写 `RMedLearn-exam`，或其他英文名称。
3. 可填写描述：`R 语言医学数据分析考试与教师批改工具`。
4. 可见性选择 **Private**。本版本保留教师标准答案和当前试卷。
5. 如使用网页上传，可以勾选初始化 README，方便进入仓库文件页面；后续上传本包的 `README.md` 时会替换该初始说明。若改用命令行推送已有本地仓库，则不要初始化远端 README。
6. 点击 **Create repository**。

## 3. 上传解压后的内容

1. 在仓库文件页面，点击 **Add file → Upload files**。
2. 打开解压后的 `RMedLearn-exam` 文件夹，将其中的文件和 `学生端`、`教师端` 两个文件夹一起拖入上传区域。
3. 保留原有目录结构；确保仓库根目录直接包含 `README.md`、`学生端` 和 `教师端`。
4. 确认 `.gitignore` 也在上传列表中。如果文件管理器没有显示它，可单独找到并补传。
5. 提交说明可以填写：`Initial upload of RMedLearn exam tools`。
6. 按页面提示确认提交。如果选择新分支提交，再创建并合并对应的 pull request。

**请上传解压后的文件内容。**直接把整个压缩包作为一个文件上传，只会得到一个可下载附件，不会展开成可以逐个查看和修改的项目文件。

本次整理包含 42 个文件，最大单文件约 314 KiB，符合网页单次最多上传 100 个文件、单文件不超过 25 MiB 的限制。

## 4. 上传后检查

- 仓库标识为 **Private**。
- 首页显示 README 使用说明。
- `学生端` 中包含启动脚本、`exam_paper.json`、RMedLearn 安装包和 10 份 CSV 数据。
- `教师端` 中包含 A/B 两个入口脚本、教师工具、当前试卷和 10 份数据。
- 仓库中没有本次排除的 `56_56.txt`、已有成绩表和已有答题记录表。

## 后续更新

经常修改时，可以使用 GitHub Desktop 管理本地副本和更新记录。使用 Git 或 GitHub Desktop 时，附带的 `.gitignore` 会排除其中列出的生成文件。

网页手动拖拽上传时，应自行检查文件列表，不能依靠 `.gitignore` 自动过滤拖入的文件。不要重新上传实际学生提交、答题记录或成绩文件。

本目录的学生端和教师端分别保留原有安装包，便于各自独立发放和运行。不要随意改名或移动脚本引用的文件；如需调整，必须同步修改对应路径。

## 公开发布或在线考试

公开发布需要另行移出当前考试的答案、真实试卷和含答案的代码，并确认数据及包内材料可以公开。本上传包适用于私有仓库。

如果目标是让学生通过公网网址直接考试，还需要另外部署 R / Shiny 服务。把文件上传到 GitHub 不会自动启动在线考试系统。

## 官方操作说明

- [创建 GitHub 仓库](https://docs.github.com/en/repositories/creating-and-managing-repositories/creating-a-new-repository)
- [通过网页上传文件](https://docs.github.com/en/repositories/working-with-files/managing-files/adding-a-file-to-a-repository)
- [GitHub Pages 的用途](https://docs.github.com/en/pages/getting-started-with-github-pages/what-is-github-pages)
