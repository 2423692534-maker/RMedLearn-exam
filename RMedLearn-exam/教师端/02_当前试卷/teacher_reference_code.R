# ============================================================
# RMedLearn 最终 9 题试卷：教师参考代码
# 10 套数据代码逻辑相同，仅 xN.csv 和数值答案不同。
# ============================================================

# Q01 工作空间 + R包 ------------------------------------------------
setwd("C:/你的考试文件夹")
getwd()
library(foreign)
library(dplyr)

# Q02 导入数据 -------------------------------------------------------
data1 <- read.csv("x1.csv", stringsAsFactors = FALSE)
head(data1)
dim(data1)
str(data1)

# Q03 年龄筛选 -------------------------------------------------------
data2 <- subset(data1, age >= 40 & age <= 75)
dim(data2)
str(data2)

# Q04 bmi + bmi1 ----------------------------------------------------
data2$bmi <- data2$weight_kg / (data2$height_cm / 100)^2
data2$bmi1 <- ifelse(data2$bmi < 18, 1,
                     ifelse(data2$bmi < 24, 2,
                            ifelse(data2$bmi < 28, 3, 4)))
str(data2)
table(data2$bmi1)

# Q05 tg_cat ---------------------------------------------------------
data2$tg_cat <- ifelse(data2$tg >= 2.3, 1, 0)
mean(data2$tg_cat == 1, na.rm = TRUE)

# Q06 因子化 ---------------------------------------------------------
data2$sex <- factor(data2$sex)
data2$huji <- factor(data2$huji)
data2$bmi1 <- factor(data2$bmi1)
data2$smoking <- factor(data2$smoking)
data2$insomnia <- factor(data2$insomnia)
data2$tg_cat <- factor(data2$tg_cat)
str(data2)

# Q07 参考组 ---------------------------------------------------------
data2$bmi1 <- relevel(data2$bmi1, ref = "2")
data2$sex <- relevel(data2$sex, ref = "Female")
levels(data2$bmi1)
levels(data2$sex)

# Q08 差异比较 -------------------------------------------------------
sex_test <- t.test(health ~ sex, data = data2)
bmi_aov <- aov(health ~ bmi1, data = data2)
insomnia_test <- t.test(health ~ insomnia, data = data2)
sex_test
summary(bmi_aov)
insomnia_test
sex_test$p.value
summary(bmi_aov)[[1]][["Pr(>F)"]][1]

# Q09 logistic 回归 --------------------------------------------------
fit <- glm(
  tg_cat ~ insomnia + sex + huji + bmi1 + smoking + sugar,
  family = binomial(), data = data2
)
summary(fit)
exp(coef(fit))
exp(coef(fit)["sexMale"])
summary(fit)$coefficients["sugar", "Pr(>|z|)"]
