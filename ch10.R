#  10장. 모델 성능 평가

sms_results <- read.csv("sms_results.csv")
head(sms_results)

head(subset(sms_results, prob_spam > 0.40 & prob_spam < 0.60))

head(subset(sms_results, actual_type != predict_type))

table(sms_results$actual_type, sms_results$predict_type)

library(gmodels) 
CrossTable(sms_results$actual_type, sms_results$predict_type) 

library(caret) 
confusionMatrix(sms_results$predict_type, sms_results$actual_type, positive = "spam") 

confusionMatrix(table(sms_results$predict_type, sms_results$actual_type))

CrossTable(sms_results$actual_type, sms_results$predict_type)

pr_a <- 0.865 + 0.109
pr_a

pr_e <- 0.888 * 0.868 + 0.112 * 0.132
pr_e

kap <- (pr_a - pr_e) / (1 - pr_e)
kap

# Kappa 자동으로 계산해주는 함수

install.packages("vcd")
library(vcd)

Kappa(table(sms_results$actual_type, sms_results$predict_type))

install.packages("irr")
library(irr)

kappa2(sms_results[1:2])

# 특이도, 민감도

# 민감도

sens <- 152 / 183
sens

# 특이도
spec <- 1203/ 1207
spec

# 특이도, 민감도 구하는 함수 이용


sensitivity(sms_results$predict_type, sms_results$actual_type, positive = "spam")

sensitivity(table(sms_results$predict_type, sms_results$actual_type), positive = "spam")

specificity(table(sms_results$predict_type, sms_results$actual_type), negative = "ham")

# 정밀도와 재현율

# 정밀도 

prec <- 152 / 156
prec

# Recall

rec <- 152 / 183
rec

posPredValue(table(sms_results$predict_type, sms_results$actual_type), positive = "spam")

posPredValue(table(sms_results$actual_type, sms_results$predict_type), positive = "spam")

# F1-척도

f1 <- (2 * rec * prec) / (rec + prec)
f1

install.packages("pROC")
library(pROC)

sms_roc <- roc(sms_results$actual_type, sms_results$prob_spam)
plot(sms_roc, main = "ROC curve for SMS spam filter", col = "blue", lwd = 2, legacy.axes = TRUE)

# -----------------------------------

# knn 이용

sms_results_knn <- read.csv("sms_results_knn.csv")

sms_roc_knn <- multiclass.roc(sms_results$actual_type, sms_results_knn$p_spam)
sms_roc_knn <- roc(sms_results$actual_type, sms_results_knn$p_spam)

plot(sms_roc_knn, col = "red", lwd = 2, add = TRUE)

auc(sms_roc)
auc(sms_roc_knn)