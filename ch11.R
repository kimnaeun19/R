# 11장. 모델 성능 개선

credit <- read.csv("Credit_ch11.csv")

modelLookup("C5.0")

library(caret)
set.seed(300)
m <- train(default ~ ., data = credit, method = "C5.0")
str(m)
m

p <- predict(m, credit)
p

table(p, credit$default)

head(predict(m, credit, type = "prob"))
plot(m)

# 배깅

credit$default <- as.factor(credit$default)

library(ipred)
set.seed(300)
mybag <- bagging(default ~ ., data = credit, nbagg = 25)

credit_pred <- predict(mybag, credit)
table(credit_pred, credit$default)

# 미래 성능으로 어떻게 변하는지 혹인

library(caret)
set.seed(300)
ctrl <- trainControl(method = "cv", number = 10)
train(default ~ ., data = credit, method = "treebag",
      trControl = ctrl)
