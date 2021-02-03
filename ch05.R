# 5장. 분활 정복 : 의사 결정 트리와 규칙 기반의 분류

-0.60 * log2(0.60) - 0.04 * log2(0.04)

curve(-x * log2(x) - (1 - x) * log2(1-x), 
      col = "red", xlab = "x", ylab = "Entropy", lwd = 4)

# 예제 - 의사 결정 트리를 이용해 간단한 대출 승인 모델 개발

# 1단계 : 데이터 수집
# 2단계 : 데이터 탐색과 준비

credit <- read.csv("credit.csv")
str(credit)

# 수표 계좌 잔고
table(credit$checking_balance)
# 저축 계좌 잔고
table(credit$savings_balance)

# 대출 기간
summary(credit$months_loan_duration)
# 대출 금액
summary(credit$amount)

# 채무 이행 / 불이행
table(credit$default)

# 데이터 준비 : 랜덤한 훈련 및 테스트 데이터셋 생성

# 랜덤으로 훈련 데이터
set.seed(123)
train_sample <- sample(1000, 900)

str(train_sample)

credit_train <- credit[train_sample, ]
credit_test <- credit[-train_sample, ]

prop.table(table(credit_train$default))
prop.table(table(credit_test$default))


# 3단계 : 데이터로 모델 훈련

install.packages("C50")
library(C50)

credit_model <- C5.0(credit_train[-17], credit_train$default)

credit_model

summary(credit_model)

credit_pred <- predict(credit_model, credit_test)

library(gmodels)
CrossTable(credit_test$default, credit_pred,
           prop.chisq = F, prop.c = F, prop.r = F,
           dnn = c("actual default", "predicted default"))

# 5단계 : 모델 성능 개선
# 의사 결정 트리의 정확도 향상

# 부스팅 이용!

credit_boost10 <- C5.0(credit_train[-17], credit_train$default, trials = 10)

credit_boost10

summary(credit_boost10)

credit_boost_pred10 <- predict(credit_boost10, credit_test)
CrossTable(credit_test$default, credit_boost_pred10,
           prop.chisq = F, prop.c = F, prop.r = F,
           dnn = c("actual default", "predictde default"))

matrix_dimensions <- list(c("no", "yes"), c("no", "yes"))
names(matrix_dimensions) <- c("predicted", "actual")

matrix_dimensions

error_cost <- matrix(c(0, 1, 4, 0), nrow = 2, dimnames = matrix_dimensions)
error_cost

credit_cost <- C5.0(credit_train[-17], credit_train$default,
                   costs = error_cost)
credit_cost_pred <- predict(credit_cost, credit_test)
CrossTable(credit_test$default, credit_cost_pred,
           prop.chisq = F, prop.c = F, prop.r = F,
           dnn = c("actual default", "predicted default"))