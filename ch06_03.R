# 회귀 트리와 모델 트리의 이해

# 예제 : 회귀 트리와 모델 트리로 와인 품질 평가

# 1단계 : 데이터 수집
# 2단계 : 데이터 탐색과 준비

wine <- read.csv("whitewines.csv")

str(wine)

hist(wine$quality)

summary(wine)

# 훈련 /  테스트 나누기

wine_train <- wine[1:3750, ]
wine_test <- wine[3751:4898, ]

# 3단계 : 데이터로 모델 훈련

install.packages("rpart")
library(rpart)

m.rpart <- rpart(quality ~., data = wine_train)

m.rpart

summary(m.rpart)

# 의사 결정 트리 시각화

install.packages("rpart.plot")
library(rpart.plot)

rpart.plot(m.rpart, digits = 3)
rpart.plot(m.rpart, digits = 4, fallen.leaves = T, type = 3, extra = 101)

# 4단계 : 모델 성능 평가

p.rpart <- predict(m.rpart, wine_test)
summary(p.rpart)
summary(wine_test$quality)

cor(p.rpart, wine_test$quality)

# 평균 절대 오차로 성능 측정

MAE <- function(actual, predicted){
  mean(abs(actual - predicted))
}
MAE(p.rpart, wine_test$quality)
mean(wine_test$quality)

mean(wine_train$quality)
MAE(5.87, wine_test$quality)

# 5단계 : 모델 성능 개선

# 성능을 개선하고자 수치 예측에 있어 좀 더 복잡한 트리 으용인 모델 트리 알고리즘 적용해보기.

install.packages("Cubist")
library(Cubist)

m.cubist <- cubist(x = wine_train[-12], y = wine_train$quality)

m.cubist

summary(m.cubist)

p.cubist <- predict(m.cubist, wine_test)
summary(p.cubist)

cor(p.cubist, wine_test$quality)
MAE(wine_test$quality, p.cubist)
