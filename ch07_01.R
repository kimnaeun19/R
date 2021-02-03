# 7장. 블랙박스 방법 : 신경망과 서포트 벡터 머신

# 예제 : ANN으로 콘크리트 강도 모델링

# 1단계 : 데이터 수집
# 2단계 : 데이터 탐색과 준비

concrete <- read.csv("concrete.csv")
str(concrete)

# 정규화하기

normalize <- function(x){ 
  return((x - min(x)) / (max(x)- min(x))) 
}

concret_norm <- as.data.frame(lapply(concrete, normalize))

summary(concret_norm$strength)
summary(concrete$strength)

concrete_train <- concret_norm[1 : 773, ]
concrete_test <- concret_norm[774 : 1030, ]

# 3단계 : 데이터로 모델 훈련

install.packages("neuralnet")
library(neuralnet)

concrete_model <- neuralnet(strength ~ cement + slag + ash + water + superplastic +
                              coarseagg + fineagg + age, data = concrete_train)

plot(concrete_model)

# 4단계 : 모델 성능 평가

model_results <- compute(concrete_model, concrete_test[1:8])

predicted_strength <- model_results$net.result

cor(predicted_strength,concrete_test$strength)

# 5단계 : 모델 성능 개선

concrete_model2 <- neuralnet(strength ~ cement + slag + ash + water + superplastic +
                               coarseagg + fineagg + age, data = concrete_train,
                             hidden = 5)
plot(concrete_model2)

model_results2 <- compute(concrete_model2, concrete_test[1:8])
predicted_strength2 <- model_results2$net.result

cor(predicted_strength2, concrete_test$strength)

# 소프트플러스

softplus <- function(x){
  log(1 + exp(x))
}

set.seed(12345)
concrete_model3 <- neuralnet(strength ~ cement + slag + ash + water + superplastic +
                                coarseagg + fineagg + age, data = concrete_train,
                              hidden = c(5, 5),
                             act.fct = softplus)
plot(concrete_model3)

