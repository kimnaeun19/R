# 예졔 : 규칙 학습자를 이용한 독버섯 식별

# 1단계 : 데이터 수집
# 2단계 : 데이터 탐색과 준비

mushrooms <- read.csv("mushrooms.csv", stringsAsFactors = T)
str(mushrooms)

mushrooms$veil_type <- NULL

table(mushrooms$type)

# 3단계 : 데이터로 모델 훈련

install.packages("OneR")
library(OneR)

mushrooms_1R <- OneR(type ~., data = mushrooms)

mushrooms_1R
# -> 결과를 보고 냄새(odor)이 고약하면(fishy, foul, ...)이면 독이 있을 가능성이 있다.
# 단순한 경험적 규칙인 " 버섯이 입맛이 떨어지는 냄새가 나면 독이 있을 가능성이 있다."로 요약 가능

# 4단계 : 모델 성능 평가

mushrooms_1R_pred <- predict(mushrooms_1R, mushrooms)
table(actual = mushrooms$type, predicted = mushrooms_1R_pred)

# 5단계 : 모델 성능 개선

install.packages("RWeka")
library(RWeka)

mushrooms_JRip <- JRip(type ~., data = mushrooms)
mushrooms_JRip
