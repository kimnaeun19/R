# 예제 :  선형 회귀를 이용한 의료비 예측

# 1단계 : 데이터 수집
# 2단계 : 데이터 탐색과 준비

insurance <- read.csv("insurance.csv", stringsAsFactors = T)
str(insurance)

cor(insurance[c("age", "bmi", "children", "expenses")])

pairs(insurance[c("age", "bmi", "children", "expenses")])

install.packages("psych")
library(psych)

pairs.panels(insurance[c("age", "bmi", "children", "expenses")])

# 3단계 : 데이터로 모델 훈련

ins_model <- lm(expenses ~ age + children + bmi + sex + smoker + region,
                data = insurance)
ins_model

# 4단계 : 모델 성능 평가

summary(ins_model)

# 5단계 : 모델 성능 개선

# 모델 명시 :  비선형 관계 추가

insurance$age2 <- insurance$age^2

# 변환 : 수치 변수를 이진 지시 변수로 전환

insurance$bmi30 <- ifelse(insurance$bmi >= 30, 1, 0)

# 모델명시 : 상호작용 영향 추가
# 모두 합치기 : 개선된 회귀 모델

ins_model2 <- lm(expenses ~ age + age2 + children + bmi + sex + 
                   bmi30*smoker + region, data = insurance)

summary(ins_model2)

# 회귀 모델로 예측

insurance$pred <- predict(ins_model2, insurance)
cor(insurance$pred, insurance$expenses)

plot(insurance$pred, insurance$expenses)
abline(a = 0, b = 1, col = 'red', lwd = 3, lty = 2)

predict(ins_model2,
        data.frame(age = 30, age2 = 30^2, children = 2,
                   bmi = 30, sex = "male", bmi30 = 1,
                   smoker = "no", region = "northeast"))

predict(ins_model2,
        data.frame(age = 30, age2 = 30^2, children = 2,
                   bmi = 30, sex = "female", bmi30 = 1,
                   smoker = "no", region = "northeast"))

predict(ins_model2,
        data.frame(age = 30, age2 = 30^2, children = 0,
                   bmi = 30, sex = "female", bmi30 = 1,
                   smoker = "no", region = "northeast"))