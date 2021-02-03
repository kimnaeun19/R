# 8장 패턴찾기 : 연관 규칙을 이용한 장바구니 분석

# 예제 : 연관 규칙으로 자주 구매되는 식료품 식별

# 1단계 : 데이터 수집
# 2단계 : 데이터 탐색과 준비

install.packages("arules")
library(arules)

groceries <- read.transactions("groceries.csv", sep = ",")

summary(groceries)

inspect(groceries[1:5])

itemFrequency(groceries[, 1:3])


# 아이템 지지도 시각화 : 아이템 빈도 그래프

itemFrequencyPlot(groceries, support = 0.1)
itemFrequencyPlot(groceries, support = 0.05)

itemFrequencyPlot(groceries, topN = 20)

# 거래 데이터 시각화 : 희소행렬 도표화

image(groceries[1:5])

image(sample(groceries, 100))

# 3단계 : 데이터로 모델 훈련

groceryrules <- apriori(groceries, parameter = list(support = 0.006, confidence = 0.25, minlen = 2))

groceryrules

# 4단계 : 모델 성능 평가

summary(groceryrules)

inspect(groceryrules[1:3, ])

# 5단계 : 모델 성능 개선

inspect(sort(groceryrules, by = 'lift')[1:5])

# 연관규칙의 부분집합 구하기

berryrules <- subset(groceryrules, items %in% "berries")
inspect(berryrules)

# 연관 규칙을 파일이나 데이터 프레임에 저장

write(groceryrules, file = "groceryrules.csv",
      sep = ",", quote = T, row.names = F)
groceryrules_df <- as(groceryrules, "data.frame")
str(groceryrules_df)
