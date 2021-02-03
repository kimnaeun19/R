# 09장. 데이터 그룹 찾기 : k - 평군 군집화

# 예제 : k - 평균 군집화를 이용한 십대 시장 세분화 발굴

# 1단계 : 데이터 수집
# 2단계 : 데이터 탐색과준비

getwd()
teens <- read.csv("snsdata.csv")

str(teens)


table(teens$gender)
table(teens$gender, useNA = "ifany")

summary(teens)
summary(teens$age)

teens$age <- ifelse(teens$age >= 13 & teens$age < 20, teens$age, NA)

summary(teens$age)

# 데이터 준비 : 결측치 더미 코딩

teens$female <- ifelse(teens$gender == "F" & !is.na(teens$gender), 1, 0)
teens$no_gender <- ifelse(is.na(teens$gender), 1, 0)

table(teens$gender, useNA = "ifany")
table(teens$female, useNA = "ifany")
table(teens$no_gender, useNA = "ifany")

# 데이터 준비 : 결측치 대체

mean(teens$age)
mean(teens$age, na.rm = T)

aggregate(data = teens, age ~ gradyear, mean, na.rm = TRUE) 

ave_age <- ave(teens$age, teens$gradyear, FUN = function(x) mean(x, na.rm = TRUE)) 

teens$age <- ifelse(is.na(teens$age), ave_age, teens$age) 

summary(teens$age) 

# 3단계 : 데이터로 모델 훈련

interests <- teens[5:40]

interests_Z <- as.data.frame(lapply(interests, scale))

summary(interests$basketball)
summary(interests_Z$basketball)

set.seed(2345)
teens_clusters <- kmeans(interests_Z,5)

# 4단계 : 모델 성능 평가

teens_clusters$size

teens_clusters$centers

teens[1:5, c("cluster", "gender", "age", "friends")]
# 5단계 : 모델 성능 개선

teens$cluster <- teens_clusters$cluster

aggregate(data = teens, age ~ cluster, mean)
aggregate(data = teens, female ~ cluster, mean)
aggregate(data = teens, age ~ cluster, mean)