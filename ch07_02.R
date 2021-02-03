# 예제 : SVM으로 OCR 수행

# 1단계 : 데이터 수집
# 2단계 : 데이터 탐색과 준비

letters <- read.csv("letterdata.csv")
str(letters)


letters_train <- letters[1:16000, ]
letters_test <- letters[16001:20000, ]

# 3단계 : 데이터로 모델 훈련

install.packages("kernlab")
library(kernlab)

letter_classifier <- ksvm(letter ~ ., data = letters_train,
                          kernel = 'vanilladot')

install.packages("e1071")

# 4단계 : 모델 성능 평가

letter_predictions <- predict(letter_classifier, letters_test)

table(letter_predictions, letters_test$letter)

agreement <- letter_predictions == letters_test$letter

table(agreement)
prop.table(table(agreement))

# 5단계 : 모델 성능 향상

# svm 커널 함수 변경

letter_classifier_rbf <- ksvm(letter ~ ., data = letters_train,
                              kernel = "rbfdot")

letter_predictions_rbf <- predict(letter_classifier_rbf, letters_test)

# 기존과의 비교

agreement_rbf <- letter_predictions_rbf == letters_test$letter
table(agreement_rbf)
prop.table(table(agreement_rbf))

# 최적 svm 비용 파라미터 알아내기

cost_values <- c(1, seq(from = 5, to = 40, by = 5))

accuracy_values <- sapply(cost_values, function(X){
  set.seed(12345)
  m <- ksvm(letter ~ ., data = letters_train,
            kernel = "rbfdot", C = x)
  pred <- predict(m, letters_test)
  agree <- ifelse(pred == letters_test$letter, 1, 0)
  accuracy <- sum(agree)/ nrow(letters_test)
  return(accuracy)
})

plot(cost_values, accuracy_values, type = "b")