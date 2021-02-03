# e1071 사용해보기

letters_02 <- read.csv("letterdata.csv")
str(letters_02)

letters_02_train <- letters_02[1:16000,]
letters_02_test <- letters[16001:20000,]

install.packages("e1071")
library(e1071)
letter_classifier_02 <- svm(letter ~ ., data = letters_02_train,
                            kernel = "linear", type = "C-classification", scale = FALSE)
