# 11장. 모델 성능 평가

credit <- read.csv("credit.csv") 
library(caret) 


 ## Creating a simple tuned model ---- 
 # automated parameter tuning of C5.0 decision tree  
RNGversion("3.5.2") # use an older random number generator to match the book 
set.seed(300) 
m <- train(default ~ ., data = credit, method = "C5.0") 

