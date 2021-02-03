install.packages("RWeka")
library(RWeka)

# ch2. 데이터의 관리와 이해

# 백터
subject_name <- c('Jhon Doe', 'Jane Doe', 'Steve Gravers')
temperature <- c(98.1, 98.6, 101.4)
flu_status <- c(F, F, T)

temperature[-2]
temperature[c(F, F, T)]

# 팩터

symptoms <- factor(c('SEVERE', 'MILD', 'MODERATE'),
                  levels = c('MILD', 'MODERATE', 'SEVERE'),
                  ordered = TRUE)
symptoms

symptoms > 'MODERATE'

gender <- factor(c('MALE', 'FEMALE', 'FEMALE'))
gender
 
blood <- factor(c('O', 'AB', 'A'),
                level = c('A', 'B', 'O', "AB"))
blood



# 리스트

subject_name[1]
temperature[1]
flu_status[1]
gender[1]
blood[1]
symptoms[1]

# 리스트 이용하여 한번에 나타내기

subject1 <- list(fullname = subject_name[1],
                 temperature = temperature[1],
                 flu_status = flu_status[1],
                 gender = gender[1],
                 blood = blood[1],
                 symptoms = symptoms[1])
subject1
subject1[2]

subject1[c('temperature', 'flu_status')]

# 데이터 프레임

# 환자 데이터셋용 데이터 프레임 생성

pt_data <- data.frame(subject_name, temperature,
                      flu_status, gender, blood, symptoms,
                      stringsAsFactors = F)
pt_data
pt_data$subject_name

pt_data[c('temperature', 'flu_status')]

# 새로운 열을 데이터 프레임에 추가

pt_data$temp_c <- (pt_data$temperature - 32) * (5 / 9)
pt_data$temp_c
pt_data


# 행렬

m <- matrix(c(1, 2, 3, 4), nrow = 2)
m2 <- matrix(c(1, 2, 3, 4), nrow = 2, byrow = T)

m
m2

m3 <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2)
m4 <- matrix(c(1, 2, 3, 4, 5, 6), ncol = 2)

m3
m4

m3[1,]
m4[1,]