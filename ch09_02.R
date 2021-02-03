teens <- read.csv("snsdata.csv") 
str(teens) 
 

# look at missing data for female variable 
table(teens$gender) 
table(teens$gender, useNA = "ifany") 
 
# look at missing data for age variable 
summary(teens$age) 
 

# eliminate age outliers 
teens$age <- ifelse(teens$age >= 13 & teens$age < 20, teens$age, NA) 
summary(teens$age) 
 

# reassign missing gender values to "unknown" 
teens$female <- ifelse(teens$gender == "F" & !is.na(teens$gender), 1, 0) 
teens$no_gender <- ifelse(is.na(teens$gender), 1, 0) 
 

# check our recoding work 
table(teens$gender, useNA = "ifany") 
table(teens$female, useNA = "ifany") 
table(teens$no_gender, useNA = "ifany") 


# finding the mean age by cohort 
mean(teens$age) # doesn't work 
mean(teens$age, na.rm = TRUE) # works 
# age by cohort 
aggregate(data = teens, age ~ gradyear, mean, na.rm = TRUE) 

# create a vector with the average age for each gradyear, repeated by person 
ave_age <- ave(teens$age, teens$gradyear, FUN = function(x) mean(x, na.rm = TRUE)) 

teens$age <- ifelse(is.na(teens$age), ave_age, teens$age) 

# check the summary results to ensure missing values are eliminated 
summary(teens$age) 


