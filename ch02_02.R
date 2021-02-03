# 데이터 탐색과 이해

usedcars <- read.csv("C:\\Users\\naeun\\Desktop\\R\\machinelearning_with_R\\usedcars.csv", stringsAsFactors = F)

str(usedcars)
summary
summary(usedcars)

summary(usedcars$year)

range(usedcars$price)
diff(range(usedcars$price))
summary(usedcars$price)

quantile(usedcars$price, probs = c(0.01, 0.99))
quantile(usedcars$price, probs = c(0.50))

boxplot(usedcars$price, main = "Boxplot of Used Car Prices",
        ylab = "Price($)")
boxplot(usedcars$mileage, main = "Boxplot of Used Car Mileage",
        ylab = "Odometer(mi)")

summary(usedcars[c('price', 'mileage')])

hist(usedcars$price, main = "Histogram of Used Car Prices",
     xlab = "Price($)")
hist(usedcars$mileage, main = "Histogram of Used Car Mileage",
     xlab = "Odometer(mi)")

table(usedcars$price)
table(usedcars$model)

color_table <- table(usedcars$color)
color_pct <- prop.table(color_table)
color_pct

color_pct <- prop.table(color_table) * 100
round(color_pct, digits = 1)

table(usedcars$color)
table(usedcars$model)

plot(x = usedcars$mileage, y = usedcars$price,
     main = "Scatterplot of Price vs. Mileage",
     xlab = "Used Car Odometer(mi.)",
     ylab =  "Used Car Price($)")

install.packages("gmodels")

usedcars$conservative <-
  usedcars$color %in% c("Black", 'Gray', 'Silver', 'White')

table(usedcars$conservative)

library(gmodels)


CrossTable(x = usedcars$model, y = usedcars$conservative)

CrossTable(x = usedcars$model, y = usedcars$conservative, chisq = T)
