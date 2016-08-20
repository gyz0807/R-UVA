# STAT 6430
# R Homework 3
# Yizhe Ge (Jake)
# yg2kj

# ** Please set the working directory to your source file location.

########################################### Problem 1
library(readr)
library(stringr)
ts <- read_lines("transactions.txt")
ts1 <- ts[grep("Library", ts)]
ts2 <- gsub("[0-9]{2}\\t.*?lane\\t", "", ts1)
ts3 <- gsub("\\t1.*\\t", "", ts2)
write.table(ts3, file="yg2kj-hw3-p1.txt", row.names = FALSE, col.names = FALSE)

########################################### Problem 2
train <- read_csv("hw3-p2-train.csv")
predict <- read_csv("hw3-p2-predict.csv")

# linear regression
ed.lm1 <- lm(wt ~ disp, data=train)
mse1 <- sum(ed.lm1$residuals^2)
mse1
# [1] 8138352

# quadratic regression
ed.lm2 <- lm(wt ~ poly(disp, 2), data=train)
mse2 <- sum(ed.lm2$residuals^2)
mse2
# [1] 7311671

# cubic regression
ed.lm3 <- lm(wt ~ poly(disp, 3), data=train)
mse3 <- sum(ed.lm3$residuals^2)
mse3
# [1] 6943802

# degree of 4 regression
ed.lm4 <- lm(wt ~ poly(disp, 4), data=train)
mse4 <- sum(ed.lm4$residuals^2)
mse4
# [1] 6333054

# degree of 5 regression
ed.lm5 <- lm(wt ~ poly(disp, 5), data=train)
mse5 <- sum(ed.lm5$residuals^2)
mse5
# [1] 6331113

# We can see that mse drops a lot from mse1 to mse4, but it only drops a little from mse4 
# to mse5. In order to prevent from over-fitting, I choose to use the 4th degree polynomial
# regression model.

prediction <- data.frame(wt=unname(predict(ed.lm4, newdata = predict)))
write_csv(prediction, "yg2kj-hw3-p2-predictions.csv")

########################################### Problem 3
auto.train <- read_csv("hw3-p3-train.csv")
auto.predict <- read_csv("hw3-p3-predict.csv")

# transform cyl into factors
auto.train$cyl <- factor(auto.train$cyl)
auto.predict$cyl <- factor(auto.predict$cyl)

# model 1
auto.lm1 <- lm(mpg ~ ., data=auto.train)
mse1 <- sum(auto.lm1$residuals^2)
mse1
# [1] 988.6081

# model 2: get the significant predictor variables from lm1
auto.lm2 <- lm(mpg ~ wt + year, data=auto.train)
mse2 <- sum(auto.lm2$residuals^2)
mse2
# 1081.013

# model 3
auto.lm3 <- lm(mpg ~ .-cyl, data=auto.train)
mse3 <- sum(auto.lm3$residuals^2)
mse3
# [1] 1069.067

# model 4: stepwise model selection
auto.lm4 <- step(auto.lm1, data=auto.train, direction = "both")
mse4 <- sum(auto.lm4$residuals^2)
mse4
# [1] 1002.524

# the smallest mse (988.6081) I can get is from model 1.

predictions <- data.frame(mpg = unname(predict(auto.lm1, newdata = auto.predict)))
write_csv(predictions, "yg2kj-hw3-p3-predictions.csv")







