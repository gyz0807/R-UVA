# Team 3
# Leigh Harton (klh8mr), Isabelle Yang (yy3fs), Yizhe Ge (yg2kj), Thomas Molinari (tgm4br)

###########################
#                         #
#   Team Assignment 5     #
#                         #
###########################

## Please submit one set of answers per team.                                  ##
## Your answers should be submitted as a .csv file per the instructions below. ##
## You should also submit your annotated R code per the instructions below.    ##
#################################################################################


# For this team assignment you will use the file "teamassign05train.csv" to develop
# a linear model using whatever methods you consider appropriate. You will then use
# the model that you have developed to predict the values of the response variable
# corresponding to the explanatory variable values given in the file
# "teamassign05test.csv". 
#
# Once you have predicted the values of the response variable for the testing set,
# you should save them to a vector called predvect and write them into a .csv file 
# using the following code:
# write.table(predvect, file="teamassign05preds.csv", row.names=F, col.names=F, sep=",")
#
# Your annotated R code should explain the reasoning behind your choices in 
# model selection and should be neatly organized.
#
# Your grade on this team assignment will be based on how well your model predicts
# the observed values relative to the other teams.

library(leaps)
library(dplyr)
library(boot)
library(car)

training <- read.csv("teamassign05train.csv")
testing <- read.csv("teamassign05test.csv")

set.seed(1234)
glm.null <- glm(y ~ 1, data = training)
glm.full <- glm(y ~ ., data = training)

# Forward
fit1 <- step(glm.null, scope = list(lower = glm.null, upper = glm.full), direction = "forward")
# y ~ x3 + x7 + x6 + x4 + x1 + x2

# Backward
fit2 <- step(glm.full, scope = list(lower = glm.null, upper = glm.full), direction = "backward")
# y ~ x1 + x2 + x3 + x4 + x6 + x7

# Stepwise
fit3 <- step(glm.null, scope = list(lower = glm.null, upper = glm.full), direction = "both")
# y ~ x3 + x7 + x6 + x4 + x1 + x2
cv.glm(training, fit3)$delta[1]

# Backward CV
full.preds <- 7
cv.err <- matrix(NA, nrow = full.preds, ncol = full.preds)
for (num.predictors in full.preds:1) {
  assign(paste("cv.err", num.predictors, sep = ""), rep(0, num.predictors))
  
  if (num.predictors == full.preds) {
    assign(paste("model", num.predictors, sep = ""), combn(1:num.predictors, num.predictors))
    temp.model <- as.formula(paste("y ~ ", paste(paste("x", get(paste("model", num.predictors, sep = ""))[,1], sep = ""), collapse = " + ")))
    temp.glm <- glm(temp.model, data = training)
    temp.cv.err <- cv.glm(training, temp.glm)
    cv.err[num.predictors, 1] <- temp.cv.err$delta[1]
    assign("temp.min", which.min(cv.err[num.predictors, ]))
  } else {
    assign(paste("model", num.predictors, sep = ""), combn(get(paste("model", num.predictors+1, sep = ""))[, temp.min], num.predictors))
    
    for (j in 1:ncol(get(paste("model", num.predictors, sep = "")))) {
      temp.model <- as.formula(paste("y ~ ", paste(paste("x", get(paste("model", num.predictors, sep = ""))[,j], sep = ""), collapse = " + ")))
      temp.glm <- glm(temp.model, data = training)
      temp.cv.err <- cv.glm(training, temp.glm)
      cv.err[num.predictors, j] <- temp.cv.err$delta[1]
      assign("temp.min", which.min(cv.err[num.predictors, ]))
    }
  }
}

cv.err
#          [,1]     [,2]     [,3]     [,4]     [,5]     [,6]     [,7]
# [1,] 49.64672 75.91970       NA       NA       NA       NA       NA
# [2,] 22.83668 42.96524 28.09638       NA       NA       NA       NA
# [3,] 17.15146 17.66660 38.50745 22.77308       NA       NA       NA
# [4,] 12.57675 14.97752 15.76122 28.11186 19.37723       NA       NA
# [5,] 12.58741 14.93258 15.77746 11.74873 14.37212 16.89759       NA
# [6,] 12.67200 14.98791 11.72127 15.81069 11.80803 14.43654 16.94151
# [7,] 11.78156       NA       NA       NA       NA       NA       NA

# We can see that the third model with 6 predictors has the lowest cross validation MSE

model6
#      [,1] [,2] [,3] [,4] [,5] [,6] [,7]
# [1,]    1    1    1    1    1    1    2
# [2,]    2    2    2    2    2    3    3
# [3,]    3    3    3    3    4    4    4
# [4,]    4    4    4    5    5    5    5
# [5,]    5    5    6    6    6    6    6
# [6,]    6    7    7    7    7    7    7

# The third model with 6 predictor is y ~ x1 + x2 + x3 + x4 + x6 + x7

final.fit <- glm(y ~ . - x5, data = training)
cv.glm(training, final.fit, K = 10)$delta[1] # 11.85246

# If we remove the variable with high vif
final.fit1 <- glm(y ~ . - x5 - x1, data = training)
cv.glm(training, final.fit1, K = 10)$delta[1] # 17.05875

final.fit2 <- glm(y ~ . - x5 - x1 - x3, data = training)
cv.glm(training, final.fit2, K = 10)$delta[1] # 19.35484

final.fit3 <- glm(y ~ . - x5 - x3 - x4, data = training)
cv.glm(training, final.fit3, K = 10)$delta[1] # 15.98402
# y ~ x1 + x2 + x6 + x7

predvect <- predict(final.fit3, newdata = testing)

write.table(predvect, file="teamassign05preds.csv", row.names=F, col.names=F, sep=",")


# First, we elected to use stepwise subset selection to find a reasonable candidate model. 
# Looking at AIC , we arrived at our response regressed against x1, x2, x3, x4, x5, x6, and 
# x7. For comparison, we utilized backwards subset selection method using cross-validation MSE
# as our measuring metric. To ensure that our model escaped any multicollinearity issues, we 
# then looked at the VIF for our regressors and noticed a potential collinearity between x1 and x4. 
# Additionally, x3 appeared to be multicollinear with x1 and x4. We elected to drop x3 and x4, and 
# believe that this model will perform well against the test set without overfitting wildly.
# 
# We attempted to transform the response variable on a log scale, and achieved an increase in 
# our cross-validated MSE. However, we were concerned about the interpretability of our MSE and 
# if it was actually overfitting to our training data. We also looked into applying interaction 
# terms to our model but we were unsure how to proceed given our complete ignorance of the interplay 
# of our regressors.


