
###########################
#                         #
#   Team Assignment 7     #
#                         #
###########################

## Please submit one set of answers per team.                                  ##
## Your answers should be submitted as a .csv file per the instructions below. ##
## You should also submit your annotated R code per the instructions below.    ##
#################################################################################


# For this team assignment you will use the file "teamassign07train.csv" to develop
# a linear model using whatever methods you consider appropriate. You will then use
# the model that you have developed to predict the values of the response variable
# corresponding to the explanatory variable values given in the file
# "teamassign07test.csv". 

library(boot)
library(dplyr)
library(mice)

training <- read.csv("teamassign07train.csv") %>% 
  mutate(A16 = as.factor(A16),
         A2 = as.numeric(as.character(A2)),
         A14 = as.numeric(as.character(A14)))
training[training == "?"] <- NA
testing <- read.csv("teamassign07test.csv") %>% 
  mutate(A2 = as.numeric(as.character(A2)),
         A14 = as.numeric(as.character(A14)))
testing[testing == "?"] <- NA

# Eliminate ? factor level
training$A1 <- factor(training$A1)
training$A4 <- factor(training$A4)
training$A5 <- factor(training$A5)
training$A6 <- factor(training$A6)
training$A7 <- factor(training$A7)

# random forest imputation
training.complete <- complete(mice(training, method = "rf"))
testing.complete <- complete(mice(testing, method = "cart"))

# logistic regression
glmfit <- glm(A16 ~ ., data = training.complete, family = binomial)
preds <- predict(glmfit, newdata = training.complete, type = "response")
preds <- as.numeric(preds > 0.5)
x <- data.frame(y = training.complete$A16, preds)
sum(x$y == x$preds)/400
# 0.8825

# testing
predvect <- predict(glmfit, newdata = testing.complete, type = "response")
predvect <- as.numeric(predvect > 0.5)

#
# These data are from credit card applications with the variable names and values
# changed for confidentiality. Information regarding the variables is given below.
#
# Once you have predicted the values of the response variable for the testing set,
# you should save them to a vector called predvect and write them into a .csv file 
# using the following code:
write.table(predvect, file="teamassign07preds.csv", row.names=F, col.names=F, sep=",")
#
# Your annotated R code should explain the reasoning behind your choices in 
# model selection and should be neatly organized.
#
# Your grade on this team assignment will be based on how well your model predicts
# the observed values relative to the other teams.
#
#
# List of variables and values:
# A1:  b, a.
# A2:  continuous.
# A3:	 continuous.
# A4:	 u, y, l, t.
# A5:	 g, p, gg.
# A6:	 c, d, cc, i, j, k, m, r, q, w, x, e, aa, ff.
# A7:	 v, h, bb, j, n, z, dd, ff, o.
# A8:  continuous.
# A9:	 t, f.
# A10: t, f.
# A11: continuous.
# A12: t, f.
# A13: g, p, s.
# A14: continuous.
# A15: continuous.
# A16: 0,1 (whether the application was approved or denied)
