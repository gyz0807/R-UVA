# Team 3
# Leigh Harton (klh8mr), Isabelle Yang (yy3fs), Yizhe Ge (yg2kj), Thomas Molinari (tgm4br)

###########################
#                         #
#   Team Assignment 3     #
#                         #
###########################

## Please submit one set of answers per team.                  ##
## Your answers may be submitted as an annotated R file.       ##
## Please submit your plots in a PDF as a separate attachment. ##
#################################################################


#################
## Question 1: ##
#################

# For this problem you will use the files "teamassign03data01.csv" and "teamassign03data02.csv" to 
# demonstrate through simulation the effects of multicollinearity on the variance of the regression 
# coefficients and how they influence the accuracy of predictions.
#
#   (a) Repeat the following 1000 times:
#       (1) Select a random sample of 100 observations from data01.
#       (2) Fit a linear model to the 100 observations using all four variables. Save the values
#           of the estimated coefficients in separate vectors.
#       (3) Use your linear model to predict the y-values given in data02 then compute the MSE
#           using these residuals. Save this value in a vector.
#       (4) Compute the standard deviation for the vectors containing the coefficients and compute
#           the mean of the vector containing the MSEs. Record these values.
set.seed(1234)
df1 <- read.csv("teamassign03data01.csv")
df2 <- read.csv("teamassign03data02.csv")

mse <- numeric(1000)
beta0 <- numeric(1000)
beta1 <- numeric(1000)
beta2 <- numeric(1000)
beta3 <- numeric(1000)
beta4 <- numeric(1000)

for (i in 1:1000){
  temp.rows <- sample(1:nrow(df1), 100)
  temp.df1 <- df1[temp.rows, ]
  temp.lm <- lm(y ~ ., data = temp.df1)
  temp.coef <- unname(temp.lm$coefficients)
  beta0[i] <- temp.coef[1]
  beta1[i] <- temp.coef[2]
  beta2[i] <- temp.coef[3]
  beta3[i] <- temp.coef[4]
  beta4[i] <- temp.coef[5]
  temp.yvals <- unname(predict(temp.lm, newdata = df2))
  temp.resids <- df2$y - temp.yvals
  temp.mse <- sum(temp.resids^2)/(nrow(df2)-5)
  mse[i] <- temp.mse
}

beta0.sd <- sd(beta0) # 8.704385
beta1.sd <- sd(beta1) # 3.513183
beta2.sd <- sd(beta2) # 3.521479
beta3.sd <- sd(beta3) # 3.522491
beta4.sd <- sd(beta4) # 0.1400547
mse.mean <- mean(mse) # 411.68

#   (b) Choose a suitable variable to remove from the model. Repeat (1)-(4) given in part (a)
#       1000 times using this model.

# We choose to remove x1 from the model
set.seed(1234)
mse <- numeric(1000)
beta0 <- numeric(1000)
beta2 <- numeric(1000)
beta3 <- numeric(1000)
beta4 <- numeric(1000)

for (i in 1:1000){
  temp.rows <- sample(1:nrow(df1), 100)
  temp.df1 <- df1[temp.rows, ]
  temp.lm <- lm(y ~ .-x1, data = temp.df1)
  temp.coef <- unname(temp.lm$coefficients)
  beta0[i] <- temp.coef[1]
  beta2[i] <- temp.coef[2]
  beta3[i] <- temp.coef[3]
  beta4[i] <- temp.coef[4]
  temp.yvals <- unname(predict(temp.lm, newdata = df2))
  temp.resids <- df2$y - temp.yvals
  temp.mse <- sum(temp.resids^2)/(nrow(df2)-4)
  mse[i] <- temp.mse
}

beta0.sd <- sd(beta0) # 8.660312
beta2.sd <- sd(beta2) # 0.1900898
beta3.sd <- sd(beta3) # 0.1339504
beta4.sd <- sd(beta4) # 0.1391929
mse.mean <- mean(mse) # 405.4327

#   (c) How do the results from parts (a)(4) and (b)(4) compare? Explain what you observe.
# The standard errors for all betas decrease, which means that the regressors
# become more significant if we remove x1 from the model. In addition, mse decreases
# by a little bit, which means that our model fits the data better.


#################
## Question 2: ##
#################

# For this problem you will use the file "data-table-B2.XLS".
#
#   (a) Fit the model using all explanatory variables. Iteratively remove insignificant variables
#       one-by-one until the all remaining variables are significant. Which variables remain in your model?
library(readxl)
df.q2 <- read_excel("data-table-B2.XLS")
lm.fit.q2 <- lm(y ~ ., data = df.q2)
lm.fit.q2 <- update(lm.fit.q2, ~.-x5)
lm.fit.q2 <- update(lm.fit.q2, ~.-x1)
lm.fit.q2 <- update(lm.fit.q2, ~.-x2)
# x3 and x4 remain in the model

#   (b) Compute each of the five types of residuals discussed in the textbook:
#       Residuals; Standardized residuals; Studentized residuals; PRESS residuals; R-student residuals.
#       You may use R functions.
residuals <- lm.fit.q2$residuals
standardized.residuals <- residuals/sqrt(sum(lm.fit.q2$residuals^2)/lm.fit.q2$df.residual)
studentized.residuals <- rstandard(lm.fit.q2)
PRESS.residuals <- residuals(lm.fit.q2)/(1-lm.influence(lm.fit.q2)$hat)
rstudent.residuals <- rstudent(lm.fit.q2)

#   (c) Use the results from part(a) to decide if there appear to be any outliers and/or high 
#       influence points.
plot(lm.fit.q2$fitted.values, rstudent.residuals, ylim = c(-3, 3))
abline(h = 0)
abline(h = 3, lty = 2, col = "blue")
abline(h = -3, lty = 2, col = "blue")
out <- which(abs(rstudent(lm.fit.q2)) > qt(0.975, df = lm.fit.q2$df.residual))
outliers <- data.frame(fitted.val = lm.fit.q2$fitted.values[out],
                       rstudent = rstudent(lm.fit.q2)[out],
                       pt.num = out)
high.lvg <- which(apply(influence.measures(lm.fit.q2)$is.inf, 1, any))
high.lvg.points <- data.frame(fitted.val = lm.fit.q2$fitted.values[high.lvg],
                              rstudent = rstudent(lm.fit.q2)[high.lvg],
                              pt.num = high.lvg)
text(x = outliers$fitted.val, y = outliers$rstudent, labels = outliers$pt.num,
     adj = 1.5, col = "blue")
text(x = high.lvg.points$fitted.val, y = high.lvg.points$rstudent, labels = high.lvg.points$pt.num,
     adj = 1.5, col = "red")
# From the plot, we can see that point 22 is an outlier, point 24, point 1, and point 22 are high influential points

#   (d) Produce a normal probability plot of the R-student residuals and evaluate the plot for 
#       signs of departures from normality.
qqnorm(rstudent.residuals)
qqline(rstudent.residuals)
# The plot indicates that the distribution is light tailed, a little bit right skewed

#   (e) Produce plots of the R-student residuals vs. 
#       (1) the predicted values, and
#       (2) each explanatory variable.
#       What assumptions may not be satisfied? Explain.
library(car)
plot(lm.fit.q2$fitted.values, rstudent.residuals); abline(h = 0)

plot(df.q2$x3, rstudent.residuals); abline(h = 0)
plot(df.q2$x4, rstudent.residuals); abline(h = 0)

# Assumptions:
# 1. The relationship between response and explainatory variables is linear: from the r-student residuals vs
# predicted values plot, we can see a wave-like pattern. We consider they have a non-linear relationship
# 2. error has mean 0: satisfy because the points seems to be evenly distributed above and below the 
# line rstudent = 0
# 3. errors are uncorrelated: does not satisfy because there seems to be a pattern in rstudent
# vs predicted value plot
# 4. errors has constant variance: does not satisfy because the variance tends to decrease with 
# larger predicted value (rstudent vs predicted values plot)
# 5. errors are normally distributed: cannot be determined from these plots, but we can determine this from
# the qqnorm plot. The qqnorm plot shows that the distribution looks normal but we cannot say for sure because 
# it shows the distribution to be light-tailed and right skewed.





