# Team 3
# Leigh Harton (klh8mr), Isabelle Yang (yy3fs), Yizhe Ge (yg2kj), Thomas Molinari (tgm4br)

###########################
#                         #
#   Team Assignment 4     #
#                         #
###########################

## Please submit one set of answers per team.                    ##
## Your answers may be submitted as an annotated R file.         ##
## Please submit your plots in one PDF as a separate attachment. ##
###################################################################


#################
## Question 1: ##
#################

# For each part of this problem, you are to create (or find) a data set with about 30 observations 
# that is suitable for simple linear regression and satisfies the given specifications. If it is 
# not possible, explain why it is not possible. For each part, include a plot that shows the 
# interesting points.
#
#   (a) The data set has a point that is clearly visible for all four types of residuals 
#       discussed -- standardized, studentized, PRESS, R-student.
#       discussed -- standardized, studentized, PRESS, R-student.
set.seed(1234)
x <- seq(1:30)
y <- x + rnorm(30)
data.1a <- data.frame(x, y)
data.1a$x[3] <- 3
data.1a$y[3] <- 30

lm.1a <- lm(y~x, data = data.1a)
mse.1a <- sum(residuals(lm.1a)^2)/lm.1a$df.residual
standardized.residuals.1a <- residuals(lm.1a)/sqrt(mse.1a) # standardized
studentized.residuals.1a <- rstandard(lm.1a) # studentized
PRESS.residuals.1a <- residuals(lm.1a)/(1-lm.influence(lm.1a)$hat) # PRESS
R.student.residuals.1a <- rstudent(lm.1a) # R-student

## Plots ## 
par(mfrow = c(2,2))
plot(lm.1a$fitted.values, standardized.residuals.1a, 
     main = 'Standardized Residuals Plot'); abline(h = 0)
plot(lm.1a$fitted.values, studentized.residuals.1a, 
     main = 'Studentized Residuals Plot'); abline(h = 0)
plot(lm.1a$fitted.values, PRESS.residuals.1a, 
     main = 'PRESS Residuals Plot'); abline(h = 0)
plot(lm.1a$fitted.values, R.student.residuals.1a, 
     main = 'R-Student Residuals Plot'); abline(h = 0)

data.1a
# Dataset:
#     x          y
# 1   1 -0.2070657
# 2   2  2.2774292
# 3   3 30.0000000
# 4   4  1.6543023
# 5   5  5.4291247
# 6   6  6.5060559
# 7   7  6.4252600
# 8   8  7.4533681
# 9   9  8.4355480
# 10 10  9.1099622
# 11 11 10.5228073
# 12 12 11.0016136
# 13 13 12.2237461
# 14 14 14.0644588
# 15 15 15.9594941
# 16 16 15.8897145
# 17 17 16.4889905
# 18 18 17.0888046
# 19 19 18.1628283
# 20 20 22.4158352
# 21 21 21.1340882
# 22 22 21.5093141
# 23 23 22.5594521
# 24 24 24.4595894
# 25 25 24.3062798
# 26 26 24.5517951
# 27 27 27.5747557
# 28 28 26.9763443
# 29 29 28.9848617
# 30 30 29.0640514
##################
# Observation 3 is clearly visible for all four types of residuals.

#   (b) The data set has a point that stands out when viewing studentized residuals but not 
#       when viewing standardized residuals.
#       when viewing standardized residuals.
set.seed(1234)
x <- seq(1:30)
y <- x + rnorm(30)
data.1b <- data.frame(x, y)
data.1b$x[3] <- 100
data.1b$y[3] <- 90

lm.1b <- lm(y~x, data = data.1b)
standardized.residuals.1b <- residuals(lm.1b)/sum(residuals(lm.1a)^2)/lm.1a$df.residual # standardized
studentized.residuals.1b <- rstandard(lm.1b) # studentized

## Plots ## 
par(mfrow = c(1,2))
plot(lm.1b$fitted.values, standardized.residuals.1b, 
     main = 'Standardized Residuals Plot'); abline(h = 0)
plot(lm.1b$fitted.values, studentized.residuals.1b, 
     main = 'Studentized Residuals Plot'); abline(h = 0)

data.1b
# Dataset: 
#      x          y
# 1    1 -0.2070657
# 2    2  2.2774292
# 3  100 90.0000000
# 4    4  1.6543023
# 5    5  5.4291247
# 6    6  6.5060559
# 7    7  6.4252600
# 8    8  7.4533681
# 9    9  8.4355480
# 10  10  9.1099622
# 11  11 10.5228073
# 12  12 11.0016136
# 13  13 12.2237461
# 14  14 14.0644588
# 15  15 15.9594941
# 16  16 15.8897145
# 17  17 16.4889905
# 18  18 17.0888046
# 19  19 18.1628283
# 20  20 22.4158352
# 21  21 21.1340882
# 22  22 21.5093141
# 23  23 22.5594521
# 24  24 24.4595894
# 25  25 24.3062798
# 26  26 24.5517951
# 27  27 27.5747557
# 28  28 26.9763443
# 29  29 28.9848617
# 30  30 29.0640514
###################
# Observation 3 stands out for studentized residuals but not for standardized residuals. 

#   (c) The data set has a point that stands out when viewing PRESS residuals but not when
#       viewing standardized residuals.

PRESS.residuals.1b <- residuals(lm.1b)/(1-lm.influence(lm.1b)$hat) # PRESS
plot(lm.1b$fitted.values, PRESS.residuals.1b, 
     main = 'PRESS Residuals Plot'); abline(h = 0)
plot(lm.1b$fitted.values, standardized.residuals.1b, 
     main = 'Standardized Residuals Plot'); abline(h = 0)

# If we use the same dataset in 1(b), observation 3 also stands out for PRESS residuals
# but not for standardized residuals. 

#   (d) The data set has a point that stands out when viewing R-student residuals but not when 
#       viewing standardized residuals.

R.student.residuals.1b <- rstudent(lm.1b) # R-student
plot(lm.1b$fitted.values, R.student.residuals.1b, 
     main = 'R-Student Residuals Plot'); abline(h = 0)
plot(lm.1b$fitted.values, standardized.residuals.1b, 
     main = 'Standardized Residuals Plot'); abline(h = 0)

# If we use the same dataset in 1(b), observation 3 also standsout for R-student residuals
# but not for standarized residuals.

#   (e) The data set has a point that stands out when viewing PRESS residuals but not when 
#       viewing studentized residuals.

set.seed(1234)
x <- seq(1:30)
y <- x + rnorm(30)
data.1e <- data.frame(x, y)
data.1e$x[3] <- 800
data.1e$y[3] <- 780

lm.1e <- lm(y~x, data = data.1e)
studentized.residuals.1e <- rstandard(lm.1e) # studentized
PRESS.residuals.1e <- residuals(lm.1e)/(1-lm.influence(lm.1e)$hat) # PRESS

## Plots ## 
plot(lm.1e$fitted.values, studentized.residuals.1e, 
     main = 'Studentized Residuals Plot'); abline(h = 0)
plot(lm.1e$fitted.values, PRESS.residuals.1e, 
     main = 'PRESS Residuals Plot'); abline(h = 0)

data.1e
# Dataset: 
#      x           y
# 1    1  -0.2070657
# 2    2   2.2774292
# 3  800 780.0000000
# 4    4   1.6543023
# 5    5   5.4291247
# 6    6   6.5060559
# 7    7   6.4252600
# 8    8   7.4533681
# 9    9   8.4355480
# 10  10   9.1099622
# 11  11  10.5228073
# 12  12  11.0016136
# 13  13  12.2237461
# 14  14  14.0644588
# 15  15  15.9594941
# 16  16  15.8897145
# 17  17  16.4889905
# 18  18  17.0888046
# 19  19  18.1628283
# 20  20  22.4158352
# 21  21  21.1340882
# 22  22  21.5093141
# 23  23  22.5594521
# 24  24  24.4595894
# 25  25  24.3062798
# 26  26  24.5517951
# 27  27  27.5747557
# 28  28  26.9763443
# 29  29  28.9848617
# 30  30  29.0640514
####################
# Observation 3 stands out for PRESS residuals but not for studentized residuals. 


#   (f) The data set has a point that stands out when viewing R-student residuals but not when 
#       viewing PRESS residuals.
par(mfrow = c(1,2))
x <- seq(1:30)
y <- x + rnorm(30, sd = 0.05)
df <- data.frame(x = x, y = y)

df[4, ] <- c(4, 4.3)
lm.f <- lm(y ~ x, data = df)
rstu.residuals <- rstudent(lm.f)
press.residuals <- residuals(lm.f)/(1-lm.influence(lm.f)$hat)
plot(lm.f$fitted.values, rstu.residuals, ylim = c(-2, 7)); abline(h = 0)
plot(lm.f$fitted.values, press.residuals, ylim = c(-2, 7)); abline(h = 0)

#################
## Question 2: ##
#################

# For each part of this problem, you are to create (or find) a data set with about 30 observations 
# that is suitable for simple linear regression and satisfies the given specifications on the variance
# of the residuals. For each part, include a plot of your residuals that shows the required characteristic.
#
#   (a) The residuals have constant variance.

# Assume E(y) = 1:15, variance = 1, so sd = sqrt(variance) = 1
par(mfrow = c(1,1))
x <- rep(seq(1:15), times = 2)
y <- c(1:15 + 1, 1:15 - 1)
lm.fit <- lm(y ~ x)
plot(lm.fit$fitted.values, lm.fit$residuals); abline(h = 0)

#   (b) The residuals have variance proportional to E(y).

# variance is proportional to E(y) == sd is proportional to sqrt(E(y))
x <- rep(seq(1:15), times = 2)
y <- c(1:15 + sqrt(1:15), 1:15 - sqrt(1:15))
lm.fit <- lm(y ~ x)
plot(lm.fit$fitted.values, lm.fit$residuals); abline(h = 0)

#   (c) The residuals have variance proportional to E(y)^2.

# variance is proportional to E(y)^2 == sd is proportional to E(y)
x <- rep(seq(1:15), times = 2)
y <- c(1:15 + 1:15, 1:15 - 1:15)
lm.fit <- lm(y ~ x)
plot(lm.fit$fitted.values, lm.fit$residuals); abline(h = 0)

#   (d) The residuals have variance proportional to 1/E(y).

# variance is proportional to 1/E(y) == sd is proportional to sqrt(1/E(y))
x <- rep(seq(1:15), times = 2)
y <- c(1:15 + sqrt(1/1:15), 1:15 - sqrt(1/1:15))
lm.fit <- lm(y ~ x)
plot(lm.fit$fitted.values, lm.fit$residuals); abline(h = 0)

#   (e) The residuals have variance proportional to C-E(y) for some constant C.

# Assume C = 20
# variance is proportional to C-E(y) == sd is proportional to sqrt(C-E(y))
x <- rep(seq(1:15), times = 2)
y <- c(1:15 + sqrt(20 - 1:15), 1:15 - sqrt(20 - 1:15))
lm.fit <- lm(y ~ x)
plot(lm.fit$fitted.values, lm.fit$residuals); abline(h = 0)

#   (f) The residuals have variance proportional to E(y)(C-E(y)) for some constant C.

# Assume C = 20
# variance is proportional to E(y)(C-E(y)) == sd is proportional to sqrt(E(y)(C-E(y)))
x <- rep(seq(1:15), times = 2)
y <- c(1:15 + sqrt(1:15*(20 - 1:15)), 1:15 - sqrt(1:15 * (20 - 1:15)))
lm.fit <- lm(y ~ x)
plot(lm.fit$fitted.values, lm.fit$residuals); abline(h = 0)


