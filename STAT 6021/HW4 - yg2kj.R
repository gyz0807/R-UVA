library(car)
library(readxl)
library(MPV)

################### Problem 4.2
# Part a
p2 <- read_excel("data-table-B1.xls")
lm.p2 <- lm(y ~ x2 + x7 + x8, data = p2)
qqnorm(lm.p2$residuals)
qqline(lm.p2$residuals)
# The plot shows a little light tailed

# Part b
plot(lm.p2$fitted.values, lm.p2$residuals)
abline(h = 0)
# The model seems to be adequate since the residuals are evenly 
# spreaded above and below the line y = 0

# Part c
plot(p2$x2, lm.p2$residuals); abline(h = 0)
# The plot shows non constant variance by comparing left and right sides
plot(p2$x7, lm.p2$residuals); abline(h = 0)
# The plot shows non constant variance
plot(p2$x8, lm.p2$residuals); abline(h = 0)
# The plot looks ok

# Part d
avPlots(lm.p2)
# The partial regression plot shows that the relationship between y and
# x7 might not be a strong linear relationship

# Part e
stu.res <- rstandard(lm.p2)
rstu.res <- rstudent(lm.p2)
plot(lm.p2$fitted.values, rstu.res)
# These scaled residuals can be used to identify the potential outliers and
# influential points. In this case, the scaled residuals indicate that the
# point 1 might be an outlier since it has a scaled residual of 2.45.

################### Problem 4.4
# Part a
p4 <- read_excel("data-table-B3.xls")
lm.p4 <- lm(y ~ x1 + x6, data = p4)
qqnorm(lm.p4$residuals)
qqline(lm.p4$residuals)
# The plot seems to be a slightly light tailed

# Part b
plot(lm.p4$fitted.values, lm.p4$residuals); abline(h = 0)
# The distribution of the residuals seems not linear

# Part c
avPlots(lm.p4)
# Holding other regressors the same, x1 has a linear relationship with y,
# x6 does not have a linear relationship with y (we might want to exclude this regressor)

# Part d
stu.res <- rstandard(lm.p4)
rstu.res <- rstudent(lm.p4)
plot(lm.p4$fitted.values, rstu.res)
# These scaled residuals can be used to identify the potential outliers and
# influential points. In this case, the scaled residuals indicate that the
# point 12 and 15 might be outliers since they have scaled residuals of 2.41 and -2.7.

################### Problem 4.8
# Part a
p8 <- p2.12
lm.p8 <- lm(usage ~ temp, data = p8)
qqnorm(lm.p8$residuals)
qqline(lm.p8$residuals)
# The plot seems to be a little right skewed, but not a big issue

# Part b
plot(lm.p8$fitted.values, lm.p8$residuals)
abline(h = 0)
# There is a pattern in the plot

# Part c
plot(lm.p8$residuals)
lines(lm.p8$residuals)
abline(h = 0)
# The points are positive auto-correlated

################### Problem 4.13
p13 <- read_excel("data-table-B5.xls")
# Model 1
lm.p13.1 <- lm(y ~ x6 + x7, data = p13)
qqnorm(lm.p13.1$residuals); qqline(lm.p13.1$residuals)
# the normal probability plot shows a light tailed distribution
residualPlot(lm.p13.1)
# the residual vs fitted value plot shows that the variance is not constant
plot(lm.p13.1$fitted.values, rstudent(lm.p13.1))
abline(h = 0)
abline(h = 3, lty = 2, col = "blue")
abline(h = -3, lty = 2, col = "blue")
out <- which(abs(rstudent(lm.p13.1)) > 2.5)
outliers <- data.frame(fitted.val = lm.p13.1$fitted.values[out],
                       rstudent = rstudent(lm.p13.1)[out],
                       pt.num = out)
text(x = outliers$fitted.val, y = outliers$rstudent, labels = outliers$pt.num,
    adj = 1.5)
# the r-student residual vs fitted value plot shows that points 17 and 26 are
# potential outliers

# Model 2
lm.p13.2 <- lm(y ~ x6, data = p13)
qqnorm(lm.p13.2$residuals); qqline(lm.p13.2$residuals)
# the normal probability plot shows a light tailed distribution
residualPlot(lm.p13.2)
# the residual plot shows non-constant variance and a pattern
plot(lm.p13.2$fitted.values, rstudent(lm.p13.2))
abline(h = 0)
abline(h = 3, lty = 2, col = "blue")
abline(h = -3, lty = 2, col = "blue")
out <- which(abs(rstudent(lm.p13.2)) > 2.5)
outliers <- data.frame(fitted.val = lm.p13.2$fitted.values[out],
                       rstudent = rstudent(lm.p13.2)[out],
                       pt.num = out)
text(x = outliers$fitted.val, y = outliers$rstudent, labels = outliers$pt.num,
     adj = 1.5)
# the r-student residual vs fitted value plot shows that points 17 and 26 are
# potential outliers

lm.p13.1.press <- PRESS(lm.p13.1) # 3388.604
lm.p13.2.press <- PRESS(lm.p13.2) # 3692.881
# Since the residual plot shows deviation from the normality and non-constant variances,
# we cannot determine the best choice of model for the data by just looking at the residual
# plots we generated

################### Problem 4.25
p25 <- read_excel("data-table-B16.xls")
# Part a
lm.p25.1 <- lm(LifeExp ~ `People-per-TV` + `People-per-Dr`, data = p25)
qqnorm(lm.p25.1$residuals); qqline(lm.p25.1$residuals)

lm.p25.2 <- lm(LifeExpMale ~ `People-per-TV` + `People-per-Dr`, data = p25)
qqnorm(lm.p25.2$residuals); qqline(lm.p25.2$residuals)

lm.p25.3 <- lm(LifeExpFemale ~ `People-per-TV` + `People-per-Dr`, data = p25)
qqnorm(lm.p25.3$residuals); qqline(lm.p25.3$residuals)
# All the normal probability plots show the distributions to be light tailed

# Part b
plot(lm.p25.1$fitted.values, lm.p25.1$residuals)
plot(lm.p25.2$fitted.values, lm.p25.2$residuals)
plot(lm.p25.3$fitted.values, lm.p25.3$residuals)
# All the plots show non-constant variances and non-linear pattern

################### Problem 4.29
p29 <- read_excel("data-table-B20.xls")
lm.p29 <- lm(y ~ ., data = p29)
vif(lm.p29)
# Since x2 and x3 have vif greater than 5, we drop one of them (x2)
lm.p29 <- lm(y ~ . - x2, data = p29)
vif(lm.p29)
# after we drop x2, all vifs drop below 5, which minimizes the multicolinearity
summary(lm.p29)
lm.p29 <- lm(y ~ . - x2 - x4, data = p29)
# we drop the insignificant variable x4
summary(lm.p29)
lm.p29 <- lm(y ~ . - x2 - x4 - x5, data = p29)
# we drop the insignificant variable x5
summary(lm.p29)
# all variables are significant now
qqnorm(lm.p29$residuals); qqline(lm.p29$residuals)
# the normal probability plot shows the distribution to be right skewed
plot(lm.p29$fitted.values, lm.p29$residuals); abline(h = 0)
# The residuals vs fitted values plot shows non-linear pattern and non-constant
# variances, which means that the model is not a good one

################### Problem 5.2
p2 <- p5.2
# Part a
plot(p2$temp, p2$vapor)
# the plot shows an non-linear relationship

# Part b
lm.p2 <- lm(vapor ~ temp, data = p2)
summary(lm.p2)
# Since the p-values for the coefficients and F-statistics are both
# very small, the coefficients and the model are both significant.
qqnorm(lm.p2$residuals); qqline(lm.p2$residuals)
plot(lm.p2$fitted.values, lm.p2$residuals)
abline(h = 0)
# The normal probability plot shows that there are some issues with
# the normal distribution. The residual vs fitted values plot shows
# a non-linear pattern.

# Part c
lm.p2.1 <- lm(log(vapor) ~ I(-1/temp), data = p2)
qqnorm(lm.p2.1$residuals); qqline(lm.p2.1$residuals)
plot(lm.p2.1$fitted.values, lm.p2.1$residuals)
abline(h = 0)
# The model is improved a little bit

################### Problem 5.5
p5 <- p5.5
# Part a
lm.p5 <- lm(defects ~ weeks, data = p5)
plot(lm.p5$fitted.values, lm.p5$residuals); abline(h = 0)
# the residuals vs fitted values plot shows a non-linear pattern

# Part b
lm.p5.1 <- lm(log(defects) ~ weeks, data = p5)
plot(lm.p5.1$fitted.values, lm.p5.1$residuals); abline(h = 0)
# transform defects to log(defects) would make the model more adequate

################### Problem 5.7
p7 <- read_excel("data-table-B20.xls")
lm.p7 <- lm(y ~ ., data = p7)
vif(lm.p7)
# Since x2 and x3 have vif greater than 5, we drop one of them (x2)
lm.p7 <- lm(y ~ . - x2, data = p7)
vif(lm.p7)
# after we drop x2, all vifs drop below 5, which minimizes the multicolinearity
summary(lm.p7)
lm.p7 <- lm(y ~ . - x2 - x4, data = p7)
# we drop the insignificant variable x4
summary(lm.p7)
lm.p7 <- lm(y ~ . - x2 - x4 - x5, data = p7)
# we drop the insignificant variable x5
summary(lm.p7)
# all variables are significant now
qqnorm(lm.p7$residuals); qqline(lm.p7$residuals)
# the normal probability plot shows the distribution to be right skewed
plot(lm.p7$fitted.values, lm.p7$residuals); abline(h = 0)
# The residuals vs fitted values plot shows non-linear pattern and non-constant
# variances

lm.p7 <- lm(log(y) ~ . - x2 - x4 - x5, data = p7)
summary(lm.p7)
lm.p7 <- lm(log(y) ~ . - x2 - x4 - x5 - x1, data = p7)
# x1 is dropped since it's no longer significant
plot(lm.p7$fitted.values, lm.p7$residuals); abline(h = 0)
# The residuals vs fitted values plot for the transformed model looks much better,
# the variances tend to be more constant, and no pattern can be observed.

################### Problem 5.9
p9 <- table.b8
# Part a
lm.p9 <- lm(y ~ ., data = p9)
summary(lm.p9)
vif(lm.p9)
# all coefficients are significant, and vifs look good
qqnorm(lm.p9$residuals); qqline(lm.p9$residuals)
# the normal probability plot shows a light tailed normal distribution
plot(lm.p9$fitted.values, lm.p9$residuals); abline(h = 0)
# the residuals vs fitted values plot shows the variance increases from left
# to right by a little bit, and there exists outliers

# Part b
lm.p9.1 <- lm(sqrt(y) ~ ., data = p9)
summary(lm.p9.1)
vif(lm.p9.1)
qqnorm(lm.p9.1$residuals); qqline(lm.p9.1$residuals)
# the normal probability plot shows a light tailed normal distribution
plot(lm.p9.1$fitted.values, lm.p9.1$residuals); abline(h = 0)
# the residuals vs fitted values plot looks good

################### Problem 5.10
# Part a
p10 <- table.b9
lm.p10 <- lm(y ~ ., data = p10)
qqnorm(lm.p10$residuals)
qqline(lm.p10$residuals)
# the normal probability plot looks good
plot(lm.p10$fitted.values, lm.p10$residuals)
abline(h = 0)
# the residual vs fitted value plot shows non-constant variance and non-linear pattern

# Part b
lm.p10.1 <- lm(log(y) ~ ., data = p10)
qqnorm(lm.p10.1$residuals)
qqline(lm.p10.1$residuals)
# the normal probability plot looks great
plot(lm.p10.1$fitted.values, lm.p10.1$residuals)
abline(h = 0)
# the residual plot looks better

