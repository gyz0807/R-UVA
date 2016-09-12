######################### Problem 2.1
library(MPV)
data("table.b1")

### Part a
p1.lm <- lm(y ~ x8, data=table.b1)
# y = 21.788251 - 0.007025x8

### Part b
anova(p1.lm)
# Analysis of Variance Table
# 
# Response: y
#           Df Sum Sq Mean Sq F value    Pr(>F)    
# x8         1 178.09 178.092  31.103 7.381e-06 ***
# Residuals 26 148.87   5.726                      
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

### Part c
confint(p1.lm, level = 0.95)[2,]
# (-0.009614347, -0.004435854)

### Part d
summary(p1.lm) 
# r-squared is 0.5447, so 54.47% of the variability in y is explained by this model

### Part e
newdata <- data.frame(x8=2000)
predict(p1.lm, newdata, interval = "confidence", level = 0.95)
#       fit      lwr      upr
# 1 7.73805 6.765753 8.710348

######################### Problem 2.2
newdata <- data.frame(x8 = 1800)
predict(p1.lm, newdata, interval = "prediction", level = 0.9)
#       fit      lwr      upr
# 1 9.14307 4.936392 13.34975

######################### Problem 2.4
### Part a
data("table.b3")
lm.p4 <- lm(y ~ x1, data=table.b3)
# y = 33.72268 - 0.04736x1

### Part b
anova(lm.p4)
# Analysis of Variance Table
# 
# Response: y
#           Df Sum Sq Mean Sq F value    Pr(>F)    
# x1         1 955.72  955.72  101.74 3.743e-11 ***
# Residuals 30 281.82    9.39                      
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

### Part c
summary(lm.p4)
# 77.23% of the total variability in gasoline mileage is accounted for by
# the linear relationship with engine displacement

### Part d
newdata.p4 <- data.frame(x1=275)
pred <- predict(lm.p4, newdata.p4)
# 20.69879
conf.interval <- predict(lm.p4, newdata.p4, interval = "confidence", level = 0.95)
#        fit      lwr      upr
# 1 20.69879 19.58807 21.80952

### Part e
pred.interval <- predict(lm.p4, newdata.p4, interval = "predict", level = 0.95)
#        fit      lwr      upr
# 1 20.69879 14.34147 27.05611

### Part f
# The prediction interval is always wider than the confidence interval because
# it must account for both the uncertainty in know the value of the population
# mean, plus data scatter (future observation).

######################### Problem 2.5
### Part a
lm.p5 <- lm(y ~ x10, data=table.b3)
# y = 40.852431 - 0.005752x

### Part b
anova(lm.p5)
# Analysis of Variance Table
# 
# Response: y
#           Df Sum Sq Mean Sq F value    Pr(>F)    
# x10        1 921.53  921.53  87.482 2.121e-10 ***
# Residuals 30 316.02   10.53                      
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

### Part c
summary(lm.p5)
# 74.46%

# Based on the results, I cannot conclude that x1 is better than x10. R-squares
# are almost the same, and the coefficients are significant for both of them.

######################### Problem 2.12
data(p2.12)
### Part a
lm.p12 <- lm(usage ~ temp, data=p2.12)
# usage = -6.332 + 9.208*temp

### Part b
summary(lm.p12)
# F statistic 74120 has a p-value smaller than 2.2e-16, so the regression is significant 
# at 95% confidence level

### Part c
# H0: beta1 = 10
# Ha: beta1 != 10
xbar <- 9.20847
mu0 <- 10
se <- 0.03382
t <- (xbar - mu0)/se
# t = -23.4042

alpha <- 0.05
upper.t <- qt(1-alpha/2, df = 10)
c(-upper.t, upper.t)
# (-2.228139, 2.228139)
# Since t = -23.4042 < -2.228139, we reject the null hypothesis that beta1 = 10. In other words
# the data do not support this statement.

### Part d
newdata <- data.frame(temp = 58)
predict(lm.p12, newdata, interval = "prediction", level = 0.99)
#       fit      lwr      upr
# 1 527.759 521.2237 534.2944



