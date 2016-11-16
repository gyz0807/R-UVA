library(readxl)

############################ Problem 3.1
# Part a
p1 <- read_excel("data-table-B1.xls")
lm.p1 <- lm(y ~ x2 + x7 + x8, data = p1)
# Call:
# lm(formula = y ~ x2 + x7 + x8, data = p1)
# 
# Coefficients:
# (Intercept)           x2           x7           x8  
#   -1.808372     0.003598     0.193960    -0.004815  

# Part b
anova(lm.p1)
# Analysis of Variance Table
# 
# Response: y
#   Df  Sum Sq Mean Sq F value    Pr(>F)    
#   x2         1  76.193  76.193  26.172 3.100e-05 ***
#   x7         1 139.501 139.501  47.918 3.698e-07 ***
#   x8         1  41.400  41.400  14.221 0.0009378 ***
#   Residuals 24  69.870   2.911                      
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
summary(lm.p1)
# Since the F value has a p-value of 3.273e-08, which is extremely small, 
# the regression is significant with 95% confidence level.

# Part c
# H0: beta2 = 0
summary(lm.p1)$coef[2, 3:4]
# t-value = 5.177090e+00, p-value = 2.655723e-05, which is smaller than 0.05. 
# With 95% of confidence level, we reject the null hypothesis that beta2 = 0

# H0: beta7 = 0
summary(lm.p1)$coef[3, 3:4]
# t-value = 2.19826168, p-value = 0.03781516, which is smaller than 0.05. 
# With 95% of confidence level, we reject the null hypothesis that beta7 = 0

# H0: beta8 = 0
summary(lm.p1)$coef[4, 3:4]
# t-value = -3.7710364517, p-value = 0.0009377699, which is smaller than 0.05. 
# With 95% of confidence level, we reject the null hypothesis that beta8 = 0

# Part d
summary(lm.p1)$r.squared # 0.7863069
summary(lm.p1)$adj.r.squared # 0.7595953

# Part e
lm.p1.1 <- lm(y ~ x2 + x8, data = p1)
anova(lm.p1.1, lm.p1)
# Analysis of Variance Table
# Model 1: y ~ x2 + x8
# Model 2: y ~ x2 + x7 + x8
#   Res.Df    RSS Df Sum of Sq      F  Pr(>F)  
# 1     25 83.938                              
# 2     24 69.870  1    14.068 4.8324 0.03782 *
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# From the results, we can see that x7 decreases the RSS by 14.608. P-value of 0.03782
# shows that the F statistic is significant at confidence level of 95%. The partial F 
# statistic equals to t^2 for t of beta7 calculated in part c.

############################ Problem 3.3
# Part a
confint(lm.p1)[3,]
#      2.5 %     97.5 % 
# 0.01185532 0.37606510 

# Part b
newdata <- data.frame(x2 = 2300, x7 = 56, x8 = 2100)
predict(lm.p1, newdata, interval = "confidence")
#      fit      lwr      upr
# 7.216424 6.436203 7.996645

############################ Problem 3.4
# Part a
lm.p4 <- lm(y ~ x7 + x8, data = p1)
summary(lm.p4)
# F-statistic: 15.13 on 2 and 25 DF,  p-value: 4.935e-05. Since p-value is smaller
# than 0.05, the regression is significant with 95% confidence level.

# Part b
summary(lm.p4)$r.squared # 0.5476628
summary(lm.p4)$adj.r.squared # 0.5114759
# R squared is decreased by 23.86411%, and adjusted R squared is decreased by 24.81194%.
# The decreased R squares means that without including x2, we can explain less variability
# in y by using the explanatory variables.

# Part c
confint(lm.p4)[2,]
#      2.5 %     97.5 % 
# -0.1971643  0.2939060 
newdata <- data.frame(x7 = 56, x8 = 2100)
predict(lm.p4, newdata, interval = "confidence")
#      fit      lwr      upr
# 6.926243 5.828643 8.023842

# In this question, the length of CI of beta7 is 0.4910703
# In this question, the length of CI of y is 2.195199
# In Problem 3.3, the length of CI of beta7 is 0.3642098
# In Problem 3.3, the length of CI of y is 1.560442
# We can see that the lengths of the CIs become larger if we exclude x2 from the model

# Part d
# Omitting an important regressor from a model would affect r-squared, adjusted r-squared,
# coefficients of other regressors, and the confidence intervals.

############################ Problem 3.5
# Part a
p5 <- read_excel("data-table-B3.xls")
lm.p5 <- lm(y ~ x1 + x6, data = p5)
# Call:
# lm(formula = y ~ x1 + x6, data = p5)
# 
# Coefficients:
#   (Intercept)           x1           x6  
#      32.88455     -0.05315      0.95922 

# Part b
anova(lm.p5)
# Analysis of Variance Table
# 
# Response: y
#           Df Sum Sq Mean Sq F value    Pr(>F)    
# x1         1 955.72  955.72 105.290 3.666e-11 ***
# x6         1  18.59   18.59   2.048    0.1631    
# Residuals 29 263.23    9.08                      
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
summary(lm.p5)
# F-statistic: 53.67 on 2 and 29 DF, p-value: 1.79e-10, since the p-value is
# smaller than 0.05, we can say that the regression is significant with 95%
# confidence level.

# Part c
summary(lm.p5)$r.squared # 0.7872928
summary(lm.p5)$adj.r.squared # 0.7726233

# In problem 2.4
lm.p2.4 <- lm(y ~ x1, data = p5)
summary(lm.p2.4)$r.squared # 0.7722712
summary(lm.p2.4)$adj.r.squared # 0.7646803
# Comparing to problem 2.4, both r squared and adjusted r squared become larger after
# we introduce x6 into the model. However, the they only differ by a little bit.

# Part d
confint(lm.p5)[2,]
#       2.5 %      97.5 % 
# -0.06569892 -0.04059641 

# Part e
summary(lm.p5)$coef[2,3:4]
#       t value      Pr(>|t|) 
# -8.660425e+00  1.549965e-09 
summary(lm.p5)$coef[3,3:4]
#   t value  Pr(>|t|) 
# 1.4310845 0.1630948
# From the test statistics, we can say that, with 95% confidence level, we can reject
# the null hypothesis that beta1=0, but we cannot reject the null hypothesis that 
# beta6=0.

# Part f
newdata <- data.frame(x1 = 275, x6 = 2)
predict(lm.p5, newdata, interval = "confidence")
#      fit      lwr      upr
# 20.18739 18.87221 21.50257

# Part g
predict(lm.p5, newdata, interval = "prediction")
#      fit     lwr      upr
# 20.18739 13.8867 26.48808

############################ Problem 3.6
# For problem 2.4
newdata <- data.frame(x1 = 275)
predict(lm.p2.4, newdata, interval = "confidence")
#      fit      lwr      upr
# 20.69879 19.58807 21.80952
predict(lm.p2.4, newdata, interval = "prediction")
#      fit      lwr      upr
# 20.69879 14.34147 27.05611
# Length of CI = 2.22145
# Length of prediction interval = 12.71464

# For problem 3.5
# Length of CI = 2.63036
# Length of prediction interval = 12.60138

# The length of CI becomes larger and the length of PI becomes smaller. However, both of
# them only changed a small amount. They show that x6 might not be a great predictor.

############################ Problem 3.8
# Part a
p8 <- read_excel("data-table-B5.xls")
lm.p8 <- lm(y ~ x6 + x7, data = p8)
# Call:
# lm(formula = y ~ x6 + x7, data = p8)
# 
# Coefficients:
# (Intercept)           x6           x7  
#     2.52646      0.01852      2.18575  

# Part b
summary(lm.p8)
# F-statistic: 27.95 on 2 and 24 DF,  p-value: 5.391e-07, the small p-value shows that
# the regression is significant with 95% confidence level
summary(lm.p8)$r.squared # 0.699644
summary(lm.p8)$adj.r.squared # 0.6746144

# Part c
summary(lm.p8)$coef[2:3, 3:4]
#     t value     Pr(>|t|)
# x6 6.742016 5.662928e-07
# x7 2.247109 3.409779e-02
# Both x6 and x7 are significant with 95% confidence level

# Part d
confint(lm.p8)[2:3,]
#         2.5 %     97.5 %
# x6 0.01285196 0.02419204
# x7 0.17820756 4.19329833

# Part e
lm.p8.1 <- lm(y ~ x6, data = p8)
summary(lm.p8.1)
# F-statistic: 43.77 on 1 and 25 DF,  p-value: 6.238e-07, so the regression is significant
# with 95% confidence level
summary(lm.p8.1)$r.squared # 0.6364504
summary(lm.p8.1)$adj.r.squared # 0.6219084
# The statistics are almost the same as those with x7 as a regressor. To prevent from over
# fitting, we might just want to use x6 as the single regressor.

# Part f
confint(lm.p8.1)[2,]
#      2.5 %     97.5 % 
# 0.01335688 0.02543261 
# length = 0.01207573
# For part d, length = 0.01134008
# Since the lengths are almost the same, x7 does not contribute a lot to the model.

# Part g
# For the model in part a
sum(lm.p8$residuals^2)/lm.p8$df.residual # 98.49313
# For the model in part e
sum(lm.p8.1$residuals^2)/lm.p8.1$df.residual # 114.447
# MSRes becomes lower after we introduce x7 into the model

############################ Problem 3.11
# Part a
p11 <- read_excel("data-table-B7.xls")
lm.p11 <- lm(y ~ ., data = p11)
# Call:
# lm(formula = y ~ ., data = p11)
# 
# Coefficients:
#   (Intercept)           x1           x2           x3           x4  
#     5.208e+01    5.556e-02    2.821e-01    1.250e-01    1.776e-16  
#         x5  
# -1.606e+01  

# Part b
summary(lm.p11)
# F-statistic: 29.86 on 5 and 10 DF,  p-value: 1.055e-05. Because of the 
# small p-value, with 95% confidence level, we can say that the regression
# is significant

# Part c
summary(lm.p11)$coef[2:6, 3:4]
#          t value     Pr(>|t|)
# x1  1.859806e+00 9.254448e-02
# x2  4.897489e+00 6.254494e-04
# x3  3.099676e-01 7.629488e-01
# x4  8.809810e-16 1.000000e+00
# x5 -1.103485e+01 6.401184e-07

# With 95% confidence level, x2 and x5 are significant to the model

# Part d
summary(lm.p11)$r.squared # 0.9372286
summary(lm.p11)$adj.r.squared # 0.9058429

lm.p11.1 <- lm(y ~ x2 + x5, data = p11)
summary(lm.p11.1)$r.squared # 0.9149136
summary(lm.p11.1)$adj.r.squared # 0.9018234
# r-squared decreases by 2% if we just use temperature and particle size
# as predictors. However, adjusted r squared are almost the same, which 
# means that more variables lead to the penalization of r-squared for the
# first model. It might be sufficient for us to just use temperature and
# particle size as predictors.

# Part e
confint(lm.p11)[3,]
#     2.5 %    97.5 % 
# 0.1537804 0.4105053
confint(lm.p11.1)[2,]
#     2.5 %    97.5 % 
# 0.1550559 0.4092298 

# the length of confidence interval becomes a little smaller after we keep only
# temperature and particle size as regressors, which again consolidate our
# point that temperature and particle size are sufficient to use as predictors.

############################ Problem 3.16
p16 <- read_excel("data-table-B16.xls")

# Part a
lm.p16.1 <- lm(LifeExp ~ `People-per-TV` + `People-per-Dr`, data = p16)
# Call:
# lm(formula = LifeExp ~ `People-per-TV` + `People-per-Dr`, data = p16)
# 
# Coefficients:
#   (Intercept)  `People-per-TV`  `People-per-Dr`  
#     70.236265        -0.022607        -0.000447  
lm.p16.2 <- lm(LifeExpMale ~ `People-per-TV` + `People-per-Dr`, data = p16)
# Call:
# lm(formula = LifeExpMale ~ `People-per-TV` + `People-per-Dr`, 
#      data = p16)
# 
# Coefficients:
#   (Intercept)  `People-per-TV`  `People-per-Dr`  
#    73.0919445       -0.0256825       -0.0004785 
lm.p16.3 <- lm(LifeExpFemale ~ `People-per-TV` + `People-per-Dr`, data = p16)
s# Call:
# lm(formula = LifeExpFemale ~ `People-per-TV` + `People-per-Dr`, 
#      data = p16)
# 
# Coefficients:
#   (Intercept)  `People-per-TV`  `People-per-Dr`  
#    67.4297595       -0.0198637       -0.0004086  

# Part b
summary(lm.p16.1)
# F-statistic: 13.46 on 2 and 35 DF,  p-value: 4.623e-05
summary(lm.p16.2)
# F-statistic: 12.53 on 2 and 35 DF,  p-value: 7.863e-05
summary(lm.p16.3)
# F-statistic: 14.07 on 2 and 35 DF,  p-value: 3.279e-05
# Since all models have small p-values, with 95% confidence level, these
# models are significant

# Part c
summary(lm.p16.1)$coef[2:3, 3:4]
#                   t value   Pr(>|t|)
# `People-per-TV` -2.354823 0.02426882
# `People-per-Dr` -2.217491 0.03317745
summary(lm.p16.2)$coef[2:3, 3:4]
#                   t value   Pr(>|t|)
# `People-per-TV` -2.337096 0.02528241
# `People-per-Dr` -2.073791 0.04552052
summary(lm.p16.3)$coef[2:3, 3:4]
#                   t value   Pr(>|t|)
# `People-per-TV` -2.362156 0.02386039
# `People-per-Dr` -2.313711 0.02667785

# Since all p-values are smaller than 0.05, with 95% confidence level,
# we can say that all predictors in each model are significant.

# Part d
summary(lm.p16.1)$r.squared # 0.4346959
summary(lm.p16.1)$adj.r.squared # 0.4023928
summary(lm.p16.2)$r.squared # 0.4172765
summary(lm.p16.2)$adj.r.squared # 0.383978
summary(lm.p16.3)$r.squared # 0.4456856
summary(lm.p16.3)$adj.r.squared # 0.4140105

# Part e
confint(lm.p16.1)[3,]
#         2.5 %        97.5 % 
# -8.563196e-04 -3.777668e-05 
confint(lm.p16.2)[3,]
#         2.5 %        97.5 % 
# -9.470177e-04 -1.008023e-05 
confint(lm.p16.3)[3,]
#         2.5 %        97.5 % 
# -7.670492e-04 -5.007977e-05 











