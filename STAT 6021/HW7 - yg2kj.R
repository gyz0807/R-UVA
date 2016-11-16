library(MPV)
library(aod)
library(readxl)
library(lmtest)

### Problem 11.1
# Part a
p1 <- table.b1
lm.p1 <- lm(y ~ x2 + x7 + x8, data = p1)
# Call:
#   lm(formula = y ~ x2 + x7 + x8, data = p1)
# 
# Coefficients:
#   (Intercept)           x2           x7           x8  
#     -1.808372     0.003598     0.193960    -0.004815  
press <- PRESS(lm.p1) # 87.46123
r2.pred <- 1 - press/sum((p1$y - mean(p1$y))^2)
r2.pred
# 0.7325052, which indicates a great predict power

# Part b
del.rows <- sample(nrow(p1), nrow(p1)/2)
p1.b <- p1[-del.rows,]
lm.p1.b <- lm(y ~ x2 + x7 + x8, data = p1.b)
# Call:
#   lm(formula = y ~ x2 + x7 + x8, data = p1.b)
# 
# Coefficients:
#   (Intercept)           x2           x7           x8  
#     -1.343722     0.003555     0.182379    -0.004764  

# The coefficients have changed, but not dramatically

preds <- predict(lm.p1.b, newdata = p1[del.rows, ])
df <- data.frame(y = p1[del.rows, ]$y, preds = preds)
#     y     preds
# 28  0  1.560804
# 4  13 11.116445
# 10  2  4.136920
# 11  7  6.776562
# 24 10 10.195435
# 3  11  7.996495
# 14  9  9.037698
# 7  10 11.583456
# 5  10  9.693319
# 2  11  8.851941
# 13  9  8.915317
# 22  3  1.628653
# 9   4  1.801705
# 18  5  5.342702

# The results show that it doesn't predict well

# Part c
del.rows <- c(8, 7, 17, 26, 11, 9)
p1.c <- p1[-del.rows,]
lm.p1.c <- lm(y ~ x2 + x7 + x8, data = p1.b)
preds <- predict(lm.p1.c, newdata = p1[del.rows, ])
df <- data.frame(y = p1[del.rows, ]$y, preds = preds)
#     y     preds
# 8  11 10.289715
# 7  10 11.583456
# 17  5  5.197535
# 26  8  7.880415
# 11  7  6.776562
# 9   4  1.801705

# The results show that the model is doing very well

### Problem 11.2
set.seed(1)
p2 <- table.b1
pred.rows <- sample(nrow(p2), nrow(p2)/2)
estimation.data <- p2[-pred.rows, ]
prediction.data <- p2[pred.rows, ]
range(estimation.data$y) # 0 11
range(prediction.data$y) # 2 13
# So the team with 13 wins (y = 13) is an extrapolation
lm.p2 <- lm(y ~ ., data = estimation.data)
# Call:
#   lm(formula = y ~ ., data = estimation.data)
# 
# Coefficients:
#   (Intercept)           x1           x2           x3           x4  
# 65.517955     0.004411    -0.005815    -1.320194     0.130843  
# x5           x6           x7           x8           x9  
# 0.282964    -0.016467     0.239139     0.005028    -0.012218  
preds <- predict(lm.p2, newdata = prediction.data)
df <- data.frame(y = prediction.data$y, predicted = preds)
#     y    predicted
# 8  11   7.35839834
# 11  7  12.57537121
# 15  6  10.56735771
# 23  4   6.98819876
# 5  10  15.03505633
# 21  3   3.02643732
# 25  6  -2.24949388
# 14  9   6.70484575
# 13  9  -0.08462765
# 2  11   9.71863661
# 4  13 -13.17916850
# 18  5  -3.56455766
# 27  2  -9.04000276
# 6  11   0.58846082

# The prediction results show that this is not a good model

### Problem 11.3
press <- PRESS(lm.p2)
press # 125.2631
r2.pred <- 1 - press/sum((estimation.data$y - mean(estimation.data$y))^2)
# 0.2308404, which means this model does not predict well

### Problem 11.11
summary(lm.p2)$coef[, 2]
#  (Intercept)           x1           x2           x3           x4 
# 23.963727226  0.002265262  0.002792590  0.456285913  0.058301978 
#          x5           x6           x7           x8           x9 
# 0.080669732  0.005758882  0.174159472  0.003337454  0.003023748 

lm.p3.1 <- lm(y ~ ., data = table.b1)
summary(lm.p3.1)$coef[, 2]
#  (Intercept)           x1           x2           x3           x4 
# 1.281277e+01 2.006415e-03 8.409753e-04 2.589546e-01 4.160420e-02 
#           x5           x6           x7           x8           x9 
# 4.683586e-02 3.247561e-03 1.520695e-01 2.051858e-03 1.417072e-03 

# The standard error is much larger than the model in Problem 3.5

### Problem 11.12
# Part a
lm.p12 <- lm(y ~ ., data = prediction.data)
# Call:
# lm(formula = y ~ ., data = prediction.data)
# 
# Coefficients:
# (Intercept)           x1           x2           x3           x4  
#   9.2657700    0.0072295    0.0090463   -1.4585032   -0.0946429  
#         x5           x6           x7           x8           x9  
# -0.0578732   -0.0016178    0.4461901    0.0001501   -0.0007055 

lm.p2
# Call:
#   lm(formula = y ~ ., data = estimation.data)
# 
# Coefficients:
#   (Intercept)           x1           x2           x3           x4  
#     65.517955     0.004411    -0.005815    -1.320194     0.130843  
#       x5           x6           x7           x8           x9  
# 0.282964    -0.016467     0.239139     0.005028    -0.012218  

preds.p12 <- predict(lm.p12, newdata = prediction.data)
preds.est <- predict(lm.p2, newdata = prediction.data)
df <- data.frame(pred = preds.p12, est = preds.est)
#         pred          est
# 8  11.804288   7.35839834
# 11  6.952556  12.57537121
# 15  6.354167  10.56735771
# 23  3.105727   6.98819876
# 5   9.725397  15.03505633
# 21  3.613877   3.02643732
# 25  6.710872  -2.24949388
# 14  8.426045   6.70484575
# 13 10.079174  -0.08462765
# 2   9.757365   9.71863661
# 4  11.721711 -13.17916850
# 18  5.166380  -3.56455766
# 27  1.950679  -9.04000276
# 6  11.631762   0.58846082

# The two sets of coefficients are much different from each other
# Two sets of estimated values also differ a lot from each other

# Part b
preds <- predict(lm.p12, newdata = estimation.data)
df <- data.frame(y = estimation.data$y, preds = preds)
df
#     y     preds
# 1  10  3.619709
# 3  11  7.916161
# 7  10 13.298371
# 9   4 -8.213728
# 10  2  1.449904
# 12 10  4.900167
# 16  5  2.147068
# 17  5  1.867617
# 19  6  6.173598
# 20  4 -4.697477
# 22  3 -4.209557
# 24 10 17.274123
# 26  8  5.418167
# 28  0 -6.259455

# The results show that this model doesn't work well on the original
# estimation data set

### Problem 13.1
# Part a
p13.1 <- p13.1
glm.p13.1 <- glm(y ~ x, data = p13.1, family = "binomial")
# Coefficients:
#   (Intercept)            x  
#        6.0709      -0.0177 

# Part b
dev.res <- residuals(glm.p13.1, c = "deviance")
anova(glm.p13.1, test = "Chisq")
# Df Deviance Resid. Df Resid. Dev  Pr(>Chi)    
# NULL                    24     34.617              
# x     1   14.254        23     20.364 0.0001597 ***

# Since the p-value is smaller than 0.05, we can say that the model is adequate

# Part c
1 - exp(glm.p13.1$coefficients[2])
# For each unit increase in x, the odds will decrease by 1.754889%

# Part d
glm.p13.1.d <- glm(y ~ poly(x, 2), data = p13.1, family = "binomial")
anova(glm.p13.1, glm.p13.1.d)
# Analysis of Deviance Table
# 
# Model 1: y ~ x
# Model 2: y ~ poly(x, 2)
# Resid. Df Resid. Dev Df   Deviance
# 1        23     20.364              
# 2        22     20.363  1 0.00019281

# Since the deviance only decreases by 0.00019281 after adding the
# quadratic term, there is no need for a quadratic terms

### Problem 13.2
# Part a
p13.2 <- p13.2
glm.p13.2 <- glm(y ~ ., data = p13.2, family = binomial)
# Coefficients:
#   (Intercept)            x  
#    -8.7395139    0.0002009  

# Part b
anova(glm.p13.2, test = "Chisq")
# Df Deviance Resid. Df Resid. Dev Pr(>Chi)  
# NULL                    19     27.526           
# x     1   5.0906        18     22.435  0.02406 *

# Since the p-value is smaller than 0.05, we can say that the model is adequate


# Part c
exp(glm.p13.2$coefficients[2])
# One unit increase in x will lead to 0.0201% increase in log odds

# Part d
glm.p13.2.d <- glm(y ~ poly(x, 2), data = p13.2, family = "binomial")
anova(glm.p13.2, glm.p13.2.d)
# Analysis of Deviance Table
# 
# Model 1: y ~ x
# Model 2: y ~ poly(x, 2)
# Resid. Df Resid. Dev Df Deviance
# 1        18     22.435            
# 2        17     21.326  1   1.1086

# Since the deviance only decreases by 1.1086 after adding the
# quadratic term, there is no need for a quadratic terms

### Problem 13.5
# Part a
p13.5 <- p13.5
glm.p13.5 <- glm(y ~ ., data = p13.5, family = binomial)
glm.p13.5
# Coefficients:
#   (Intercept)           x1           x2  
# -7.047e+00    7.382e-05    9.879e-01  

# Part b
anova(glm.p13.5, test = "Chisq")

# Df Deviance Resid. Df Resid. Dev Pr(>Chi)  
# NULL                    19     27.726           
# x1    1   0.7349        18     26.991  0.39129  
# x2    1   5.9094        17     21.081  0.01506 *

# Since the p-value is smaller than 0.05, we can say that the model is adequate

# Part c
exp(7.382e-05)
# beta1: one unit increase in x1 will lead to 0.0074% increase in odds
exp(9.879e-01)
# beta2: one unit increase in x2 will lead to 168.5589% increase in odds

# Part d
predict(glm.p13.5, newdata = data.frame(x1 = 45000,
                                        x2 = 5),
        type = "response")
# 0.7710279 

# Part e
glm.p13.5.e <- glm(y ~ . + x1:x2, data = p13.5)
anova(glm.p13.5, glm.p13.5.e)
# Analysis of Deviance Table
# 
# Model 1: y ~ x1 + x2
# Model 2: y ~ x1 + x2 + x1:x2
# Resid. Df Resid. Dev Df Deviance
# 1        17    21.0815            
# 2        16     3.3039  1   17.778

# The deviance is decreased by 17.778 after adding in interaction term
# So the interaction term is required

# Part f
glm.p13.5.1 <- glm(y ~ x1, data = p13.5, family = binomial)
glm.p13.5.2 <- glm(y ~ x2, data = p13.5, family = binomial)
lrtest(glm.p13.5, glm.p13.5.1)
# Likelihood ratio test
# 
# Model 1: y ~ x1 + x2
# Model 2: y ~ x1
# #Df  LogLik Df  Chisq Pr(>Chisq)  
# 1   3 -10.541                       
# 2   2 -13.495 -1 5.9094    0.01506 *
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Since Likelihood ratio test is significant, we reject the null hypothesis that model 2 is better


lrtest(glm.p13.5, glm.p13.5.2)
# Likelihood ratio test
# 
# Model 1: y ~ x1 + x2
# Model 2: y ~ x2
# #Df  LogLik Df  Chisq Pr(>Chisq)
# 1   3 -10.541                     
# 2   2 -11.282 -1 1.4818     0.2235

# Since Likelihood ratio test is not significant, we cannot reject the null hypothesis that model 2 is better

# Part g
confint(glm.p13.5)
#                     2.5 %       97.5 %
# (Intercept) -1.805544e+01 1.0275430082
# x1          -4.361540e-05 0.0002184223
# x2           1.544228e-01 2.2872127855

### Problem 13.10
p13.10 <- p13.5
dev.res <- residuals(glm.p13.5, c = "deviance")
qqnorm(dev.res); qqline(dev.res)
# The normal probability plot shows that the distribution is light tailed
plot(glm.p13.5$fitted.values, dev.res); abline(h = 0)
# The deviance plot shows a clear non-linear pattern
# The model is not good

### Problem 13.25
p13.25 <- read_excel("data-prob-13-25.xls")

# Part a
glm.p13.25 <- glm(`At Least One O-ring Failure` ~ ., data = p13.25, family = binomial)
preds <- predict(glm.p13.25, newdata = p13.25, type = "response")
plot(p13.25); lines(y = preds, x = p13.25$`Temperature at Launch`)
# It model seems to fit well

# Part b
exp(-0.1713)
# 0.8425688. Each unit increase in Temperature at Launch will lead to 15.74312% decrease
# in At Least One O-ring Failure

# Part c
df <- data.frame(x = 50)
names(df) <- "Temperature at Launch"
predict(glm.p13.25, newdata = df, type = "response")
# 0.9096463

# Part d
df <- data.frame(x = 75)
names(df) <- "Temperature at Launch"
predict(glm.p13.25, newdata = df, type = "response")
# 0.1219933

# Part e
df <- data.frame(x = 31)
names(df) <- "Temperature at Launch"
predict(glm.p13.25, newdata = df, type = "response")
# 0.9961828
# Estimating the extrapolation point can involve a certain level
# of uncertainty. However, from the model plot in part a, we can see
# that low temperature has a large possibility of being failure.

# Part f
dev.res <- residuals(glm.p13.25, c = "deviance")
qqnorm(dev.res); qqline(dev.res)
# The normal probability plot shows that the distribution is not normal
plot(glm.p13.25$fitted.values, dev.res); abline(h = 0)
# The deviance plot shows a clear non-linear pattern

# Part g
glm.p13.25.g <- glm(`At Least One O-ring Failure` ~ poly(`Temperature at Launch`, 2), data = p13.25, family = binomial)
anova(glm.p13.25, glm.p13.25.g)
# Analysis of Deviance Table
# 
# Model 1: `At Least One O-ring Failure` ~ `Temperature at Launch`
# Model 2: `At Least One O-ring Failure` ~ poly(`Temperature at Launch`, 
#                                               2)
# Resid. Df Resid. Dev Df Deviance
# 1        22     23.030            
# 2        21     22.589  1  0.44144

# Since deviance is decreased by only 0.44144, there is no evidence that 
# the ploynomial term improves the models





