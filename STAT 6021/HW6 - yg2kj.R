library(readxl)
library(car)
library(perturb)
library(glmnet)
library(boot)
library(leaps)
library(MPV)
library(dplyr)

### Problem 9.7
# Part a
p9.7 <- read_excel("data-table-B3.xls")
cor(p9.7[, -1])
#             x1         x2 x3          x4         x5          x6         x7         x8         x9        x10        x11
# x1   1.0000000  0.9452080 NA -0.33015370 -0.6315968  0.65906008 -0.7814778  0.8551981  0.8013975  0.9456621  0.8354239
# x2   0.9452080  1.0000000 NA -0.29205832 -0.5170425  0.77190992 -0.6431558  0.7973892  0.7176056  0.8834004  0.7266835
# x3          NA         NA  1          NA         NA          NA         NA         NA         NA         NA         NA
# x4  -0.3301537 -0.2920583 NA  1.00000000  0.3737462 -0.04933889  0.4938104 -0.2581079 -0.3187643 -0.2772185 -0.3683612
# x5  -0.6315968 -0.5170425 NA  0.37374620  1.0000000 -0.20535194  0.8428620 -0.5481227 -0.4343576 -0.5424247 -0.7032485
# x6   0.6590601  0.7719099 NA -0.04933889 -0.2053519  1.00000000 -0.3005751  0.4251881  0.3156727  0.5206424  0.4173378
# x7  -0.7814778 -0.6431558 NA  0.49381043  0.8428620 -0.30057509  1.0000000 -0.6630802 -0.6682373 -0.7178265 -0.8549981
# x8   0.8551981  0.7973892 NA -0.25810785 -0.5481227  0.42518809 -0.6630802  1.0000000  0.8849771  0.9475859  0.6863079
# x9   0.8013975  0.7176056 NA -0.31876434 -0.4343576  0.31567268 -0.6682373  0.8849771  1.0000000  0.9015431  0.6507213
# x10  0.9456621  0.8834004 NA -0.27721850 -0.5424247  0.52064243 -0.7178265  0.9475859  0.9015431  1.0000000  0.7722283
# x11  0.8354239  0.7266835 NA -0.36836123 -0.7032485  0.41733783 -0.8549981  0.6863079  0.6507213  0.7722283  1.0000000

# The correlation matrix indicates that there exists multicollinearity (lots of value s)

# Part b
lm.p9.7 <- lm(y ~ ., data = p9.7)
vif(lm.p9.7)
#         x1         x2         x3         x4         x5         x6         x7         x8         x9        x10 
# 119.487804  42.800811 149.234409   2.060036   7.729187   5.324730  11.761341  20.917632   9.397108  85.744344 
#      x11 
# 5.145052 
# the vifs indicate multicolliniarity because lots of vifs are greater than 5

### Problem 9.13
p9.13 <- read_excel("data-table-B18.xls")
names(p9.13)[1] <- "y"
lm.p9.13 <- lm(y ~ ., data = p9.13)
vif(lm.p9.13)
#       x1         x2         x3         x4         x5         x6         x7         x8 
# 1.000000   1.900541 168.467420  43.103776  60.791320 275.472571 185.707184  44.363364 
# there are multiple vifs greater than 5, which indicates multicollinearity

### Problem 9.14
p9.14 <- read_excel("data-table-B19.xls")
lm.p9.14 <- lm(y ~ ., data = p9.14)
summary(lm.p9.14)
# Coefficients: (2 not defined because of singularities)
# Estimate Std. Error t value Pr(>|t|)  
# (Intercept) -12.20843   14.61153  -0.836   0.4120  
# x1           -0.84577    0.58596  -1.443   0.1624  
# x2            7.41839    3.51235   2.112   0.0457 *
#   x3            0.01046    0.00857   1.220   0.2347  
# x4           -1.94732    2.22110  -0.877   0.3897  
# x5            4.89518    3.21850   1.521   0.1419  
# x6           -1.43382    1.81263  -0.791   0.4370  
# x7                 NA         NA      NA       NA  
# x8          -11.42517    7.88120  -1.450   0.1606  
# x9           -0.10802    0.22040  -0.490   0.6287  
# x10                NA         NA      NA       NA  
# 
# We delete x7 and x10 from the model
lm.p9.14 <- lm(y ~ . - x7 - x10, data = p9.14)
vif(lm.p9.14)
#       x1         x2         x3         x4         x5         x6         x8         x9 
# 1.970605   4.092086   4.513202 603.518791 511.870261  33.319560   7.930630  36.170717 
# there are multiple vifs greater than 5, which indicates multicollinearity

### Problem 9.15
p9.15 <- read_excel("data-table-B20.xls")
names(p9.15)[1] <- "y"
lm.p9.15 <- lm(y ~ ., data = p9.15)
vif(lm.p9.15)
#       x1        x2        x3        x4        x5 
# 1.519064 26.283999 26.447032  2.202201  1.922689
# there are multiple vifs greater than 5, which indicates multicollinearity

### Problem 9.19
p9.19 <- read_excel("data-table-B3.xls") %>% 
  na.omit()
p9.19.matrix <- as.matrix(p9.19)
# Part a
grid <- seq(0, 1, length.out = 25)
ridge.p9.19 <- glmnet(p9.19.matrix[, -1], p9.19.matrix[, 1], alpha = 0, lambda = grid)
plot(ridge.p9.19, xvar = "lambda", label = TRUE)
# from the plot, we can see that the ridge trace plot stabilizes at around log(lambda) = -1.5
lambda <- exp(-1.5)
lambda
# 0.2231302

p9.19.ridge <- glmnet(p9.19.matrix[, -1], p9.19.matrix[, 1], alpha = 0, lambda = lambda)
preds <- predict(p9.19.ridge, newx = p9.19.matrix[, -1])
residuals <- p9.19$y - preds
plot(preds, residuals); abline(h = 0)
# the residual plot shows a non linear pattern, so the model is not adequate

# Part b
p9.19.glm <- glm(y ~ ., data = p9.19)
anova(p9.19.glm) 
# RSS = 187.40
preds <- predict(p9.19.ridge, newx = p9.19.matrix[, -1])
rss <- sum((p9.19.matrix[, 1] - preds)^2)
rss
# RSS = 219.3789
# rss has inflated by 31.9789
  
# Part c
p9.19.lm <- lm(y ~ ., data = p9.19)
summary(p9.19.lm)$r.squared
# 0.8354843
ssr <- sum((preds - mean(p9.19$y))^2)
sst <- sum((p9.19$y - mean(p9.19$y))^2)
r2 <- ssr/sst
# 0.7697105
# r squared has decreased by 0.0657738


### Problem 9.20
p9.20 <- read_excel("data-table-B3.xls") %>% 
  na.omit()
lm.p9.20 <- lm(y ~ ., data = p9.20)
beta.hats <- coef(lm.p9.20)
p <- length(beta.hats)-1
sigma.hat.squared <- sum((lm.p9.20$fitted.values - p9.20$y)^2)/lm.p9.20$df.residual
k <- p*sigma.hat.squared/sum(beta.hats*beta.hats)
k
# 0.3290422


### Problem 10.1
p10.1 <- read_excel("data-table-B1.xls")
lm.full.10.1 <- lm(y ~ ., data= p10.1)
lm.null.10.1 <- lm(y ~ 1, data = p10.1)
# Part a
lm.fwd.10.1 <- step(lm.null.10.1, scope = list(lower = lm.null.10.1, upper = lm.full.10.1), direction = "forward")
lm.fwd.10.1
# Call:
# lm(formula = y ~ x8 + x2 + x7 + x9, data = p10.1)
# 
# Coefficients:
# (Intercept)           x8           x2           x7           x9  
#   -1.821703    -0.004015     0.003819     0.216894    -0.001635  

# Part b
lm.bwd.10.1 <- step(lm.full.10.1, scope = list(lower = lm.null.10.1, upper = lm.full.10.1), direction = "backward")
lm.bwd.10.1
# Call:
# lm(formula = y ~ x2 + x7 + x8 + x9, data = p10.1)
# 
# Coefficients:
# (Intercept)           x2           x7           x8           x9  
#   -1.821703     0.003819     0.216894    -0.004015    -0.001635 

# Part c
lm.stp.10.1 <- step(lm.null.10.1, scope = list(lower = lm.null.10.1, upper = lm.full.10.1), direction = "both")
lm.stp.10.1
# Call:
# lm(formula = y ~ x8 + x2 + x7 + x9, data = p10.1)
# 
# Coefficients:
# (Intercept)           x8           x2           x7           x9  
#   -1.821703    -0.004015     0.003819     0.216894    -0.001635  

# Part d
# These three procedures choose the same model

### Problem 10.2
p10.2 <- read_excel("data-table-B1.xls")
bestmod <- regsubsets(y ~ x1 + x2 + x4 + x7 + x8 + x9, data = p10.2)
summary(bestmod)
# Subset selection object
# Call: regsubsets.formula(y ~ x1 + x2 + x4 + x7 + x8 + x9, data = p10.2)
# 6 Variables  (and intercept)
# Forced in Forced out
# x1     FALSE      FALSE
# x2     FALSE      FALSE
# x4     FALSE      FALSE
# x7     FALSE      FALSE
# x8     FALSE      FALSE
# x9     FALSE      FALSE
# 1 subsets of each size up to 6
# Selection Algorithm: exhaustive
#           x1  x2  x4  x7  x8  x9 
# 1  ( 1 ) " " " " " " " " "*" " "
# 2  ( 1 ) " " "*" " " " " "*" " "
# 3  ( 1 ) " " "*" " " "*" "*" " "
# 4  ( 1 ) " " "*" " " "*" "*" "*"
# 5  ( 1 ) "*" "*" " " "*" "*" "*"
# 6  ( 1 ) "*" "*" "*" "*" "*" "*"
summary(bestmod)$cp
# 26.472221  6.457655  3.688101  4.038492  5.406361  7.000000
summary(bestmod)$rss
# 148.87197  83.93820  69.87000  65.00435  63.13983  61.94123
summary(bestmod)$adjr2
# 0.5271722 0.7227426 0.7595953 0.7666123 0.7630023 0.7564299

# I recommend the fourth model with x2, x7, x8, x9
# This model has a high adjusted R^2, relatively low RSS, and a Cp close to k+1

### Problem 10.14
p10.14 <- read_excel("data-table-B11.xls")
# Part a
p10.14$Region <- as.factor(p10.14$Region)
bestmod.p10.14 <- regsubsets(Quality ~ ., data = p10.14)
summary(bestmod.p10.14)
# Subset selection object
# Call: regsubsets.formula(Quality ~ ., data = p10.14)
# 7 Variables  (and intercept)
# Forced in Forced out
# Clarity      FALSE      FALSE
# Aroma        FALSE      FALSE
# Body         FALSE      FALSE
# Flavor       FALSE      FALSE
# Oakiness     FALSE      FALSE
# Region2      FALSE      FALSE
# Region3      FALSE      FALSE
# 1 subsets of each size up to 7
# Selection Algorithm: exhaustive
#         Clarity Aroma Body Flavor Oakiness Region2 Region3
# 1  ( 1 ) " "     " "   " "  "*"    " "      " "     " "    
# 2  ( 1 ) " "     " "   " "  "*"    " "      "*"     " "    
# 3  ( 1 ) " "     " "   " "  "*"    " "      "*"     "*"    
# 4  ( 1 ) " "     " "   " "  "*"    "*"      "*"     "*"    
# 5  ( 1 ) " "     "*"   " "  "*"    "*"      "*"     "*"    
# 6  ( 1 ) " "     "*"   "*"  "*"    "*"      "*"     "*"    
# 7  ( 1 ) "*"     "*"   "*"  "*"    "*"      "*"     "*"   
summary(bestmod.p10.14)$cp
# 35.418978  9.392859  2.473672  2.240659  4.103294  6.000137  8.000000
# The fourth model (Flavor Oakiness Region2 Region3) has the smallest Cp value

# Part b
# Model 1
par(mfrow = c(1, 2))
lm.fit.1 <- lm(Quality ~ Flavor + Oakiness + Region, data = p10.14)
plot(lm.fit.1$fitted.values, lm.fit.1$residuals, ylim = c(-2, 2)); abline(h = 0)
# Model 2
lm.fit.2 <- lm(Quality ~ Flavor + Region, data = p10.14)
plot(lm.fit.2$fitted.values, lm.fit.2$residuals, ylim = c(-2, 2)); abline(h = 0)
# The residual plots look almost the same, but the plot for the first model looks more
# compact, which means the first model is better.

# Part c
PRESS(lm.fit.1) # 33.08173
PRESS(lm.fit.2) # 33.99346
# Since the fist model has smaller PRESS statistic, it's better

### Problem 10.15
p10.15 <- read_excel("data-table-B11.xls")
p10.15$Region <- as.factor(p10.15$Region)
lm.full.p10.15 <- lm(Quality ~ ., data = p10.15)
lm.null.p10.15 <- lm(Quality ~ 1, data = p10.15)
lm.stp.p10.15 <- step(lm.null.p10.15, 
                      scope = list(lower = lm.null.p10.15, upper = lm.full.p10.15),
                      direction = "both")
lm.stp.p10.15
# Call:
# lm(formula = Quality ~ Flavor + Region + Oakiness, data = p10.15)
# 
# Coefficients:
# (Intercept)       Flavor      Region2      Region3     Oakiness  
#      8.1208       1.1920      -1.5155       1.0935      -0.3183  

# This model is the same as the model I found in 10.4 part a

### Problem 10.16
p10.16 <- read_excel("data-table-B11.xls") %>% 
  select(-Region)
# Part a
bestmod.p10.16 <- regsubsets(Quality ~ ., data = p10.16)
summary(bestmod.p10.16)
# Subset selection object
# Call: regsubsets.formula(Quality ~ ., data = p10.16)
# 5 Variables  (and intercept)
# Forced in Forced out
# Clarity      FALSE      FALSE
# Aroma        FALSE      FALSE
# Body         FALSE      FALSE
# Flavor       FALSE      FALSE
# Oakiness     FALSE      FALSE
# 1 subsets of each size up to 5
# Selection Algorithm: exhaustive
#        Clarity Aroma Body Flavor Oakiness
# 1  ( 1 ) " "     " "   " "  "*"    " "     
# 2  ( 1 ) " "     " "   " "  "*"    "*"     
# 3  ( 1 ) " "     "*"   " "  "*"    "*"     
# 4  ( 1 ) "*"     "*"   " "  "*"    "*"     
# 5  ( 1 ) "*"     "*"   "*"  "*"    "*" 
summary(bestmod.p10.16)$cp
# 9.043605 6.813160 3.927790 4.674678 6.000000
lm.p10.16 <- lm(Quality ~ Aroma + Flavor + Oakiness, data = p10.16)
summary(lm.p10.16)$adj.r # 0.677629
anova(lm.p10.16) # MSE = 1.349

lm.p10.14 <- lm(Quality ~ Flavor + Oakiness + Region, data = p10.14)
summary(lm.p10.14)$adj.r # 0.8164362
anova(lm.p10.14) # MSE = 0.768

# The third model (Aroma Flavor Oakiness) has adjusted r square of 0.677629 and MSE of 1.349
# The fourth model (Flavor Oakiness Region2 Region3) in 10.14 has adjusted r square of 
# 0.8164362 and MSE of 0.768, both of which are better than the model in 10.16

# Part b
lm.p10.16 <- lm(Quality ~ Aroma + Flavor + Oakiness, data = p10.16)
confint.p10.16 <- predict(lm.p10.16, interval = "confidence") %>% 
  apply(2, mean)
confint.p10.16
#    fit      lwr      upr 
# 12.43684 11.69338 13.18031 

lm.p10.14 <- lm(Quality ~ Flavor + Oakiness + Region, data = p10.14)
confint.p10.14 <- predict(lm.p10.14, interval = "confidence") %>% 
  apply(2, mean)
confint.p10.14
#     fit      lwr      upr 
# 12.43684 11.79928 13.07441 

# Since the 95% confidence interval for model in p10.14 is smaller than
# the interval for model in p10.16, I prefer the model in Problem 10.14

