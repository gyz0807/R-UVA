# Yizhe Ge (yg2kj)
library(readxl)

# Problem 6.12
p12 <- read_excel("data-table-B11.xls")
lm.p12 <- lm(Quality ~ Clarity + Aroma + Body + Flavor + Oakiness, data = p12)
summary(influence.measures(lm.p12))
# Potentially influential observations of
# lm(formula = Quality ~ Clarity + Aroma + Body + Flavor + Oakiness,      data = p12) :
#   
#    dfb.1_ dfb.Clrt dfb.Arom dfb.Body dfb.Flvr dfb.Okns dffit   cov.r   cook.d hat  
# 12  0.83  -0.97    -0.40     0.23    -0.01    -0.04     1.38_*  1.16    0.30   0.39
# 14 -0.03   0.11    -0.16    -0.05     0.05     0.08    -0.28    1.84_*  0.01   0.36
# 20 -0.04  -0.38     0.46    -1.06_*   0.65     0.60    -1.54_*  0.30_*  0.31   0.20
# 32 -0.04   0.04    -0.03     0.02     0.04    -0.03    -0.07    1.57_*  0.00   0.23
# 37 -0.06  -0.15     0.04     0.28    -0.37     0.39     0.61    1.72_*  0.06   0.38

# There is no influential point

# Problem 6.13
p13 <- read_excel("data-table-B12.xls")
lm.p13 <- lm(pitch ~ ., data = p13)
summary(influence.measures(lm.p13))
# Potentially influential observations of
# lm(formula = pitch ~ ., data = p13) :
#   
#   dfb.1_  dfb.temp dfb.sktm dfb.skpc dfb.dfft dfb.dffp dffit   cov.r   cook.d  hat    
# 5   0.18   -0.23     0.18     0.03    -0.06     0.02     0.37    1.85_*  0.02    0.35  
# 28 -1.16_*  1.24_*  -0.38     0.50    -0.02     0.04     1.51_*  0.90    0.34    0.38  
# 29  1.33_* -1.31_*  -0.11    -0.44     0.25    -1.45_*   2.06_*  0.08_*  0.45    0.21  
# 31  0.02    0.04     0.07    -0.17     0.03     0.04     0.46    1.75_*  0.04    0.34  
# 32  0.00    0.11    -1.78_*  -0.32     0.65     0.03    -2.40_*  3.14_*  0.93_*  0.74_*

# Point 32 is the influential point

# Problem 6.14
p14 <- read_excel("data-table-B13.xls")
lm.p14 <- lm(y ~ ., data = p14)
summary(influence.measures(lm.p14))
# Potentially influential observations of
# lm(formula = y ~ ., data = p14) :
#   
#   dfb.1_  dfb.x1  dfb.x2  dfb.x3  dfb.x4  dfb.x5  dfb.x6  dffit   cov.r   cook.d  hat    
# 6   0.02   -0.07    0.08   -0.04    0.07    0.01    0.09   -0.20    1.74_*  0.01    0.30  
# 9   0.01   -0.07    0.00   -0.01    0.07    0.06   -0.05    0.19    1.65_*  0.01    0.27  
# 10 -0.06   -0.01    0.05    0.05   -0.01   -0.11    0.10   -0.32    1.72_*  0.02    0.31  
# 11  0.86    1.45_*  0.21   -0.75   -1.15_*  0.08    0.48    1.65_*  1.39    0.37    0.50  
# 20 -1.51_* -0.88   -4.41_*  1.76_*  1.63_*  1.41_* -1.18_* -5.13_*  0.82    3.01_*  0.74_*

# Point 20 is the influential point

# Problem 6.15
p15 <- read_excel("data-table-B14.xls")
lm.p15 <- lm(y ~ x1 + x2 + x3 + x4, data = p15)
summary(influence.measures(lm.p15))
# Potentially influential observations of
# lm(formula = y ~ x1 + x2 + x3 + x4, data = p15) :
#   
#   dfb.1_  dfb.x1 dfb.x2  dfb.x3  dfb.x4  dffit   cov.r   cook.d  hat  
# 2   1.67_*  0.21  -3.77_*  0.05   -1.07_* -4.67_*  0.04_*  1.98_*  0.47
# 4  -0.59    0.20  -0.57   -0.94    2.34_*  2.50_*  0.91    1.04_*  0.56
# 8   0.41   -0.60   1.46_* -0.54   -0.29    1.57_*  0.62    0.41    0.34
# 9  -0.13   -0.09  -0.11    1.05_* -0.51    1.15    1.38    0.26    0.41
# 10  0.19   -0.28   0.07   -0.19    0.02   -0.33    1.98_*  0.02    0.38

# Points 2, 4 are influential points

# Problem 8.5
# Part a
p5 <- read_excel("data-table-B3.xls")
lm.p5 <- lm(y ~ x10 + x11, data = p5)
summary(lm.p5)
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 39.1919052  2.5570509  15.327 1.92e-15 ***
#   x10         -0.0047484  0.0009544  -4.975 2.72e-05 ***
#   x11         -2.6958431  1.9805597  -1.361    0.184    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# since the p-value is 0.184 for x11, it's not significant

# Part b
lm.p5.1 <- lm(y ~ x10 + x11 + x10*x11, data = p5)
summary(lm.p5.1)

# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  58.108420   5.077985  11.443 4.53e-12 ***
#   x10          -0.012517   0.002055  -6.090 1.44e-06 ***
#   x11         -26.724910   6.107349  -4.376 0.000152 ***
#   x10:x11       0.009035   0.002217   4.076 0.000342 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# when the transmission is automatic, yhat = 31.38351 - 0.003482x10
# so gasoline mileage will decrease by 0.003482 with every unit increase in vehicle weight
# when the transmission is manual, yhat = 58.108420 - 0.012517x10
# so gasoline mileage will decrease by 0.012517 with every unit increase in vehicle weight

# Problem 8.6
p6 <- read_excel("data-table-B1.xls")
# let x5.1 = -1 when x5 < 0
# x5.1 = 0 when x5 = 0
# x5.1 > 0 when x5 > 0
x5.1 <- rep(0, times = nrow(p6))
x5.1[which(p6$x5 < 0)] <- -1
x5.1[which(p6$x5 == 0)] <- 0
x5.1[which(p6$x5 > 0)] <- 1
x5.1 <- as.factor(x5.1)
lm.p6 <- lm(y ~ x8 + x7 + x5.1, data = p6)
lm.p6.1 <- lm(y ~ x8 + x7, data = p6)
anova(lm.p6.1, lm.p6)

# Analysis of Variance Table
# 
# Model 1: y ~ x8 + x7
# Model 2: y ~ x8 + x7 + x5.1
# Res.Df    RSS Df Sum of Sq      F Pr(>F)
# 1     25 147.90                           
# 2     23 125.62  2    22.276 2.0392  0.153

# from the anova table, we can see that the F value for the model adding x5.1 has a p-value
# of 0.153, which is not significant. So the effect of turnovers on the number of games
# won is not significant.

