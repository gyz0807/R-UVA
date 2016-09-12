################### Problem 2.20
data.p2.20 <- data.frame(y = c(343,356,344,356,352,361,372,355,375,359,364,357,368,360,372,352),
                   x5 = c(220,220,223,223,221,221,190,190,180,180,180,180,176,176,175,175))
# H0: beta1 = 0, Ha: beta1 != 0
lm.p2.20 <- lm(y ~ x5, data=data.p2.20)
beta1 <- 0
beta1hat <- unname(lm.p2.20$coefficients[2])
se.beta1hat <- summary(lm.p2.20)$coef[2,2]
t <- (beta1hat - beta1)/se.beta1hat
t.stats <- qt(0.025, df = lm.p2.20$df.residual)
# t = -2.74122, t.stats = -2.144787, so t < t.stats
# With 95% confidence level, we reject the null hypothesis that there is no relationship between the two
# The data support the engineer's belief

summary(lm.p2.20)
# R-squared is 34.93%, which means that only 34.93% of the variability in fuel consumption can be explained
# by the initial boiling point of the fuel. There is still a large part of the variability of fuel consumption 
# cannot be explained.

################### Problem 2.21
data.p2.21 <- data.frame(y = c(19.2,18.3,17.1,17.3,16.8,16.5,15.8,15.2,15.2,14.0,14.0,13.8,13.6,12.8,18.5,17.3,16.3,16.3,16.0,16.0,15.7,15.5,15.3,15.3,14.8,14.3,14.3,14.2,14.0,13.8,12.5,11.5),
                         x3 = c(66,79,73,99,75,61,66,86,78,178,81,108,92,96,89,59,22,77,58,85,120,94,122,144,10,100,73,301,104,67,89,192))
# H0: beta1 >= 0, Ha: beta1 < 0
lm.p2.21 <- lm(y ~ x3, data=data.p2.21)
beta1hat <- unname(lm.p2.21$coefficients[2])
beta1 <- 0
se.beta1hat <- summary(lm.p2.21)$coef[2,2]
t <- (beta1hat - beta1)/se.beta1hat
t.stats <- qt(0.05, df = lm.p2.21$df.residual)
# t = -2.221769, t.stats = -1.697261, so t < t.stats
# With 95% confidence level, we reject the null hypothesis that the sulfur content has no or positive impact on the taste
# The data support the winemakers' belief

summary(lm.p2.21)
# R-squared is 14.13%, which means that only 14.13% of the variability in taste can be explained by the sulfur content. A
# large part of the variability cannot be explained.

################### Problem 2.22
data.p2.22 <- data.frame(y = c(1.1,4.2,94.2,20.7,15.7,15.9,14.7,10.8,9.6,12.7,7.1,9.0,96.0,78.4,78.3,71.4,0.5,3.1),
                         x5 = c(1.30,1.16,1.25,1.57,1.55,2.71,0.54,0.74,1.01,1.12,0.86,0.45,1.74,0.94,0.93,0.94,0.90,0.91))
# H0: beta1 = 0, Ha: beta1 != 0
lm.p2.22 <- lm(y ~ x5, data = data.p2.22)
beta1hat <- unname(lm.p2.22$coefficients[2])
beta1 <- 0
se.beta1hat <- summary(lm.p2.22)$coef[2,2]
t <- (beta1hat - beta1)/se.beta1hat
t.stats <- qt(0.975, df = lm.p2.22$df.residual)
# t = 0.4648991, -2.119905 <= t.stats <= 2.119905, so t < |t.stats|
# We fail to reject the null hypothesis that ratio of inlet oxygen to the inlet methanol controls the conversion process
# The data does not support the chemist's belief

summary(lm.p2.22)
# R-squared is 1.33%, which means that only 1.33% of the variability in percent conversion can be explained by the ratio 
# of inlet oxygen to inlet methanol, which is pretty low.

################### Problem 2.30
# Part a
library(MPV)
data("p2.12")
cor(p2.12$usage, p2.12$temp)
# 0.9999326

# Part b
# H0: rho = 0, H1: rho != 0
test <- cor.test(p2.12$usage, p2.12$temp)
# Pearson's product-moment correlation
# 
# data:  p2.12$usage and p2.12$temp
# t = 272.25, df = 10, p-value < 2.2e-16
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
# 0.9997509 0.9999817
# sample estimates:
# cor 
# 0.9999326 

# From the results, we can see that p-value < 2.2e-16, which is extremely small.
# With 95% confidence interval, since p-value < 0.025, we reject the null hypothesis that rho = 0.

# Citation: https://stat.ethz.ch/R-manual/R-devel/library/stats/html/cor.test.html

# Part c
# H0: rho = 0.5, Ha: rho != 0.5
r <- cor(p2.12$usage, p2.12$temp)
n <- nrow(p2.12)
mean <- atanh(r)
var <- (n-3)^(-1)

Z <- (atanh(r) - atanh(0.5))*(n-3)^(1/2)
p.value <- pnorm(q = Z, mean = mean, sd = sqrt(var))
0.025 < p.value & p.value < 0.975
# FALSE, so with 95% confidence level, we reject the null hypothesis that rho = 0.5

# Part d
lower <- tanh(mean - qnorm(0.995)/sqrt(n-3))
upper <- tanh(mean + qnorm(0.995)/sqrt(n-3))
# 99% CI for rho is (0.9996244, 0.9999879)





