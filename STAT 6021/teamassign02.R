
###########################
#                         #
#   Team Assignment 2     #
#                         #
###########################

## Please submit one set of answers per team.            ##
## Your answers may be submitted as an annotated R file. ##
###########################################################


#################
## Question 1: ##
#################

# For this problem you will use the data in the file "teamassign02data01.csv" to implement   
# a simple form of the bootstrap resampling method.  Repeat (a) and (b) 1000 times:
#
#   (a) From the data set, select **with replacement** 100 random pairs (x,y).
#       You will have some repeats -- which is OK and expected.
#   (b) Use your sample to generate a regression equation. Save the values of 
#       hat(beta_0) and hat(beta_1).
set.seed(1234)
data <- read.csv("teamassign02data01.csv")

betas <- data.frame(matrix(0, 1000, 2))
names(betas) <- c("beta0", "beta1")
for (i in 1:1000){
  random.rows <- sample(1:100, 100, replace = T)
  temp.df <- data[random.rows, ]
  temp.lm <- lm(y ~ x, data = temp.df)
  temp.betas <- unname(temp.lm$coefficient)
  betas[i, ] <- temp.betas
}

#   (c) Find and report a 95% confidence interval for beta_0 and beta_1 by determining
#       the 2.5th and 97.5th percentiles for each set of values.  Do the confidence 
#       intervals contain the true parameter values?
confint.beta0 <- quantile(betas$beta0, probs = c(0.025, 0.975))
#     2.5%    97.5% 
# 13.96970 30.42344
confint.beta1 <- quantile(betas$beta1, probs = c(0.025, 0.975))
#     2.5%    97.5% 
# 3.854850 4.246991
true.values <- unname(lm(y ~ x, data = data)$coef)
# 21.273694  4.072949
# So the confidence intervals do contain the true parameter values

#################
## Question 2: ##
#################

# Import the data set "teamassign02data02.csv" which contains 100 sets of data for 
# the variables x1, x2, ..., x20.  Repeat (a)-(c) 100 times: 
#
#   (a) Generate 100 y values according to the model y ~ N(10, var=5^2) and pair up 
#       the y-values with corresponding rows from the data set of x-values.
#   (b) On the data set from part (a), generate a multiple regression model with
#       all of the x-values are explanatory variables.
#   (c) Determine the number of significant explanatory variables at the 5% level.
#   (d) Determine and report the proportion of significant variables in the 100
#       simulations. Compare this proportion with the expected theoretical value.

num.significant <- numeric()
for (i in 1:100){
  data2 <- read.csv("teamassign02data02.csv")
  yvalues <- rnorm(100, mean = 10, sd = sqrt(5^2))
  data2$y <- yvalues
  
  lm.2b <- lm(y ~ ., data = data2)
  
  p.explanatory <- summary(lm.2b)$coef[2:21, 4]
  num.significant[i] <- sum(p.explanatory < 0.05)
}

proportion <- sum(num.significant) / (20*100)
# 0.056, which means that 5.6% of the explanatory variables are significant.

# For each simulation, E[sig] = n*p = 20*5% = 1
# For 100 simulations, E[sig'] = 100*E[sig] = 100
# Expected theoretical value = 100/(20*100) = 5%

# 5.6% we got is close to the theoretical value of 5%.
