# Team 3
# Leigh Harton (klh8mr), Isabelle Yang (yy3fs), Yizhe Ge (yg2kj), Thomas Molinari (tgm4br)

###########################
#                         #
#   Team Assignment 1     #
#                         #
###########################

## Please submit one set of answers per team.            ##
## Your answers may be submitted as an annotated R file. ##
###########################################################

## Start with given x-values
set.seed(1234)
x <- read.table("teamassign01data.txt")[,1]
x

## Generate corresponding y-values according to the model y ~ 25 + 4x + e, where e~N(0,var=12^2)
y <- 25 + 4*x + rnorm(100, mean=0, sd = 12)

## Plot the relationship
plot(x,y, pch=20, cex=0.3)


#################
## Question 1: ##
#################

# Using the (x,y) from above, generate a linear model. 
#
#   (a) Report the coefficients hat(beta_0) and hat(beta_1).
lm.model <- lm(y~x)
# Coefficients:
# (Intercept)            x  
#      21.242        4.047  

#   (b) Report the predicted value of y for x=18.
newdata <- data.frame(x=18)
predict(lm.model, newdata)
# 94.08764 

#   (c) Report MS_Res.
sum(lm.model$residuals^2)/lm.model$df.residual
# 146.4796

#################
## Question 2: ##
#################

# Generate the linear model requested in Question 1 1000 times. Create a new vector of y-values 
# for each repetition.
#
#   (a) Determine and report the mean and variance of the generated coefficients.

x <- read.table("teamassign01data.txt")[,1]
yvalues <- data.frame(matrix(0, 1000, 100))
raw.data <- data.frame(matrix(0, 1000, 2))
names(raw.data) <- c("yIntercept", "slope")

set.seed(1234)

for (i in 1:1000){
        y <- 25 + 4*x + rnorm(100, mean=0, sd = 12)
        yvalues[i, ] <- y
}

for (i in 1:1000){
        temp.y <- unname(unlist(yvalues[i, ]))
        temp.lm <- lm(temp.y ~ x)
        raw.data[i, ] <- unname(coef(temp.lm))
}

report <- data.frame(matrix(0, 2, 2), row.names = c("mean", "variance"))
names(report) <- c("yIntercept", "slope")
report[1,1] <- mean(raw.data$yIntercept)
report[1,2] <- mean(raw.data$slope)
report[2,1] <- var(raw.data$yIntercept)
report[2,2] <- var(raw.data$slope)

#          yIntercept      slope
# mean       25.07691 3.99895456
# variance   20.40745 0.01175604

#   (b) Based on theoretical considerations, what should the mean and variance of the  
#       generated coefficients be? Explain your answer.

# The true mean of the y intercept should be 25 because we assume that beta 0 hat is unbiased.
# The true mean of the slope should be 4 because we assumed that beta 1 hat is unbiased.
# Sxx <- sum(x^2) - ((sum(x))^2)/100
Sxx <- sum((x-mean(x))^2)
var.beta0hat <- 12^2*(1/100 + ((mean(x))^2)/Sxx)
var.beta1hat <- (12^2)/Sxx
# The true variance of y intercept should be 20.31578, and the true variance of the slope should be 0.01182987

#   (c) Find a 95% confidence interval centered at each coefficient. Determine and report  
#       the percentage of intervals that contain the true value of the coefficient. 
#       What should the percentage be?

intercept.interval <- data.frame(matrix(0, 1000, 4))
names(intercept.interval) <- c("intercept", "lower", "upper", "withinInterval")
slope.interval <- data.frame(matrix(0, 1000, 4))
names(slope.interval) <- c("slope", "lower", "upper", "withinInterval")

for (i in 1:1000){
        temp.y <- unname(unlist(yvalues[i, ]))
        temp.lm <- lm(temp.y ~ x)
        temp.intercept.confint <- unname(confint(temp.lm))[1,]
        temp.intercept <- unname(temp.lm$coefficients[1])
        temp.in <- temp.intercept.confint[1]<25 & temp.intercept.confint[2] > 25
        intercept.interval[i, ] <- c(temp.intercept, temp.intercept.confint, temp.in)
        
        temp.slope.confint <- unname(confint(temp.lm))[2,]
        temp.slope <- unname(temp.lm$coefficients[2])
        temp.in.slope <- temp.slope.confint[1]<4 & temp.slope.confint[2] > 4
        slope.interval[i, ] <- c(temp.slope, temp.slope.confint, temp.in.slope)
}

sum(intercept.interval$withinInterval) / nrow(intercept.interval)
# 0.949, so 94.9% of the intercept confident intervals contain the true intercept

sum(slope.interval$withinInterval) / nrow(slope.interval)
# 0.948, so 94.8% of the slope confident intervals contain the true slope

#   (d) Carry out the hypothesis test H0: beta_1 = 4 vs H1: beta_1 not= 4 at a 5% significance level. 
#       Determine and report the proportion of times that the null hypothesis is rejected, 
#       implying that beta_1 not= 4.
ht <- logical()
for (i in 1:1000){
        temp.y <- unname(unlist(yvalues[i, ]))
        temp.lm <- lm(temp.y ~ x)
        se <- summary(temp.lm)$coef[2,2]
        beta1 <- 4
        beta1hat <- unname(coef(temp.lm)[2])
        
        t <- (beta1hat - beta1)/se
        t.stats <- qt(0.975, temp.lm$df.residual)
        interval <- c(-t.stats, t.stats)
        NullIsRejected <- t < interval[1] | t > interval[2]
        
        ht <- c(ht, NullIsRejected)
}

sum(ht)/nrow(yvalues)
# 0.052, which means that the null hypothesis is rejected 5.2% of the time

#   (e) For each set of coefficients, find a 95% confidence interval for the mean
#       response associated with x = 18. Determine and report the percentage of your
#       intervals that contain the true value. What should the percentage be?

true.y <- 25 + 4*18
newdata <- data.frame(x=18)
contain.true <- logical()

for (i in 1:1000){
        temp.y <- unname(unlist(yvalues[i, ]))
        temp.lm <- lm(temp.y ~ x)
        interval <- predict(temp.lm, newdata, interval = "confidence", level = 0.95)
        conf.int <- c(interval[2], interval[3])
        containTrue <- conf.int[1] < true.y & conf.int[2] > true.y
        contain.true <- c(contain.true, containTrue)
}

sum(contain.true)/nrow(yvalues)
# 0.948, which means 94.8% of the intervals contain the true value

#   (f) For each estimated mean response from interval = part (e), find a corresponding
#       95% prediction interval for the response y. Generate one random response y based 
#       on the true model. Determine and report the percentage of intervals that contain the response.
#       What should the percentage be?
random.y <- 25 + 4*18 + rnorm(1, mean=0, sd = 12)
newdata <- data.frame(x=18)
contain.true <- logical()

for (i in 1:1000){
        temp.y <- unname(unlist(yvalues[i, ]))
        temp.lm <- lm(temp.y ~ x)
        interval <- predict(temp.lm, newdata, interval = "prediction", level = 0.95)
        pred.int <- c(interval[2], interval[3])
        containTrue <- pred.int[1] < random.y & pred.int[2] > random.y
        contain.true <- c(contain.true, containTrue)
}

sum(contain.true)/nrow(yvalues)

# 1, which means that 100% of prediction intervals contain my randomly generated y


#   (g) Find and report a 95% confidence interval for sigma^2 by finding the 2.5th and 97.5th 
#       percentiles of the generated values of MS_Res to give the lower and upper confidence limits.
mse <- numeric()
for (i in 1:1000){
        temp.y <- unname(unlist(yvalues[i, ]))
        temp.lm <- lm(temp.y ~ x)
        temp.mse <- sum(temp.lm$residuals^2)/temp.lm$df.residual
        mse <- c(mse, temp.mse)
}

quantile(mse, probs = c(0.025, 0.975))
#     2.5%    97.5% 
# 107.3006 184.5563 