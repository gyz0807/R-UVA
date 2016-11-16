# Yizhe Ge (yg2kj)

####################
#                  #
#   Homework 8     #
#                  #
####################


## Your answers may be submitted as an annotated R file. ##
###########################################################

##############
## Question ##
##############

# For this problem you will use the files "homework08data01.csv", "homework08data02.csv", and "homework08data03.csv" to 
# demonstrate through simulation the issues that can result from using the "traditional" methods to treat missing data. Each
# of the three data sets contains real estate information for 1000 records that detail home selling price (y), 
# home size (x1), and a quality rating (x2). There are 400 missing values in each data set. The missing mechanism is 
# MCAR, MAR, and MNAR, respectively. The true relationship is y = 29.3 + 5.6*x1 + 3.8*x2 + epsilon and the standard 
# deviation of epsilon is 21.
# 
# Follow the procedures below for each of the three data sets.
#
# For each "traditional" method listed below, do the following:
#   (1) Repeat the following process 1000 times. 
#       (a) Take a simple random sample of size 500 from the data set.
#       (b) Treat the missing data according to the particular method.
#       (c) Estimate and record the three regression parameters. 
#       (d) Use the estimated regression parameters to calculate MSE.
#       (e) Determine the 95% confidence interval for each of the regression parameters and 
#           record whether or not it contains the true parameter value.
#   (2) Determine and report the mean and variance of the generated coefficients and the MSE. 
#   (3) Determine and report the coverage of the confidence intervals of the parameters.

# The "traditional" methods to use:
#   (1) Listwise deletion
#   (2) Pairwise deletion
#   (3) Arithmetic mean imputation
#   (4) Regression imputation
#   (5) Stochastic regression imputation
#   (6) Hot-Deck imputation
#   (7) Similar resonse pattern imputation
#   (8) Indicator method imputation
#
# Summarize your findings in a table and discuss your observations.


################################## Data set 1 (Listwise deletion)
data1 <- read.csv("homework08data01.csv", header = FALSE, col.names = c("y", "x1", "x2"))
set.seed(1)
coefs <- data.frame(matrix(0, 1000, 3))
names(coefs) <- c("Intercept", "beta1", "beta2")
mse <- numeric(1000)
containTrue <- data.frame(matrix(0, 1000, 3))
names(containTrue) <- c("Intercept", "beta1", "beta2")

### Repeat the following process 1000 times.
for (i in 1:1000){
  ### Take a simple random sample of size 500 from the data set.
  temp.rows <- sample(x = 1:nrow(data1), size = 500, replace = TRUE)
  temp.data <- data1[temp.rows, ]
  
  ### Treat the missing data according to the particular method.
  temp.data <- na.omit(temp.data)
  
  ### Estimate and record the three regression parameters. 
  temp.lm <- lm(y ~ ., data = temp.data)
  coefs[i, ] <- coef(temp.lm)
  
  ### Use the estimated regression parameters to calculate MSE.
  temp.mse <- sum(temp.lm$residuals^2)/temp.lm$df.residual
  mse[i] <- temp.mse
  
  ### Determine the 95% confidence interval for each of the regression parameters and 
  ### record whether or not it contains the true parameter value.
  temp.contain.inter <- confint(temp.lm)[1, 1] <= 29.3 & 29.3 <= confint(temp.lm)[1, 2]
  temp.contain.beta1 <- confint(temp.lm)[2, 1] <= 5.6 & 5.6 <= confint(temp.lm)[2, 2]
  temp.contain.beta2 <- confint(temp.lm)[3, 1] <= 3.8 & 3.8 <= confint(temp.lm)[3, 2]
  temp.contain <- c(temp.contain.inter, temp.contain.beta1, temp.contain.beta2)
  containTrue[i, ] <- temp.contain
}

### Determine and report the mean and variance of the generated coefficients and the MSE. 
coef.mean.1.1 <- apply(coefs, 2, mean)
# Intercept     beta1     beta2 
# 29.266365  5.684869  3.743970 

coef.sd.1.1 <- apply(coefs, 2, var)
# Intercept      beta1      beta2 
# 45.0066423  0.0949563  0.3497948 

mse.mean.1.1 <- mean(mse)
# 477.1175

mse.var.1.1 <- var(mse)
# 1594.341

### Determine and report the coverage of the confidence intervals of the parameters.
# Interval
coverage.inter.1.1 <- sum(containTrue$Intercept) / nrow(containTrue)
# 0.944

# Beta1
coverage.b1.1.1 <- sum(containTrue$beta1) / nrow(containTrue)
# 0.951

# Beta2
coverage.b2.1.1 <- sum(containTrue$beta2) / nrow(containTrue)
# 0.947

################################## Data set 1 (Pairwise deletion)
data1 <- read.csv("homework08data01.csv", header = FALSE, col.names = c("y", "x1", "x2"))
set.seed(1)
coefs <- data.frame(matrix(0, 1000, 3))
names(coefs) <- c("Intercept", "beta1", "beta2")
mse <- numeric(1000)
containTrue <- data.frame(matrix(0, 1000, 3))
names(containTrue) <- c("Intercept", "beta1", "beta2")

### Repeat the following process 1000 times.
for (i in 1:1000){
  # Take a simple random sample of size 500 from the data set.
  temp.rows <- sample(x = 1:nrow(data1), size = 500, replace = TRUE)
  temp.data <- data1[temp.rows, ]
  
  ### Treat the missing data according to the particular method.
  # beta.hat = rxy * R^-1
  cov.x1y <- cov(na.omit(temp.data[, c("y", "x1")]))[1, 2]
  cov.x2y <- cov(na.omit(temp.data[, c("y", "x2")]))[1, 2]
  cov.x1x1 <- cov(na.omit(temp.data[, c("x1", "x2")]))[1, 1]
  cov.x1x2 <- cov(na.omit(temp.data[, c("x1", "x2")]))[1, 2]
  cov.x2x2 <- cov(na.omit(temp.data[, c("x1", "x2")]))[2, 2]
  rxy <- matrix(c(cov.x1y, cov.x2y), 1, 2)
  R <- matrix(c(cov.x1x1, cov.x1x2, cov.x1x2, cov.x2x2), 2, 2)
  
  ### Estimate and record the three regression parameters. 
  betas <- rxy %*% solve(R)
  beta1 <- betas[1]
  beta2 <- betas[2]
  beta0 <- mean(na.omit(temp.data)[, 1]) - betas[1]*mean(na.omit(temp.data)[, 2]) - betas[2]*mean(na.omit(temp.data)[, 3])
  betas <- c(beta0, beta1, beta2)
  coefs[i, ] <- betas
  
  ### Use the estimated regression parameters to calculate MSE.
  temp.data.nomissing <- na.omit(temp.data)
  temp.model.matrix <- model.matrix(y ~ ., temp.data.nomissing)
  betas.matrix <- matrix(betas, 3, 1)
  temp.preds <- temp.model.matrix %*% betas.matrix
  temp.mse <- sum((temp.data.nomissing$y - temp.preds)^2)/(nrow(temp.data.nomissing) - 3)
  mse[i] <- temp.mse
  
  ### Determine the 95% confidence interval for each of the regression parameters and 
  ### record whether or not it contains the true parameter value.
  X <- temp.model.matrix
  var.betaHat <- temp.mse * solve(t(X) %*% X)
  se.betas <- sqrt(diag(var.betaHat))
  
  confint.inter <- betas[1] + c(-1, 1) * qt(0.975, df = nrow(temp.data.nomissing) - 3) * se.betas[1]
  temp.contain.inter <- confint.inter[1] <= 29.3 & 29.3 <= confint.inter[2]
  
  confint.beta1 <- betas[2] + c(-1, 1) * qt(0.975, df = nrow(temp.data.nomissing) - 3) * se.betas[2]
  temp.contain.beta1 <- confint.beta1[1] <= 5.6 & 5.6 <= confint.beta1[2]
  
  confint.beta2 <- betas[3] + c(-1, 1) * qt(0.975, df = nrow(temp.data.nomissing) - 3) * se.betas[3]
  temp.contain.beta2 <- confint(temp.lm)[3, 1] <= 3.8 & 3.8 <= confint(temp.lm)[3, 2]
  temp.contain <- c(temp.contain.inter, temp.contain.beta1, temp.contain.beta2)
  containTrue[i, ] <- temp.contain
}

### Determine and report the mean and variance of the generated coefficients and the MSE. 
coef.mean.1.2 <- apply(coefs, 2, mean)
# Intercept     beta1     beta2 
# 27.386435  5.762389  3.832479 

coef.sd.1.2 <- apply(coefs, 2, var)
#  Intercept      beta1      beta2 
# 83.3131721  0.2015829  0.4940917 

mse.mean.1.2 <- mean(mse)
# 480.2774

mse.var.1.2 <- var(mse)
# 1596.682

### Determine and report the coverage of the confidence intervals of the parameters.
# Interval
coverage.inter.1.2 <- sum(containTrue$Intercept) / nrow(containTrue)
# 0.838

# Beta1
coverage.b1.1.2 <- sum(containTrue$beta1) / nrow(containTrue)
# 0.814

# Beta2
coverage.b2.1.2 <- sum(containTrue$beta2) / nrow(containTrue)
# 1

################################## Data set 1 (Arithmetic mean imputation)
data1 <- read.csv("homework08data01.csv", header = FALSE, col.names = c("y", "x1", "x2"))
set.seed(1)
coefs <- data.frame(matrix(0, 1000, 3))
names(coefs) <- c("Intercept", "beta1", "beta2")
mse <- numeric(1000)
containTrue <- data.frame(matrix(0, 1000, 3))
names(containTrue) <- c("Intercept", "beta1", "beta2")

### Repeat the following process 1000 times.
for (i in 1:1000){
  ### Take a simple random sample of size 500 from the data set.
  temp.rows <- sample(x = 1:nrow(data1), size = 500, replace = TRUE)
  temp.data <- data1[temp.rows, ]
  
  ### Treat the missing data according to the particular method.
  y.mean <- mean(temp.data$y, na.rm = TRUE)
  x1.mean <- mean(temp.data$x1, na.rm = TRUE)
  x2.mean <- mean(temp.data$x2, na.rm = TRUE)
  temp.data$y[is.na(temp.data$y)] <- y.mean
  temp.data$x1[is.na(temp.data$x1)] <- x1.mean
  temp.data$x2[is.na(temp.data$x2)] <- x2.mean
  
  ### Estimate and record the three regression parameters. 
  temp.lm <- lm(y ~ ., data = temp.data)
  coefs[i, ] <- coef(temp.lm)
  
  ### Use the estimated regression parameters to calculate MSE.
  temp.mse <- sum(temp.lm$residuals^2)/temp.lm$df.residual
  mse[i] <- temp.mse
  
  ### Determine the 95% confidence interval for each of the regression parameters and 
  ### record whether or not it contains the true parameter value.
  temp.contain.inter <- confint(temp.lm)[1, 1] <= 29.3 & 29.3 <= confint(temp.lm)[1, 2]
  temp.contain.beta1 <- confint(temp.lm)[2, 1] <= 5.6 & 5.6 <= confint(temp.lm)[2, 2]
  temp.contain.beta2 <- confint(temp.lm)[3, 1] <= 3.8 & 3.8 <= confint(temp.lm)[3, 2]
  temp.contain <- c(temp.contain.inter, temp.contain.beta1, temp.contain.beta2)
  containTrue[i, ] <- temp.contain
}

### Determine and report the mean and variance of the generated coefficients and the MSE. 
coef.mean.1.3 <- apply(coefs, 2, mean)
# Intercept     beta1     beta2 
# 54.441162  4.585849  2.850499 

coef.sd.1.3 <- apply(coefs, 2, var)
#  Intercept      beta1      beta2 
# 42.5488221  0.0910279  0.2831758 

mse.mean.1.3 <- mean(mse)
# 501.4628

mse.var.1.3 <- var(mse)
# 1028.579

### Determine and report the coverage of the confidence intervals of the parameters.
# Interval
coverage.inter.1.3 <- sum(containTrue$Intercept) / nrow(containTrue)
# 0.011

# Beta1
coverage.b1.1.3 <- sum(containTrue$beta1) / nrow(containTrue)
# 0.041

# Beta2
coverage.b2.1.3 <- sum(containTrue$beta2) / nrow(containTrue)
# 0.548

################################## Data set 1 (Regression imputation)
data1 <- read.csv("homework08data01.csv", header = FALSE, col.names = c("y", "x1", "x2"))
set.seed(1)
coefs <- data.frame(matrix(0, 1000, 3))
names(coefs) <- c("Intercept", "beta1", "beta2")
mse <- numeric(1000)
containTrue <- data.frame(matrix(0, 1000, 3))
names(containTrue) <- c("Intercept", "beta1", "beta2")

### Repeat the following process 1000 times.
for (i in 1:1000){
  ### Take a simple random sample of size 500 from the data set.
  temp.rows <- sample(x = 1:nrow(data1), size = 500, replace = TRUE)
  temp.data <- data1[temp.rows, ]
  
  ### Treat the missing data according to the particular method.
  miss.two.value.rows <- which(apply(is.na(temp.data), 1, sum) == 2)
  if (length(miss.two.value.rows) == 0) {
    temp.data.miss.1 <- temp.data
  } else {
    temp.data.miss.1 <- temp.data[-miss.two.value.rows, ]
  }
  
  # Situation 1: only miss y
  lm.impute.y <- lm(y ~ ., data = temp.data.miss.1)
  na.y.rows <- which(is.na(temp.data.miss.1$y))
  y.preds <- unname(predict(lm.impute.y, newdata = temp.data[na.y.rows, ]))

  # Situation 2: only miss x2
  lm.impute.x2 <- lm(x2 ~ ., data = temp.data.miss.1)
  na.x2.rows <- which(is.na(temp.data.miss.1$x2))
  x2.preds <- unname(predict(lm.impute.x2, newdata = temp.data[na.x2.rows, ]))
  
  # Situation 3: miss both y and x2
  lm.impute.both.y <- lm(y ~ x1, data = temp.data)
  both.y.preds <- unname(predict(lm.impute.both.y, newdata = temp.data[miss.two.value.rows, ]))
  lm.impute.both.x2 <- lm(x2 ~ x1, data = temp.data)
  both.x2.preds <- unname(predict(lm.impute.both.x2, newdata = temp.data[miss.two.value.rows, ]))
  
  # Impute missing values
  # only miss y
  temp.data$y[na.y.rows] <- y.preds
  # only miss x2
  temp.data$x2[na.x2.rows] <- x2.preds
  # miss both
  temp.data$y[miss.two.value.rows] <- both.y.preds
  temp.data$x2[miss.two.value.rows] <- both.x2.preds
  
  ### Estimate and record the three regression parameters. 
  temp.lm <- lm(y ~ ., data = temp.data)
  coefs[i, ] <- coef(temp.lm)
  
  ### Use the estimated regression parameters to calculate MSE.
  temp.mse <- sum(temp.lm$residuals^2)/temp.lm$df.residual
  mse[i] <- temp.mse
  
  ### Determine the 95% confidence interval for each of the regression parameters and 
  ### record whether or not it contains the true parameter value.
  temp.contain.inter <- confint(temp.lm)[1, 1] <= 29.3 & 29.3 <= confint(temp.lm)[1, 2]
  temp.contain.beta1 <- confint(temp.lm)[2, 1] <= 5.6 & 5.6 <= confint(temp.lm)[2, 2]
  temp.contain.beta2 <- confint(temp.lm)[3, 1] <= 3.8 & 3.8 <= confint(temp.lm)[3, 2]
  temp.contain <- c(temp.contain.inter, temp.contain.beta1, temp.contain.beta2)
  containTrue[i, ] <- temp.contain
}

### Determine and report the mean and variance of the generated coefficients and the MSE. 
coef.mean.1.4 <- apply(coefs, 2, mean)
# Intercept     beta1     beta2 
# 24.564848  5.723999  4.552261 

coef.sd.1.4 <- apply(coefs, 2, var)
#   Intercept       beta1       beta2 
# 38.89429505  0.07725538  0.44264291 

mse.mean.1.4 <- mean(mse)
# 367.4

mse.var.1.4 <- var(mse)
# 837.8692

### Determine and report the coverage of the confidence intervals of the parameters.
# Interval
coverage.inter.1.4 <- sum(containTrue$Intercept) / nrow(containTrue)
# 0.72

# Beta1
coverage.b1.1.4 <- sum(containTrue$beta1) / nrow(containTrue)
# 0.826

# Beta2
coverage.b2.1.4 <- sum(containTrue$beta2) / nrow(containTrue)
# 0.544

################################## Data set 1 (Stochastic regression imputation)
data1 <- read.csv("homework08data01.csv", header = FALSE, col.names = c("y", "x1", "x2"))
set.seed(1)
coefs <- data.frame(matrix(0, 1000, 3))
names(coefs) <- c("Intercept", "beta1", "beta2")
mse <- numeric(1000)
containTrue <- data.frame(matrix(0, 1000, 3))
names(containTrue) <- c("Intercept", "beta1", "beta2")

### Repeat the following process 1000 times.
for (i in 1:1000){
  ### Take a simple random sample of size 500 from the data set.
  temp.rows <- sample(x = 1:nrow(data1), size = 500, replace = TRUE)
  temp.data <- data1[temp.rows, ]
  
  ### Treat the missing data according to the particular method.
  miss.two.value.rows <- which(apply(is.na(temp.data), 1, sum) == 2)
  if (length(miss.two.value.rows) == 0) {
    temp.data.miss.1 <- temp.data
  } else {
    temp.data.miss.1 <- temp.data[-miss.two.value.rows, ]
  }
  
  # Situation 1: only miss y
  lm.impute.y <- lm(y ~ ., data = temp.data.miss.1)
  na.y.rows <- which(is.na(temp.data.miss.1$y))
  y.preds <- unname(predict(lm.impute.y, newdata = temp.data[na.y.rows, ]))
  err.term.y <- rnorm(length(y.preds), mean = 0, sd = sigma(lm.impute.y))
  y.preds <- y.preds + err.term.y
  
  # Situation 2: only miss x2
  lm.impute.x2 <- lm(x2 ~ ., data = temp.data.miss.1)
  na.x2.rows <- which(is.na(temp.data.miss.1$x2))
  x2.preds <- unname(predict(lm.impute.x2, newdata = temp.data[na.x2.rows, ]))
  err.term.x2 <- rnorm(length(x2.preds), mean = 0, sd = sigma(lm.impute.x2))
  x2.preds <- x2.preds + err.term.x2
  
  # Situation 3: miss both y and x2
  lm.impute.both.y <- lm(y ~ x1, data = temp.data)
  both.y.preds <- unname(predict(lm.impute.both.y, newdata = temp.data[miss.two.value.rows, ]))
  err.term.both.y <- rnorm(length(both.y.preds), mean = 0, sd = sigma(lm.impute.both.y))
  both.y.preds <- both.y.preds + err.term.both.y
  
  lm.impute.both.x2 <- lm(x2 ~ x1, data = temp.data)
  both.x2.preds <- unname(predict(lm.impute.both.x2, newdata = temp.data[miss.two.value.rows, ]))
  err.term.both.x2 <- rnorm(length(both.x2.preds), mean = 0, sd = sigma(lm.impute.both.x2))
  both.x2.preds <- both.x2.preds + err.term.both.x2
  
  # Impute missing values
  # only miss y
  temp.data$y[na.y.rows] <- y.preds
  # only miss x2
  temp.data$x2[na.x2.rows] <- x2.preds
  # miss both
  temp.data$y[miss.two.value.rows] <- both.y.preds
  temp.data$x2[miss.two.value.rows] <- both.x2.preds
  
  ### Estimate and record the three regression parameters. 
  temp.lm <- lm(y ~ ., data = temp.data)
  coefs[i, ] <- coef(temp.lm)
  
  ### Use the estimated regression parameters to calculate MSE.
  temp.mse <- sum(temp.lm$residuals^2)/temp.lm$df.residual
  mse[i] <- temp.mse
  
  ### Determine the 95% confidence interval for each of the regression parameters and 
  ### record whether or not it contains the true parameter value.
  temp.contain.inter <- confint(temp.lm)[1, 1] <= 29.3 & 29.3 <= confint(temp.lm)[1, 2]
  temp.contain.beta1 <- confint(temp.lm)[2, 1] <= 5.6 & 5.6 <= confint(temp.lm)[2, 2]
  temp.contain.beta2 <- confint(temp.lm)[3, 1] <= 3.8 & 3.8 <= confint(temp.lm)[3, 2]
  temp.contain <- c(temp.contain.inter, temp.contain.beta1, temp.contain.beta2)
  containTrue[i, ] <- temp.contain
}

### Determine and report the mean and variance of the generated coefficients and the MSE. 
coef.mean.1.5 <- apply(coefs, 2, mean)
# Intercept     beta1     beta2 
# 28.434327  5.745493  3.771609 

coef.sd.1.5 <- apply(coefs, 2, var)
#   Intercept       beta1       beta2 
# 43.47943903  0.08964982  0.41212723

mse.mean.1.5 <- mean(mse)
# 476.4345

mse.var.1.5 <- var(mse)
# 1503.987

### Determine and report the coverage of the confidence intervals of the parameters.
# Interval
coverage.inter.1.5 <- sum(containTrue$Intercept) / nrow(containTrue)
# 0.872

# Beta1
coverage.b1.1.5 <- sum(containTrue$beta1) / nrow(containTrue)
# 0.837

# Beta2
coverage.b2.1.5 <- sum(containTrue$beta2) / nrow(containTrue)
# 0.83

################################## Data set 1 (Hot-Deck imputation)
library(hot.deck)
data1 <- read.csv("homework08data01.csv", header = FALSE, col.names = c("y", "x1", "x2"))
set.seed(1)
coefs <- data.frame(matrix(0, 1000, 3))
names(coefs) <- c("Intercept", "beta1", "beta2")
mse <- numeric(1000)
containTrue <- data.frame(matrix(0, 1000, 3))
names(containTrue) <- c("Intercept", "beta1", "beta2")

### Repeat the following process 1000 times.
for (i in 1:1000){
  ### Take a simple random sample of size 500 from the data set.
  temp.rows <- sample(x = 1:nrow(data1), size = 500, replace = TRUE)
  temp.data <- data1[temp.rows, ]
  
  ### Treat the missing data according to the particular method.
  temp.data <- hot.deck(temp.data, sdCutoff = 1.5)$data[[1]]
  
  ### Estimate and record the three regression parameters. 
  temp.lm <- lm(y ~ ., data = temp.data)
  coefs[i, ] <- coef(temp.lm)
  
  ### Use the estimated regression parameters to calculate MSE.
  temp.mse <- sum(temp.lm$residuals^2)/temp.lm$df.residual
  mse[i] <- temp.mse
  
  ### Determine the 95% confidence interval for each of the regression parameters and 
  ### record whether or not it contains the true parameter value.
  temp.contain.inter <- confint(temp.lm)[1, 1] <= 29.3 & 29.3 <= confint(temp.lm)[1, 2]
  temp.contain.beta1 <- confint(temp.lm)[2, 1] <= 5.6 & 5.6 <= confint(temp.lm)[2, 2]
  temp.contain.beta2 <- confint(temp.lm)[3, 1] <= 3.8 & 3.8 <= confint(temp.lm)[3, 2]
  temp.contain <- c(temp.contain.inter, temp.contain.beta1, temp.contain.beta2)
  containTrue[i, ] <- temp.contain
}

### Determine and report the mean and variance of the generated coefficients and the MSE. 
coef.mean.1.6 <- apply(coefs, 2, mean)
# Intercept     beta1     beta2 
# 44.132533  5.250582  2.581519

coef.sd.1.6 <- apply(coefs, 2, var)
#   Intercept       beta1       beta2 
# 37.92310106  0.09384647  0.27674608 

mse.mean.1.6 <- mean(mse)
# 584.7838

mse.var.1.6 <- var(mse)
# 2042.785

### Determine and report the coverage of the confidence intervals of the parameters.
# Interval
coverage.inter.1.6 <- sum(containTrue$Intercept) / nrow(containTrue)
# 0.262

# Beta1
coverage.b1.1.6 <- sum(containTrue$beta1) / nrow(containTrue)
# 0.721

# Beta2
coverage.b2.1.6 <- sum(containTrue$beta2) / nrow(containTrue)
# 0.318

################################## Data set 1 (Similar resonse pattern imputation)
library(VIM)
data1 <- read.csv("homework08data01.csv", header = FALSE, col.names = c("y", "x1", "x2"))
set.seed(1)
coefs <- data.frame(matrix(0, 1000, 3))
names(coefs) <- c("Intercept", "beta1", "beta2")
mse <- numeric(1000)
containTrue <- data.frame(matrix(0, 1000, 3))
names(containTrue) <- c("Intercept", "beta1", "beta2")

### Repeat the following process 1000 times.
for (i in 1:1000){
  ### Take a simple random sample of size 500 from the data set.
  temp.rows <- sample(x = 1:nrow(data1), size = 500, replace = TRUE)
  temp.data <- data1[temp.rows, ]
  
  ### Treat the missing data according to the particular method.
  temp.data <- kNN(temp.data)[, 1:3]

  ### Estimate and record the three regression parameters. 
  temp.lm <- lm(y ~ ., data = temp.data)
  coefs[i, ] <- coef(temp.lm)
  
  ### Use the estimated regression parameters to calculate MSE.
  temp.mse <- sum(temp.lm$residuals^2)/temp.lm$df.residual
  mse[i] <- temp.mse
  
  ### Determine the 95% confidence interval for each of the regression parameters and 
  ### record whether or not it contains the true parameter value.
  temp.contain.inter <- confint(temp.lm)[1, 1] <= 29.3 & 29.3 <= confint(temp.lm)[1, 2]
  temp.contain.beta1 <- confint(temp.lm)[2, 1] <= 5.6 & 5.6 <= confint(temp.lm)[2, 2]
  temp.contain.beta2 <- confint(temp.lm)[3, 1] <= 3.8 & 3.8 <= confint(temp.lm)[3, 2]
  temp.contain <- c(temp.contain.inter, temp.contain.beta1, temp.contain.beta2)
  containTrue[i, ] <- temp.contain
}

### Determine and report the mean and variance of the generated coefficients and the MSE. 
apply(coefs, 2, mean)
# Intercept     beta1     beta2 
# 29.554067  5.557168  4.149792 

apply(coefs, 2, var)
# Intercept       beta1       beta2 
# 40.33331822  0.08647988  0.45888415 

mean(mse)
# 419.8654

var(mse)
# 1184.431

### Determine and report the coverage of the confidence intervals of the parameters.
# Interval
sum(containTrue$Intercept) / nrow(containTrue)
# 0.856

# Beta1
sum(containTrue$beta1) / nrow(containTrue)
# 0.868

# Beta2
sum(containTrue$beta2) / nrow(containTrue)
# 0.737

################################## Data set 1 (Indicator method imputation)
data1 <- read.csv("homework08data01.csv", header = FALSE, col.names = c("y", "x1", "x2"))
set.seed(1)
coefs <- data.frame(matrix(0, 1000, 3))
names(coefs) <- c("Intercept", "beta1", "beta2")
mse <- numeric(1000)
containTrue <- data.frame(matrix(0, 1000, 3))
names(containTrue) <- c("Intercept", "beta1", "beta2")

### Repeat the following process 1000 times.
for (i in 1:1000){
  ### Take a simple random sample of size 500 from the data set.
  temp.rows <- sample(x = 1:nrow(data1), size = 500, replace = TRUE)
  temp.data <- data1[temp.rows, ]
  
  ### Treat the missing data according to the particular method.
  temp.data <- complete(mice(temp.data, method = "ri", printFlag = FALSE))
  
  ### Estimate and record the three regression parameters. 
  temp.lm <- lm(y ~ ., data = temp.data)
  coefs[i, ] <- coef(temp.lm)
  
  ### Use the estimated regression parameters to calculate MSE.
  temp.mse <- sum(temp.lm$residuals^2)/temp.lm$df.residual
  mse[i] <- temp.mse
  
  ### Determine the 95% confidence interval for each of the regression parameters and 
  ### record whether or not it contains the true parameter value.
  temp.contain.inter <- confint(temp.lm)[1, 1] <= 29.3 & 29.3 <= confint(temp.lm)[1, 2]
  temp.contain.beta1 <- confint(temp.lm)[2, 1] <= 5.6 & 5.6 <= confint(temp.lm)[2, 2]
  temp.contain.beta2 <- confint(temp.lm)[3, 1] <= 3.8 & 3.8 <= confint(temp.lm)[3, 2]
  temp.contain <- c(temp.contain.inter, temp.contain.beta1, temp.contain.beta2)
  containTrue[i, ] <- temp.contain
}

### Determine and report the mean and variance of the generated coefficients and the MSE. 
coef.mean.1.8 <- apply(coefs, 2, mean)
# Intercept     beta1     beta2 
# 29.708616  5.773363  3.397920 

coef.sd.1.8 <- apply(coefs, 2, var)
#   Intercept       beta1       beta2 
# 54.32023981  0.09518933  0.53444156

mse.mean.1.8 <- mean(mse)
# 529.5997

mse.var.1.8 <- var(mse)
# 6769.824

### Determine and report the coverage of the confidence intervals of the parameters.
# Interval
coverage.inter.1.8 <- sum(containTrue$Intercept) / nrow(containTrue)
# 0.832

# Beta1
coverage.b1.1.8 <- sum(containTrue$beta1) / nrow(containTrue)
# 0.826

# Beta2
coverage.b2.1.8 <- sum(containTrue$beta2) / nrow(containTrue)
# 0.713

######################################################################################################



################################## Data set 2 (Listwise deletion)
data2 <- read.csv("homework08data02.csv", header = FALSE, col.names = c("y", "x1", "x2"))
set.seed(1)
coefs <- data.frame(matrix(0, 1000, 3))
names(coefs) <- c("Intercept", "beta1", "beta2")
mse <- numeric(1000)
containTrue <- data.frame(matrix(0, 1000, 3))
names(containTrue) <- c("Intercept", "beta1", "beta2")

### Repeat the following process 1000 times.
for (i in 1:1000){
  ### Take a simple random sample of size 500 from the data set.
  temp.rows <- sample(x = 1:nrow(data2), size = 500, replace = TRUE)
  temp.data <- data2[temp.rows, ]
  
  ### Treat the missing data according to the particular method.
  temp.data <- na.omit(temp.data)
  
  ### Estimate and record the three regression parameters. 
  temp.lm <- lm(y ~ ., data = temp.data)
  coefs[i, ] <- coef(temp.lm)
  
  ### Use the estimated regression parameters to calculate MSE.
  temp.mse <- sum(temp.lm$residuals^2)/temp.lm$df.residual
  mse[i] <- temp.mse
  
  ### Determine the 95% confidence interval for each of the regression parameters and 
  ### record whether or not it contains the true parameter value.
  temp.contain.inter <- confint(temp.lm)[1, 1] <= 29.3 & 29.3 <= confint(temp.lm)[1, 2]
  temp.contain.beta1 <- confint(temp.lm)[2, 1] <= 5.6 & 5.6 <= confint(temp.lm)[2, 2]
  temp.contain.beta2 <- confint(temp.lm)[3, 1] <= 3.8 & 3.8 <= confint(temp.lm)[3, 2]
  temp.contain <- c(temp.contain.inter, temp.contain.beta1, temp.contain.beta2)
  containTrue[i, ] <- temp.contain
}

### Determine and report the mean and variance of the generated coefficients and the MSE. 
coef.mean.2.1 <- apply(coefs, 2, mean)
# Intercept     beta1     beta2 
# 19.237552  6.040835  4.129242 

coef.sd.2.1 <- apply(coefs, 2, var)
# Intercept      beta1      beta2 
# 73.7095234  0.1487214  0.3691359 

mse.mean.2.1 <- mean(mse)
# 501.0084

mse.var.2.1 <- var(mse)
# 1619.21

### Determine and report the coverage of the confidence intervals of the parameters.
# Interval
coverage.inter.2.1 <- sum(containTrue$Intercept) / nrow(containTrue)
# 0.747

# Beta1
coverage.b1.2.1 <- sum(containTrue$beta1) / nrow(containTrue)
# 0.745

# Beta2
coverage.b2.2.1 <- sum(containTrue$beta2) / nrow(containTrue)
# 0.907


################################## Data set 2 (Pairwise deletion)
data2 <- read.csv("homework08data02.csv", header = FALSE, col.names = c("y", "x1", "x2"))
set.seed(1)
coefs <- data.frame(matrix(0, 1000, 3))
names(coefs) <- c("Intercept", "beta1", "beta2")
mse <- numeric(1000)
containTrue <- data.frame(matrix(0, 1000, 3))
names(containTrue) <- c("Intercept", "beta1", "beta2")

### Repeat the following process 1000 times.
for (i in 1:1000){
  # Take a simple random sample of size 500 from the data set.
  temp.rows <- sample(x = 1:nrow(data2), size = 500, replace = TRUE)
  temp.data <- data2[temp.rows, ]
  
  ### Treat the missing data according to the particular method.
  # beta.hat = rxy * R^-1
  cov.x1y <- cov(na.omit(temp.data[, c("y", "x1")]))[1, 2]
  cov.x2y <- cov(na.omit(temp.data[, c("y", "x2")]))[1, 2]
  cov.x1x1 <- cov(na.omit(temp.data[, c("x1", "x2")]))[1, 1]
  cov.x1x2 <- cov(na.omit(temp.data[, c("x1", "x2")]))[1, 2]
  cov.x2x2 <- cov(na.omit(temp.data[, c("x1", "x2")]))[2, 2]
  rxy <- matrix(c(cov.x1y, cov.x2y), 1, 2)
  R <- matrix(c(cov.x1x1, cov.x1x2, cov.x1x2, cov.x2x2), 2, 2)
  
  ### Estimate and record the three regression parameters. 
  betas <- rxy %*% solve(R)
  beta1 <- betas[1]
  beta2 <- betas[2]
  beta0 <- mean(na.omit(temp.data)[, 1]) - betas[1]*mean(na.omit(temp.data)[, 2]) - betas[2]*mean(na.omit(temp.data)[, 3])
  betas <- c(beta0, beta1, beta2)
  coefs[i, ] <- betas
  
  ### Use the estimated regression parameters to calculate MSE.
  temp.data.nomissing <- na.omit(temp.data)
  temp.model.matrix <- model.matrix(y ~ ., temp.data.nomissing)
  betas.matrix <- matrix(betas, 3, 1)
  temp.preds <- temp.model.matrix %*% betas.matrix
  temp.mse <- sum((temp.data.nomissing$y - temp.preds)^2)/(nrow(temp.data.nomissing) - 3)
  mse[i] <- temp.mse
  
  ### Determine the 95% confidence interval for each of the regression parameters and 
  ### record whether or not it contains the true parameter value.
  X <- temp.model.matrix
  var.betaHat <- temp.mse * solve(t(X) %*% X)
  se.betas <- sqrt(diag(var.betaHat))
  
  confint.inter <- betas[1] + c(-1, 1) * qt(0.975, df = nrow(temp.data.nomissing) - 3) * se.betas[1]
  temp.contain.inter <- confint.inter[1] <= 29.3 & 29.3 <= confint.inter[2]
  
  confint.beta1 <- betas[2] + c(-1, 1) * qt(0.975, df = nrow(temp.data.nomissing) - 3) * se.betas[2]
  temp.contain.beta1 <- confint.beta1[1] <= 5.6 & 5.6 <= confint.beta1[2]
  
  confint.beta2 <- betas[3] + c(-1, 1) * qt(0.975, df = nrow(temp.data.nomissing) - 3) * se.betas[3]
  temp.contain.beta2 <- confint(temp.lm)[3, 1] <= 3.8 & 3.8 <= confint(temp.lm)[3, 2]
  temp.contain <- c(temp.contain.inter, temp.contain.beta1, temp.contain.beta2)
  containTrue[i, ] <- temp.contain
}

### Determine and report the mean and variance of the generated coefficients and the MSE. 
apply(coefs, 2, mean)
# Intercept     beta1     beta2 
# 27.709318  5.716147  3.795028 

apply(coefs, 2, var)
#   Intercept       beta1       beta2 
# 100.0357762   0.2016489   0.5186757 

mean(mse)
# 505.7834

var(mse)
# 1656.731

### Determine and report the coverage of the confidence intervals of the parameters.
# Interval
sum(containTrue$Intercept) / nrow(containTrue)
# 0.89

# Beta1
sum(containTrue$beta1) / nrow(containTrue)
# 0.876

# Beta2
sum(containTrue$beta2) / nrow(containTrue)
# 1

################################## Data set 2 (Arithmetic mean imputation)
data2 <- read.csv("homework08data02.csv", header = FALSE, col.names = c("y", "x1", "x2"))
set.seed(1)
coefs <- data.frame(matrix(0, 1000, 3))
names(coefs) <- c("Intercept", "beta1", "beta2")
mse <- numeric(1000)
containTrue <- data.frame(matrix(0, 1000, 3))
names(containTrue) <- c("Intercept", "beta1", "beta2")

### Repeat the following process 1000 times.
for (i in 1:1000){
  ### Take a simple random sample of size 500 from the data set.
  temp.rows <- sample(x = 1:nrow(data2), size = 500, replace = TRUE)
  temp.data <- data2[temp.rows, ]
  
  ### Treat the missing data according to the particular method.
  y.mean <- mean(temp.data$y, na.rm = TRUE)
  x1.mean <- mean(temp.data$x1, na.rm = TRUE)
  x2.mean <- mean(temp.data$x2, na.rm = TRUE)
  temp.data$y[is.na(temp.data$y)] <- y.mean
  temp.data$x1[is.na(temp.data$x1)] <- x1.mean
  temp.data$x2[is.na(temp.data$x2)] <- x2.mean
  
  ### Estimate and record the three regression parameters. 
  temp.lm <- lm(y ~ ., data = temp.data)
  coefs[i, ] <- coef(temp.lm)
  
  ### Use the estimated regression parameters to calculate MSE.
  temp.mse <- sum(temp.lm$residuals^2)/temp.lm$df.residual
  mse[i] <- temp.mse
  
  ### Determine the 95% confidence interval for each of the regression parameters and 
  ### record whether or not it contains the true parameter value.
  temp.contain.inter <- confint(temp.lm)[1, 1] <= 29.3 & 29.3 <= confint(temp.lm)[1, 2]
  temp.contain.beta1 <- confint(temp.lm)[2, 1] <= 5.6 & 5.6 <= confint(temp.lm)[2, 2]
  temp.contain.beta2 <- confint(temp.lm)[3, 1] <= 3.8 & 3.8 <= confint(temp.lm)[3, 2]
  temp.contain <- c(temp.contain.inter, temp.contain.beta1, temp.contain.beta2)
  containTrue[i, ] <- temp.contain
}

### Determine and report the mean and variance of the generated coefficients and the MSE. 
apply(coefs, 2, mean)
# Intercept     beta1     beta2 
# 60.602681  4.457581  2.875696 

apply(coefs, 2, var)
#   Intercept       beta1       beta2 
# 45.46573847  0.08774017  0.27187196  

mean(mse)
# 513.4797

var(mse)
# 1125.626

### Determine and report the coverage of the confidence intervals of the parameters.
# Interval
sum(containTrue$Intercept) / nrow(containTrue)
# 0.002

# Beta1
sum(containTrue$beta1) / nrow(containTrue)
# 0.018

# Beta2
sum(containTrue$beta2) / nrow(containTrue)
# 0.555

### Determine and report the coverage of the confidence intervals of the parameters.
# Interval
sum(containTrue$Intercept) / nrow(containTrue)
# 0.89

# Beta1
sum(containTrue$beta1) / nrow(containTrue)
# 0.876

# Beta2
sum(containTrue$beta2) / nrow(containTrue)
# 1


################################## Data set 2 (Regression imputation)
data2 <- read.csv("homework08data02.csv", header = FALSE, col.names = c("y", "x1", "x2"))
set.seed(1)
coefs <- data.frame(matrix(0, 1000, 3))
names(coefs) <- c("Intercept", "beta1", "beta2")
mse <- numeric(1000)
containTrue <- data.frame(matrix(0, 1000, 3))
names(containTrue) <- c("Intercept", "beta1", "beta2")

### Repeat the following process 1000 times.
for (i in 1:1000){
  ### Take a simple random sample of size 500 from the data set.
  temp.rows <- sample(x = 1:nrow(data2), size = 500, replace = TRUE)
  temp.data <- data2[temp.rows, ]
  
  ### Treat the missing data according to the particular method.
  miss.two.value.rows <- which(apply(is.na(temp.data), 1, sum) == 2)
  if (length(miss.two.value.rows) == 0) {
    temp.data.miss.1 <- temp.data
  } else {
    temp.data.miss.1 <- temp.data[-miss.two.value.rows, ]
  }
  
  # Situation 1: only miss y
  lm.impute.y <- lm(y ~ ., data = temp.data.miss.1)
  na.y.rows <- which(is.na(temp.data.miss.1$y))
  y.preds <- unname(predict(lm.impute.y, newdata = temp.data[na.y.rows, ]))
  
  # Situation 2: only miss x2
  lm.impute.x2 <- lm(x2 ~ ., data = temp.data.miss.1)
  na.x2.rows <- which(is.na(temp.data.miss.1$x2))
  x2.preds <- unname(predict(lm.impute.x2, newdata = temp.data[na.x2.rows, ]))
  
  # Situation 3: miss both y and x2
  lm.impute.both.y <- lm(y ~ x1, data = temp.data)
  both.y.preds <- unname(predict(lm.impute.both.y, newdata = temp.data[miss.two.value.rows, ]))
  lm.impute.both.x2 <- lm(x2 ~ x1, data = temp.data)
  both.x2.preds <- unname(predict(lm.impute.both.x2, newdata = temp.data[miss.two.value.rows, ]))
  
  # Impute missing values
  # only miss y
  temp.data$y[na.y.rows] <- y.preds
  # only miss x2
  temp.data$x2[na.x2.rows] <- x2.preds
  # miss both
  temp.data$y[miss.two.value.rows] <- both.y.preds
  temp.data$x2[miss.two.value.rows] <- both.x2.preds
  
  ### Estimate and record the three regression parameters. 
  temp.lm <- lm(y ~ ., data = temp.data)
  coefs[i, ] <- coef(temp.lm)
  
  ### Use the estimated regression parameters to calculate MSE.
  temp.mse <- sum(temp.lm$residuals^2)/temp.lm$df.residual
  mse[i] <- temp.mse
  
  ### Determine the 95% confidence interval for each of the regression parameters and 
  ### record whether or not it contains the true parameter value.
  temp.contain.inter <- confint(temp.lm)[1, 1] <= 29.3 & 29.3 <= confint(temp.lm)[1, 2]
  temp.contain.beta1 <- confint(temp.lm)[2, 1] <= 5.6 & 5.6 <= confint(temp.lm)[2, 2]
  temp.contain.beta2 <- confint(temp.lm)[3, 1] <= 3.8 & 3.8 <= confint(temp.lm)[3, 2]
  temp.contain <- c(temp.contain.inter, temp.contain.beta1, temp.contain.beta2)
  containTrue[i, ] <- temp.contain
}

### Determine and report the mean and variance of the generated coefficients and the MSE. 
apply(coefs, 2, mean)
# Intercept     beta1     beta2 
# 18.517733  5.896234  4.824601 

apply(coefs, 2, var)
#   Intercept       beta1       beta2 
# 51.98888072  0.09917874  0.43922744 

mean(mse)
# 367.4844

var(mse)
# 869.2693

### Determine and report the coverage of the confidence intervals of the parameters.
# Interval
sum(containTrue$Intercept) / nrow(containTrue)
# 0.408

# Beta1
sum(containTrue$beta1) / nrow(containTrue)
# 0.631

# Beta2
sum(containTrue$beta2) / nrow(containTrue)
# 0.407


################################## Data set 2 (Stochastic regression imputation)
data2 <- read.csv("homework08data02.csv", header = FALSE, col.names = c("y", "x1", "x2"))
set.seed(1)
coefs <- data.frame(matrix(0, 1000, 3))
names(coefs) <- c("Intercept", "beta1", "beta2")
mse <- numeric(1000)
containTrue <- data.frame(matrix(0, 1000, 3))
names(containTrue) <- c("Intercept", "beta1", "beta2")

### Repeat the following process 1000 times.
for (i in 1:1000){
  ### Take a simple random sample of size 500 from the data set.
  temp.rows <- sample(x = 1:nrow(data2), size = 500, replace = TRUE)
  temp.data <- data2[temp.rows, ]
  
  ### Treat the missing data according to the particular method.
  miss.two.value.rows <- which(apply(is.na(temp.data), 1, sum) == 2)
  if (length(miss.two.value.rows) == 0) {
    temp.data.miss.1 <- temp.data
  } else {
    temp.data.miss.1 <- temp.data[-miss.two.value.rows, ]
  }
  
  # Situation 1: only miss y
  lm.impute.y <- lm(y ~ ., data = temp.data.miss.1)
  na.y.rows <- which(is.na(temp.data.miss.1$y))
  y.preds <- unname(predict(lm.impute.y, newdata = temp.data[na.y.rows, ]))
  err.term.y <- rnorm(length(y.preds), mean = 0, sd = sigma(lm.impute.y))
  y.preds <- y.preds + err.term.y
  
  # Situation 2: only miss x2
  lm.impute.x2 <- lm(x2 ~ ., data = temp.data.miss.1)
  na.x2.rows <- which(is.na(temp.data.miss.1$x2))
  x2.preds <- unname(predict(lm.impute.x2, newdata = temp.data[na.x2.rows, ]))
  err.term.x2 <- rnorm(length(x2.preds), mean = 0, sd = sigma(lm.impute.x2))
  x2.preds <- x2.preds + err.term.x2
  
  # Situation 3: miss both y and x2
  lm.impute.both.y <- lm(y ~ x1, data = temp.data)
  both.y.preds <- unname(predict(lm.impute.both.y, newdata = temp.data[miss.two.value.rows, ]))
  err.term.both.y <- rnorm(length(both.y.preds), mean = 0, sd = sigma(lm.impute.both.y))
  both.y.preds <- both.y.preds + err.term.both.y
  
  lm.impute.both.x2 <- lm(x2 ~ x1, data = temp.data)
  both.x2.preds <- unname(predict(lm.impute.both.x2, newdata = temp.data[miss.two.value.rows, ]))
  err.term.both.x2 <- rnorm(length(both.x2.preds), mean = 0, sd = sigma(lm.impute.both.x2))
  both.x2.preds <- both.x2.preds + err.term.both.x2
  
  # Impute missing values
  # only miss y
  temp.data$y[na.y.rows] <- y.preds
  # only miss x2
  temp.data$x2[na.x2.rows] <- x2.preds
  # miss both
  temp.data$y[miss.two.value.rows] <- both.y.preds
  temp.data$x2[miss.two.value.rows] <- both.x2.preds
  
  ### Estimate and record the three regression parameters. 
  temp.lm <- lm(y ~ ., data = temp.data)
  coefs[i, ] <- coef(temp.lm)
  
  ### Use the estimated regression parameters to calculate MSE.
  temp.mse <- sum(temp.lm$residuals^2)/temp.lm$df.residual
  mse[i] <- temp.mse
  
  ### Determine the 95% confidence interval for each of the regression parameters and 
  ### record whether or not it contains the true parameter value.
  temp.contain.inter <- confint(temp.lm)[1, 1] <= 29.3 & 29.3 <= confint(temp.lm)[1, 2]
  temp.contain.beta1 <- confint(temp.lm)[2, 1] <= 5.6 & 5.6 <= confint(temp.lm)[2, 2]
  temp.contain.beta2 <- confint(temp.lm)[3, 1] <= 3.8 & 3.8 <= confint(temp.lm)[3, 2]
  temp.contain <- c(temp.contain.inter, temp.contain.beta1, temp.contain.beta2)
  containTrue[i, ] <- temp.contain
}

### Determine and report the mean and variance of the generated coefficients and the MSE. 
apply(coefs, 2, mean)
# Intercept     beta1     beta2 
# 22.605200  5.922125  3.996748 

apply(coefs, 2, var)
#  Intercept      beta1      beta2 
# 57.8773846  0.1050887  0.4070207 

mean(mse)
# 484.6769

var(mse)
# 1589.572

### Determine and report the coverage of the confidence intervals of the parameters.
# Interval
sum(containTrue$Intercept) / nrow(containTrue)
# 0.66

# Beta1
sum(containTrue$beta1) / nrow(containTrue)
# 0.672

# Beta2
sum(containTrue$beta2) / nrow(containTrue)
# 0.818


################################## Data set 2 (Hot-Deck imputation)
library(hot.deck)
data2 <- read.csv("homework08data02.csv", header = FALSE, col.names = c("y", "x1", "x2"))
set.seed(1)
coefs <- data.frame(matrix(0, 1000, 3))
names(coefs) <- c("Intercept", "beta1", "beta2")
mse <- numeric(1000)
containTrue <- data.frame(matrix(0, 1000, 3))
names(containTrue) <- c("Intercept", "beta1", "beta2")

### Repeat the following process 1000 times.
for (i in 1:1000){
  ### Take a simple random sample of size 500 from the data set.
  temp.rows <- sample(x = 1:nrow(data2), size = 500, replace = TRUE)
  temp.data <- data2[temp.rows, ]
  
  ### Treat the missing data according to the particular method.
  temp.data <- hot.deck(temp.data, sdCutoff = 1.5)$data[[1]]
  
  ### Estimate and record the three regression parameters. 
  temp.lm <- lm(y ~ ., data = temp.data)
  coefs[i, ] <- coef(temp.lm)
  
  ### Use the estimated regression parameters to calculate MSE.
  temp.mse <- sum(temp.lm$residuals^2)/temp.lm$df.residual
  mse[i] <- temp.mse
  
  ### Determine the 95% confidence interval for each of the regression parameters and 
  ### record whether or not it contains the true parameter value.
  temp.contain.inter <- confint(temp.lm)[1, 1] <= 29.3 & 29.3 <= confint(temp.lm)[1, 2]
  temp.contain.beta1 <- confint(temp.lm)[2, 1] <= 5.6 & 5.6 <= confint(temp.lm)[2, 2]
  temp.contain.beta2 <- confint(temp.lm)[3, 1] <= 3.8 & 3.8 <= confint(temp.lm)[3, 2]
  temp.contain <- c(temp.contain.inter, temp.contain.beta1, temp.contain.beta2)
  containTrue[i, ] <- temp.contain
}

### Determine and report the mean and variance of the generated coefficients and the MSE. 
apply(coefs, 2, mean)
# Intercept     beta1     beta2 
# 41.913570  5.355348  2.636419 

apply(coefs, 2, var)
#   Intercept       beta1       beta2 
# 41.59429340  0.09026352  0.27311093 

mean(mse)
# 558.7249

var(mse)
# 2154.359

### Determine and report the coverage of the confidence intervals of the parameters.
# Interval
sum(containTrue$Intercept) / nrow(containTrue)
# 0.364

# Beta1
sum(containTrue$beta1) / nrow(containTrue)
# 0.802

# Beta2
sum(containTrue$beta2) / nrow(containTrue)
# 0.361

################################## Data set 2 (Similar resonse pattern imputation)
library(VIM)
data2 <- read.csv("homework08data02.csv", header = FALSE, col.names = c("y", "x1", "x2"))
set.seed(1)
coefs <- data.frame(matrix(0, 1000, 3))
names(coefs) <- c("Intercept", "beta1", "beta2")
mse <- numeric(1000)
containTrue <- data.frame(matrix(0, 1000, 3))
names(containTrue) <- c("Intercept", "beta1", "beta2")

### Repeat the following process 1000 times.
for (i in 1:1000){
  ### Take a simple random sample of size 500 from the data set.
  temp.rows <- sample(x = 1:nrow(data2), size = 500, replace = TRUE)
  temp.data <- data2[temp.rows, ]
  
  ### Treat the missing data according to the particular method.
  temp.data <- kNN(temp.data)[, 1:3]
  
  ### Estimate and record the three regression parameters. 
  temp.lm <- lm(y ~ ., data = temp.data)
  coefs[i, ] <- coef(temp.lm)
  
  ### Use the estimated regression parameters to calculate MSE.
  temp.mse <- sum(temp.lm$residuals^2)/temp.lm$df.residual
  mse[i] <- temp.mse
  
  ### Determine the 95% confidence interval for each of the regression parameters and 
  ### record whether or not it contains the true parameter value.
  temp.contain.inter <- confint(temp.lm)[1, 1] <= 29.3 & 29.3 <= confint(temp.lm)[1, 2]
  temp.contain.beta1 <- confint(temp.lm)[2, 1] <= 5.6 & 5.6 <= confint(temp.lm)[2, 2]
  temp.contain.beta2 <- confint(temp.lm)[3, 1] <= 3.8 & 3.8 <= confint(temp.lm)[3, 2]
  temp.contain <- c(temp.contain.inter, temp.contain.beta1, temp.contain.beta2)
  containTrue[i, ] <- temp.contain
}

### Determine and report the mean and variance of the generated coefficients and the MSE. 
apply(coefs, 2, mean)
# Intercept     beta1     beta2 
# 29.498461  5.467481  4.486586 

apply(coefs, 2, var)
# Intercept      beta1      beta2 
# 76.7949817  0.1289055  0.6536542

mean(mse)
# 424.7303

var(mse)
# 1776.908

### Determine and report the coverage of the confidence intervals of the parameters.
# Interval
sum(containTrue$Intercept) / nrow(containTrue)
# 0.711

# Beta1
sum(containTrue$beta1) / nrow(containTrue)
# 0.734

# Beta2
sum(containTrue$beta2) / nrow(containTrue)
# 0.538


################################## Data set 2 (Indicator method imputation)
data2 <- read.csv("homework08data02.csv", header = FALSE, col.names = c("y", "x1", "x2"))
set.seed(1)
coefs <- data.frame(matrix(0, 1000, 3))
names(coefs) <- c("Intercept", "beta1", "beta2")
mse <- numeric(1000)
containTrue <- data.frame(matrix(0, 1000, 3))
names(containTrue) <- c("Intercept", "beta1", "beta2")

### Repeat the following process 1000 times.
for (i in 1:1000){
  ### Take a simple random sample of size 500 from the data set.
  temp.rows <- sample(x = 1:nrow(data2), size = 500, replace = TRUE)
  temp.data <- data2[temp.rows, ]
  
  ### Treat the missing data according to the particular method.
  temp.data <- complete(mice(temp.data, method = "ri", printFlag = FALSE))
  
  ### Estimate and record the three regression parameters. 
  temp.lm <- lm(y ~ ., data = temp.data)
  coefs[i, ] <- coef(temp.lm)
  
  ### Use the estimated regression parameters to calculate MSE.
  temp.mse <- sum(temp.lm$residuals^2)/temp.lm$df.residual
  mse[i] <- temp.mse
  
  ### Determine the 95% confidence interval for each of the regression parameters and 
  ### record whether or not it contains the true parameter value.
  temp.contain.inter <- confint(temp.lm)[1, 1] <= 29.3 & 29.3 <= confint(temp.lm)[1, 2]
  temp.contain.beta1 <- confint(temp.lm)[2, 1] <= 5.6 & 5.6 <= confint(temp.lm)[2, 2]
  temp.contain.beta2 <- confint(temp.lm)[3, 1] <= 3.8 & 3.8 <= confint(temp.lm)[3, 2]
  temp.contain <- c(temp.contain.inter, temp.contain.beta1, temp.contain.beta2)
  containTrue[i, ] <- temp.contain
}

### Determine and report the mean and variance of the generated coefficients and the MSE. 
apply(coefs, 2, mean)
# Intercept     beta1     beta2 
# 39.965323  5.276715  3.547646 

apply(coefs, 2, var)
#   Intercept       beta1       beta2 
# 390.6091260   0.7058280   0.6342975 

mean(mse)
# 555.6583

var(mse)
# 12638.97

### Determine and report the coverage of the confidence intervals of the parameters.
# Interval
sum(containTrue$Intercept) / nrow(containTrue)
# 0.353

# Beta1
sum(containTrue$beta1) / nrow(containTrue)
# 0.427

# Beta2
sum(containTrue$beta2) / nrow(containTrue)
# 0.746



#####################################################################################################

################################## Data set 3 (Listwise deletion)
data3 <- read.csv("homework08data03.csv", header = FALSE, col.names = c("y", "x1", "x2"))
set.seed(1)
coefs <- data.frame(matrix(0, 1000, 3))
names(coefs) <- c("Intercept", "beta1", "beta2")
mse <- numeric(1000)
containTrue <- data.frame(matrix(0, 1000, 3))
names(containTrue) <- c("Intercept", "beta1", "beta2")

### Repeat the following process 1000 times.
for (i in 1:1000){
  ### Take a simple random sample of size 500 from the data set.
  temp.rows <- sample(x = 1:nrow(data3), size = 500, replace = TRUE)
  temp.data <- data3[temp.rows, ]
  
  ### Treat the missing data according to the particular method.
  temp.data <- na.omit(temp.data)
  
  ### Estimate and record the three regression parameters. 
  temp.lm <- lm(y ~ ., data = temp.data)
  coefs[i, ] <- coef(temp.lm)
  
  ### Use the estimated regression parameters to calculate MSE.
  temp.mse <- sum(temp.lm$residuals^2)/temp.lm$df.residual
  mse[i] <- temp.mse
  
  ### Determine the 95% confidence interval for each of the regression parameters and 
  ### record whether or not it contains the true parameter value.
  temp.contain.inter <- confint(temp.lm)[1, 1] <= 29.3 & 29.3 <= confint(temp.lm)[1, 2]
  temp.contain.beta1 <- confint(temp.lm)[2, 1] <= 5.6 & 5.6 <= confint(temp.lm)[2, 2]
  temp.contain.beta2 <- confint(temp.lm)[3, 1] <= 3.8 & 3.8 <= confint(temp.lm)[3, 2]
  temp.contain <- c(temp.contain.inter, temp.contain.beta1, temp.contain.beta2)
  containTrue[i, ] <- temp.contain
}

### Determine and report the mean and variance of the generated coefficients and the MSE. 
apply(coefs, 2, mean)
# Intercept     beta1     beta2 
# 50.900566  5.167289  2.958353 

apply(coefs, 2, var)
# Intercept      beta1      beta2 
# 57.5391294  0.1146458  0.2836147  

mean(mse)
# 434.682

var(mse)
# 1339.044

### Determine and report the coverage of the confidence intervals of the parameters.
# Interval
sum(containTrue$Intercept) / nrow(containTrue)
# 0.142

# Beta1
sum(containTrue$beta1) / nrow(containTrue)
# 0.692

# Beta2
sum(containTrue$beta2) / nrow(containTrue)
# 0.673



################################## Data set 3 (Pairwise deletion)
data3 <- read.csv("homework08data03.csv", header = FALSE, col.names = c("y", "x1", "x2"))
set.seed(1)
coefs <- data.frame(matrix(0, 1000, 3))
names(coefs) <- c("Intercept", "beta1", "beta2")
mse <- numeric(1000)
containTrue <- data.frame(matrix(0, 1000, 3))
names(containTrue) <- c("Intercept", "beta1", "beta2")

### Repeat the following process 1000 times.
for (i in 1:1000){
  # Take a simple random sample of size 500 from the data set.
  temp.rows <- sample(x = 1:nrow(data3), size = 500, replace = TRUE)
  temp.data <- data3[temp.rows, ]
  
  ### Treat the missing data according to the particular method.
  # beta.hat = rxy * R^-1
  cov.x1y <- cov(na.omit(temp.data[, c("y", "x1")]))[1, 2]
  cov.x2y <- cov(na.omit(temp.data[, c("y", "x2")]))[1, 2]
  cov.x1x1 <- cov(na.omit(temp.data[, c("x1", "x2")]))[1, 1]
  cov.x1x2 <- cov(na.omit(temp.data[, c("x1", "x2")]))[1, 2]
  cov.x2x2 <- cov(na.omit(temp.data[, c("x1", "x2")]))[2, 2]
  rxy <- matrix(c(cov.x1y, cov.x2y), 1, 2)
  R <- matrix(c(cov.x1x1, cov.x1x2, cov.x1x2, cov.x2x2), 2, 2)
  
  ### Estimate and record the three regression parameters. 
  betas <- rxy %*% solve(R)
  beta1 <- betas[1]
  beta2 <- betas[2]
  beta0 <- mean(na.omit(temp.data)[, 1]) - betas[1]*mean(na.omit(temp.data)[, 2]) - betas[2]*mean(na.omit(temp.data)[, 3])
  betas <- c(beta0, beta1, beta2)
  coefs[i, ] <- betas
  
  ### Use the estimated regression parameters to calculate MSE.
  temp.data.nomissing <- na.omit(temp.data)
  temp.model.matrix <- model.matrix(y ~ ., temp.data.nomissing)
  betas.matrix <- matrix(betas, 3, 1)
  temp.preds <- temp.model.matrix %*% betas.matrix
  temp.mse <- sum((temp.data.nomissing$y - temp.preds)^2)/(nrow(temp.data.nomissing) - 3)
  mse[i] <- temp.mse
  
  ### Determine the 95% confidence interval for each of the regression parameters and 
  ### record whether or not it contains the true parameter value.
  X <- temp.model.matrix
  var.betaHat <- temp.mse * solve(t(X) %*% X)
  se.betas <- sqrt(diag(var.betaHat))
  
  confint.inter <- betas[1] + c(-1, 1) * qt(0.975, df = nrow(temp.data.nomissing) - 3) * se.betas[1]
  temp.contain.inter <- confint.inter[1] <= 29.3 & 29.3 <= confint.inter[2]
  
  confint.beta1 <- betas[2] + c(-1, 1) * qt(0.975, df = nrow(temp.data.nomissing) - 3) * se.betas[2]
  temp.contain.beta1 <- confint.beta1[1] <= 5.6 & 5.6 <= confint.beta1[2]
  
  confint.beta2 <- betas[3] + c(-1, 1) * qt(0.975, df = nrow(temp.data.nomissing) - 3) * se.betas[3]
  temp.contain.beta2 <- confint(temp.lm)[3, 1] <= 3.8 & 3.8 <= confint(temp.lm)[3, 2]
  temp.contain <- c(temp.contain.inter, temp.contain.beta1, temp.contain.beta2)
  containTrue[i, ] <- temp.contain
}

### Determine and report the mean and variance of the generated coefficients and the MSE. 
apply(coefs, 2, mean)
# Intercept     beta1     beta2 
# 42.602248  5.733197  2.463507

apply(coefs, 2, var)
# Intercept      beta1      beta2 
# 84.5145845  0.1807600  0.4154608 

mean(mse)
# 443.4421

var(mse)
# 1357.66

### Determine and report the coverage of the confidence intervals of the parameters.
# Interval
sum(containTrue$Intercept) / nrow(containTrue)
# 0.501

# Beta1
sum(containTrue$beta1) / nrow(containTrue)
# 0.855

# Beta2
sum(containTrue$beta2) / nrow(containTrue)
# 0



################################## Data set 3 (Arithmetic mean imputation)
data3 <- read.csv("homework08data03.csv", header = FALSE, col.names = c("y", "x1", "x2"))
set.seed(1)
coefs <- data.frame(matrix(0, 1000, 3))
names(coefs) <- c("Intercept", "beta1", "beta2")
mse <- numeric(1000)
containTrue <- data.frame(matrix(0, 1000, 3))
names(containTrue) <- c("Intercept", "beta1", "beta2")

### Repeat the following process 1000 times.
for (i in 1:1000){
  ### Take a simple random sample of size 500 from the data set.
  temp.rows <- sample(x = 1:nrow(data3), size = 500, replace = TRUE)
  temp.data <- data3[temp.rows, ]
  
  ### Treat the missing data according to the particular method.
  y.mean <- mean(temp.data$y, na.rm = TRUE)
  x1.mean <- mean(temp.data$x1, na.rm = TRUE)
  x2.mean <- mean(temp.data$x2, na.rm = TRUE)
  temp.data$y[is.na(temp.data$y)] <- y.mean
  temp.data$x1[is.na(temp.data$x1)] <- x1.mean
  temp.data$x2[is.na(temp.data$x2)] <- x2.mean
  
  ### Estimate and record the three regression parameters. 
  temp.lm <- lm(y ~ ., data = temp.data)
  coefs[i, ] <- coef(temp.lm)
  
  ### Use the estimated regression parameters to calculate MSE.
  temp.mse <- sum(temp.lm$residuals^2)/temp.lm$df.residual
  mse[i] <- temp.mse
  
  ### Determine the 95% confidence interval for each of the regression parameters and 
  ### record whether or not it contains the true parameter value.
  temp.contain.inter <- confint(temp.lm)[1, 1] <= 29.3 & 29.3 <= confint(temp.lm)[1, 2]
  temp.contain.beta1 <- confint(temp.lm)[2, 1] <= 5.6 & 5.6 <= confint(temp.lm)[2, 2]
  temp.contain.beta2 <- confint(temp.lm)[3, 1] <= 3.8 & 3.8 <= confint(temp.lm)[3, 2]
  temp.contain <- c(temp.contain.inter, temp.contain.beta1, temp.contain.beta2)
  containTrue[i, ] <- temp.contain
}

### Determine and report the mean and variance of the generated coefficients and the MSE. 
apply(coefs, 2, mean)
# Intercept     beta1     beta2 
# 65.169543  4.489469  2.228419 

apply(coefs, 2, var)
# Intercept       beta1       beta2 
# 38.15649863  0.07540146  0.21071388  

mean(mse)
# 493.4768

var(mse)
# 1137.488

### Determine and report the coverage of the confidence intervals of the parameters.
# Interval
sum(containTrue$Intercept) / nrow(containTrue)
# 0

# Beta1
sum(containTrue$beta1) / nrow(containTrue)
# 0.01

# Beta2
sum(containTrue$beta2) / nrow(containTrue)
# 0.118



################################## Data set 3 (Regression imputation)
data3 <- read.csv("homework08data03.csv", header = FALSE, col.names = c("y", "x1", "x2"))
set.seed(1)
coefs <- data.frame(matrix(0, 1000, 3))
names(coefs) <- c("Intercept", "beta1", "beta2")
mse <- numeric(1000)
containTrue <- data.frame(matrix(0, 1000, 3))
names(containTrue) <- c("Intercept", "beta1", "beta2")

### Repeat the following process 1000 times.
for (i in 1:1000){
  ### Take a simple random sample of size 500 from the data set.
  temp.rows <- sample(x = 1:nrow(data3), size = 500, replace = TRUE)
  temp.data <- data3[temp.rows, ]
  
  ### Treat the missing data according to the particular method.
  miss.two.value.rows <- which(apply(is.na(temp.data), 1, sum) == 2)
  if (length(miss.two.value.rows) == 0) {
    temp.data.miss.1 <- temp.data
  } else {
    temp.data.miss.1 <- temp.data[-miss.two.value.rows, ]
  }
  
  # Situation 1: only miss y
  lm.impute.y <- lm(y ~ ., data = temp.data.miss.1)
  na.y.rows <- which(is.na(temp.data.miss.1$y))
  y.preds <- unname(predict(lm.impute.y, newdata = temp.data[na.y.rows, ]))
  
  # Situation 2: only miss x2
  lm.impute.x2 <- lm(x2 ~ ., data = temp.data.miss.1)
  na.x2.rows <- which(is.na(temp.data.miss.1$x2))
  x2.preds <- unname(predict(lm.impute.x2, newdata = temp.data[na.x2.rows, ]))
  
  # Situation 3: miss both y and x2
  lm.impute.both.y <- lm(y ~ x1, data = temp.data)
  both.y.preds <- unname(predict(lm.impute.both.y, newdata = temp.data[miss.two.value.rows, ]))
  lm.impute.both.x2 <- lm(x2 ~ x1, data = temp.data)
  both.x2.preds <- unname(predict(lm.impute.both.x2, newdata = temp.data[miss.two.value.rows, ]))
  
  # Impute missing values
  # only miss y
  temp.data$y[na.y.rows] <- y.preds
  # only miss x2
  temp.data$x2[na.x2.rows] <- x2.preds
  # miss both
  temp.data$y[miss.two.value.rows] <- both.y.preds
  temp.data$x2[miss.two.value.rows] <- both.x2.preds
  
  ### Estimate and record the three regression parameters. 
  temp.lm <- lm(y ~ ., data = temp.data)
  coefs[i, ] <- coef(temp.lm)
  
  ### Use the estimated regression parameters to calculate MSE.
  temp.mse <- sum(temp.lm$residuals^2)/temp.lm$df.residual
  mse[i] <- temp.mse
  
  ### Determine the 95% confidence interval for each of the regression parameters and 
  ### record whether or not it contains the true parameter value.
  temp.contain.inter <- confint(temp.lm)[1, 1] <= 29.3 & 29.3 <= confint(temp.lm)[1, 2]
  temp.contain.beta1 <- confint(temp.lm)[2, 1] <= 5.6 & 5.6 <= confint(temp.lm)[2, 2]
  temp.contain.beta2 <- confint(temp.lm)[3, 1] <= 3.8 & 3.8 <= confint(temp.lm)[3, 2]
  temp.contain <- c(temp.contain.inter, temp.contain.beta1, temp.contain.beta2)
  containTrue[i, ] <- temp.contain
}

### Determine and report the mean and variance of the generated coefficients and the MSE. 
apply(coefs, 2, mean)
# Intercept     beta1     beta2 
# 38.997756  5.348950  3.803028 

apply(coefs, 2, var)
# Intercept       beta1       beta2 
# 42.30149838  0.07745526  0.46096999 

mean(mse)
# 382.0797

var(mse)
# 952.959

### Determine and report the coverage of the confidence intervals of the parameters.
# Interval
sum(containTrue$Intercept) / nrow(containTrue)
# 0.446

# Beta1
sum(containTrue$beta1) / nrow(containTrue)
# 0.692

# Beta2
sum(containTrue$beta2) / nrow(containTrue)
# 0.807


################################## Data set 3 (Stochastic regression imputation)
data3 <- read.csv("homework08data03.csv", header = FALSE, col.names = c("y", "x1", "x2"))
set.seed(1)
coefs <- data.frame(matrix(0, 1000, 3))
names(coefs) <- c("Intercept", "beta1", "beta2")
mse <- numeric(1000)
containTrue <- data.frame(matrix(0, 1000, 3))
names(containTrue) <- c("Intercept", "beta1", "beta2")

### Repeat the following process 1000 times.
for (i in 1:1000){
  ### Take a simple random sample of size 500 from the data set.
  temp.rows <- sample(x = 1:nrow(data3), size = 500, replace = TRUE)
  temp.data <- data3[temp.rows, ]
  
  ### Treat the missing data according to the particular method.
  miss.two.value.rows <- which(apply(is.na(temp.data), 1, sum) == 2)
  if (length(miss.two.value.rows) == 0) {
    temp.data.miss.1 <- temp.data
  } else {
    temp.data.miss.1 <- temp.data[-miss.two.value.rows, ]
  }
  
  # Situation 1: only miss y
  lm.impute.y <- lm(y ~ ., data = temp.data.miss.1)
  na.y.rows <- which(is.na(temp.data.miss.1$y))
  y.preds <- unname(predict(lm.impute.y, newdata = temp.data[na.y.rows, ]))
  err.term.y <- rnorm(length(y.preds), mean = 0, sd = sigma(lm.impute.y))
  y.preds <- y.preds + err.term.y
  
  # Situation 2: only miss x2
  lm.impute.x2 <- lm(x2 ~ ., data = temp.data.miss.1)
  na.x2.rows <- which(is.na(temp.data.miss.1$x2))
  x2.preds <- unname(predict(lm.impute.x2, newdata = temp.data[na.x2.rows, ]))
  err.term.x2 <- rnorm(length(x2.preds), mean = 0, sd = sigma(lm.impute.x2))
  x2.preds <- x2.preds + err.term.x2
  
  # Situation 3: miss both y and x2
  lm.impute.both.y <- lm(y ~ x1, data = temp.data)
  both.y.preds <- unname(predict(lm.impute.both.y, newdata = temp.data[miss.two.value.rows, ]))
  err.term.both.y <- rnorm(length(both.y.preds), mean = 0, sd = sigma(lm.impute.both.y))
  both.y.preds <- both.y.preds + err.term.both.y
  
  lm.impute.both.x2 <- lm(x2 ~ x1, data = temp.data)
  both.x2.preds <- unname(predict(lm.impute.both.x2, newdata = temp.data[miss.two.value.rows, ]))
  err.term.both.x2 <- rnorm(length(both.x2.preds), mean = 0, sd = sigma(lm.impute.both.x2))
  both.x2.preds <- both.x2.preds + err.term.both.x2
  
  # Impute missing values
  # only miss y
  temp.data$y[na.y.rows] <- y.preds
  # only miss x2
  temp.data$x2[na.x2.rows] <- x2.preds
  # miss both
  temp.data$y[miss.two.value.rows] <- both.y.preds
  temp.data$x2[miss.two.value.rows] <- both.x2.preds
  
  ### Estimate and record the three regression parameters. 
  temp.lm <- lm(y ~ ., data = temp.data)
  coefs[i, ] <- coef(temp.lm)
  
  ### Use the estimated regression parameters to calculate MSE.
  temp.mse <- sum(temp.lm$residuals^2)/temp.lm$df.residual
  mse[i] <- temp.mse
  
  ### Determine the 95% confidence interval for each of the regression parameters and 
  ### record whether or not it contains the true parameter value.
  temp.contain.inter <- confint(temp.lm)[1, 1] <= 29.3 & 29.3 <= confint(temp.lm)[1, 2]
  temp.contain.beta1 <- confint(temp.lm)[2, 1] <= 5.6 & 5.6 <= confint(temp.lm)[2, 2]
  temp.contain.beta2 <- confint(temp.lm)[3, 1] <= 3.8 & 3.8 <= confint(temp.lm)[3, 2]
  temp.contain <- c(temp.contain.inter, temp.contain.beta1, temp.contain.beta2)
  containTrue[i, ] <- temp.contain
}

### Determine and report the mean and variance of the generated coefficients and the MSE. 
apply(coefs, 2, mean)
# Intercept     beta1     beta2 
# 42.279828  5.380231  3.097474

apply(coefs, 2, var)
# Intercept       beta1       beta2 
# 42.40526621  0.08580806  0.40047107 

mean(mse)
# 479.6714

var(mse)
# 1455.438

### Determine and report the coverage of the confidence intervals of the parameters.
# Interval
sum(containTrue$Intercept) / nrow(containTrue)
# 0.322

# Beta1
sum(containTrue$beta1) / nrow(containTrue)
# 0.796

# Beta2
sum(containTrue$beta2) / nrow(containTrue)
# 0.613


################################## Data set 3 (Hot-Deck imputation)
library(hot.deck)
data3 <- read.csv("homework08data03.csv", header = FALSE, col.names = c("y", "x1", "x2"))
set.seed(1)
coefs <- data.frame(matrix(0, 1000, 3))
names(coefs) <- c("Intercept", "beta1", "beta2")
mse <- numeric(1000)
containTrue <- data.frame(matrix(0, 1000, 3))
names(containTrue) <- c("Intercept", "beta1", "beta2")

### Repeat the following process 1000 times.
for (i in 1:1000){
  ### Take a simple random sample of size 500 from the data set.
  temp.rows <- sample(x = 1:nrow(data3), size = 500, replace = TRUE)
  temp.data <- data3[temp.rows, ]
  
  ### Treat the missing data according to the particular method.
  temp.data <- hot.deck(temp.data, sdCutoff = 1.5)$data[[1]]
  
  ### Estimate and record the three regression parameters. 
  temp.lm <- lm(y ~ ., data = temp.data)
  coefs[i, ] <- coef(temp.lm)
  
  ### Use the estimated regression parameters to calculate MSE.
  temp.mse <- sum(temp.lm$residuals^2)/temp.lm$df.residual
  mse[i] <- temp.mse
  
  ### Determine the 95% confidence interval for each of the regression parameters and 
  ### record whether or not it contains the true parameter value.
  temp.contain.inter <- confint(temp.lm)[1, 1] <= 29.3 & 29.3 <= confint(temp.lm)[1, 2]
  temp.contain.beta1 <- confint(temp.lm)[2, 1] <= 5.6 & 5.6 <= confint(temp.lm)[2, 2]
  temp.contain.beta2 <- confint(temp.lm)[3, 1] <= 3.8 & 3.8 <= confint(temp.lm)[3, 2]
  temp.contain <- c(temp.contain.inter, temp.contain.beta1, temp.contain.beta2)
  containTrue[i, ] <- temp.contain
}

### Determine and report the mean and variance of the generated coefficients and the MSE. 
apply(coefs, 2, mean)
# Intercept     beta1     beta2 
# 42.776021  5.269387  2.885322

apply(coefs, 2, var)
# Intercept       beta1       beta2 
# 32.97319255  0.07184221  0.27363764

mean(mse)
# 545.203

var(mse)
# 1896.334

### Determine and report the coverage of the confidence intervals of the parameters.
# Interval
sum(containTrue$Intercept) / nrow(containTrue)
# 0.293

# Beta1
sum(containTrue$beta1) / nrow(containTrue)
# 0.743

# Beta2
sum(containTrue$beta2) / nrow(containTrue)
# 0.531

################################## Data set 3 (Similar resonse pattern imputation)
library(VIM)
data3 <- read.csv("homework08data03.csv", header = FALSE, col.names = c("y", "x1", "x2"))
set.seed(1)
coefs <- data.frame(matrix(0, 1000, 3))
names(coefs) <- c("Intercept", "beta1", "beta2")
mse <- numeric(1000)
containTrue <- data.frame(matrix(0, 1000, 3))
names(containTrue) <- c("Intercept", "beta1", "beta2")

### Repeat the following process 1000 times.
for (i in 1:1000){
  ### Take a simple random sample of size 500 from the data set.
  temp.rows <- sample(x = 1:nrow(data3), size = 500, replace = TRUE)
  temp.data <- data3[temp.rows, ]
  
  ### Treat the missing data according to the particular method.
  temp.data <- kNN(temp.data)[, 1:3]
  
  ### Estimate and record the three regression parameters. 
  temp.lm <- lm(y ~ ., data = temp.data)
  coefs[i, ] <- coef(temp.lm)
  
  ### Use the estimated regression parameters to calculate MSE.
  temp.mse <- sum(temp.lm$residuals^2)/temp.lm$df.residual
  mse[i] <- temp.mse
  
  ### Determine the 95% confidence interval for each of the regression parameters and 
  ### record whether or not it contains the true parameter value.
  temp.contain.inter <- confint(temp.lm)[1, 1] <= 29.3 & 29.3 <= confint(temp.lm)[1, 2]
  temp.contain.beta1 <- confint(temp.lm)[2, 1] <= 5.6 & 5.6 <= confint(temp.lm)[2, 2]
  temp.contain.beta2 <- confint(temp.lm)[3, 1] <= 3.8 & 3.8 <= confint(temp.lm)[3, 2]
  temp.contain <- c(temp.contain.inter, temp.contain.beta1, temp.contain.beta2)
  containTrue[i, ] <- temp.contain
}

### Determine and report the mean and variance of the generated coefficients and the MSE. 
apply(coefs, 2, mean)
# Intercept     beta1     beta2 
# 46.547545  5.185778  3.132548 

apply(coefs, 2, var)
# Intercept      beta1      beta2 
# 60.2706547  0.1057928  0.7332391 

mean(mse)
# 441.7849

var(mse)
# 1355.273

### Determine and report the coverage of the confidence intervals of the parameters.
# Interval
sum(containTrue$Intercept) / nrow(containTrue)
# 0.157

# Beta1
sum(containTrue$beta1) / nrow(containTrue)
# 0.545

# Beta2
sum(containTrue$beta2) / nrow(containTrue)
# 0.574

################################## Data set 3 (Indicator method imputation)
data3 <- read.csv("homework08data03.csv", header = FALSE, col.names = c("y", "x1", "x2"))
set.seed(1)
coefs <- data.frame(matrix(0, 1000, 3))
names(coefs) <- c("Intercept", "beta1", "beta2")
mse <- numeric(1000)
containTrue <- data.frame(matrix(0, 1000, 3))
names(containTrue) <- c("Intercept", "beta1", "beta2")

### Repeat the following process 1000 times.
for (i in 1:1000){
  ### Take a simple random sample of size 500 from the data set.
  temp.rows <- sample(x = 1:nrow(data3), size = 500, replace = TRUE)
  temp.data <- data3[temp.rows, ]
  
  ### Treat the missing data according to the particular method.
  temp.data <- complete(mice(temp.data, method = "ri", printFlag = FALSE))
  
  ### Estimate and record the three regression parameters. 
  temp.lm <- lm(y ~ ., data = temp.data)
  coefs[i, ] <- coef(temp.lm)
  
  ### Use the estimated regression parameters to calculate MSE.
  temp.mse <- sum(temp.lm$residuals^2)/temp.lm$df.residual
  mse[i] <- temp.mse
  
  ### Determine the 95% confidence interval for each of the regression parameters and 
  ### record whether or not it contains the true parameter value.
  temp.contain.inter <- confint(temp.lm)[1, 1] <= 29.3 & 29.3 <= confint(temp.lm)[1, 2]
  temp.contain.beta1 <- confint(temp.lm)[2, 1] <= 5.6 & 5.6 <= confint(temp.lm)[2, 2]
  temp.contain.beta2 <- confint(temp.lm)[3, 1] <= 3.8 & 3.8 <= confint(temp.lm)[3, 2]
  temp.contain <- c(temp.contain.inter, temp.contain.beta1, temp.contain.beta2)
  containTrue[i, ] <- temp.contain
}

### Determine and report the mean and variance of the generated coefficients and the MSE. 
apply(coefs, 2, mean)
# Intercept     beta1     beta2 
# 46.542007  5.343489  2.554881 

apply(coefs, 2, var)
# Intercept       beta1       beta2 
# 259.9566950   0.3029792   1.5997564 

mean(mse)
# 545.6962

var(mse)
# 8305.927

### Determine and report the coverage of the confidence intervals of the parameters.
# Interval
sum(containTrue$Intercept) / nrow(containTrue)
# 0.293

# Beta1
sum(containTrue$beta1) / nrow(containTrue)
# 0.577

# Beta2
sum(containTrue$beta2) / nrow(containTrue)
# 0.452



#############################################################################################################################

### Summarize the results into tables

# Data 1
df1 <- data.frame(matrix(0, 11, 8),
                  row.names = c("Intercept Mean", "Beta1 Mean", "Beta2 Mean", "Intercept sd", "Beta1 sd", "Beta2 sd",
                                "Mean MSE", "Variance MSE", "Intercept Coverage", "Beta1 Coverage", "Beta2 Coverage"))
names(df1) <- c("Listwise", "Pairwise", "Arithmetic", "Regression",
                "Stochastic", "Hot-Deck", "SRPI",
                "Indicator")
col1 <- c(29.266365, 5.684869,  3.743970, 45.0066423,  0.0949563,  0.3497948, 477.1175, 1594.341, 0.944, 0.951, 0.947)
df1$Listwise <- col1
col2 <- c(27.386435,  5.762389,  3.832479, 83.3131721,  0.2015829,  0.4940917, 480.2774, 1596.682, 0.838, 0.814, 1)
df1$Pairwise <- col2
col3 <- c(54.441162,  4.585849,  2.850499, 42.5488221,  0.0910279,  0.2831758, 501.4628, 1028.579, 0.011, 0.041, 0.548)
df1$Arithmetic <- col3
col4 <- c(24.564848,  5.723999,  4.552261, 38.89429505,  0.07725538,  0.44264291, 367.4, 837.8692, 0.72, 0.826, 0.544)
df1$Regression <- col4
col5 <- c(28.434327,  5.745493,  3.771609, 43.47943903,  0.08964982,  0.41212723, 476.4345, 1503.987, 0.872, 0.837, 0.83)
df1$Stochastic <- col5
col6 <- c(44.132533,  5.250582,  2.581519, 37.92310106,  0.09384647,  0.27674608 , 584.7838 ,2042.785, 0.262 ,0.721, 0.318)
df1$`Hot-Deck` <- col6
col7 <- c(29.554067,  5.557168 , 4.149792 ,40.33331822,  0.08647988 , 0.45888415, 419.8654, 1184.431, 0.856, 0.868, 0.737)
df1$SRPI <- col7
col8 <- c(29.708616,  5.773363 , 3.397920, 54.32023981,  0.09518933,  0.53444156 ,529.5997, 6769.824, 0.832, 0.826, 0.713)
df1$Indicator <- col8
  
# Data 2
df2 <- data.frame(matrix(0, 11, 8),
                  row.names = c("Intercept Mean", "Beta1 Mean", "Beta2 Mean", "Intercept sd", "Beta1 sd", "Beta2 sd",
                                "Mean MSE", "Variance MSE", "Intercept Coverage", "Beta1 Coverage", "Beta2 Coverage"))
names(df2) <- c("Listwise", "Pairwise", "Arithmetic", "Regression",
                "Stochastic", "Hot-Deck", "SRPI",
                "Indicator")
col1 <- c(19.237552,  6.040835,  4.129242, 73.7095234,  0.1487214,  0.3691359, 501.0084, 1619.21, 0.747, 0.745, 0.907)
df2$Listwise <- col1
col2 <- c(27.709318,  5.716147 , 3.795028,  100.0357762 ,  0.2016489  , 0.5186757 ,505.7834, 1656.731, 0.89, 0.876, 1)
df2$Pairwise <- col2
col3 <- c(60.602681 , 4.457581 , 2.875696 ,45.46573847,  0.08774017,  0.27187196,  513.4797, 1125.626, 0.002, 0.018 ,0.555)
df2$Arithmetic <- col3
col4 <- c(18.517733,  5.896234,  4.824601, 51.98888072,  0.09917874,  0.43922744 ,367.4844, 869.2693, 0.408, 0.631, 0.407)
df2$Regression <- col4
col5 <- c(22.605200,  5.922125 , 3.996748 ,57.8773846,  0.1050887,  0.4070207, 484.6769, 1589.572 ,0.66, 0.672, 0.818)
df2$Stochastic <- col5
col6 <- c(41.913570 , 5.355348 , 2.636419, 41.59429340,  0.09026352 , 0.27311093 ,558.7249, 2154.359, 0.364, 0.802 ,0.361)
df2$`Hot-Deck` <- col6
col7 <- c(29.498461,  5.467481,  4.486586 ,76.7949817 , 0.1289055 , 0.6536542, 424.7303, 1776.908, 0.711, 0.734, 0.538)
df2$SRPI <- col7
col8 <- c(39.965323 , 5.276715 , 3.547646, 390.6091260,   0.7058280 ,  0.6342975 ,555.6583, 12638.97, 0.353, 0.427, 0.746)
df2$Indicator <- col8

# Data 3
df3 <- data.frame(matrix(0, 11, 8),
                  row.names = c("Intercept Mean", "Beta1 Mean", "Beta2 Mean", "Intercept sd", "Beta1 sd", "Beta2 sd",
                                "Mean MSE", "Variance MSE", "Intercept Coverage", "Beta1 Coverage", "Beta2 Coverage"))
names(df3) <- c("Listwise", "Pairwise", "Arithmetic", "Regression",
                "Stochastic", "Hot-Deck", "SRPI",
                "Indicator")
col1 <- c(50.900566 , 5.167289 , 2.958353 ,57.5391294,  0.1146458 , 0.2836147 ,434.682, 1339.044 ,0.142, 0.692, 0.673)
df3$Listwise <- col1
col2 <- c(42.602248 , 5.733197,  2.463507, 84.5145845 , 0.1807600 , 0.4154608, 443.4421, 1357.66 ,0.501, 0.855, 0)
df3$Pairwise <- col2
col3 <- c(65.169543,  4.489469 , 2.228419 ,38.15649863 , 0.07540146 , 0.21071388 ,493.4768 ,1137.488, 0, 0.01, 0.118)
df3$Arithmetic <- col3
col4 <- c(38.997756,  5.348950,  3.803028, 42.30149838 , 0.07745526 , 0.46096999, 382.0797, 952.959, 0.446 ,0.692, 0.807)
df3$Regression <- col4
col5 <- c(42.279828 , 5.380231,  3.097474, 42.40526621 , 0.08580806 , 0.40047107, 479.6714 ,1455.438 ,0.322 ,0.796 ,0.613)
df3$Stochastic <- col5
col6 <- c(42.776021,  5.269387 , 2.885322, 32.97319255,  0.07184221 , 0.27363764, 545.203, 1896.334, 0.293, 0.743, 0.531)
df3$`Hot-Deck` <- col6
col7 <- c(46.547545,  5.185778 , 3.132548, 60.2706547 , 0.1057928,  0.7332391, 441.7849, 1355.273, 0.157, 0.545 ,0.574)
df3$SRPI <- col7
col8 <- c(46.542007 , 5.343489 , 2.554881 ,259.9566950 ,  0.3029792  , 1.5997564, 545.6962, 8305.927 ,0.293, 0.577 ,0.452)
df3$Indicator <- col8


# y = 29.3 + 5.6*x1 + 3.8*x2 + epsilon and the standard deviation of epsilon is 21.

# Dataset 1 (MCAR)
#                        Listwise     Pairwise   Arithmetic   Regression   Stochastic     Hot-Deck         SRPI    Indicator
# Intercept Mean       29.2663650   27.3864350   54.4411620  24.56484800 2.843433e+01 4.413253e+01 2.955407e+01 2.970862e+01
# Beta1 Mean            5.6848690    5.7623890    4.5858490   5.72399900 5.745493e+00 5.250582e+00 5.557168e+00 5.773363e+00
# Beta2 Mean            3.7439700    3.8324790    2.8504990   4.55226100 3.771609e+00 2.581519e+00 4.149792e+00 3.397920e+00
# Intercept sd         45.0066423   83.3131721   42.5488221  38.89429505 4.347944e+01 3.792310e+01 4.033332e+01 5.432024e+01
# Beta1 sd              0.0949563    0.2015829    0.0910279   0.07725538 8.964982e-02 9.384647e-02 8.647988e-02 9.518933e-02
# Beta2 sd              0.3497948    0.4940917    0.2831758   0.44264291 4.121272e-01 2.767461e-01 4.588841e-01 5.344416e-01
# Mean MSE            477.1175000  480.2774000  501.4628000 367.40000000 4.764345e+02 5.847838e+02 4.198654e+02 5.295997e+02
# Variance MSE       1594.3410000 1596.6820000 1028.5790000 837.86920000 1.503987e+03 2.042785e+03 1.184431e+03 6.769824e+03
# Intercept Coverage    0.9440000    0.8380000    0.0110000   0.72000000 8.720000e-01 2.620000e-01 8.560000e-01 8.320000e-01
# Beta1 Coverage        0.9510000    0.8140000    0.0410000   0.82600000 8.370000e-01 7.210000e-01 8.680000e-01 8.260000e-01
# Beta2 Coverage        0.9470000    1.0000000    0.5480000   0.54400000 8.300000e-01 3.180000e-01 7.370000e-01 7.130000e-01

# Dataset 2 (MAR)
#                        Listwise     Pairwise   Arithmetic   Regression   Stochastic     Hot-Deck         SRPI    Indicator
# Intercept Mean       19.2375520   27.7093180 6.060268e+01  18.51773300   22.6052000 4.191357e+01   29.4984610 3.996532e+01
# Beta1 Mean            6.0408350    5.7161470 4.457581e+00   5.89623400    5.9221250 5.355348e+00    5.4674810 5.276715e+00
# Beta2 Mean            4.1292420    3.7950280 2.875696e+00   4.82460100    3.9967480 2.636419e+00    4.4865860 3.547646e+00
# Intercept sd         73.7095234  100.0357762 4.546574e+01  51.98888072   57.8773846 4.159429e+01   76.7949817 3.906091e+02
# Beta1 sd              0.1487214    0.2016489 8.774017e-02   0.09917874    0.1050887 9.026352e-02    0.1289055 7.058280e-01
# Beta2 sd              0.3691359    0.5186757 2.718720e-01   0.43922744    0.4070207 2.731109e-01    0.6536542 6.342975e-01
# Mean MSE            501.0084000  505.7834000 5.134797e+02 367.48440000  484.6769000 5.587249e+02  424.7303000 5.556583e+02
# Variance MSE       1619.2100000 1656.7310000 1.125626e+03 869.26930000 1589.5720000 2.154359e+03 1776.9080000 1.263897e+04
# Intercept Coverage    0.7470000    0.8900000 2.000000e-03   0.40800000    0.6600000 3.640000e-01    0.7110000 3.530000e-01
# Beta1 Coverage        0.7450000    0.8760000 1.800000e-02   0.63100000    0.6720000 8.020000e-01    0.7340000 4.270000e-01
# Beta2 Coverage        0.9070000    1.0000000 5.550000e-01   0.40700000    0.8180000 3.610000e-01    0.5380000 7.460000e-01

# Dataset 3 (MNAR)
#                        Listwise     Pairwise   Arithmetic   Regression   Stochastic     Hot-Deck         SRPI    Indicator
# Intercept Mean       50.9005660   42.6022480 6.516954e+01  38.99775600 4.227983e+01 4.277602e+01   46.5475450   46.5420070
# Beta1 Mean            5.1672890    5.7331970 4.489469e+00   5.34895000 5.380231e+00 5.269387e+00    5.1857780    5.3434890
# Beta2 Mean            2.9583530    2.4635070 2.228419e+00   3.80302800 3.097474e+00 2.885322e+00    3.1325480    2.5548810
# Intercept sd         57.5391294   84.5145845 3.815650e+01  42.30149838 4.240527e+01 3.297319e+01   60.2706547  259.9566950
# Beta1 sd              0.1146458    0.1807600 7.540146e-02   0.07745526 8.580806e-02 7.184221e-02    0.1057928    0.3029792
# Beta2 sd              0.2836147    0.4154608 2.107139e-01   0.46096999 4.004711e-01 2.736376e-01    0.7332391    1.5997564
# Mean MSE            434.6820000  443.4421000 4.934768e+02 382.07970000 4.796714e+02 5.452030e+02  441.7849000  545.6962000
# Variance MSE       1339.0440000 1357.6600000 1.137488e+03 952.95900000 1.455438e+03 1.896334e+03 1355.2730000 8305.9270000
# Intercept Coverage    0.1420000    0.5010000 0.000000e+00   0.44600000 3.220000e-01 2.930000e-01    0.1570000    0.2930000
# Beta1 Coverage        0.6920000    0.8550000 1.000000e-02   0.69200000 7.960000e-01 7.430000e-01    0.5450000    0.5770000
# Beta2 Coverage        0.6730000    0.0000000 1.180000e-01   0.80700000 6.130000e-01 5.310000e-01    0.5740000    0.4520000

# From the results we can see that List-wise deletion seems to perform the best when the data is MCAR. Pairwise outperforms
# list-wise when data is MAR. Arithmetic mean imputation is always a bad imputation option. Stochastic imputation seems to 
# perform better than regression imputation when the data is not MNAR. There is not a huge difference between the performances
# of Hot-Deck method on three different datasets. SPRI performs better when data is MCAR and worse when data is MNAR. Indicator 
# imputation method performs better when data is MCAR and worse when data is MNAR.
