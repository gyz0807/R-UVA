library(boot)

train <- read.csv("teamassign06train.csv")
test <- read.csv("teamassign06test.csv")

# check if NA exists
apply(is.na(train), 2, sum)
# school        sex        age    address    famsize    Pstatus       Medu       Fedu       Mjob       Fjob 
# 0          0          0          0          0          0          0          0          0          0 
# reason   guardian traveltime  studytime   failures  schoolsup     famsup       paid activities    nursery 
# 0          0          0          0          0          0          0          0          0          0 
# higher   internet   romantic     famrel   freetime      goout       Dalc       Walc     health   absences 
# 0          0          0          0          0          0          0          0          0          0 
# Grade 
# 0 
# The results indicate that NA does not exist

# Backward CV
nms <- paste("x", 1:30, sep = "")
training <- train
names(training) <- c(nms, "y")

full.preds <- 30
cv.err <- matrix(NA, nrow = full.preds, ncol = full.preds)
for (num.predictors in full.preds:1) {
  assign(paste("cv.err", num.predictors, sep = ""), rep(0, num.predictors))
  
  if (num.predictors == full.preds) {
    assign(paste("model", num.predictors, sep = ""), combn(1:num.predictors, num.predictors))
    temp.model <- as.formula(paste("y ~ ", paste(paste("x", get(paste("model", num.predictors, sep = ""))[,1], sep = ""), collapse = " + ")))
    temp.glm <- glm(temp.model, data = training)
    temp.cv.err <- cv.glm(training, temp.glm)
    cv.err[num.predictors, 1] <- temp.cv.err$delta[1]
    assign("temp.min", which.min(cv.err[num.predictors, ]))
  } else {
    assign(paste("model", num.predictors, sep = ""), combn(get(paste("model", num.predictors+1, sep = ""))[, temp.min], num.predictors))
    
    for (j in 1:ncol(get(paste("model", num.predictors, sep = "")))) {
      temp.model <- as.formula(paste("y ~ ", paste(paste("x", get(paste("model", num.predictors, sep = ""))[,j], sep = ""), collapse = " + ")))
      temp.glm <- glm(temp.model, data = training)
      temp.cv.err <- cv.glm(training, temp.glm)
      cv.err[num.predictors, j] <- temp.cv.err$delta[1]
      assign("temp.min", which.min(cv.err[num.predictors, ]))
    }
  }
}

cv.err

prcomp(train[, -31])
glm(y ~ ., data = training)







