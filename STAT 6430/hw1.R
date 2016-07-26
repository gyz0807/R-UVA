# STAT 6430
# R Homework 1
# Yizhe Ge (Jake)
# yg2kj

# ** Please set the working directory to your source file location.

########################################### Problem 1
library(readr)
samplegrades <- read_csv("samplegrades.csv")

# check if there is any na value
apply(is.na(samplegrades), 2, sum)

# students have max homework scores
maxhw <- max(samplegrades$`Homework [200.0]`)
cavg.maxhw <- subset(samplegrades, `Homework [200.0]`==maxhw)
cavg.maxhw <- cavg.maxhw$`Course Average [100.0]`
confint.maxhw <- mean(cavg.maxhw) + c(-1, 1)*(2*sd(cavg.maxhw)/sqrt(length(cavg.maxhw)))

# students have bottom 10% homework scores
bottom.ten <- quantile(samplegrades$`Homework [200.0]`, probs = 0.1)
cavg.bottom.ten <- samplegrades[samplegrades$`Homework [200.0]` <= bottom.ten,]
cavg.bottom.ten <- cavg.bottom.ten$`Course Average [100.0]`
confint.bottom.ten <- mean(cavg.bottom.ten) + c(-1, 1)*(2*sd(cavg.bottom.ten)/sqrt(length(cavg.bottom.ten)))

# put two sets into a data.frame to compare them
comparison <- data.frame(MaxHomework = confint.maxhw, BottomTenPercent = confint.bottom.ten)
comparison$PercentDifference <- 100*(comparison$MaxHomework - comparison$BottomTenPercent)/comparison$BottomTenPercent
comparison$PercentDifference <- paste(as.character(round(comparison$PercentDifference, 2)), "%", sep = "")
row.names(comparison) <- c("lowerBound", "upperBound")
comparison

#             MaxHomework    BottomTenPercent    PercentDifference
# lowerBound    85.95184         58.59651            46.68%
# upperBound    87.97987         67.52072             30.3%

# From the output, we can see that students who score well on homework do 30%-40% better on course average than those
# do now well on homework.

########################################### Problem 2
# students who have midterm and final exam scores within 2% with each other
midterm <- samplegrades$`Midterm [100.0]`
final <- samplegrades$`Final Exam [100.0]`
cavg.2score <- samplegrades[abs(midterm - final) <= 2,]
cavg.2score <- cavg.2score$`Course Average [100.0]`
cavg.2score <- cavg.2score[!is.na(cavg.2score)]
confint.2score <- mean(cavg.2score) + c(-1, 1)*(2*sd(cavg.2score)/sqrt(length(cavg.2score)))

# students who have midterm and final exam scores within 10% with each other
cavg.10score <- samplegrades[abs(midterm - final)>=10,]
cavg.10score <- cavg.10score$`Course Average [100.0]`
cavg.10score <- cavg.10score[!is.na(cavg.10score)]
confint.10score <- mean(cavg.10score) + c(-1, 1)*(2*sd(cavg.10score)/sqrt(length(cavg.10score)))

comparison <- data.frame(twoScore = confint.2score, tenScore = confint.10score)
comparison$PercentDifference <- 100*(comparison$twoScore - comparison$tenScore)/comparison$twoScore
comparison$PercentDifference <- paste(round(comparison$PercentDifference, 2), "%", sep = "")
row.names(comparison) <- c("lowerBound", "upperBound")
comparison

#            twoScore tenScore       PercentDifference
# lowerBound 79.17462 77.16021             2.54%
# upperBound 83.79727 80.23843             4.25%


########################################### Problem 3
################ (a)
library(dplyr)
samplegrades <- tbl_df(samplegrades)
samplegrades.newCA <- samplegrades %>% 
        mutate(newCourseAverage = `Course Average [100.0]`*0.95+5)

################ (b)
lettergrade <- function(cavg){
        lettergrades = character()
        for (i in 1:length(cavg)){
                if (cavg[i] >= 90 & cavg[i] <= 100){
                        lettergrades[i] <- "A"
                }else if (cavg[i] >= 80 & cavg[i] < 90){
                        lettergrades[i] <- "B"
                }else if (cavg[i] >= 70 & cavg[i] < 80){
                        lettergrades[i] <- "C"
                }else if (cavg[i] >= 60 & cavg[i] < 70){
                        lettergrades[i] <- "D"
                }else{
                        lettergrades[i] <- "F"
                }
        }
        return(lettergrades)
}

samplegrades.letter <- samplegrades.newCA %>% 
        mutate(letterGrade = lettergrade(newCourseAverage))

################ (c)
table(samplegrades.letter$letterGrade)
# A   B   C   D   F 
# 89 278 142  38  19 

################ (d)
samplegrades.oldletter <- samplegrades.letter %>% 
        mutate(oldLetter = lettergrade(`Course Average [100.0]`))
sum(samplegrades.oldletter$oldLetter != samplegrades.oldletter$letterGrade)
# there are 57 students






