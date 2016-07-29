# STAT 6430
# R Homework 2
# Yizhe Ge (Jake)
# yg2kj

# ** Please set the working directory to your source file location.

########################################### Problem 1
library(readr)
library(dplyr)

ff <- read_csv("fastfood.csv")

# check if there is any NA value
apply(is.na(ff), 2, sum)

confint.all <- mean(ff$secs) + c(-1, 1)*2*sd(ff$secs)/sqrt(nrow(ff))
# [1] 215.4346 217.2197

########################################### Problem 2
100*sum(ff$secs > 4*60) / nrow(ff)
# [1] 32.22818

########################################### Problem 3
quantile(ff$secs, probs = c(0.05, 0.95))
# 5% 95% 
# 52 519

########################################### Problem 4
library(ggplot2)
ff2 <- ff %>% 
        group_by(dayofweek) %>% 
        summarize(meanW = mean(secs)) %>% 
        mutate(dayofweek = factor(dayofweek, c("Mon", "Tues", "Wed", "Thur", "Fri"))) %>% 
        arrange(dayofweek)
# A tibble: 5 x 2
#      dayofweek    meanW
#       <fctr>      <dbl>
# 1       Mon     216.7747
# 2      Tues     215.7423
# 3       Wed     216.2824
# 4      Thur     216.6051
# 5       Fri     216.2349

ff$dayofweek <- factor(ff$dayofweek, c("Mon", "Tues", "Wed", "Thur", "Fri"))
p <- ggplot(ff, aes(x = dayofweek, y = secs)) +
        geom_boxplot(aes(fill=dayofweek))
p

########################################### Problem 5
ff1 <- ff %>% 
        group_by(storenum) %>% 
        summarize(meanT = mean(secs), sdT = sd(secs), nT = n()) %>% 
        mutate(lower95 = meanT - 2*sdT/sqrt(nT), upper95 = meanT + 2*sdT/sqrt(nT)) %>% 
        select(storenum, meanT, lower95, upper95)
ff1

# 2 standard deviations below
lower2sd <- mean(ff1$meanT) - 2*sd(ff1$meanT)
lower2sd.stores <- ff1$storenum[ff1$meanT < lower2sd]
lower2sd.stores.sorted <- sort(lower2sd.stores)
lower2sd.stores.sorted
# [1]  27  43  53 122 201 243 312 500 511 514 550 570 651 699 722 852 859

# 2 standard deviations above
upper2sd <- mean(ff1$meanT) + 2*sd(ff1$meanT)
upper2sd.stores <- ff1$storenum[ff1$meanT > upper2sd]
upper2sd.stores.sorted <- sort(upper2sd.stores)
upper2sd.stores.sorted
# [1]  30  47  59 128 149 154 155 231 233 281 318 387 392 402 422 452 474 528 614 621 657 718 723 725 726
# [26] 887

########################################### Problem 6
# 10 fastest stores
stores.fastest <- head(sort(ff1$meanT), 10)
stores.slowest <- tail(sort(ff1$meanT), 10)

fastest.confint <- mean(stores.fastest) + c(-1, 1)*2*sd(stores.fastest)/sqrt(length(stores.fastest))
slowest.confint <- mean(stores.slowest) + c(-1, 1)*2*sd(stores.slowest)/sqrt(length(stores.slowest))
fastest.confint
# [1] 179.2205 184.9704
slowest.confint
# [1] 249.6207 255.7751

########################################### Problem 7
ff.week.high <- ff %>% 
        group_by(dayofweek, storenum) %>% 
        summarize(meanT = mean(secs)) %>% 
        arrange(meanT) %>% 
        slice(1:10)
high.list <- as.data.frame.table(table(ff.week.high$storenum))
as.numeric(as.character(high.list[high.list$Freq > 1, "Var1"]))
# [1] 225

ff.week.low <- ff %>% 
        group_by(dayofweek, storenum) %>% 
        summarize(meanT = mean(secs)) %>% 
        arrange(desc(meanT)) %>% 
        slice(1:10)
low.list <- as.data.frame.table(table(ff.week.low$storenum))
as.numeric(as.character(low.list[low.list$Freq > 1, "Var1"]))
# [1] 149

########################################### Problem 8
stores.fastest.median <- ff %>% 
        group_by(storenum) %>% 
        summarize(medianT = median(secs)) %>% 
        arrange(medianT) %>% 
        slice(1:10)

stores.slowest.median <- ff %>% 
        group_by(storenum) %>% 
        summarize(medianT = median(secs)) %>% 
        arrange(desc(medianT)) %>% 
        slice(1:10)

fastest.median <- stores.fastest.median$storenum
slowest.median <- stores.slowest.median$storenum

fastest.mean <- head(ff1[order(ff1$meanT),], 10)$storenum
slowest.mean <- head(ff1[order(ff1$meanT, decreasing = TRUE),], 10)$storenum

# identify the stores that are "highest" for both the mean and median
fastest.median[fastest.median %in% fastest.mean]
# [1] 243  27 122

# identify the stores that are "lowest" for both the mean and median
slowest.median[slowest.median %in% slowest.mean]
# [1] 657 149










