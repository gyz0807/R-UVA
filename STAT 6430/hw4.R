# STAT 6430
# R Homework 4
# Yizhe Ge (Jake)
# yg2kj

# ** Please set the working directory to your source file location.

########################################### Problem 1
library(readr)
library(stringr)
library(dplyr)
filenames <- list.files(pattern = ".csv")
dfnames <- gsub(".csv", "", filenames)

df <- as.data.frame(matrix(nrow = 0, ncol = 6))
names(df) <- c("Date", "Open", "High", "Low", "Close", "Volume")

for (i in 1:length(filenames)){
        # read in all .csv files
        assign(dfnames[i], read_csv(filenames[i]))
        
        # filter all data frames based on the criteria
        temp <-  get(dfnames[i]) %>% 
                filter(Date %in% as.Date("2004-01-02"):as.Date("2012-12-31")) %>% 
                select(-7)
        assign(dfnames[i], temp)
        rm("temp")
}

for (i in 1:length(dfnames)){
        # combine all small files into a large file
        df <- rbind(df, get(dfnames[i]))
}

# Calculate the MSDS Index
MSDSIndex <- df %>% 
        group_by(Date) %>% 
        mutate(Open=Open*Volume, High=High*Volume, Low=Low*Volume, Close=Close*Volume) %>% 
        summarize(Open=sum(as.numeric(Open)), High=sum(as.numeric(High)), Low=sum(as.numeric(Low)), 
                  Close=sum(as.numeric(Close)), Volume=sum(as.numeric(Volume))) %>% 
        mutate(Open=Open/Volume, High=High/Volume, Low=Low/Volume, Close=Close/Volume) %>% 
        arrange(desc(Date))

dates <- as.Date(as.Date("2004-01-02"):as.Date("2012-12-31"), origin="1970-01-01")
df.date <- data.frame(Date = dates)
MSDSIndex.allDates <- left_join(df.date, MSDSIndex, by="Date") %>% 
        arrange(desc(Date))

names(MSDSIndex)[6] <- "Total Volume"
names(MSDSIndex.allDates)[6] <- "Total Volume"

# write out the file
write_csv(MSDSIndex, "yg2kj-hw4-p1-msdsindex.csv")

head(MSDSIndex)
# # A tibble: 6 x 6
#      Date      Open     High     Low     Close     Volume
#     <date>     <dbl>    <dbl>    <dbl>    <dbl>     <dbl>
# 1 2012-12-31 35.59642 36.44086 35.45659 36.33949 1522331000
# 2 2012-12-28 35.60213 35.93396 35.35446 35.52456 1125928800
# 3 2012-12-27 34.71340 34.91773 34.10593 34.62919 1337505700
# 4 2012-12-26 35.10164 35.40223 34.69651 34.95781 1088246500
# 5 2012-12-24 35.58212 35.92377 35.35031 35.70256  594960500
# 6 2012-12-21 39.75805 40.15186 39.36981 39.85248 2437835600


########################################### Problem 2
close.dates <- data.frame(Date=MSDSIndex.allDates$Date[apply(is.na(MSDSIndex.allDates), 1, sum)==5]) %>% 
        arrange(desc(Date))

# write out the file
write_csv(close.dates, "yg2kj-hw4-p2-closed.csv")

# number of days the market was closed
nrow(close.dates)
# [1] 1022

# first 20 closed dates
head(close.dates, 20)
#      Date
# 1  2012-12-30
# 2  2012-12-29
# 3  2012-12-25
# 4  2012-12-23
# 5  2012-12-22
# 6  2012-12-16
# 7  2012-12-15
# 8  2012-12-09
# 9  2012-12-08
# 10 2012-12-02
# 11 2012-12-01
# 12 2012-11-25
# 13 2012-11-24
# 14 2012-11-22
# 15 2012-11-18
# 16 2012-11-17
# 17 2012-11-11
# 18 2012-11-10
# 19 2012-11-04
# 20 2012-11-03

########################################### Problem 3
for (i in 1:length(dfnames)){
        
        # get all missing dates
        stock <- get(dfnames[i])
        if(nrow(stock)==0){next}
        counted.dates <- dates[dates >= range(stock$Date)[1] & dates <= range(stock$Date)[2]]
        close.and.missing <- counted.dates[!counted.dates %in% stock$Date]
        missing <- close.and.missing[!close.and.missing %in% close.dates$Date]
        if(length(missing)==0){
                assign(paste(dfnames[i],".imputed", sep=""), stock)
                next
        }
        
        # create a data frame with all missing dates
        temp.df <- as.data.frame(matrix(nrow = length(missing), ncol = 6))
        names(temp.df) <- c("Date", "Open", "High", "Low", "Close", "Volume")
        temp.df$Date <- missing
        
        # combine the data frame with missing dates with the stock datasets
        temp.df1 <- rbind(stock, temp.df) %>% 
                arrange(desc(Date))
        
        missing.rows <- which(apply(is.na(temp.df1), 1, sum)==5)
        not.missing.rows <- which(apply(is.na(temp.df1), 1, sum)==0)
        
        # since there might be consecutive missing values, we want to find t1 and t3 that 
        # do not have missing values for calculation
        for (j in 1:length(missing.rows)){
                nrow.t3 <- max(not.missing.rows[not.missing.rows < missing.rows[j]])
                nrow.t2 <- missing.rows[j]
                nrow.t1 <- min(not.missing.rows[not.missing.rows > missing.rows[j]])
                
                p3 <- temp.df1[nrow.t3, 2:6]
                p1 <- temp.df1[nrow.t1, 2:6]
                t3 <- temp.df1[nrow.t3, 1]$Date
                t2 <- temp.df1[nrow.t2, 1]$Date
                t1 <- temp.df1[nrow.t1, 1]$Date
                
                t3.minus.t2 <- as.numeric(t3-t2)
                t2.minus.t1 <- as.numeric(t2-t1)
                t3.minus.t1 <- as.numeric(t3-t1)
                
                temp.df1[nrow.t2, 2:6] <- (t3.minus.t2*p1 + t2.minus.t1*p3)/t3.minus.t1
        }
        assign(paste(dfnames[i],".imputed", sep=""), temp.df1)
}

dfnames1 <- paste(dfnames, ".imputed", sep="")

df1 <- as.data.frame(matrix(nrow = 0, ncol = 6))
names(df1) <- c("Date", "Open", "High", "Low", "Close", "Volume")
for (i in 1:length(dfnames1)){
        # combine a(ll small files into a large file
        if(!dfnames1[i] %in% ls()){next}
        df1 <- rbind(df1, get(dfnames1[i]))
}

# Calculate the MSDS Index
MSDSIndex1 <- df1 %>% 
        group_by(Date) %>% 
        mutate(Open=Open*Volume, High=High*Volume, Low=Low*Volume, Close=Close*Volume) %>% 
        summarize(Open=sum(as.numeric(Open)), High=sum(as.numeric(High)), Low=sum(as.numeric(Low)), 
                  Close=sum(as.numeric(Close)), Volume=sum(as.numeric(Volume))) %>% 
        mutate(Open=Open/Volume, High=High/Volume, Low=Low/Volume, Close=Close/Volume) %>% 
        arrange(desc(Date))

# dates <- as.Date(as.Date("2004-01-02"):as.Date("2012-12-31"), origin="1970-01-01")
# df.date <- data.frame(Date = dates)
# MSDSIndex1 <- left_join(df.date, MSDSIndex1, by="Date") %>% 
#         arrange(desc(Date))

names(MSDSIndex1)[6] <- "Total Volume"

write_csv(MSDSIndex1, "yg2kj-hw4-p3-msdsindex-imputed.csv")










