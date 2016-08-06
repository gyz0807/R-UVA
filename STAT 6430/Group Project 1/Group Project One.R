################################# Parsing Data
library(readr)
library(stringr)
raw <- read_lines("pizza_requests.txt")

# get variable name from the front of each line
varsLong <- character(length(raw))
for (i in 1:length(raw)) {
        line <- str_trim(raw[i])
        line <- unlist(strsplit(line, ": "))
        if (grepl('^\\"', line[1]) & length(line) > 1 ) { #ignore %%%% and subreddits
                varsLong[i] <- gsub('\\"', "", line[1])
        }
}

vars <- unique(varsLong) # get the vector variable names
vars <- vars[!vars==""]
df <- data.frame(matrix(0, ncol = length(vars), nrow = 0))
names(df) <- vars

count <- 1
for (i in 1:length(raw)) {
        line <- str_trim(raw[i]) #trim off leading or trailing whitespace
        if (grepl('^\\"', line)) { #if line starts with \" (i.e. is not %%%%%%)
                #split on first ": "
                line <- sub(": ", "@@@@@@@@", line)
                line <- unlist(strsplit(line, "@@@@@@@@"))
                if (length(line) == 1) {
                        df$requester_subreddits_at_request[count] <- paste(df$requester_subreddits_at_request[count], 
                                                                           gsub('\\"', "", line))
                } else {
                        #seperate var name and content and remove " and trailing comma
                        var <- gsub('\\"', "", line[1])
                        content <- gsub('\\"', "", line[2])
                        content <- gsub(',$', '', content)
                        #put content in data frame
                        df[count, var] <- content
                }
                #if it's a %%% delimiter, increment the row count instead
        } else if (grepl('^%%%%', line)) {
                count <- count + 0.5
        }
}

df$requester_subreddits_at_request <- gsub('\\{ ', '', df$requester_subreddits_at_request)

################################# Looking at Word Frequencies
library(gridExtra)
library(stylo)

#load the data
df <- readRDS("pizzaDataDF")

# subset pizza from no pizza
dfT <- df[df$requester_received_pizza=="true", ]
dfF <- df[df$requester_received_pizza=="false", ]
#subset only character variables
dfTw <- dfT[, c("request_text", "request_title")]
dfFw <- dfF[, c("request_text", "request_title")]

# tokenize into word vectors
textT <- txt.to.words(dfTw[,1], preserve.case = F)
titleT <- txt.to.words(dfTw[,2], preserve.case = F)
textF <- txt.to.words(dfFw[,1], preserve.case = F)
titleF <- txt.to.words(dfFw[,2], preserve.case = F)

# make word frequency tables
textTDF <- as.data.frame(table(textT), stringsAsFactors = F) 
textTDF <- textTDF[order(textTDF$Freq, decreasing = T),]
titleTDF <- as.data.frame(table(titleT), stringsAsFactors = F) 
titleTDF <- titleTDF[order(titleTDF$Freq, decreasing = T),]

textFDF <- as.data.frame(table(textF), stringsAsFactors = F) 
textFDF <- textFDF[order(textFDF$Freq, decreasing = T),]
titleFDF <- as.data.frame(table(titleF), stringsAsFactors = F) 
titleFDF <- titleFDF[order(titleFDF$Freq, decreasing = T),]

# Find the words that are unique to people that got pizza
inTrue <- function(num, vecTrue, vecFalse) { # vecs need to be sorted by Freq
        cutTrue <- vecTrue[1:num]
        cutFalse <- vecFalse[1:num]
        cutTrue[!cutTrue %in% cutFalse]
}

# inTrue(10, textTDF[,1], textFDF[,1])
# inTrue(10, titleTDF[,1], titleFDF[,1])

# which are in true, but not in False Top 100
inTrueF100 <- function(num, vecTrue, vecFalse) { # vecs need to be sorted by Freq
        cutTrue <- vecTrue[1:num]
        cutFalse <- vecFalse[1:100]
        cutTrue[!cutTrue %in% cutFalse]
}

# inTrueF100(200, textTDF[,1], textFDF[,1])
# inTrueF100(200, titleTDF[,1], titleFDF[,1])

#####
winners <- as.data.frame(matrix(NA, ncol=5, nrow=4))
names(winners) <- c("threshold", "text", "title", "textF100", "titleF100")

thresh <- c(10, 50, 100, 200)
row <- 1
for (i in thresh) {
        winners[row, 1] <- i
        winners[row, 2] <- paste(inTrue(i, textTDF[,1], textFDF[,1]), collapse = ", ")
        winners[row, 3] <- paste(inTrue(i, titleTDF[,1], titleFDF[,1]), collapse = ", ")
        winners[row, 4] <- paste(inTrueF100(i, textTDF[,1], textFDF[,1]), collapse = ", ")
        winners[row, 5] <- paste(inTrueF100(i, titleTDF[,1], titleFDF[,1]), collapse = ", ")
        row <- row + 1
}

pickDF <- function(threshold, set) {
        words <- winners[winners$threshold==threshold, set]
        words <- unlist(strsplit(words, ", "))
        #words
        titleTDF[titleTDF$titleT %in% words, ]
}

pickDF <- function(threshold, set) {
        words <- winners[winners$threshold==threshold, set]
        words <- unlist(strsplit(words, ", "))
        #words
        if (substr(set,1,3)=="tit") {
                titleTDF[titleTDF$titleT %in% words, ]
        } else {
                textTDF[textTDF$textT %in% words, ]
        }
}

resultsTitle <- pickDF(100, "title")
resultsText <- pickDF(200, "text")

row.names(resultsTitle) <- NULL
names(resultsTitle)[1] <- "words in title"
row.names(resultsText) <- NULL
names(resultsText)[1] <- "words in text"

library(grid)
mytheme <- gridExtra::ttheme_default(
        core = list(fg_params=list(cex = 0.6)),
        colhead = list(fg_params=list(cex = 0.6)),
        rowhead = list(fg_params=list(cex = 0.6)))
p1 <- tableGrob(resultsText, theme = mytheme)
p2 <- tableGrob(resultsTitle, theme = mytheme)
grid.draw(arrangeGrob(p1, p2, ncol=2))

################################# Warm-up Analysis
#####Data Analysis#####
library(readr)
library(dplyr)
library(ggplot2)
library(formattable)

df$requester_upvotes_minus_downvotes_at_request <- as.numeric(df$requester_upvotes_minus_downvotes_at_request)
df$requester_upvotes_plus_downvotes_at_request <- as.numeric(df$requester_upvotes_plus_downvotes_at_request)
df$requester_upvotes_minus_downvotes_at_retrieval <- as.numeric(df$requester_upvotes_minus_downvotes_at_retrieval)
df$requester_upvotes_plus_downvotes_at_retrieval <- as.numeric(df$requester_upvotes_plus_downvotes_at_retrieval)
df$request_number_of_comments_at_retrieval <- as.numeric(df$request_number_of_comments_at_retrieval)
df$number_of_downvotes_of_request_at_retrieval <- as.numeric(df$number_of_downvotes_of_request_at_retrieval)
df$number_of_upvotes_of_request_at_retrieval <- as.numeric(df$number_of_upvotes_of_request_at_retrieval)
by_result = group_by(df,requester_received_pizza)
avg<-summarise(by_result,avg1 = mean(requester_upvotes_plus_downvotes_at_request),
             avg2 = mean(requester_upvotes_minus_downvotes_at_request),
             avg3 = mean(requester_upvotes_plus_downvotes_at_retrieval),
             avg4 = mean(requester_upvotes_minus_downvotes_at_retrieval),
             avg5 = mean(number_of_upvotes_of_request_at_retrieval),
             avg6 = mean(number_of_downvotes_of_request_at_retrieval),
             avg7 = mean(request_number_of_comments_at_retrieval))
avg
colours <- c("blue","orange")
par(mfrow=c(2,1))
barplot(as.matrix(avg[,6:8]), main="Mean Difference on Votes and comments", ylab = "Numbers of Votes/comments for the request",
        names.arg = c("Number of upvotes of request at retrieval",
                      "Number of downvotes of request at retrieval",
                      "Request number of comments at retrieval"),beside=TRUE, col=colours)
legend("topleft",c("No Pizza","Pizza"),fill = colours, bty = "n")

colours <- c("blue","orange")
barplot(as.matrix(avg[,2:5]), main="Mean Difference on Votes", ylab = "Numbers of Votes",
        names.arg = c("Upvotes plus Downvotes at request",
                      "Upvotes minus Downvotes at request",
                      "Upvotes plus Downvotes at retrieval",
                      "Upvotes minus Downvotes at retrieval"),beside=TRUE, col=colours)
legend("topleft",c("No Pizza","Pizza"),fill = colours, bty = "n")

#check correlation between requester activity in Reddit and getting pizza
df$requester_number_of_comments_at_request <- as.numeric(df$requester_number_of_comments_at_request)
df$requester_number_of_comments_at_retrieval <- as.numeric(df$requester_number_of_comments_at_retrieval)
df$requester_number_of_comments_in_raop_at_request <- as.numeric(df$requester_number_of_comments_in_raop_at_request)
df$requester_number_of_comments_in_raop_at_retrieval <- as.numeric(df$requester_number_of_comments_in_raop_at_retrieval)
df$requester_number_of_posts_at_request <- as.numeric(df$requester_number_of_posts_at_request)
df$requester_number_of_posts_at_retrieval <- as.numeric(df$requester_number_of_posts_at_retrieval)
df$requester_number_of_posts_on_raop_at_request <- as.numeric(df$requester_number_of_posts_on_raop_at_request)
df$requester_number_of_posts_on_raop_at_retrieval <- as.numeric(df$requester_number_of_posts_on_raop_at_retrieval)
by_result = group_by(df,requester_received_pizza)
comments <-summarise(by_result,c1 = mean(requester_number_of_comments_at_request),
                               c2 = mean(requester_number_of_comments_at_retrieval),
                               c3 = mean(requester_number_of_comments_in_raop_at_request),
                               c4 = mean(requester_number_of_comments_in_raop_at_retrieval),
                               c5 = mean(requester_number_of_posts_at_request),
                               c6 = mean(requester_number_of_posts_at_retrieval),
                               c7 = mean(requester_number_of_posts_on_raop_at_request),
                               c8 = mean(requester_number_of_posts_on_raop_at_retrieval))
comments
colours <- c("blue","orange")
par(mfrow=c(2,2))
barplot(as.matrix(comments[,2:3]), main="Number of comments on Reddit by Requester", ylab = "Numbers of comments by requester",
        names.arg = c("Requester # of comments at request",
                      "Requester # of comments at retrieval"),beside=TRUE, col=colours)
legend("topleft",c("No Pizza","Pizza"),fill = colours, bty = "n")

barplot(as.matrix(comments[,4:5]), main="Number of comments in RAOP by Requester", ylab = "Numbers of comments by requester",
        names.arg = c("Requester # of comments in raop at request",
                      "Requester # of comments in raop at retrieval"),beside=TRUE, col=colours)
legend("topleft",c("No Pizza","Pizza"),fill = colours, bty = "n")

barplot(as.matrix(comments[,6:7]), main="Number of posts on Reddit by Requester", ylab = "Numbers of posts by requester",
        names.arg = c("Requester # of posts at request",
                      "Requester # of posts at retrieval"),beside=TRUE, col=colours)
legend("topleft",c("No Pizza","Pizza"),fill = colours, bty = "n")

barplot(as.matrix(comments[,8:9]), main="Number of posts in RAOP by Requester", ylab = "Numbers of posts by requester",
        names.arg = c("Requester # of posts in raop at request",
                      "Requester # of posts in raop at retrieval"),beside=TRUE, col=colours)
legend("topleft",c("No Pizza","Pizza"),fill = colours, bty = "n")

################################# Mean Difference Analysis
library(ggplot2)
getDiff <- function(varName) {
        var <- as.numeric(df[,varName])
        mnv <- mean(var)
        sdv <- sd(var)
        
        varT <- as.numeric(df[df$requester_received_pizza=="true", varName])
        varF <- as.numeric(df[df$requester_received_pizza=="false", varName])
        diffRaw <- mean(varT) - mean(varF)
        
        varTnorm <- (varT - mnv) / sdv
        varFnorm <- (varF - mnv) / sdv
        diffNorm <- mean(varTnorm) - mean(varFnorm)
        
        c(diffRaw, diffNorm)
}

vars <- names(df)
diffsDF <- data.frame(matrix(NA, nrow=ncol(df), ncol=4))
names(diffsDF) <- c("variable", "diffRaw", "diffNorm", "req.ret")

for (i in 1:ncol(df)) {
        diffsDF[i,1] <- vars[i]
        diffsDF[i,2:3] <- getDiff(vars[i])
        if (grepl("retrieval$", vars[i])) {
                diffsDF[i,4] <- "at retrieval"
        } else {
                diffsDF[i,4] <- "at request"
        }
}

diffsDF <- diffsDF[order(diffsDF$diffNorm, decreasing = T),]

diffsDF$req.ret <- as.factor(diffsDF$req.ret)
diffsDF <- diffsDF[!is.na(diffsDF$diffRaw), ]

ggplot(diffsDF,
       aes(x=reorder(variable, diffNorm), y=diffNorm, fill = req.ret)) +
        geom_bar(stat="identity") +
        theme_bw(base_family = "Helvetica") + coord_flip() +
        labs(list(y="Normalized Difference", x="Variable Names")) +
        scale_fill_discrete(name="") +
        theme(text = element_text(size = 10),
              axis.title.x = element_text(size = 9),
              axis.title.y = element_text(size=9))


################################# When Should You Request For a Pizza? (Time Analysis)
library(dplyr)
library(ggplot2)
library(lubridate)
df$unix_timestamp_of_request <- as.POSIXct(as.numeric(df$unix_timestamp_of_request), origin="1970-01-01")

# hour analysis
df.hours <- df %>% 
        select(unix_timestamp_of_request, requester_received_pizza) %>% 
        mutate(requester_received_pizza = (requester_received_pizza=="true")*1,
               hours = hour(unix_timestamp_of_request)) %>% 
        group_by(hours) %>% 
        summarize(success = sum(requester_received_pizza), total = n(), successRate = round(success/total, 2)) %>% 
        arrange(desc(successRate))
hour.order <- df.hours$hours
df.hours$hours <- factor(df.hours$hours, levels = hour.order)
plot.bar <- ggplot(data=df.hours, aes(x = hours, y = successRate)) +
        labs(list(y = "Prob. Receive Pizza", x = "Hour")) +
        geom_bar(stat="identity", fill = "lightblue", col = "black")
plot.bar

################################# Predicting using Decision Trees
df2 <- df %>% 
        select(-request_id, -request_text, -request_text_edit_aware, -request_title, 
               -requester_subreddits_at_request, -requester_username, -in_test_set,
               -post_was_edited, -unix_timestamp_of_request, -requester_user_flair,
               -giver_username_if_known) %>% 
        mutate(requester_received_pizza = factor(requester_received_pizza))

for (i in c(1:16, 18:22)){
        df2[,i] <- as.numeric(as.character(df2[,i]))
}

library(caret)
library(grid); library(gridExtra)
set.seed(666)
inTrain <- createDataPartition(df2$requester_received_pizza, p=0.7, list=FALSE)
training <- df2[inTrain, ]
testing <- df2[-inTrain, ]

modFit <- train(requester_received_pizza ~ ., data=training, method="rpart")

pred <- predict(modFit, newdata = testing)
tab <- tableGrob(confusionMatrix(pred, testing$requester_received_pizza)$table, theme=mytheme)
grid.draw(tab)

# you have to install XQuartz for this plot, so I commented it out
# library(rattle)
# fancyRpartPlot(modFit$finalModel, sub = "")

################################# Appendix
# How many requests resulted in granting pizza, and how many did not?
sum(df$requester_received_pizza == "true") #1397
sum(df$requester_received_pizza == "false") #4274
# What is the average number of subReddits subscribed to by those who did not get a pizza?
df$requester_number_of_subreddits_at_request <- as.numeric(df$requester_number_of_subreddits_at_request)
mean(df$requester_number_of_subreddits_at_request[df$requester_received_pizza == "false"]) # 17.37833
# What is the average number of down votes at retrieval for those requests that were successful?
df$number_of_downvotes_of_request_at_retrieval <- as.numeric(df$number_of_downvotes_of_request_at_retrieval)
mean(df$number_of_downvotes_of_request_at_retrieval[df$requester_received_pizza == "true"]) # 2.631353






