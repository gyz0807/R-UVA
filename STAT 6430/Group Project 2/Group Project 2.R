############################# Data Merging
library(dplyr)
#install.packages("lubridate")
library(lubridate)
library(gridExtra)

zipcodes= read.csv("zipcodes.txt", header = TRUE)
reviewer= read.csv("reviewers.txt", header= FALSE, sep = "|")
review= read.csv("reviews.txt", header= FALSE, sep = "\t")
genres= read.csv("genres.txt", header=FALSE, sep= "|" )

names(reviewer) = c("reviewer id", "age", "gender", "occupation", "zip code")
names(review) = c("reviewer id", "movie id", "rating", "timestamp.")
movie_gen= c("movie id", "movie title","release date","video release date",
             "IMDb URL","unknown","Action","Adventure","Animation","Children's","Comedy","Crime","Documentary","Drama","Fantasy", 
             "Film-Noir","Horror","Musical","Mystery","Romance","Sci-Fi","Thriller","War","Western")
names(genres)= movie_gen
genres$`video release date`=year(dmy(genres$`release date`))

reviews2= merge(reviewer, review, by= "reviewer id")

#Final merged data set. Have not put zipcode informtion in this yet. 
reviews3= merge(reviews2, genres, by= "movie id")

zipsU <- zipcodes[zipcodes$Country == 'US',c('Zipcode', 'State')]
zipsU <- zipsU[!duplicated(zipsU$Zipcode),]
zipsU$State <- as.character(zipsU$State)
zipsU[!zipsU$State %in% c(state.abb, 'DC'), 2] <- 'Territory'
names(zipsU) <- c(names(reviews3)[6], 'Location')
zipsU$`zip code` <- as.character(zipsU$`zip code`)
for (i in 1:nrow(zipsU)) {
        while (nchar(zipsU$`zip code`[i]) < 5) {
                zipsU$`zip code`[i] <- paste('0', zipsU$`zip code`[i], sep='')
        }
}

reviews3$`zip code` <- as.character(reviews3$`zip code`)

reviewsMaster <- merge(reviews3, zipsU, by= 'zip code', all.x = T)
reviewsMaster$Location[is.na(reviewsMaster$Location)] <- 'other'
reviewsMaster <- reviewsMaster[order(reviewsMaster$`movie id`), ]

reviewsMaster$Location[grepl('[A-Za-z]', reviewsMaster$`zip code`)] <- "Canada"

# checked to see if there was any territory data. Can't find any
terrzips <- zipsU[!zipsU$Location %in% c(state.abb, 'DC'), 1]
reviewsMaster$`zip code`[reviewsMaster$`zip code` %in% terrzips]

############################# Occupation, Gender, Age versus Ratings in General
# join genre and review
df1 <- left_join(genres, review, by="movie id")

for (i in c(6:24)){
        df1[,i] <- factor(df1[, i])
}

library(lubridate)
df1 <- df1 %>% 
        mutate(`release date` = dmy(`release date`))

# join df1 and reviewer
df2 <- left_join(df1, reviewer, by="reviewer id") %>% 
        mutate(`zip code` = as.numeric(as.character(`zip code`)))

# lat and lon
latlon <- zipcodes %>% 
        group_by(Zipcode) %>% 
        summarize(Lat = mean(Lat), Lon = mean(Long), State = first(State))

# join latlon and df2
df3 <- left_join(df2, latlon, by=c("zip code" = "Zipcode"))

library(ggplot2)
df3.occ <- df3 %>% 
        group_by(occupation) %>% 
        summarize(avg.rating = mean(rating)) %>% 
        arrange(avg.rating)
occ <- as.character(df3.occ$occupation)
df3.occ$occupation <- factor(df3.occ$occupation, levels=occ)
p1 <- ggplot(data=df3.occ, aes(x = occupation, y = avg.rating))+
        geom_bar(stat="identity", fill = "lightblue", col = "black")+
        coord_flip(ylim = c(2.5,4)) +
        labs(list(y = "Average Rating", x = "Occupation")) +
        theme(axis.title = element_text(size=8),
              axis.text = element_text(size=4),
              legend.position="none")

library(ggplot2)
df3.gender <- df3 %>% 
        group_by(gender) %>% 
        summarize(avg.rating = mean(rating)) %>% 
        arrange(avg.rating)
gender <- as.character(df3.gender$gender)
df3.gender$gender <- factor(df3.gender$gender, levels=gender)
p2 <- ggplot(data=df3.gender, aes(x = gender, y = avg.rating))+
        geom_bar(stat="identity", fill = "lightblue", col = "black")+
        labs(list(x = "Gender", y = "Average Rating"))+
        coord_flip(ylim = c(3, 4)) +
        theme(axis.title = element_text(size=8),
              axis.text = element_text(size=5),
              legend.position="none")

df3.age <- df3 %>% 
        mutate(ageGroup = cut(age, breaks=5, dig.lab=2)) %>% 
        group_by(ageGroup) %>% 
        summarize(avg.rating = mean(rating), sd.rating = sd(rating)) %>% 
        arrange(avg.rating)
ageGroup <- as.character(df3.age$ageGroup)
df3.age$ageGroup <- factor(df3.age$ageGroup, levels = ageGroup)
p3 <- ggplot(df3.age, aes(x = ageGroup, y = avg.rating))+
        geom_bar(stat="identity", fill = "lightblue", col = "black")+
        labs(list(x = "Age Group", y = "Average Rating"))+
        coord_flip(ylim = c(3, 4)) +
        theme(axis.title = element_text(size=8),
              axis.text = element_text(size=5),
              legend.position="none")
grid.arrange(p1, p2, p3, nrow=1, ncol=3)

############################# Further Inspection on Gender versus Ratings
library(dplyr)
library(tidyr)
library(gridExtra)
library(grid)

df4 <- df3 %>% 
        select(unknown:Western, rating, gender) %>% 
        gather(genre, isOrNot, unknown:Western) %>% 
        filter(isOrNot != 0) %>% 
        group_by(gender, genre) %>% 
        summarize(avgRating = mean(rating)) %>% 
        arrange(desc(avgRating)) %>% 
        slice(1:10)

df4.male <- df4 %>% 
        filter(gender=="M")
df4.female <- df4 %>% 
        filter(gender=="F")

mytheme <- gridExtra::ttheme_default(
        core = list(fg_params=list(cex = 0.6)),
        colhead = list(fg_params=list(cex = 0.6)),
        rowhead = list(fg_params=list(cex = 0.6)))

t1 <- tableGrob(df4.male, theme = mytheme)
t2 <- tableGrob(df4.female, theme = mytheme)
grid.arrange(t1, t2, nrow=1, ncol=2)

############################# Genres versus Ratings
library(ggplot2)
library(scales)
library(tidyr)
category <- c("unknown","Action","Adventure","Animation","Children's","Comedy","Crime",
              "Documentary","Drama","Fantasy","Film-Noir","Horror","Musical","Mystery",
              "Romance","Sci-Fi","Thriller","War","Western")

x <- reviewsMaster %>% 
        select(unknown:Western, rating) %>% 
        gather(genre, num, unknown:Western) %>% 
        filter(num!=0) %>% 
        group_by(genre) %>% 
        summarize(avgrate = mean(rating)) %>% 
        arrange(avgrate)

genre.seq <- x$genre
x$genre <- factor(x$genre, levels=genre.seq)

ggplot(data=x, aes(x=genre, y=avgrate, fill=genre))+
        geom_bar(stat="identity") +
        labs(list(x="", y="Average Rating"))+
        coord_flip() +
        scale_y_continuous(limits = c(3,4), oob=rescale_none) +
        theme(axis.title = element_text(size=8),
              axis.text = element_text(size=5),
              legend.position="none")

#percentage of top rating group by category
x1 <- reviewsMaster %>% 
        select(unknown:Western, rating) %>% 
        gather(genre, num, unknown:Western) %>% 
        filter(num!=0) %>% 
        group_by(genre) %>% 
        summarize(count = n())

x2 <- reviewsMaster %>% 
        select(unknown:Western, rating) %>% 
        gather(genre, num, unknown:Western) %>% 
        filter(num!=0 & rating==5) %>% 
        group_by(genre) %>% 
        summarize(five = n()) %>% 
        inner_join(x1, by="genre") %>% 
        mutate(percentFive = five/count) %>% 
        arrange(percentFive)

genre.seq1 <- x2$genre
x2$genre <- factor(x2$genre, levels=genre.seq1)

ggplot(data=x2, aes(x=genre, y=percentFive, fill=genre))+
        geom_bar(stat="identity") +
        labs(list(x="", y="Probability of Rating 5"))+
        coord_flip() +
        scale_y_continuous(limits = c(0.1,0.35), oob=rescale_none) +
        theme(axis.title = element_text(size=8),
              axis.text = element_text(size=5),
              legend.position="none")


############################# Geography: Comparing States
#calculate the average rating and the percentage of 5-star ratings for each state
stateStats <- data.frame(state = state.abb, avg=0, percFive=0, users=0, stringsAsFactors = F)
for (i in 1:nrow(stateStats)) {
        stateRating <- reviewsMaster$rating[reviewsMaster$Location==stateStats$state[i]]
        stateStats$avg[i] <- mean(stateRating)
        stateStats$percFive[i] <- sum(stateRating==5)/length(stateRating)
        stateUsers <- reviewsMaster[reviewsMaster$Location==stateStats$state[i], c(3, ncol(reviewsMaster))]
        stateStats$users[i] <- length(unique(stateUsers$`reviewer id`))
}

#change the state abbreviations to lower case full names (for maps package)
for (i in 1:nrow(stateStats)) {
        stateStats$state[i] <- tolower(state.name[grep(stateStats$state[i], state.abb)])
}

library(maps)
library(ggplot2)

# get state map data
states_map <- map_data("state")

#plot average rating
ggplot(stateStats, aes(map_id = state)) +
        geom_map(aes(fill = avg), map = states_map) +
        labs(list(y="", x="")) +
        scale_y_continuous(breaks=NULL) + scale_x_continuous(breaks=NULL) +
        scale_fill_continuous(name="") +
        expand_limits(x = states_map$long, y = states_map$lat)

#plot percentage of 5-star ratings
ggplot(stateStats, aes(map_id = state)) +
        geom_map(aes(fill = percFive), map = states_map) +
        labs(list(y="", x="")) +
        scale_y_continuous(breaks=NULL) + scale_x_continuous(breaks=NULL) + 
        scale_fill_continuous(breaks= c(.65, .5, .35, .20), 
                              labels=c("65%", "50%", "35%","20%"), 
                              name="") +
        expand_limits(x = states_map$long, y = states_map$lat)

#show unique users for states
head(stateStats[order(stateStats$percFive, decreasing=T), c(1,3,4)])

############################# History: Looking at Movies by Release Date
# need to load reviewsMaster
library(dplyr)
library(ggplot2)
# get the average rating for each release year
year <- group_by(reviewsMaster, `video release date`)
yearSum <- summarize(year, mean(rating))
names(yearSum) <- c("Release Year", "Average Rating")
yearSum <- yearSum[!is.na(yearSum$`Release Year`),]


#plot data
ggplot(yearSum, aes(x=`Release Year`, y=`Average Rating`)) + 
        geom_point() + 
        geom_smooth() +
        theme(axis.title = element_text(size=8))

# filter out any year that had less than three movies represented in the data
uniqMovies <- reviewsMaster[!duplicated(reviewsMaster$`movie id`),]
#table(uniqMovies$`video release date`)
removeDF <- data.frame(table(uniqMovies$`video release date`))
remove <- removeDF$Var1[removeDF$Freq < 3]
remove <- as.numeric(as.character(remove))
yearSumSparse <- yearSum[!yearSum$`Release Year` %in% remove, ]

#plot data with sparse years removed
ggplot(yearSumSparse, aes(x=`Release Year`, y=`Average Rating`)) + 
        geom_point() + 
        geom_smooth() +
        theme(axis.title = element_text(size=8))

# count how many movies from each release year are reviewed in our data set
yearCount <- data.frame(table(uniqMovies$`video release date`))
names(yearCount) <- c("Year", "# of Movies")

ggplot(yearCount) + 
        geom_bar(stat="identity", aes(x=Year, y=`# of Movies`)) +
        scale_x_discrete(breaks=seq(1940, 2000, by=10)) +
        theme(axis.title = element_text(size=8))

############################# Appendix
#1)
num_reviews=as.data.frame(table(reviews3$`reviewer id`))
x <- num_reviews$Var1[num_reviews$Freq==sort(num_reviews$Freq)[length(num_reviews$Freq)]] 
paste("Reviewer ID: ", as.numeric(as.character(x)), sep="")
#reviewer 405 reviewed the most movie (737).

#2)
Top5 <- as.data.frame(table(reviewsMaster$Location))
Top5 <- Top5[order(Top5$Freq, decreasing=T),]
print(Top5[1:5,])
# Var1  Freq
# CA 13842
# MN  7635
# NY  6882
# IL  5740
# TX  5042

#3)
genres["Sum Genres"] = rowSums(genres[,6:24])
moviegenre <- merge(genres, review, by='movie id')
proportion_greater_equal2= sum(moviegenre$`Sum Genres`>=2)/nrow(moviegenre)
paste(round(proportion_greater_equal2*100,2), "%", sep="")
#69.94%

#4)
movies= as.data.frame(table(reviews3$`movie id`))
new= as.data.frame(table(movies$Freq))
new["Percentage"]= new$Freq/length(movies$Var1)*100
names(new)= c("Number of Reviews", "Freq", "Percentage")
x <- new %>% 
        select(`Number of Reviews`, Percentage)
print(x, row.names = FALSE)

















