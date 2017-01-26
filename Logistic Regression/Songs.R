setwd("C:/Users/Бригада/Downloads/New/Analytical Edge")

songs <- read.csv("songs.csv")
# How many observations (songs) are from the year 2010?
str(songs)
songs2010 <- subset(songs, songs$year >= 2010)
str(songs2010)

# How many songs does the dataset include for which the artist name is "Michael Jackson"?
songsJackson <- subset(songs, songs$artistname == "Michael Jackson")
str(songsJackson)
nrow(songsJackson)

# Which of these songs by Michael Jackson made it to the Top 10?
table(songsJackson$Top10)

songsJackson[which(songsJackson$Top10 == TRUE),2]

songsJackson[c("songtitle", "Top10")]

# What are the values of timesignature variable that occur in our dataset?
table(songs$timesignature)

# Highest tempo
songs[which.max(songs$tempo), "songtitle"]

# Train and Test

SongsTrain <- subset(songs, songs$year <= 2009)
SongsTest <- subset(songs, songs$year > 2009)

nrow(SongsTrain)

#  CREATING OUR PREDICTION MODEL  

nonvars = c("year", "songtitle", "artistname", "songID", "artistID")

SongsTrain = SongsTrain[ , !(names(SongsTrain) %in% nonvars) ]
SongsTest = SongsTest[ , !(names(SongsTest) %in% nonvars) ]

SongsLog1 = glm(Top10 ~ ., data=SongsTrain, family=binomial)
summary(SongsLog1)

# MULTICOLLINEARITY ISSUES!
cor(songs$loudness, songs$energy)

SongsLog2 = glm(Top10 ~ . - loudness, data=SongsTrain, family=binomial)
summary(SongsLog2)

SongsLog3 = glm(Top10 ~ . - energy, data=SongsTrain, family=binomial)
summary(SongsLog3)

# VALIDATING OUR MODEL  
predictTest = predict(SongsLog3, type="response", newdata=SongsTest)

# Confusion matrix with threshold of 0.45
table(SongsTest$Top10, predictTest > 0.45)

# Accuracy TN + TP / N. 87.9%
(309 + 19) / nrow(SongsTest)

# What would the accuracy of the baseline model be on the test set?
p2 = rep(FALSE, nrow(SongsTest))
table(SongsTest$Top10, p2>=0.45)

# accurasy == FALSE & 1 / Overall
314/nrow(SongsTest)

#
table(SongsTest$Top10, predictTest > 0.45)

#or by calculating
p = predict(SongsLog3, SongsTest, type="response")
sum(p>=0.45 & SongsTest$Top10==1)
sum(p>=0.45 & SongsTest$Top10==0)

#sens TP / (TP + FN)
19/(19+40)

#spec TN / (TN + FP)
309 / (309 + 5)
