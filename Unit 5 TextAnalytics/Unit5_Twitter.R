# Unit 5 - Twitter
setwd("C:/Users/Бригада/Downloads/New/Analytical Edge")
Sys.setlocale("LC_ALL", "C")
# VIDEO 5

# Read in the data

tweets = read.csv("tweets.csv", stringsAsFactors=FALSE)
str(tweets)


# Create dependent variable
tweets$Negative = as.factor(tweets$Avg <= -1)
table(tweets$Negative)


# Install new packages
#install.packages("tm")
library(tm)
#install.packages("SnowballC")
library(SnowballC)

# Create corpus 
corpus = Corpus(VectorSource(tweets$Tweet))
# Look at corpus
corpus

writeLines(as.character(corpus[[1]]))


#preprocessing
# Convert to lower-case
corpus = tm_map(corpus, content_transformer(tolower))
corpus = tm_map(corpus, tolower)

corpus[[1]]
writeLines(as.character(corpus[[1]]))

# IMPORTANT NOTE: If you are using the latest version of the tm package, 
# you will need to run the following line before continuing (it converts 
# corpus to a Plain Text Document). This is a recent change having to do with 
# the tolower function that occurred after this video was recorded.

corpus = tm_map(corpus, PlainTextDocument)


# Remove punctuation

corpus = tm_map(corpus, removePunctuation)

writeLines(as.character(corpus[[1]]))

# Look at stop words 
stopwords("english")[1:10]

# Remove stopwords and apple
# all tweets contain apple word and it is not useful for prediction
corpus = tm_map(corpus, removeWords, c("apple", stopwords("english")))
writeLines(as.character(corpus[[1]]))

# Stem document 
corpus = tm_map(corpus, stemDocument)

writeLines(as.character(corpus[[1]]))
corpus[[1]]$content

# Video 6

# Create matrix

frequencies = DocumentTermMatrix(corpus)
frequencies

# Look at matrix 
inspect(frequencies[1000:1005,505:515])

# Check for sparsity, most popular terms
findFreqTerms(frequencies, lowfreq=20)

# Remove sparse terms

sparse = removeSparseTerms(frequencies, 0.995)
sparse

# Convert to a data frame
tweetsSparse = as.data.frame(as.matrix(sparse))

# Make all variable names R-friendly
colnames(tweetsSparse) = make.names(colnames(tweetsSparse))

# Add dependent variable
tweetsSparse$Negative = tweets$Negative

# Split the data

library(caTools)

set.seed(123)

split = sample.split(tweetsSparse$Negative, SplitRatio = 0.7)

trainSparse = subset(tweetsSparse, split==TRUE)
testSparse = subset(tweetsSparse, split==FALSE)

head(testSparse)

#quiz
findFreqTerms(frequencies, lowfreq=100)

# Video 7

# Build a CART model
library(rpart)
library(rpart.plot)

tweetCART = rpart(Negative ~ ., data=trainSparse, method="class")
prp(tweetCART)

# Evaluate the performance of the model
predictCART = predict(tweetCART, newdata=testSparse, type="class")
t <- table(testSparse$Negative, predictCART)
t
# Compute accuracy
(t[1] + t[4]) / nrow(testSparse)

#(294+18)/(294+6+37+18)

# Baseline accuracy 

table(testSparse$Negative)
300/(300+55)


# Random forest model

library(randomForest)
set.seed(123)
tweetRF = randomForest(Negative ~ ., data=trainSparse)
# Make predictions:
predictRF = predict(tweetRF, newdata=testSparse)
t <- table(testSparse$Negative, predictRF)
(t[1] + t[4]) / nrow(testSparse)
# Accuracy:
(293+21)/(293+7+34+21)

#quiz
tweetLog <- glm(Negative ~ ., data=trainSparse, family=binomial)
predictLog = predict(tweetLog, newdata=testSparse, type="response")
t <- table(testSparse$Negative, predictions>0.5)
t
(t[1] + t[4]) / nrow(testSparse)
