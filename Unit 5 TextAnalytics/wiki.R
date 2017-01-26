setwd("C:/Users/Бригада/Downloads/New/Analytical Edge")

# Bags of words
wiki <- read.csv("wiki.csv", stringsAsFactors=FALSE)
str(wiki)

wiki$Vandal <- as.factor(wiki$Vandal)

# How many cases of vandalism were detected in the history of this page?
t <- table(wiki$Vandal)
t

# BAGS OF WORDS approach
# The text already is lowercase and stripped of punctuation.
library(tm)
library(SnowballC)
# Corpus

corpusAdded <- Corpus(VectorSource(wiki$Added))
writeLines(as.character(corpusAdded[[1]]))

#REmove stopwords
corpusAdded <- tm_map(corpusAdded, removeWords, stopwords("english"))
writeLines(as.character(corpusAdded[[1]]))

#Stem the words
corpusAdded <- tm_map(corpusAdded, stemDocument)
writeLines(as.character(corpusAdded[[1]]))
corpusAdded[[1]]$content

#Building DocumentTermMatrix
dtmAdded <- DocumentTermMatrix(corpusAdded)
dtmAdded

#Filter of Sparse words
sparseAdded <- removeSparseTerms(dtmAdded, 0.997)
sparseAdded

# Convert sparseAdded to a data frame
wordsAdded <- as.data.frame(as.matrix(sparseAdded))

colnames(wordsAdded) = paste("A", colnames(wordsAdded))

###########################################################################

## THE SAME FOR WORDS REMOVED
corpusRemoved <- Corpus(VectorSource(wiki$Removed))
corpusRemoved[[3]]$content

#REmove stopwords
corpusRemoved <- tm_map(corpusRemoved, removeWords, stopwords("english"))

#Stem the words
corpusRemoved <- tm_map(corpusRemoved, stemDocument)
corpusAdded[[1]]$content

#Building DocumentTermMatrix
dtmRemoved <- DocumentTermMatrix(corpusRemoved)
dtmRemoved

#Filter of Sparse words
sparseRemoved <- removeSparseTerms(dtmRemoved, 0.997)
sparseAdded

# Convert sparseAdded to a data frame
wordsRemoved <- as.data.frame(as.matrix(sparseRemoved))
colnames(wordsRemoved) = paste("R", colnames(wordsRemoved))
str(wordsRemoved)

# COmbine two data frame
wikiWords = cbind(wordsAdded, wordsRemoved)

# Add dependent variable
wikiWords$Vandal = wiki$Vandal

# Splitting
library(caTools)

set.seed(123)
split <- sample.split(wikiWords$Vandal, SplitRatio=0.7)


wikiTrain <- subset(wikiWords, split==TRUE)
wikiTest <- subset(wikiWords, split==FALSE)

t <- table(wikiTest$Vandal)
t
t[1] / nrow(wikiTest)

# building the model
library(rpart)
library(rpart.plot)

CARTmodel <- rpart(Vandal ~ ., data=wikiTrain, method="class")
prp(CARTmodel)

testPredictCART <- predict(CARTmodel, newdata=wikiTest, type="class")
t <- table(wikiTest$Vandal, testPredictCART)
t
(t[1] + t[4]) / nrow(wikiTest)

#baseline model
t <- table(wikiTest$Vandal)
t
t[1] / nrow(wikiTest)

#

wikiWords2 = wikiWords

#Make a new column in wikiWords2 that is 1 if "http" was in Added:
wikiWords2$HTTP = ifelse(grepl("http",wiki$Added,fixed=TRUE), 1, 0)
table(wikiWords2$HTTP)

wikiTrain2 = subset(wikiWords2, split==TRUE)
wikiTest2 = subset(wikiWords2, split==FALSE)

# NEW cart

CARTmodel2 <- rpart(Vandal ~ ., data=wikiTrain2, method="class")
pred2 <- predict(CARTmodel2, newdata=wikiTest2, type="class")

t <- table(wikiTest2$Vandal, pred2)
t
(t[1] + t[4])/nrow(wikiTest2)

wikiWords2$NumWordsAdded = rowSums(as.matrix(dtmAdded))
wikiWords2$NumWordsRemoved = rowSums(as.matrix(dtmRemoved))

mean(wikiWords2$NumWordsAdded)

#
wikiTrain3 = subset(wikiWords2, split==TRUE)
wikiTest3 = subset(wikiWords2, split==FALSE)

CARTmodel3 <- rpart(Vandal ~ ., data=wikiTrain3, method="class")
pred3 <- predict(CARTmodel3, newdata=wikiTest3, type="class")
t <- table(wikiTest3$Vandal, pred3)
t
(t[1] + t[4]) / nrow(wikiTest3)

# using other metadata pieces
wikiWords3 = wikiWords2

wikiWords3$Minor = wiki$Minor
wikiWords3$Loggedin = wiki$Loggedin

wikiTrain4 = subset(wikiWords3, split==TRUE)
wikiTest4 = subset(wikiWords3, split==FALSE)

CARTmodel4 <- rpart(Vandal ~ ., data=wikiTrain4, method="class")
pred4 <- predict(CARTmodel4, newdata=wikiTest4, type="class")
t <- table(wikiTest4$Vandal, pred4)
t
(t[1] + t[4]) / nrow(wikiTest3)

prp(CARTmodel4)
