setwd("C:/Users/Бригада/Downloads/New/Analytical Edge")

trials <- read.csv("clinical_trial.csv", stringsAsFactors=FALSE)
str(trials)
summary(trials)

# How many characters are there in the longest abstract?
which.max(nchar(trials$abstract))
nchar(trials$abstract[664])

# How many search results provided no abstract?
table(nchar(trials$abstract)==0)

# Find the observation with the minimum number of characters in the title
which.min(nchar(trials$title))
trials$title[1258]

## PREPARING THE CORPUS  
library(tm)

# Convert the title variable to corpusTitle and the abstract variable to corpusAbstract.
corpusTitle <- Corpus(VectorSource(trials$title))
corpusAbstract <- Corpus(VectorSource(trials$abstract))

# Convert corpusTitle and corpusAbstract to lowercase.
corpusTitle <- tm_map(corpusTitle, tolower)
corpusAbstract <- tm_map(corpusAbstract, tolower)

# i need this, but i don't remember why. smth concern the updetes of R
corpusTitle = tm_map(corpusTitle, PlainTextDocument)
corpusAbstract = tm_map(corpusAbstract, PlainTextDocument)

# Remove the punctuation in corpusTitle and corpusAbstract.
corpusTitle <- tm_map(corpusTitle, removePunctuation)
corpusAbstract <- tm_map(corpusAbstract, removePunctuation)

# Remove the English language stop words from corpusTitle and corpusAbstract.
corpusTitle <- tm_map(corpusTitle, removeWords, stopwords("english"))
corpusAbstract <- tm_map(corpusAbstract, removeWords, stopwords("english"))

# Stem the words in corpusTitle and corpusAbstract (each stemming might take a few minutes).
corpusTitle <- tm_map(corpusTitle, stemDocument)
corpusAbstract <- tm_map(corpusAbstract, stemDocument)

# Build a document term matrix called dtmTitle from corpusTitle and dtmAbstract from corpusAbstract.
dtmTitle <- DocumentTermMatrix(corpusTitle)
dtmAbstract <- DocumentTermMatrix(corpusAbstract)

# Limit dtmTitle and dtmAbstract to terms with sparseness of at most 95% 
# (aka terms that appear in at least 5% of documents).
dtmTitle <- removeSparseTerms(dtmTitle, 0.95)
dtmAbstract <- removeSparseTerms(dtmAbstract, 0.95)

# Convert dtmTitle and dtmAbstract to data frames (keep the names dtmTitle and dtmAbstract).
dtmTitle <- as.data.frame(as.matrix(dtmTitle))
dtmAbstract <- as.data.frame(as.matrix(dtmAbstract))

# R-friendly names
colnames(dtmTitle) = make.names(colnames(dtmTitle))
colnames(dtmAbstract) = make.names(colnames(dtmAbstract))

ncol(dtmTitle)
ncol(dtmAbstract)

# What is the most frequent word stem across all the abstracts? 
# Hint: you can use colSums() to compute the frequency of a word across all the abstracts.
freq <- colSums(dtmAbstract)
which.max(freq)

## BUILDING A MODEL

colnames(dtmTitle) = paste0("T", colnames(dtmTitle))
colnames(dtmAbstract) = paste0("A", colnames(dtmAbstract))
dtm = cbind(dtmTitle, dtmAbstract)
str(dtm)

dtm$trial <- trials$trial
ncol(dtm)

#Split
set.seed(144)

spl <- sample.split(dtm$trial, SplitRatio=0.7)
train <- subset(dtm, spl==TRUE)
test <- subset(dtm, spl==FALSE)

t <- table(test$trial)
t
t[1] / nrow(test)

# build CART model
library(rpart)
library(rpart.plot)

CARTmodel <- rpart(trial ~ ., data=train, method="class")
prp(CARTmodel)

trainPred <- predict(CARTmodel)
head(trainPred)
# I need only the second column
trainPred[which.max(trainPred[,2]),]
#or
summary(trainPred)

# What is the training set accuracy of the CART model?
t <- table(train$trial, trainPred[,2]>=0.5)
t
# acc = (TN + TP) / N
(t[1] + t[4]) / nrow(train)
# sens = TP / (TP + FN)
t[4] / (t[4] + t[2])
# spec = TN / (TN + FP)
t[1] / (t[1] + t[3])

#OR
ct = table(train$trial, trainPred[,2]>=0.5)
(ct[1,1]+ct[2,2])/nrow(train)
ct[2,2]/(ct[2,1]+ct[2,2])
ct[1,1]/(ct[1,1]+ct[1,2])

# testing set
# What is the testing set accuracy?
testPred <- predict(CARTmodel, newdata=test, type="class")
t <- table(test$trial, testPred)
t
(t[1] + t[4]) / nrow(test)

# Using the ROCR package, what is the testing set AUC of the prediction model?
library(ROCR)

predROC <- predict(CARTmodel, newdata=test)[,2]

pred = prediction(predROC, test$trial)
perf = performance(pred, "tpr", "fpr")
plot(perf)
as.numeric(performance(pred, "auc")@y.values)

#  DECISION-MAKER TRADEOFFS