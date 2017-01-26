setwd("C:/Users/Бригада/Downloads/New/Analytical Edge")


emails <- read.csv("emails.csv", stringsAsFactors=FALSE)

# Exploratory analysis
str(emails)
table(emails$spam)

# Which word appears at the beginning of every email in the dataset?
emails$text[1]
emails$text[10]

# How many characters are in the longest email in the dataset?
nchar(emails$text[which.max(nchar(emails$text))])

# Which row contains the shortest email in the dataset? 
which.min(nchar(emails$text))

# PREPARING THE CORPUS
library(tm)
# 1) Build a new corpus variable called corpus.
corpus <- Corpus(VectorSource(emails$text))

# 2) Using tm_map, convert the text to lowercase.
corpus <- tm_map(corpus, tolower)

corpus <- tm_map(corpus, PlainTextDocument)
# 3) Using tm_map, remove all punctuation from the corpus.
corpus <- tm_map(corpus, removePunctuation)

# 4) Using tm_map, remove all English stopwords from the corpus.
corpus <- tm_map(corpus, removeWords, stopwords("english"))

# 5) Using tm_map, stem the words in the corpus.
corpus <- tm_map(corpus, stemDocument)

#6) Build a document term matrix from the corpus, called dtm.
dtm <- DocumentTermMatrix(corpus)
dtm

# sparse words
spdtm <- removeSparseTerms(dtm, 0.95)
spdtm

# Creat DATA FRAME
emailsSparse <- as.data.frame(as.matrix(spdtm))
colnames(emailsSparse) <- make.names(colnames(emailsSparse))

emailsSparse$spam <- emails$spam
# colSums(emailsSparse) returns the number of times a word stem appeared 
# across all the emails in the dataset.

# What is the word stem that shows up most frequently across all the emails in the dataset? 
which.max(colSums(emailsSparse))

##
#sum(colSums(emailsSparse[emailsSparse$spam==0,names(emailsSparse) !="spam"])>=5000)
#sum(colSums(emailsSparse[emailsSparse$spam==1,names(emailsSparse) !="spam"])>=1000)

# How many word stems appear at least 5000 times in the ham emails in the dataset?
sort(colSums(subset(emailsSparse, spam == 0)))
# How many word stems appear at least 1000 times in the spam emails in the dataset?
sort(colSums(subset(emailsSparse, spam == 1)))

# Building ML models
emailsSparse$spam = as.factor(emailsSparse$spam)
set.seed(123)

library(caTools)
spl <- sample.split(emailsSparse$spam, SplitRatio=0.7)

train <- subset(emailsSparse, spl==TRUE)
test <- subset(emailsSparse, spl==FALSE)

# logistic regression model
spamLog <- glm(spam ~ ., data=train, family="binomial")
# the predicted spam probabilities for the training set
predictLog = predict(spamLog, type="response")

## Be careful to obtain probabilities instead of predicted classes
## because we will be using these values to compute training set AUC values. 
## Recall that you can obtain probabilities for CART models by not passing any type 
## parameter to the predict() function, and you can obtain probabilities from a random 
## forest by adding the argument type="prob". For CART and random forest, you need 
## to select the second column of the output of the predict() function, 
## corresponding to the probability of a message being spam.

# CART model
library(rpart)
library(rpart.plot)
spamCART <- rpart(spam ~ ., data=train, method="class")
# the predicted spam probabilities for the training set
predictCART <- predict(spamCART)[,2]

# Random forest
library(randomForest)
set.seed(123)
spamRF <- randomForest(spam ~ ., data=train, method="class")

# the predicted spam probabilities for the training set
predictRF <- predict(spamRF, type="prob")[,2]

# Check the Logistic regression model
table(predictLog < 0.00001)
table(predictLog > 0.99999)
table(predictLog >= 0.00001 & predictLog <= 0.99999)

summary(spamLog)

# Check the CART model
prp(spamCART)

# What is the training set accuracy of spamLog, using a threshold of 0.5 for predictions?
t <- table(train$spam, predictLog > 0.5)
t
(t[1] + t[4]) / nrow(train)

# What is the training set AUC of spamLog?
library(ROCR)

ROCpred <- prediction(predictLog, train$spam)
ROCperf <- performance(ROCpred, "tpr", "fpr")
plot(ROCperf)
as.numeric(performance(ROCpred, "auc")@y.values)

# What is the training set accuracy of spamCART, using a threshold of 0.5 for predictions? 
t <- table(train$spam, predictCART > 0.5)
t
(t[1] + t[4]) / nrow(train)

ROCpred <- prediction(predictCART, train$spam)
as.numeric(performance(ROCpred, "auc")@y.values)

# What is the training set accuracy of spamRF, using a threshold of 0.5 for predictions?
t <- table(train$spam, predictRF > 0.5)
t
(t[1] + t[4]) / nrow(train)

ROCpred <- prediction(predictRF, train$spam)
as.numeric(performance(ROCpred, "auc")@y.values)

## TESTING SET
predictTestLog <- predict(spamLog, newdata=test, type="response")
predictTestCART <- predict(spamCART, newdata=test)[,2]
predictTestRF <- predict(spamRF, newdata=test, type="prob")[,2]

# What is the testing set accuracy of spamLog, using a threshold of 0.5 for predictions?
t <- table(test$spam, predictTestLog > 0.5)
t
(t[1] + t[4]) / nrow(test)

# What is the testing set AUC of spamLog?
ROCpred <- prediction(predictTestLog, test$spam)
as.numeric(performance(ROCpred, "auc")@y.values)
#0.9627517

# What is the testing set accuracy of spamCART, using a threshold of 0.5 for predictions?
t <- table(test$spam, predictTestCART > 0.5)
t
(t[1] + t[4]) / nrow(test)

# What is the testing set AUC of spamCART?
ROCpred <- prediction(predictTestCART, test$spam)
as.numeric(performance(ROCpred, "auc")@y.values)
#0.963176

# What is the testing set accuracy of spamRF, using a threshold of 0.5 for predictions?
t <- table(test$spam, predictTestRF > 0.5)
t
(t[1] + t[4]) / nrow(test)
# What is the testing set AUC of spamRF?
ROCpred <- prediction(predictTestRF, test$spam)
as.numeric(performance(ROCpred, "auc")@y.values)
#0.9975656

# INTEGRATING WORD COUNT INFORMATION

#Obtain the word counts for each email.
#wordCount = rowSums(as.matrix(dtm))
library(slam)
wordCount = rollup(dtm, 2, FUN=sum)$v

hist(wordCount)
hist(log(wordCount))

emailsSparse$logWordCount <- log(wordCount)
boxplot(logWordCount ~ spam, emailsSparse)

train2 <- subset(emailsSparse, spl==TRUE)
test2 <- subset(emailsSparse, spl==FALSE)

# CART
spam2CART <- rpart(spam ~., data=train2, method="class")
# random forest
set.seed(123)
spam2RF <- randomForest(spam~.,data=train2)

# test CART
predTest2CART <- predict(spam2CART, newdata=test2)[,2]
t <- table(test2$spam, predTest2CART > 0.5)
t
(t[1] + t[4]) / nrow(test2)

ROCpred <- prediction(predTest2CART, test2$spam)
as.numeric(performance(ROCpred, "auc")@y.values)

# test RF

predTest2RF <- predict(spam2RF, newdata=test2, type="prob")[,2]
t <- table(test2$spam, predTest2RF > 0.5)
t
(t[1] + t[4]) / nrow(test2)

ROCpred <- prediction(predTest2RF, test2$spam)
as.numeric(performance(ROCpred, "auc")@y.values)
