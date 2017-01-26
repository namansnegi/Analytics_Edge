setwd("C:/Users/Бригада/Downloads/New/Analytical Edge")

stocks <- read.csv("StocksCluster.csv")
nrow(stocks)

# What proportion of the observations have positive returns in December?
table(stocks$PositiveDec)[2] / (table(stocks$PositiveDec)[1] + table(stocks$PositiveDec)[2])

which.max(cor(stocks)-diag(1, 12))
cor(stocks)[119]

summary(stocks)

# split
library(caTools)
set.seed(144)
spl = sample.split(stocks$PositiveDec, SplitRatio = 0.7)
stocksTrain = subset(stocks, spl == TRUE)
stocksTest = subset(stocks, spl == FALSE)

LogModel <- glm(PositiveDec ~ ., data=stocksTrain, family="binomial")
TrainPredLog <- predict(LogModel, newdata=stocksTrain, type="response")
t <- table(stocksTrain$PositiveDec, TrainPredLog>0.5)
t
(t[1] + t[4]) / nrow(stocksTrain)

TestPredLog <- predict(LogModel, newdata=stocksTest, type="response")
t <- table(stocksTest$PositiveDec, TestPredLog>0.5)
t
(t[1] + t[4]) / nrow(stocksTest)

# acc of baseline on test
table(stocksTest$PositiveDec)[2] / nrow(stocksTest)

# Clustering
limitedTrain = stocksTrain
limitedTrain$PositiveDec = NULL
limitedTest = stocksTest
limitedTest$PositiveDec = NULL

# normalizing
library(caret)

preproc <- preProcess(limitedTrain)
normTrain <- predict(preproc, limitedTrain)
normTest <- predict(preproc, limitedTest)

summary(normTrain)
summary(normTest)

# clustering
set.seed(144)
km <- kmeans(normTrain, 3)
table(km$cluster)

# predicting 

library(flexclust)
km.kcca = as.kcca(km, normTrain)
clusterTrain = predict(km.kcca)
clusterTest = predict(km.kcca, newdata=normTest)

table(clusterTest)

#  CLUSTER-SPECIFIC PREDICTIONS
stocksTrain1 <- subset(stocksTrain, clusterTrain == 1)
stocksTrain2 <- subset(stocksTrain, clusterTrain == 2)
stocksTrain3 <- subset(stocksTrain, clusterTrain == 3)

stocksTest1 <- subset(stocksTest, clusterTest == 1)
stocksTest2 <- subset(stocksTest, clusterTest == 2)
stocksTest3 <- subset(stocksTest, clusterTest == 3)
summary(stocksTest1)
summary(stocksTest2)
summary(stocksTest3)

mean(stocksTrain1$PositiveDec) 
mean(stocksTrain2$PositiveDec)
mean(stocksTrain3$PositiveDec)

# mode;
StocksModel1 <- glm(PositiveDec ~ ., data=stocksTrain1, family="binomial")
StocksModel2 <- glm(PositiveDec ~ ., data=stocksTrain2, family="binomial")
StocksModel3 <- glm(PositiveDec ~ ., data=stocksTrain3, family="binomial")

summary(StocksModel1)
summary(StocksModel2)
summary(StocksModel3)

PredictTest1 = predict(StocksModel1, stocksTest1, type="response")
PredictTest2 = predict(StocksModel2, stocksTest2, type="response")
PredictTest3 = predict(StocksModel3, stocksTest3, type="response")
ct1 = table(stocksTest1$PositiveDec, PredictTest1>=0.5)
(ct1[1,1]+ct1[2,2])/nrow(stocksTest1)
ct2 = table(stocksTest2$PositiveDec, PredictTest2>=0.5)
(ct2[1,1]+ct2[2,2])/nrow(stocksTest2)
ct3 = table(stocksTest3$PositiveDec, PredictTest3>=0.5)
(ct3[1,1]+ct3[2,2])/nrow(stocksTest3)


AllPredictions = c(PredictTest1, PredictTest2, PredictTest3)
AllOutcomes = c(stocksTest1$PositiveDec, stocksTest2$PositiveDec, stocksTest3$PositiveDec)
ct = table(AllOutcomes, AllPredictions>=0.5)
(ct[1,1]+ct[2,2])/length(AllPredictions)
