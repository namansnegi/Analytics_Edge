setwd("C:/Users/Бригада/Downloads/New/Analytical Edge")


census = read.csv("census.csv")
library(caTools)
set.seed(2000)
split = sample.split(census$over50k, SplitRatio = 0.6)
train = subset(census, split==TRUE)
test = subset(census, split==FALSE)

#logistic regression
censusglm = glm( over50k ~ . , family="binomial", data = train)
summary(censusglm)

predictTest = predict(censusglm, newdata = test, type = "response")
t <- table(test$over50k, predictTest >= 0.5)
t
(t[1] + t[4]) / nrow(test)

table(train$over50k)
table(test$over50k)
9713/(9713+3078) 



# What is the area-under-the-curve (AUC) for this model on the test set?
p = predict(censusglm, newdata=test)
pred = prediction(p, test$over50k)
perf = performance(pred, "tpr", "fpr")
plot(perf)
as.numeric(performance(pred, "auc")@y.values)

# or

library(ROCR)
ROCRpred = prediction(predictTest, test$over50k)
as.numeric(performance(ROCRpred, "auc")@y.values)

# A CART MODEL

#cart model
library(rpart)
library(rpart.plot)
set.seed(2000)
spl = sample.split(census$over50k, SplitRatio = 0.6)
train = subset(census, spl==TRUE)
test = subset(census, spl==FALSE)

trainTree = rpart(over50k ~ ., data=train, method="class")
prp(trainTree)

predTree <- predict(trainTree, test, type="class")
t <- table(test$over50k, predTree)
t
#acc
(t[1] + t[4]) / nrow(test)

#ROC
# Note that unlike the previous question, when we call the predict function, 
# we leave out the argument type = "class" from the function call. Without this 
# extra part, we will get the raw probabilities of the dependent variable values
# for each observation, which we need in order to generate the AUC. We need to 
# take the second column of the output
predTree <- predict(trainTree, newdata=test)[,2]
ROCRpredTree = prediction(predTree, test$over50k)
as.numeric(performance(ROCRpredTree, "auc")@y.values)

#Random forest
library(randomForest)
# down-sampling
set.seed(1)
trainSmall = train[sample(nrow(train), 2000), ]

set.seed(1)
censusForest <- randomForest(over50k ~ ., data = trainSmall)

predForest <- predict(censusForest, newdata=test)
t <- table(test$over50k, predForest)
t
#acc
(t[1] + t[4]) / nrow(test)

#metric
vu = varUsed(censusForest, count=TRUE)
vusorted = sort(vu, decreasing = FALSE, index.return = TRUE)
dotchart(vusorted$x, names(censusForest$forest$xlevels[vusorted$ix]))

varImpPlot(censusForest)

#corss validation
library(caret)
library(e1071)

set.seed(2)

numFolds = trainControl( method = "cv", number = 10 )
cartGrid = expand.grid( .cp = seq(0.002,0.1,0.002))

tr <- train(over50k ~ ., data = train, method = "rpart", trControl = numFolds, tuneGrid = cartGrid)
tr$bestTune

CARTnew <- rpart(over50k ~ ., data=train, method="class", cp = tr$bestTune)
prednew <- predict(CARTnew, newdata=test, type="class")
t <- table(test$over50k, prednew)
t
#acc
(t[1] + t[4])/nrow(test)

prp(CARTnew)
