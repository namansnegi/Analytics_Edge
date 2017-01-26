data(state)
statedata = data.frame(state.x77)

# Linear Regr
stateLR <- lm(Life.Exp ~ ., data = statedata)
summary(stateLR)

predLR <- predict(stateLR)
sse <- sum((predLR - statedata$Life.Exp)^2)
sse

stateLR2 <- lm(Life.Exp ~ Population + Murder + Frost + HS.Grad, data=statedata)
summary(stateLR2)

predLR2 <- predict(stateLR2)
sse2 <- sum((predLR2 - statedata$Life.Exp)^2)
sse2

#CART
library(rpart)
library(rpart.plot)
#initial
stateCART <- rpart(Life.Exp ~ ., data = statedata)
prp(stateCART)

predCART <- predict(stateCART)
sse3 <- sum((predCART - statedata$Life.Exp)^2)
sse3

# smaller minbucket
stateCART2 <- rpart(Life.Exp ~ ., data = statedata, minbucket=5)
prp(stateCART2)

predCART2 <- predict(stateCART2)
sse4 <- sum((predCART2 - statedata$Life.Exp)^2)
sse4

# only area
stateCART3 <- rpart(Life.Exp ~ Area, data = statedata, minbucket=1)
prp(stateCART3)

predCART3 <- predict(stateCART3)
sse5 <- sum((predCART3 - statedata$Life.Exp)^2)
sse5

# CROSS-VALIDATION  
library(caret)
library(e1071)

set.seed(111)

# Define cross-validation experiment
numFolds = trainControl( method = "cv", number = 10 )
cpGrid = expand.grid( .cp = seq(0.01,0.5,0.01)) 

tr <- train(Life.Exp ~ ., data=statedata, method = "rpart", trControl= numFolds, tuneGrid= cpGrid)
tr

#
stateCART4 <- rpart(Life.Exp ~ ., data = statedata, cp = 0.12)
prp(stateCART4)

predCART4 <- predict(stateCART4)
sse6 <- sum((predCART4 - statedata$Life.Exp)^2)
sse6

# Another CV
set.seed(111)

tr2 <- train(Life.Exp ~ Area, data=statedata, method = "rpart", trControl= numFolds, tuneGrid= cpGrid)
tr2

#
stateCART5 <- rpart(Life.Exp ~ Area, data = statedata, cp = 0.02)
prp(stateCART5)

predCART5 <- predict(stateCART5)
sse7 <- sum((predCART5 - statedata$Life.Exp)^2)
sse7
