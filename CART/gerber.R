setwd("C:/Users/Бригада/Downloads/New/Analytical Edge")

# read data
gerber <- read.csv("gerber.csv")
str(gerber)

# proportion of people in this dataset voted in this election?
table(gerber$voting)
table(gerber$voting)[[2]]/ (table(gerber$voting)[[1]] + table(gerber$voting)[[2]])

# Which of the four "treatment groups" had the largest percentage of people who actually voted?
t <- table(gerber$civicduty == 1, gerber$voting)
t
t[4] / (t[4] + t[2])
# 0.3145377

t <- table(gerber$hawthorne == 1, gerber$voting)
t
t[4] / (t[4] + t[2])
# 0.3223746

t <- table(gerber$self == 1, gerber$voting)
t
t[4] / (t[4] + t[2])
# 0.3451515

t <- table(gerber$neighbors == 1, gerber$voting)
t
t[4] / (t[4] + t[2])
# 0.3779482

#OR
tapply(gerber$voting, gerber$civicduty, mean)
tapply(gerber$voting, gerber$hawthorne, mean)
tapply(gerber$voting, gerber$self, mean)
tapply(gerber$voting, gerber$neighbors, mean)

# EXPLORATION AND LOGISTIC REGRESSION
# str(gerber)
LogModel <- glm(voting ~ civicduty + hawthorne + self + neighbors, data=gerber, family=binomial)
summary(LogModel)

# Using a threshold of 0.3, what is the accuracy of the logistic regression model? 
predictLog <- predict(LogModel, type="response")
# use the table function to make a confusion matrix:
t <- table(gerber$voting, predictLog >= 0.3)
t
# We can compute the accuracy of the sum of the true positives and true negatives, 
# divided by the sum of all numbers in the table:
(t[1] + t[4])/nrow(gerber)

# for t = 0.5
t <- table(gerber$voting, predictLog >= 0.5)
t
#accuracy
(t[1])/nrow(gerber)

# EXPLORATION AND LOGISTIC REGRESSION
library(ROCR)
ROCRpred = prediction(predictLog, gerber$voting)
#AUC value of test set
as.numeric(performance(ROCRpred, "auc")@y.values)

# Even though all of our variables are significant, our model does not improve 
# over the baseline model of just predicting that someone will not vote, and the AUC is low. 
# So while the treatment groups do make a difference, this is a weak predictive model.

# TREES  
library(rpart)
library(rpart.plot)

CARTmodel = rpart(voting ~ civicduty + hawthorne + self + neighbors, data=gerber)
prp(CARTmodel)

CARTmodel2 = rpart(voting ~ civicduty + hawthorne + self + neighbors, data=gerber, cp=0.0)
prp(CARTmodel2)

CARTmodel3 = rpart(voting ~ civicduty + hawthorne + self + neighbors + sex, data=gerber, cp=0.0)
prp(CARTmodel3)

CARTmodel4 <- rpart(voting ~ control, data=gerber, cp=0.0)
prp(CARTmodel4, digits=6)
abs(0.296638-0.34)

CARTmodel5 <- rpart(voting ~ control + sex, data=gerber, cp=0.0)
prp(CARTmodel5, digits = 6)

LogControl <- glm(voting ~ control + sex, data=gerber, family=binomial)
summary(LogControl)

#
Possibilities = data.frame(sex=c(0,0,1,1),control=c(0,1,0,1))
predict(LogControl, newdata=Possibilities, type="response")

abs(0.2908065 - 0.290456)

#NEW LOG REGR with combined variable: 1 if Woman AND control
LogModel2 = glm(voting ~ sex + control + sex:control, data=gerber, family="binomial")
summary(LogModel2)

predict(LogModel2, newdata=Possibilities, type="response")
abs(0.2904558 - 0.290456)
