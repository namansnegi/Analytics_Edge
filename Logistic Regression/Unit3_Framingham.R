# Unit 3, The Framingham Heart Study
setwd("C:/Users/Бригада/Downloads/New/Analytical Edge")
# Video 3

# Read in the dataset
framingham = read.csv("framingham.csv")

# Look at structure
str(framingham)

# Load the library caTools for splitting
library(caTools)

# Randomly split the data into training and testing sets (splitRatio is what in the Training set)
set.seed(1000)
split = sample.split(framingham$TenYearCHD, SplitRatio = 0.65)

# Split up the data using subset
train = subset(framingham, split==TRUE)
test = subset(framingham, split==FALSE)

str(train)

# Logistic Regression Model
framinghamLog = glm(TenYearCHD ~ ., data = train, family=binomial)
summary(framinghamLog)

# Predictions on the test set 
#response gives us probability
predictTest = predict(framinghamLog, type="response", newdata=test)

# Confusion matrix with threshold of 0.5
table(test$TenYearCHD, predictTest > 0.5)

# Accuracy 84.8%
(1069+11)/(1069+6+187+11)

# Baseline accuracy (no CHD) 84.4%
(1069+6)/(1069+6+187+11) 

#our model barely beats the accuracy

# Test set AUC 
library(ROCR)
ROCRpred = prediction(predictTest, test$TenYearCHD)
#AUC value of test set
as.numeric(performance(ROCRpred, "auc")@y.values)

#quiz 
# sensetivity
11/(11+187)
# spec
1069/(1069+6)
