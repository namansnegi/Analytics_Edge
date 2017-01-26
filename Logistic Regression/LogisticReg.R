x <- -1.5 + 3*1 - 0.5*5
exp(x)

py <- 1 / (1 + exp(-x))
py

setwd("C:/Users/Бригада/Downloads/New/Analytical Edge")
quality <- read.csv("quality.csv")
str(quality)

table(quality$PoorCare)
#baseline model accuracy
98/(98+33)

install.packages("caTools")
library(caTools)

# let's split data on training and test set randomly 75% to 25%
# at first, make logic fector for splitting
set.seed(88)
split <- sample.split(quality$PoorCare, SplitRatio = 0.75)
# use split
qualityTrain <- subset(quality, split == TRUE)
qualityTest <- subset(quality, split == FALSE)
nrow(qualityTrain)
nrow(qualityTest)

# building a logistic regression using office Visits and nacrcoticas as ideoendent var

QualityLog <- glm(PoorCare ~ OfficeVisits + Narcotics, data=qualityTrain, family=binomial)
summary(QualityLog)

predictTrain <- predict(QualityLog, type="response")
summary(predictTrain)

#means for each outcome
tapply(predictTrain, qualityTrain$PoorCare, mean)


#quiz

fit1 <- glm(PoorCare ~ StartedOnCombination + ProviderCount, data=qualityTrain, family=binomial)
summary(fit1)

#standard treshold value
t <- 0.5
table(qualityTrain$PoorCare, predictTrain > t)

#sens <- Actual True / sum(Act + Pred)
10 / (10 + 15)
#spic <- 
70 / (70 + 4)

# increase treashold value 
table(qualityTrain$PoorCare, predictTrain > 0.7)
#sens
8/25
#spic
73/74

# decrease treashold value 
table(qualityTrain$PoorCare, predictTrain > 0.2)
#sens
16/25
#spic
54/74

# quiz
20 / 25
15 / 25

15/25

# threshold
# ROC Curve
install.packages("ROCR")
library(ROCR)

#
ROCRpred <- prediction(predictTrain, qualityTrain$PoorCare)
ROCRperf <- performance(ROCRpred, "tpr", "fpr")
plot(ROCRperf, colorize=TRUE, print.cutoffs.at=seq(0,1,0.1), text.adj=c(-0.2,1.7))


#quiz

predictTest = predict(QualityLog, type="response", newdata=qualityTest)
ROCRpredTest = prediction(predictTest, qualityTest$PoorCare)
auc = as.numeric(performance(ROCRpredTest, "auc")@y.values)
summary(auc)
auc
