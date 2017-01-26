# How many parolees are contained in the dataset?
parole <- read.csv("parole.csv")

nrow(parole)

table(parole$violator == 1)

str(parole)

summary(parole$state)
summary(as.factor(parole$state))

# SPLITTING INTO A TRAINING AND TESTING SET  
set.seed(144)
library(caTools)
split = sample.split(parole$violator, SplitRatio = 0.7)
train = subset(parole, split == TRUE)
test = subset(parole, split == FALSE)

# BUILDING A LOGISTIC REGRESSION MODEL  
str(train)

#train$male <- as.factor(train$male)
#train$race <- as.factor(train$race)
train$state <- as.factor(train$state)
#train$multiple.offenses <- as.factor(train$multiple.offenses)
train$crime <- as.factor(train$crime)
#train$violator <- as.factor(train$violator)
test$state <- as.factor(test$state)
test$crime <- as.factor(test$crime)

model1 <- glm(violator ~ ., data=train, family=binomial)
summary(model1)

x <- data.frame(
        male = 1,
        race = 1,
        age = 50,
        state = as.factor(1),
        time.served = 3,
        max.sentence = 12,
        multiple.offenses = 0,
        crime = as.factor(2)
)

pred1 <- predict(model1, type="response", newdata=x)
pred1/(1 - pred1)

pred2 <- predict(model1, type="response", newdata=test)
max(pred2)

table(test$violator, pred2 > 0.5)

sens = (12) / (12 + 11)
sens

spec = (167) / (167 + 12)
spec

acc = (167 + 12)/ nrow(test)
acc

table(test$violator, rep(FALSE, nrow(test)))
accsimple = 179 / (179 + 23)
accsimple

# The board assigns more cost to a false negative than a false 
#positive, and should therefore use a logistic regression cutoff less than 0.5. 

# EVALUATING THE MODEL ON THE TESTING SET
library(ROCR)

ROCRpredTest = prediction(pred2, test$violator)
auc = as.numeric(performance(ROCRpredTest, "auc")@y.values)
summary(auc)
auc
