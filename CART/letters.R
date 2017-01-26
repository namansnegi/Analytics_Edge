setwd("C:/Users/Бригада/Downloads/New/Analytical Edge")

letters <- read.csv("letters_ABPR.csv")

# new variable is letter B
letters$isB = as.factor(letters$letter == "B")

# splitting
set.seed(1000)
library(caTools)
spl <- sample.split(letters$isB, SplitRatio=0.5)

train <- subset(letters, spl == TRUE)
test <- subset(letters, spl == FALSE)

# What is the accuracy of this baseline method on the test set?
t <- table(test$isB)
t
t[1] / (t[1] + t[2])

# PREDICTING B OR NOT B
# Classification Tree (is B or not B)
CARTb = rpart(isB ~ . - letter, data=train, method="class")
predClassTree <- predict(CARTb, newdata=test, type="class")

t <- table(test$isB, predClassTree)
t
# acc
(t[1] + t[4]) / nrow(test)

# Random forest
set.seed(1000)
library(randomForest)
# Build random forest model
letterForest = randomForest(isB ~ .-letter, data=train)

predForrestB <- predict(letterForest, newdata=test)
t <- table(test$isB, predForrestB)
t
#acc
(t[1] + t[4]) / nrow(test)

# Now go to ORIGINAL PROBLEM: whether or not a letter is one of the four letters A, B, P or R.
# factorization
letters$letter = as.factor( letters$letter )

#new split
set.seed(2000)
split <- sample.split(letters$letter, SplitRatio=0.5)

Train <- subset(letters, split == TRUE)
Test <- subset(letters, split == FALSE)

table(Test$letter)
401 / nrow(Test)

# Predicting

CARTletters <- rpart(letter ~ .-isB, data = Train, method="class")
PredLetters <- predict(CARTletters, newdata=Test, type="class")
t <- table(Test$letter, PredLetters)
t
# acc
(sum(diag(t)))/ nrow(Test)

# RANDOM FOREST
Forestletters <- randomForest(letter ~ .-isB, data = Train)
PredForestletters <- predict(Forestletters, newdata = Test)
t <- table(Test$letter, PredForestletters)
t
(sum(diag(t)))/ nrow(Test)

