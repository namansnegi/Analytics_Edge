
loans = read.csv("loans.csv")
with(loans, sum(not.fully.paid))/nrow(loans)
library(mice)

summary(loans)
subset(loans, !complete.cases(loans))
with(subset(loans, !complete.cases(loans)), sum(not.fully.paid))/nrow(subset(loans, !complete.cases(loans)))


loans = read.csv("loans_imputed.csv")
set.seed(144)
split = sample.split(loans$not.fully.paid, SplitRatio = 0.7)
train = subset(loans, split == TRUE)
test = subset(loans, split == FALSE)

loansModel = glm(not.fully.paid ~ ., data=train, family="binomial")
summary(loansModel)

700 * (-9.317e-03)  - 710 * (-9.317e-03)
exp(700 * (-9.317e-03))  / exp( 710 * (-9.317e-03))

test$predicted.risk = predict(loansModel, test, type="response")
with(test, table(not.fully.paid, predicted.risk >= 0.5))
(2400+3)/nrow(test)

with(test, table(not.fully.paid, rep(FALSE, nrow(test))))

ROCRpredTest = prediction(test$predicted.risk, test$not.fully.paid)
auc = as.numeric(performance(ROCRpredTest, "auc")@y.values)
auc

loansModelInt = glm(not.fully.paid ~ int.rate, data=train, family="binomial")
summary(loansModelInt)

p = predict(loansModelInt, test, type="response")
max(p)
sum(p>=0.5)

ROCRpredTest = prediction(p, test$not.fully.paid)
auc = as.numeric(performance(ROCRpredTest, "auc")@y.values)
auc

10 * exp(0.06*3)

test$profit = exp(test$int.rate*3) - 1
test$profit[test$not.fully.paid == 1] = -1

max(10*test$profit)

highInterest = subset(test, int.rate >= 0.15)
mean(highInterest$profit)
prop.table(table(highInterest$not.fully.paid))



cutoff = sort(highInterest$predicted.risk, decreasing=FALSE)[100]
selectedLoans = subset(highInterest, predicted.risk <= cutoff)
sum(selectedLoans$profit)
table(selectedLoans$not.fully.paid)
