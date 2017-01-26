# VIDEO 2

# Read in data
baseball = read.csv("baseball.csv")
str(baseball)

# Subset to only include moneyball years
moneyball = subset(baseball, Year < 2002)
str(moneyball)

# Compute Run Difference
moneyball$RD = moneyball$RS - moneyball$RA
str(moneyball)

# Scatterplot to check for linear relationship
plot(moneyball$RD, moneyball$W)
cor(moneyball$RD, moneyball$W)

# Regression model to predict wins
WinsReg = lm(W ~ RD, data=moneyball)
summary(WinsReg)

#W >= 95 => beta0 + beta1*RD >= 95 => RD should be:
(95 - as.numeric(WinsReg$coef[1])) / as.numeric(WinsReg$coef[2])

# quiz
# RS = 713 RA = 614 => RD = 99, then how many gaems expect to win
# Wins = beta0 + beta1*RD

as.numeric(WinsReg$coef[1]) + as.numeric(WinsReg$coef[2])*99


# VIDEO 3

str(moneyball)

# Regression model to predict runs scored
RunsReg = lm(RS ~ OBP + SLG + BA, data=moneyball)
summary(RunsReg)

RunsReg = lm(RS ~ OBP + SLG, data=moneyball)
summary(RunsReg)

# Quiz
as.numeric(RunsReg$coef[1]) + as.numeric(RunsReg$coef[2]) * 0.311 + as.numeric(RunsReg$coef[3]) * 0.405


fit <- lm(RA ~ OOBP + OSLG, data = moneyball)
summary(fit)

as.numeric(fit$coef[1]) + as.numeric(fit$coef[2]) * 0.297 + as.numeric(fit$coef[3]) * 0.370

# Quck quiz

# We would select Jeremy Giambi and Carlos Pena, since they give 
#  the highest contribution to Runs Scored.
# We would not select Eric Chavez, since his salary consumes our entire
#  budget, and although he has the highest SLG, there are players with better OBP.
# We would not select Frank Menechino since even though he has a 
#  high OBP, his SLG is low.
# We would not select Greg Myers since he is dominated by Carlos 
#  Pena in OBP and SLG, but has a much higher salary.
players = data.frame(
        Name = c("Eric", "Jeremy", "Frank", "Greg", "Carlos"),
        OBP  = c(0.338, 0.391, 0.369, 0.313, 0.361),
        SLG  = c(0.540, 0.450, 0.374, 0.447, 0.500),
        Salary = c(1.4, 1.065, 0.295, 0.8, 0.3))
players = cbind(players, p = predict(RunsReg, players))
players

#players = merge(players, players, by = NULL)
#players$total = players$p.x + players$p.y
#players = players[(players$Name.x!=players$Name.y) & (players$Salary.x + players$Salary.y)<=1.5,]
#head(players[order(-players$total),c("Name.x", "Name.y")],1)

# world series

teamRank = c(1,2,3,3,4,4,4,4,5,5)
wins2012 <- c(94, 88, 95, 88, 93, 94, 98, 97, 93, 94)
wins2013 <- c(97, 97, 92, 93, 92, 96, 94, 96, 92, 90)

cor(teamRank, wins2013)
