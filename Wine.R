getwd()
setwd("C:/Users/Бригада/Downloads/New/Analytical Edge")

wine <- read.csv("wine.csv")
wineTest <- read.csv("wine_test.csv")

cor(wine)
cor(wine$HarvestRain, wine$WinterRain)

model4 <- lm(Price ~ WinterRain + AGST + HarvestRain + Age, data = wine)
summary(model4)

#add test
predictTest <- predict(model4, newdata = wineTest)
predictTest

SSE <- sum((wineTest$Price - predictTest)^2)
SST <- sum((wineTest$Price - mean(wine$Price))^2)
1 - SSE/SST
