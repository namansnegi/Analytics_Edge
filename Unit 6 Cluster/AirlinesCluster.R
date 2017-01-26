setwd("C:/Users/Бригада/Downloads/New/Analytical Edge")

airlines <- read.csv("AirlinesCluster.csv")

summary(airlines)

library(caret)

# create a normalized data frame 
preproc = preProcess(airlines)
airlinesNorm = predict(preproc, airlines)

summary(airlinesNorm)

# Hierarchical clustering algorithm
distances <- dist(airlinesNorm, method="euclidian")

airlinesClust <- hclust(distances, method="ward.D")

plot(airlinesClust)

airlinesGroups <- cutree(airlinesClust, k=5)

#How many data points are in Cluster 1?
hclustr1 <- subset(airlinesNorm, airlinesGroups==1)
nrow(hclustr1)

# Average values in each cluster.
colnames(airlines)

# standart 

tapply(airlines$Balance, airlinesGroups, mean)
tapply(airlines$QualMiles, airlinesGroups, mean)
tapply(airlines$BonusMiles, airlinesGroups, mean)
tapply(airlines$BonusTrans, airlinesGroups, mean)
tapply(airlines$FlightMiles, airlinesGroups, mean)
tapply(airlines$FlightTrans, airlinesGroups, mean)
tapply(airlines$DaysSinceEnroll, airlinesGroups, mean)

# better

colMeans(subset(airlines, airlinesGroups == 1))
colMeans(subset(airlines, airlinesGroups == 2))
colMeans(subset(airlines, airlinesGroups == 3))
colMeans(subset(airlines, airlinesGroups == 4))
colMeans(subset(airlines, airlinesGroups == 5))

# the best

lapply(split(airlines, airlinesGroups), colMeans)

# K mean
set.seed(88)
airlinesKmeans <- kmeans(airlinesNorm, iter.max=1000, 5)
airlinesKClust = airlinesKmeans$cluster
sort(table(airlinesKClust))

kclust1 <- subset(airlines, airlinesKClust == 1)
kclust2 <- subset(airlines, airlinesKClust == 2)
kclust3 <- subset(airlines, airlinesKClust == 3)
kclust4 <- subset(airlines, airlinesKClust == 4)
kclust5 <- subset(airlines, airlinesKClust == 5)

tapply(airlines$Balance, airlinesKClust, mean)
tapply(airlines$QualMiles, airlinesKClust, mean)
tapply(airlines$BonusMiles, airlinesKClust, mean)
tapply(airlines$BonusTrans, airlinesKClust, mean)
tapply(airlines$FlightMiles, airlinesKClust, mean)
tapply(airlines$FlightTrans, airlinesKClust, mean)
tapply(airlines$DaysSinceEnroll, airlinesKClust, mean)
