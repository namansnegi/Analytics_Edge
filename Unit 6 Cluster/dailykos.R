setwd("C:/Users/Бригада/Downloads/New/Analytical Edge")

dailykos <- read.csv("dailykos.csv")

nrow(dailykos)

# Compute distances
distances <- dist(dailykos, method="euclidian")

kosClust = hclust(distances, method="ward.D")

plot(kosClust)

clusterGroups = cutree(kosClust, k = 7)

# subset our data by cluster
hcluster1 = subset(dailykos, clusterGroups==1)
hcluster2 = subset(dailykos, clusterGroups==2)
hcluster3 = subset(dailykos, clusterGroups==3)
hcluster4 = subset(dailykos, clusterGroups==4)
hcluster5 = subset(dailykos, clusterGroups==5)
hcluster6 = subset(dailykos, clusterGroups==6)
hcluster7 = subset(dailykos, clusterGroups==7)

nrow(hcluster3)
nrow(hcluster1)
nrow(hcluster2)
nrow(hcluster3)
nrow(hcluster4)
nrow(hcluster5)
nrow(hcluster6)
nrow(hcluster7)

tail(sort(colMeans(hcluster1)))

tail(sort(colMeans(hcluster2)))

tail(sort(colMeans(hcluster5)))

tail(sort(colMeans(hcluster7)))

set.seed(1000)
kosKmeans = kmeans(dailykos, 7)
kosCluster = kosKmeans$cluster
sort(table(kosCluster))
kcluster1 = subset(dailykos, kosCluster==1)
kcluster2 = subset(dailykos, kosCluster==2)
kcluster3 = subset(dailykos, kosCluster==3)
kcluster4 = subset(dailykos, kosCluster==4)
kcluster5 = subset(dailykos, kosCluster==5)
kcluster6 = subset(dailykos, kosCluster==6)
kcluster7 = subset(dailykos, kosCluster==7)

tail(sort(colMeans(kcluster3)))
tail(sort(colMeans(kcluster2)))

table(clusterGroups, kosCluster)
