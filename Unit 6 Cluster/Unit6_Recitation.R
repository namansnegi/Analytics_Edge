#Image Segmentation

setwd("C:/Users/Бригада/Downloads/New/Analytical Edge")

# data == matrix of intensity values
flower = read.csv("flower.csv", header=FALSE)
str(flower)

# Change the data type to matrix
flowerMatrix = as.matrix(flower)
str(flowerMatrix)
#50 rows and columns => 50x50 pixels image

# Turn matrix into a vector
flowerVector = as.vector(flowerMatrix)
str(flowerVector)

# can't convert data frame to vector imidiatly
flowerVector2 = as.vector(flower)
str(flowerVector2)

# Compute distances
distance = dist(flowerVector, method = "euclidean")

# Hierarchical clustering
clusterIntensity = hclust(distance, method="ward.D")

# Plot the dendrogram
plot(clusterIntensity)

# Select 3 clusters
# plot rectangles around clusters
rect.hclust(clusterIntensity, k = 3, border = "red")

# split the data
flowerClusters = cutree(clusterIntensity, k = 3)
# factor with values corresponde to each cluster
flowerClusters

# Find mean intensity values
tapply(flowerVector, flowerClusters, mean)

# Plot the image and the clusters
# return to matrix as initially was
dim(flowerClusters) = c(50,50)
image(flowerClusters, axes = FALSE)

# Original image
image(flowerMatrix,axes=FALSE,col=grey(seq(0,1,length=256)))

#############

# MRI image of the brain
healthy = read.csv("healthy.csv", header=FALSE)
healthyMatrix = as.matrix(healthy)
str(healthyMatrix)

# Plot image
image(healthyMatrix,axes=FALSE,col=grey(seq(0,1,length=256)))

# Hierarchial clustering
healthyVector = as.vector(healthyMatrix)
distance = dist(healthyVector, method = "euclidean")

# We have an error - why?
str(healthyVector)
# too much data
n <- 365636
n*(n-1)/2

# Let's try K-means
# Specify number of clusters
k = 5

# Run k-means
set.seed(1)
KMC = kmeans(healthyVector, centers = k, iter.max = 1000)
str(KMC)

# Extract clusters
healthyClusters = KMC$cluster
KMC$centers[2]

# Plot the image with the clusters
dim(healthyClusters) = c(nrow(healthyMatrix), ncol(healthyMatrix))

image(healthyClusters, axes = FALSE, col=rainbow(k))


# Apply to a test image
 
tumor = read.csv("tumor.csv", header=FALSE)
tumorMatrix = as.matrix(tumor)
tumorVector = as.vector(tumorMatrix)

# Apply clusters from before to new image, using the flexclust package
install.packages("flexclust")
library(flexclust)

KMC.kcca = as.kcca(KMC, healthyVector)
tumorClusters = predict(KMC.kcca, newdata = tumorVector)

# Visualize the clusters
dim(tumorClusters) = c(nrow(tumorMatrix), ncol(tumorMatrix))

image(tumorClusters, axes = FALSE, col=rainbow(k))

