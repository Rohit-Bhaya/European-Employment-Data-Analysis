##Packages used
library(ggplot2)      
library(dplyr)
library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms
library(factoextra) # clustering algorithms & visualization
library(dendextend) # for comparing two dendrograms
library(corrplot)

##Data Exploration

###Data Import
employment <- read.csv("employment.txt", sep="\t")

#get country name and scale variables
row.names(employment)<-employment$Country
employment <- employment[,-1]

###Data Structure
glimpse(employment)

###Data Summary
summary(employment)
employment <- scale(employment)

###Checking for Correlation
corrplot(cor(employment), order = "hclust")

###Distance Matrix

#distance matrix
distance <- get_dist(employment)
fviz_dist(distance, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))

##k-Means Clustering
#kmeans with diff k vals
k2 <- kmeans(employment, centers = 2, nstart = 25)
k3 <- kmeans(employment, centers = 3, nstart = 25)
k4 <- kmeans(employment, centers = 4, nstart = 25)
k5 <- kmeans(employment, centers = 5, nstart = 25)

# plots to compare
p1 <- fviz_cluster(k2, geom = "point",  data = employment) + ggtitle("k = 2")
p2 <- fviz_cluster(k3, geom = "point",  data = employment) + ggtitle("k = 3")
p3 <- fviz_cluster(k4, geom = "point",  data = employment) + ggtitle("k = 4")
p4 <- fviz_cluster(k5, geom = "point",  data = employment) + ggtitle("k = 5")

library(gridExtra)
grid.arrange(p1, p2, p3, p4, nrow = 2)

###Selecting optimal number of clusters

#selecting number of clusters
set.seed(12420246)
fviz_nbclust(employment, kmeans, method = "wss")
fviz_nbclust(employment, kmeans, method = "silhouette")

###k-means with 3 clusters

#creating final kmeans model
set.seed(12420246)
final <- kmeans(employment, 3, nstart = 25)

###Profiling the clusters

#comparing cluster wise
k_means_cluster <- cbind(employment, kmeans_cluster = final$cluster)
cluster <- aggregate(k_means_cluster,by=list(final$cluster),mean)
cluster

##Hierarchical Clustering

# Dissimilarity matrix
d <- dist(employment, method = "euclidean")
# Hierarchical clustering using Complete Linkage
hc1 <- hclust(d, method = "complete" )
# Plot the obtained dendrogram
plot(hc1, cex = 0.6, hang = -1)

###Selecting optimal number of clusters

#finding number of clusters
fviz_nbclust(employment, FUN = hcut, method = "wss")
fviz_nbclust(employment, FUN = hcut, method = "silhouette")

#creating final model
seed.3clust = cutree(hc1,k=2)
table(seed.3clust)

###Visual Analysis

#plots
plot(hc1, cex = 0.6, hang=-1)
rect.hclust(hc1, k = 2, border = 2:5)
fviz_cluster(list(data = employment, cluster = seed.3clust))
