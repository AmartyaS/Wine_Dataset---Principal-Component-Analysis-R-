# Reading the Data-set file
file <- read.csv(file.choose())

# Applying Principal Component Analysis
pca <- princomp(file[,-1],cor=TRUE,scores = TRUE,covmat = NULL)
loadings(pca) 
pca$scores 
plot(pca)
summary(pca)

# Extracting the top 3 PCAs
pca$scores[,1:3]
file <- cbind(file,pca$scores[,1:3])

# H-Clustering
data <- pca$scores[,1:3]
d <- dist(data,method = "euclidean")
fit <- hclust(d,method = "complete")
fit
plot(fit,hang=-1)
group <- cutree(fit,k=3)
group
rect <- rect.hclust(fit,k=3,border = "red")
rect
H_cluster <- as.matrix(group)
file <- cbind(file,H_cluster)
View(file)

## K-Means Clustering
install.packages("plyr")
library("plyr")
install.packages("kselection")
library("kselection")
install.packages("factoextra")
library("factoextra")

k <- kselection(data)
k
fviz_nbclust(data,kmeans,method = "wss")+labs(subtitle = "Elbow Curve method")
KMeans_Cluster <- kmeans(data,3)
str(KMeans_Cluster)
K_Cluster <- KMeans_Cluster$cluster
aggregate(data,by=list(KMeans_Cluster$cluster),FUN=mean)
file <- cbind(file,K_Cluster)
View(file)
