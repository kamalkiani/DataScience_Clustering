
#--------------- Installing required packages ------------

if (!require("skimr")) {
  install.packages("skimr")    
  library(skimr)              
}
if (!require("cluster")) {
  install.packages("cluster")
  library(cluster)
}
if (!require("factoextra")) {
  install.packages("factoextra") 
  library(factoextra)
}

#--------------- Loading and preparing the data set  ------------

data_name = "Credit_Card_Ready.csv"
data <- read.csv(data_name, header= TRUE)

names(data)
summary(data) 
str(data)
skim(data)

cust_id<-data[,1]
data_numeric<-data[,2:18]

plot(credit_limit ~ purchases, data = data_numeric)
plot(credit_limit ~ balance_frequency, data = data_numeric)
plot(credit_limit ~ balance, data = data_numeric)


#--------------- K-Means clustering ------------

#finding the location of a bend or a knee in the plot which is the appropriate number of clusters
fviz_nbclust(data_numeric, kmeans, method = "wss")

#Perform k-means clustering 
set.seed(123)
km.fit <- kmeans(data_numeric, 4, nstart = 30) 
km.fit$cluster
km.fit$size

#Visualize clusters 
fviz_cluster(km.fit,data_numeric)

write.csv(km.fit[["cluster"]],"cluster fit.csv", row.names = FALSE)

#--------------- Hierarchical ------------

#calculating the distance matrix using Euclidean Distance (50 data sample)
data_numeric_subset = data_numeric[1:50,]
distance <- dist(data_numeric_subset,method = "euclidean",)
print(distance)
fviz_dist(distance)

# Cluster using complete linkage
hclust.complete <- hclust(distance, method = "complete")
plot(hclust.complete,labels=data$Company)
rect.hclust(hclust.complete, 4)





# variable importance:

install.packages("FeatureImpCluster")
install.packages("flexclust")

library(FeatureImpCluster)
library(flexclust)
res <- kcca(data_numeric,k=4)
FeatureImp_res <- FeatureImpCluster(res,as.data.table(data_numeric))
plot(FeatureImp_res)
barplot(res)









