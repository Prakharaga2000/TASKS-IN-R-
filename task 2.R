#IMPORTING THE DATA SET
 library(readxl)
 data <- read_excel("C:/Users/prakhar/OneDrive/Desktop/Iris.xlsx")
 View(data)
#to see the structure of the data
 str(data)
#to view how many no. of species are  there
 unique(data$Species)
#As K means is unsupervised learning model we use only unlabelled data
 data1<- select(data,c(1,2,3,4))
head(data1,6 )
#to remove any missing observations and to scale the data
data2<- scale(na.omit(data1))
data2
#to find th evalue of k in kmeans clustering 
l<-c(1:15)
for(i in 1:15){
  l[i]<-sum(kmeans(data2,i,iter.max = 300,nstart=10)$withinss)
}
#to plot the i vs wss plot
plot(c(1:15),l,xlab = "no.of clusters",ylab = "wss",main = "k vs reduction in variance graph",type = "b")
#from the graph it is clear that elbow point is at 3 therefore no.of clusters = 3
#kmeans clustering
clust<-kmeans(data2,3,iter.max = 300,nstart=10)
str(clust)
#to add cluster type to the original data
data<-cbind(data,ClusterType = clust$cluster)
data
#plotting the clusters visually
install.packages("factoextra")
library(factoextra)
fviz_cluster(clust,data2,palette=c(1,2,3),geom = "point",ellipse.type = "convex",ggtheme = theme_bw())
