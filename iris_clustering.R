#iris hca
library(clusterSim)
library(cluster)
library(ClusterR)

#load data
iris <- read.table("data/Iris.csv", header=TRUE, sep=",", dec=".", quote = "\"", na.strings = "NA")

label <- iris[,6]
iris2 <-iris[,-6]
label
iris2

diss=daisy(iris2)
hca <- hclust(diss, method = "complete", members = NULL)
plot(hca)
hcacut<-cutree(hca,3)
table(hcacut,label)

s = silhouette(hcacut,diss)
plot(s) 