library(TDA)
install.packages("TDAmapper", dependencies=TRUE)
library(TDAmapper)
library(ggplot2)
library(plotly)
library(igraph)
library(devtools)
devtools::install_github("paultpearson/TDAmapper")
library(TDAmapper)
a<-runif(300)
a<-matrix(data=a,nrow=100,byrow = T)
plot_ly(x=a[,1], y=a[,2], z=a[,3], type="scatter3d", mode="markers", color=temp)
X <- circleUnif(200)
X<-rbind(X,matrix(runif(200,-1,1),ncol=2,byrow=F))
Xd<-X[,1]
Yd<-X[,2]
circledf<-data.frame(Xd,Yd)
p<-ggplot(data = circledf,aes(x = Xd,y=Yd))+geom_point()
p
plot(X)
distances=dist(X)
mapp<-mapper1D(distance_matrix = distances,
             filter_values = svd(X)$u[,1],
             num_intervals = 4,
             percent_overlap = 20,
             num_bins_when_clustering = 5)
First.Example.graph <- graph.adjacency(mapp$adjacency, mode="undirected")
plot(First.Example.graph)
Xlim <- c(-1.6, 1.6); Ylim <- c(-1.7, 1.7); by <- 0.065
DiagGrid <- gridDiag(
  X = X, FUN = kde, h = 0.3, lim = cbind(Xlim, Ylim), by = by,
  sublevel = FALSE, library = "Dionysus", location = TRUE,
  printProgress = FALSE)
plot(DiagGrid[["diagram"]],
     main = "")
plot(DiagGrid[["diagram"]],main="",barcode = T)
data(iris)

test_iris<-gridDiag(iris[,1:4],FUN = kde,by=0.5,lim=cbind(c(4,8.5),c(1.5,5),c(0,7.5),c(0.1,2.5)),h=0.4,sublevel = FALSE, library = "Dionysus", location = TRUE,
                    printProgress = FALSE)
plot(test_iris[["diagram"]],
     main = "KDE Diagram")
iris_dist=dist(iris[,1:4])
mapp_iris<-mapper1D(distance_matrix = iris_dist,
               filter_values = svd(iris[,1:4])$u[,1],
               num_intervals = 6,
               percent_overlap = 10,
               num_bins_when_clustering = 5)
iris_graph <- graph.adjacency(mapp_iris$adjacency, mode="undirected")
plot(iris_graph)

Y<-torusUnif(n=500,a = 0.5,c=1)
plot_ly(x=Y[,1],y=Y[,2],z=Y[,3],type="scatter3d", mode="markers")
torus_diag=gridDiag(Y,lim=cbind(c(-2.5,2.5),c(-2.5,2.5),c(-1.5,1.5)),FUN = distFct,by=00.1)
torus_diag=ripsDiag(X = Y,maxdimension = 2,dist = "euclidean",maxscale = 1)
plot(torus_diag[["diagram"]],main="")
plot(torus_diag[["diagram"]],main="",barcode = T)
data("faithful")
faithful_dist=dist(faithful)
mapp_faith=mapper1D(distance_matrix = faithful_dist,
                       filter_values = svd(faithful)$u[,1],
                       num_intervals = 6,
                       percent_overlap = 10,
                       num_bins_when_clustering = 5)
faithful_graph <- graph.adjacency(mapp_faith$adjacency, mode="undirected")
plot(faithful_graph)
plot(faithful)


library(openxlsx)
data_df<-read.xlsx(xlsxFile = "data.xlsx")
names<-data_df[,1]
data_df<-data_df[,2:38]

#t<-rowSums(apply(apply(data_df,2,as.numeric),2,is.nan))
#t<-t!=0
#plot(data_df)
#astra_clean<-data_df[t==0,]

#astra_clean<-astra_clean[,-c(19,37)]
#astra_clean<-apply(astra_clean,2,as.numeric)
astra_clean<-data_df[-9,]
astra_clean<-apply(astra_clean,2,as.numeric)
astra_dist<-dist(astra_clean)

ast

astra_graph=mapper1D(distance_matrix = astra_dist,
                     filter_values = astra_clean[,1],
                     num_intervals = 10,
                     percent_overlap = 80,
                     num_bins_when_clustering = 10)
astra_graph_plot <- graph.adjacency(astra_graph$adjacency, mode="undirected")
plot(astra_graph_plot)
vertex.size <- rep(0,astra_graph$num_vertices)
for (i in 1:astra_graph$num_vertices){
  points.in.vertex <- astra_graph$points_in_vertex[[i]]
  vertex.size[i] <- length((astra_graph$points_in_vertex[[i]]))
}
y.mean.vertex <- rep(0,astra_graph$num_vertices)
for (i in 1:astra_graph$num_vertices){
  points.in.vertex <- astra_graph$points_in_vertex[[i]]
  y.mean.vertex[i] <-mean((astra_clean$[points.in.vertex]))
}
y.mean.vertex.grey <- grey(1-(y.mean.vertex - min(y.mean.vertex))/(max(y.mean.vertex) - min(y.mean.vertex) ))
V(astra_graph_plot)$color <- y.mean.vertex.grey
V(astra_graph_plot)$size <- vertex.size
plot(astra_graph_plot,main ="Mapper Graph")
legend(x=-2, y=-1, c("y small","y medium","large y"),pch=21,
       col="#777777", pt.bg=grey(c(1,0.5,0)), pt.cex=2, cex=.8, bty="n", ncol=1)





l=length(V(astra_graph_plot))
vertex.size<-rep(0,l)
for(i in 1:l){
  points.in.vertex <- astra_graph$points_in_vertex[[i]]
  vertex.size[i] <- length((astra_graph$points_in_vertex[[i]]))
}

col<-topo.colors(length(astra_graph$points_in_level),alpha=1)
V(astra_graph_plot)$color=col[1]
colValue = vector(mode="numeric",samplesize)
colValue[] = 0
gridDiag(iris[,1:4],FUN = kde,by=0.5,lim=cbind(),h=0.4,sublevel = FALSE, library = "Dionysus", location = TRUE,
         printProgress = FALSE)


minimum<-sapply(apply(astra_clean,2,min),as.numeric)
maximum<-sapply(apply(astra_clean,2,max),as.numeric)    
minimum<-apply(apply(astra_clean,2,as.numeric),2,min)
maximum<-apply(apply(astra_clean,2,as.numeric),2,max)
limits=rbind(minimum,maximum)

a=ripsDiag(
  X=astra_clean, maxdimension=3, maxscale=45, dist = "euclidean",
  library = "GUDHI", location = FALSE, printProgress = FALSE)
plot(a[["diagram"]])
library(onion)
install.packages(c("rgl", "car"))
bunny<-onion::bunny
bunny_sample<-bunny[sort(sample(1:length(bunny[,1]),500,replace=FALSE)),]
plot_ly(x=bunny_sample[,1],y=bunny_sample[,2],z=bunny_sample[,3],type="scatter3d")
bunny_persistance<-ripsDiag(X = bunny_sample,maxdimension = 2,dist = "euclidean",maxscale = 0.1)
plot(bunny_persistance[["diagram"]],main="")