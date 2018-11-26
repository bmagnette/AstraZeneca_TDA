library(TDA)
library(TDAmapper)
library(ggplot2)
library(plotly)
library(igraph)

a<-runif(300)
a<-matrix(data=a,nrow=100,byrow = T)
plot_ly(x=a[,1], y=a[,2], z=a[,3], type="scatter3d", mode="markers", color="blue")

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
             num_intervals = 6,
             percent_overlap = 10,
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


Y<-torusUnif(n=1000,a = 0.5,c=1)
plot_ly(x=Y[,1],y=Y[,2],z=Y[,3],type="scatter3d", mode="markers")
torus_diag=gridDiag(Y,lim=cbind(c(-2,2),c(-2,2),c(-1,1)),FUN = distFct,by=0.3,h=0.2)
plot(torus_diag[["diagram"]],main)
                    