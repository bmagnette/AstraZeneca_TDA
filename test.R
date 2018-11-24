library(TDA)
library(TDAmapper)
library(ggplot2)
library(plotly)
a<-runif(300)
a<-matrix(data=a,nrow=100,byrow = T)
plot_ly(x=a[,1], y=a[,2], z=a[,3], type="scatter3d", mode="markers", color=temp)

X <- circleUnif(200)
X<-rbind(X,matrix(runif(200,-1,1),ncol=2,byrow=F))


plot(X)
Xlim <- c(-1.6, 1.6); Ylim <- c(-1.7, 1.7); by <- 0.065
DiagGrid <- gridDiag(
  X = X, FUN = kde, h = 0.3, lim = cbind(Xlim, Ylim), by = by,
  sublevel = FALSE, library = "Dionysus", location = TRUE,
  printProgress = FALSE)
plot(DiagGrid[["diagram"]],
     main = "KDE Diagram")
data(iris)
test_iris<-gridDiag(iris[,1:4],FUN = kde,by=0.1,lim=cbind(c(4,8.5),c(1.5,5),c(0,7.5),c(0.1,2.5)),h=0.1,sublevel = FALSE, library = "Dionysus", location = TRUE,
                    printProgress = FALSE)
plot(test_iris[["diagram"]],
     main = "KDE Diagram")
