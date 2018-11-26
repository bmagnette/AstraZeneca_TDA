# This file is focusing on PCA and persistant homology for iris data
library(devtools)
library(TDA)
library(TDAmapper)
library(ggplot2)
library("FactoMineR")
library("factoextra")

# Read data
data <- read.table("data/Iris.csv", header=TRUE, sep=",", dec=".", quote = "\"", na.strings = "NA")
data <- data[, 2:5] # Pick numerical data

# PCA With normalized data
res.pca <- PCA(data, scale.unit = TRUE, ncp = 2, graph = TRUE)

# Visualization
fviz_eig(res.pca, addlabels = TRUE, ylim = c(0, 80))
fviz_pca_ind(res.pca,
             geom.ind = "point",
             col.ind = iris$Species,
             palette = c("#00AFBB", "#E7B800", "#FC4E07"),
             addEllipses = TRUE,
             legend.title = "Groups")

################################################
# Persistant homology over a grid + differents distance
Xlim <- c(min(res.pca$ind$coord[,1]), max(res.pca$ind$coord[,1]))
Ylim <- c(min(res.pca$ind$coord[,2]), max(res.pca$ind$coord[,2]))
by <- 0.05
Xseq <- seq(from = Xlim[1], to = Xlim[2], by = by)
Yseq <- seq(from = Ylim[1], to = Ylim[2], by = by)
Grid <- expand.grid(Xseq, Yseq)

# Distance functions and density estimators
distance <- distFct(X = res.pca$ind$coord, Grid = Grid)
DTM <- dtm(X = res.pca$ind$coord, Grid = Grid, m0 = 0.1)
kNN <- knnDE(X = res.pca$ind$coord, Grid = Grid, k = 60)
KDE <- kde(X = res.pca$ind$coord, Grid = Grid, h = 0.3)
Kdist <- kernelDist(X = res.pca$ind$coord, Grid = Grid, h = 0.3)
  
#####
# ! # We must change FUN argument depending on selected distance
#####

# Persistant homology example
Diag <- gridDiag(X = res.pca$ind$coord, FUN = kde, lim = cbind(Xlim, Ylim), by = by,
                 sublevel = FALSE, library = "Dionysus", printProgress = FALSE, h = 0.3)

# Bootstrap for band
band <- bootstrapBand(X = res.pca$ind$coord, FUN = kde, Grid = Grid, B = 100,
                      parallel = FALSE, alpha = 0.1, h = 0.3)

# Plotting persistence diagram
par(mfrow = c(1,3))
plot(res.pca$ind$coord, main = "Iris Sample X")
persp(x = Xseq, y = Yseq,
      z = matrix(KDE, nrow = length(Xseq), ncol = length(Yseq)),
      xlab = "", ylab = "", zlab = "", theta = -20, phi = 35, scale = FALSE,
      expand = 3, col = "red", border = NA, ltheta = 50, shade = 0.9,
      main = "Distance Selected : ")
plot(x = Diag[["diagram"]], band = 2 * band[["width"]], main = "KDE Diagram")
