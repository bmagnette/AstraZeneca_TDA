# install.packages("TDA")
# install.packages("devtools")
#devtools::install_github("paultpearson/TDAmapper")

library(devtools)
library(TDA)
library(TDAmapper)

data <- read.table("data/Iris.csv", header=TRUE, sep=",", dec=".", quote = "\"", na.strings = "NA")

# Distance functions and density estimators

# Density clustering example

# Persistant homology example