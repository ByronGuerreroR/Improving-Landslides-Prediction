setwd("...path-to-work-directory...")
dataset <- read.csv("LandslidesDataset.csv", header = TRUE, sep=",", dec = ".")

X <- dataset[ ,c(11,10,8,9,12,13)] # PrecipAcum3
X <- dataset[ ,c(11,10,8,9,12,14)] # PrecipAcum5
X <- dataset[ ,c(11,10,8,9,12,15)] # PrecipAcum7
X <- dataset[ ,c(11,10,8,9,12,16)] # PrecipAcum10
X <- dataset[ ,c(11,10,8,9,12,17)] # PrecipAcum15
X <- dataset[ ,c(11,10,8,9,12,18)] # PrecipAcum20
X <- dataset[ ,c(11,10,8,9,12,19)] # PrecipAcum30

library(RSNNS)
Xnorm <- normalizeData(X, type='0_1')
colnames(Xnorm) <- colnames(X)
colnames(Xnorm)

library(kohonen)
grid.size.x <- 10
grid.size.y <- 10
som.grid <- somgrid(xdim = grid.size.x, ydim = grid.size.y, topo = 'hexagonal')
som.model <- som(Xnorm, grid = som.grid, rlen=200, alpha=c(0.05,0.01), keep.data = TRUE)

summary(som.model)

plot(som.model, type = 'changes')

coolBlueHotRed <- function(n, alpha = 1) {rainbow(6, end=4/6, alpha=alpha)[6:1]}

plot(som.model, type = 'counts', palette.name = coolBlueHotRed)

plot(som.model, type = 'dist.neighbours', palette.name = coolBlueHotRed)

plot(som.model, type = 'codes', palette.name = coolBlueHotRed)

library(ggdendro)
dendrograma <- hclust(dist(Xnorm, method = 'euclidean'), method = 'ward.D')
ggdendrogram(dendrograma, rotate = FALSE, labels = FALSE, theme_dendro = TRUE) + labs(title = "Dendrograma")

my_palette <- c('pink','salmon','wheat3','lightblue','khaki','wheat1')
som.cluster <- cutree(hclust(dist(som.model$codes[[1]])), 6)
plot(som.model, type = 'codes', palette.name = coolBlueHotRed, bgcol = my_palette[som.cluster], main = 'Clusters') 
add.cluster.boundaries(som.model, som.cluster)

var <- 6
var_unscaled <- aggregate(as.numeric(X[,var]), by=list(som.model$unit.classif), FUN=mean, simplify=TRUE)[,2] 
plot(som.model, type = 'property', property = var_unscaled, main = colnames(getCodes(som.model))[var], palette.name=coolBlueHotRed)
