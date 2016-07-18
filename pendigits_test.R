#pendigits test
dataset <- ("uci_pendigits_18")
source("code/global_settings.R")

data <- read.csv(sprintf("data/%s.csv",dataset), header = FALSE)
trueClusters <- data[,ncol(data)]
data <- data[ ,-ncol(data)]
nClust <- length(unique(trueClusters))
nDim <- ncol(data)
N <- nrow(data)

runs <- N - sizeInit

spectralClustering_unweighted(data, nClust)
