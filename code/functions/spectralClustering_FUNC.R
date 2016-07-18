spectralClustering_unweighted <-  function(X, k, nn){
library(rARPACK)
n <- dim(X)[1]
ds <- as.matrix(dist(X))
s <- unlist(apply(ds, 1, function(x) sort(x)[nn]))
s[which(s==0)] <- min(s[which(s>0)])
W <- exp(-ds^2/((s%*%t(s))))
d <- rowSums(W)
L <- diag(n) - (1/sqrt(d))%*%t(1/sqrt(d))*W
e <- eigs_sym(L, k, sigma = 1e-10)
assign <- kmeans(e$vectors, k, nstart = 20)$cluster
return(assign)
}

spectralClustering_weighted <-  function(X, k, nn, clustSize){
library(rARPACK)
n <- dim(X)[1]
ds <- as.matrix(dist(X))
s <- unlist(apply(ds, 1, function(x) sort(x)[nn]))
s[which(s==0)] <- min(s[which(s>0)])
W <- exp(-ds^2/((s%*%t(s))))
W <- (clustSize %*% t(clustSize)) * W
d <- rowSums(W)
L <- diag(n) - (1/sqrt(d))%*%t(1/sqrt(d))*W
e <- eigs_sym(L, k, sigma = 1e-10)
assign <- kmeans(e$vectors, k, nstart = 20)$cluster
return(assign)
}
