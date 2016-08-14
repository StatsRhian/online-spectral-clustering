## make some data
rm(list = ls())

X = matrix(rnorm(1000*10), 1000, 10)
## make some kmeans
k = kmeans(X, 100)

n <- nrow(k$centers)
## distance matrix for centers
D1 = as.matrix(dist(k$centers))
## weighted affinity matrix for centers
W1 = (k$size%*%t(k$size))*exp(-D1^2)
deg1 = rowSums(W1)
## eigenvectors for corresponding Laplacian
sol1 = eigen((1/sqrt(deg1))%*%t(1/sqrt(deg1))*W1)$vectors
L1 <- diag(n) - (1/sqrt(deg1))%*%t(1/sqrt(deg1))*W1
e1 <- eigs_sym(L1, k = 2, sigma = 1e-10)$vectors

## make corresponding dataset with repeated cluster centers, the distance matrix, etc.

Xrep = c()

for(i in 1:length(k$size)){
  Xrep = rbind(Xrep, matrix(rep(k$centers[i,], k$size[i]), ncol = ncol(X), byrow = T))
}
n2 <- nrow(Xrep)
D2 = as.matrix(dist(Xrep))
W2 = exp(-D2^2)
deg2 = rowSums(W2)
sol2 = eigen((1/sqrt(deg2))%*%t(1/sqrt(deg2))*W2)$vectors
L2 <- diag(n2) - (1/sqrt(deg2))%*%t(1/sqrt(deg2))*W2
e2 <- eigs_sym(L2, k = 2, sigma = 1e-10)$vectors


## the solutions are the same in the sense that:
plot(rep(sol1[,2]/sqrt(k$size), k$size)/sol2[,2], ylim = c(-3,3))

plot(rep(e1[,1]/sqrt(k$size), k$size)/e2[,1], ylim = c(-3,3))
## is either a vector of ones or minus ones. This will also hold for other eigenvectors as well