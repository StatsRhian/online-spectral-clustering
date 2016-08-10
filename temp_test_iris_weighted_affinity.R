### Check Iris data
library("rARPACK")
data <- iris[,1:4]
true <- iris[,5]

micro <- kmeans(data, 10, nstart = 20)

similarity_all(micro$centers)

A1 <- similarity_all(micro$centers)
D1 <- diag(rowSums(A1))
L1 <- D1 - A1
e1 <- eigs_sym(L1, 3, sigma = 1e-10)
a1 <- kmeans(e1$vectors, 3, nstart = 20)$cluster
a1[micro$cluster]


A2 <- (micro$size %*% t(micro$size)) * A1
D2 <- diag(rowSums(A2))
L2 <- D2 - A2
e2 <- eigs_sym(L2, 3, sigma = 1e-10)
a2 <- kmeans(e2$vectors, 3, nstart = 20)$cluster
a2[micro$cluster]

A3 <- matrix(0,sum(micro$size), sum(micro$size))

end <- cumsum(micro$size)
start <- c(1, end[-length(end)]+1)

for(i in 1:length(micro$size)){
    for (j in 1:length(micro$size)){
  A3[start[i]:end[i], start[j]:end[j]] <- similarity(micro$centers[i], micro$centers[j])
}}


D3 <- diag(rowSums(A3))
L3 <- D3 - A3
e3 <- eigs_sym(L3, 3, sigma = 1e-10)
a3 <- kmeans(e3$vectors, 3, nstart = 20)$cluster
a3[micro$cluster]

