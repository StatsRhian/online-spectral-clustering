library(rARPACK)

A <- matrix(c(1,0.5,0.5,1),2,2)
d <- rowSums(A)
L <- (1/sqrt(d)) %*% t(1/sqrt(d))* A
e <- eigen(L)$vectors

s <- c(3,2)
Astar <- s%*%t(s) * A
dstar <- rowSums(Astar)
Lstar <- (1/sqrt(dstar)) %*% t(1/sqrt(dstar))* Astar
estar <- eigen(Lstar)$vectors

Arep <- matrix(0,5,5)  
Arep[1:3,1:3] <- A[1,1]
Arep[1:3,4:5] <- A[1,2]
Arep[4:5,1:3] <- A[1,2]
Arep[4:5,4:5] <- A[2,2]

drep <- rowSums(Arep)
Lrep <- (1/sqrt(drep)) %*% t(1/sqrt(drep))* Arep
erep <- eigen(Lrep)$vectors


plot(rep(estar[,2]/sqrt(s), s)/erep[,2], ylim = c(-3,3))
