library(rARPACK)

n1 = 1; n2 = 2

A <- matrix(c(1,0.5,0.5,1),2,2)
d <- rowSums(A)
L <- (1/sqrt(d)) %*% t(1/sqrt(d))* A
e <- eigen(L)$vectors

s <- c(n1,n2)
Astar <- s%*%t(s) * A
dstar <- rowSums(Astar)
Lstar <- (1/sqrt(dstar)) %*% t(1/sqrt(dstar))* Astar
estar <- eigen(Lstar)$vectors

Arep <- matrix(0,(n1+n2),(n1+n2))  
Arep[1:n1,1:n1] <- A[1,1]
Arep[1:n1,(n1+1):(n1+n2)] <- A[1,2]
Arep[(n1+1):(n1+n2),1:n1] <- A[1,2]
Arep[(n1+1):(n1+n2),(n1+1):(n1+n2)] <- A[2,2]

drep <- rowSums(Arep)
Lrep <- (1/sqrt(drep)) %*% t(1/sqrt(drep))* Arep
erep <- eigen(Lrep)$vectors


plot(rep(estar[,2]/sqrt(s), s)/erep[,2], ylim = c(-3,3))
