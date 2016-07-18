update_eigen_system <- function(i, j, delta_w, L, D, W, eigen, alg_rep = 2, tau = 0){
  
  eigen_new <- eigen

  u <- vector(length = dim(W)[1]); u[i] = 1; u[j] = -1
  v <- vector(length = dim(W)[1]); v[i] = 1; v[j] = 1
  delta_L <- delta_w * (u %*% t(u))
  delta_D <- delta_w * diag(v)
  
  neighbours <- which(W[i,] > tau | W[j,] > tau)
  
  for(nval in 1:length(eigen$values)){
  q <- eigen$vectors[,1]
  lambda <- eigen$values[1]
  K <- L - (lambda * D)
  
  delta_q <- vector(mode = "numeric", length = dim(W)[1])
  delta_lambda <- 0
  
  for (rep in 1:alg_rep){
    #Increment delta_lambda
    a <- (q[i] - q[j])^2 - lambda*(q[i]^2 + q[j]^2)
    b <- (q[i] - q[j])*(delta_q[i] - delta_q[j]) - lambda*(q[i]*delta_q[i] + q[j]*delta_q[j])
    c <- delta_w*(q[i]^2 + q[j]^2)
    d <- as.numeric(t(q[neighbours])  %*% D[neighbours,neighbours] %*% t(t(q[neighbours])))
    delta_lambda <- delta_w * ((a+b)/(1+c+d))
    
    #Increment delta_q
    h <- (delta_lambda*D + lambda*delta_D - delta_L)%*%q
    delta_q <- vector(mode = "numeric", length = dim(W)[1])
    delta_q[neighbours] <- as.vector(solve(t(K[,neighbours])%*% (K[,neighbours]))%*% t(K[,neighbours]) %*% h)
  }
  eigen_new$values[1] <- lambda + delta_lambda
  eigen_new$vectors[,1] <- q + delta_q
  }


  L_new <- L + delta_L
  D_new <- D + delta_D
  W_new <- W
  W_new[i,j] <-  W[i,j] + delta_w
  W_new[j,i] <-  W[j,i] + delta_w
  return(list(eigen_new = eigen_new, L_new = L_new, D_new = D_new, W_new = W_new))
}