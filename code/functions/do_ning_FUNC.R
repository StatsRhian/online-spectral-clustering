#Run Ning Spectral Clustering
do_ning <- function(t, L, D, W, eigen, data, tau = 0){

  x <- as.numeric(data[t,])
  L_new <- matrix(0,t,t)
  L_new[1:(t-1), 1:(t-1)] <- L
  D_new <- matrix(0, t, t)
  D_new[1:(t-1),1:(t-1)] <- D
  W_new <- matrix(0, t, t)
  W_new[1:(t-1), 1:(t-1)] <- W
  L <- L_new; W <- W_new; D <- D_new
  eigen$vectors <- rbind(eigen$vectors,0)
  
  sim_x <-  similarity_new_data(data[1:(t-1),], x)
  
  for (p in 1:(t-1)){
    out <- update_eigen_system(i = p, j = t, delta_w = sim_x[p], L, D, W, eigen, alg_rep = 2, tau = tau)
    eigen <- out$eigen_new
    L <- out$L_new
    D <- out$D_new
    W <- out$W_new
  }
  return(list(L = L, D = D, W = W, eigen = eigen, data = data))
  
}
