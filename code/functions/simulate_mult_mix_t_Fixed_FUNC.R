simulateMMT_fixed<- function(N, pi_vec, mu_mat, sigma_list, df){
  require("mnormt")
  data <- NULL
  numClust = nrow(mu_mat)
  
  for (k in 1:numClust){
    if (k == numClust){
      tempData <- rmt(N - nrow(data), mean = mu_mat[k, ], S = sigma_list[[k]], df = df)
    }
    else{tempData <- rmt(floor(pi_vec[k]*N), mean = mu_mat[k,], S = sigma_list[[k]], df = df)}
    
    if(ncol(tempData) > 0){data <- rbind(data, tempData)}
    rm(tempData)
  }
  
  trueClusters <- rep(1:numClust, floor(pi_vec_true*N))
  if(length(trueClusters) < N){trueClusters <- c(trueClusters, rep(numClust, N - length(trueClusters)))}
  shuffleVec <- sample(nrow(data))
  data <- data[shuffleVec, ]
  trueClusters <- trueClusters[shuffleVec]
  
  return(list(data, trueClusters))
}