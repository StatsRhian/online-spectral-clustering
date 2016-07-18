simulateMMN_fixed <- function(N, pi_vec, mu_mat, sigma_list){
  require("mixtools")
  data <- NULL
  numClust = nrow(mu_mat)
  
  for (k in 1:numClust){
    if (k == numClust){
      tempData <- rmvnorm(N - nrow(data), mu = mu_mat[k,], sigma = sigma_list[[k]])
    }
    else{tempData <- rmvnorm(floor(pi_vec[k]*N), mu = mu_mat[k,], sigma = sigma_list[[k]])}
    
    if(ncol(tempData) > 0){data <- rbind(data, tempData)}
    rm(tempData)
      }

  trueClusters <- rep(1:numClust, floor(pi_vec*N))
  if(length(trueClusters)<N){trueClusters <- c(trueClusters, rep(numClust, N - length(trueClusters)))}
  shuffleVec <- sample(nrow(data))
  data <- data[shuffleVec, ]
  trueClusters <- trueClusters[shuffleVec]
  
  return(list(data, trueClusters))
  
}