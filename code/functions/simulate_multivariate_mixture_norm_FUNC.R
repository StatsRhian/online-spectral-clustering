simulateMultivariateMixtureNorm <- function(N, numClust, numDim, seed){
  require("mixtools")
  set.seed(seed)
  pi_vec_true <- rep(1, numClust)/numClust
  data <- NULL
  
  for (k in 1:numClust){
    if (k == numClust){
      tempData <- rmvnorm(N - nrow(data), mu = runif(numDim, 1, 20), sigma = diag(numDim))
    }
    else{tempData <- rmvnorm(floor(pi_vec_true[k]*N), mu = runif(numDim, 1, 20), sigma = diag(numDim))}
    data <- rbind(data, tempData)
    rm(tempData)
  }
  
  trueClusters <- rep(1:numClust, floor(pi_vec_true*N))
  if(length(trueClusters)<N){trueClusters <- c(trueClusters, rep(numClust, N - length(trueClusters)))}
  
  shuffleVec <- sample(nrow(data))
  data <- data[shuffleVec, ]
  trueClusters <- trueClusters[shuffleVec]
  
  return(list(data, trueClusters))
}