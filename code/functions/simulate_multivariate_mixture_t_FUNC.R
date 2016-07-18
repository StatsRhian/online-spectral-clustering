simulateMultivariateMixtureT <- function(N, df, numClust, numDim, seed){
  require("mnormt")
  set.seed(seed)
  pi_vec_true <- rep(1, numClust)/numClust
  data <- NULL
  
  for (k in 1:numClust){
    if (k == numClust){
      tempData <- rmt(N - nrow(data), mean = runif(numDim, 1, 20), S = diag(numDim), df = df)
    }
    else{tempData <- rmt(floor(pi_vec_true[k]*N), mean = runif(numDim, 1, 20), S = diag(numDim), df = df)}
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
