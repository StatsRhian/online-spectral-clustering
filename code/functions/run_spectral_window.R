run_spectral_window <- function(dataset, windowSize){
  
  source("code/global_settings.R")
  
  data <- read.csv(sprintf("data/sim_data/train/%s.csv",dataset), header = FALSE)
  trueClusters <- data[,ncol(data)]
  data <- data[ ,-ncol(data)]
  nClust <- length(unique(trueClusters))
  nDim <- ncol(data)
  N <- nrow(data)
  
  test <- read.csv(sprintf("data/sim_data/test/%s.csv",dataset), header = FALSE)
  test_trueClusters <- test[,ncol(test)]
  test <- test[ ,-ncol(test)]
  
  
  runs <- N - sizeInit
  
  assignment <- vector(length = (sizeInit + runs))
  performance <- array(NA, dim = c(floor(runs/batchSize),3))
  
  # Initialisation of microclusters for Clustream
  window <- data[1:sizeInit,]
  
  for(t in (sizeInit+1):(sizeInit+runs)){
print(t)
    window <- rbind(window[-1,], data[t,])
      
    if (t%%batchSize == 0){
      sp <- spectralClustering_unweighted(window, nClust, 8)
      
      macro_centers = NULL
      for (c in 1:nClust){
        macro_centers <- rbind(macro_centers, colSums(as.matrix(window[which(sp==c),],ncol = nDim))/sum(sp==c))
      }
      
      sim_matrix <- matrix(NA, nrow = t, ncol = nClust)
      for (i in 1:t){
        for (c in 1:nClust){
          sim_matrix[i,c] <- similarity_new_data(test[i,],macro_centers[c,])
        }
      }
      
      assignment <- apply(sim_matrix, 1, which.max)
      performance[(t-sizeInit)/batchSize,] <- calc_vmeasure_purity_numClust(assigned = assignment, labels = test_trueClusters[1:t])
      
    }
  }
  
  results <- data.frame(purity = performance[,1],
                        vmeasure = performance[,2],
                        batch_number = 1:nrow(performance),
                        stringsAsFactors = FALSE) 
  
  file_name <- sprintf("spectral_window_%s", dataset)
  write.table(results, file = sprintf("results/%s.csv",file_name), sep = ",", row.names = FALSE)
}
