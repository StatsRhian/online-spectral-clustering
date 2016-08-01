run_spectral_window <- function(dataset, windowSize, nRepeats = 1){
  
  source("code/global_settings.R")
  
for (currentRepeat in 1:nRepeats){  
#Generate train data
source("code/generate_simulated_train.R")
runs <- 50#N - sizeInit
  
assignment <- vector(length = (sizeInit + runs))
performance <- array(NA, dim = c(floor(runs/batchSize),3))
  
# Initialisation of microclusters for Clustream
  window <- data[1:sizeInit,]
  
  for(t in (sizeInit+1):(sizeInit+runs)){
  print(t)
    window <- rbind(window[-1,], data[t,])
      
    if (t%%batchSize == 0){
      sp <- spectralClustering_unweighted(window, nClust, 8)
      
      #Generate test data
      source("code/generate_simulated_test.R")
      
      linked_test <- as.numeric(apply(test_data, 1, FIND_closest_microcluster, window))
      assignment <- sp[linked_test]
      performance[(t-sizeInit)/batchSize,] <- calc_vmeasure_purity_numClust(assigned = assignment, labels = test_trueClusters)
      
    }
  }
  
  results <- data.frame(purity = performance[,1],
                        vmeasure = performance[,2],
                        batch_number = 1:nrow(performance),
                        stringsAsFactors = FALSE) 
  
  file_name <- sprintf("spectral_window_%s_%i_of_%i", dataset, currentRepeat, nRepeats)
  write.table(results, file = sprintf("results/%s.csv",file_name), sep = ",", row.names = FALSE)
}
}
