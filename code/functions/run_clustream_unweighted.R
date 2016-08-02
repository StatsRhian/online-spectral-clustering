run_clustream_unweighted <- function(dataset, nMicro, nRepeats = 1){

source("code/global_settings.R")

for (currentRepeat in 1:nRepeats){  
#Generate train data
source("code/generate_simulated_train.R")
runs <- N - sizeInit

assignment <- vector(length = (sizeInit + runs))
performance <- array(NA, dim = c(floor(runs/batchSize),3))

# Initialisation of microclusters for Clustream
initialClustering <- kmeans(data[1:sizeInit,], nMicro, nstart = 20)
micro <- initialiseMicro(initialClustering$cluster, nMicro, nDim, data)
assignment[1:sizeInit] <- initialClustering$cluster
clusTime <- 1

for(t in (sizeInit+1):(sizeInit+runs)){
  print(t)
  clusTime <- clusTime + 1
  output <- do_clustream(data[t, ], micro, assignment, nDim, nMicro, clusTime, t)
  micro <- output[[1]]
  assignment <- output[[2]]
  
  if (t%%batchSize == 0){
    centers <- sweep(micro$CF1x, 1, micro$n, FUN = "/")
    sp <- spectralClustering_unweighted(centers, nClust, 8)
    
    #Generate test data
    source("code/generate_simulated_test.R", local = T)
    
    linked_test <- as.numeric(apply(test_data, 1, FIND_closest_microcluster, centers))
    assignment <- sp[linked_test]
    performance[(t-sizeInit)/batchSize,] <- calc_vmeasure_purity_numClust(assigned = assignment, labels = test_trueClusters)
    }
}

results <- data.frame(purity = performance[ ,1],
                      vmeasure = performance[ ,2],
                      batch_number = 1:nrow(performance),
                      stringsAsFactors = FALSE) 

file_name <- sprintf("clustream_unweighted_%s_%i_of_%i", dataset, currentRepeat, nRepeats)
write.table(results, file = sprintf("results/%s.csv",file_name), sep = ",", row.names = FALSE)
}
}
