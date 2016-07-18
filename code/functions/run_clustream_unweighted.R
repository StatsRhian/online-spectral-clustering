run_clustream_unweighted <- function(dataset, nMicro){

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
    macro_centers = NULL
    for (c in 1:nClust){
      macro_centers <- rbind(macro_centers, colSums(matrix(centers[which(sp==c),],ncol = nDim))/sum(sp==c))
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

results <- data.frame(purity = performance[ ,1],
                      vmeasure = performance[ ,2],
                      batch_number = 1:nrow(performance),
                      stringsAsFactors = FALSE) 

file_name <- sprintf("clustream_unweighted_%s", dataset)
write.table(results, file = sprintf("results/%s.csv",file_name), sep = ",", row.names = FALSE)
}
