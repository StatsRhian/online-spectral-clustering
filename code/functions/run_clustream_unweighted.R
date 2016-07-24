run_clustream_unweighted <- function(dataset, nMicro){

source("code/global_settings.R")

nDim = 2
nClust = 2

N <- 1000
pi_vec <- c(0.5,0.5)
mu_mat <- matrix(c(0,0,5,5), nrow = 2, ncol = 2, byrow = T)
sigma_list <- list(diag(2), diag(2))

data_complete <- simulateMMN_fixed(N, pi_vec, mu_mat, sigma_list)  
data <- data_complete[[1]]
trueClusters <- data_complete[[2]]

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
    
    sim_complete <- simulateMMN_fixed(N, pi_vec, mu_mat, sigma_list)  
    sim_data <- sim_complete[[1]]
    sim_trueClusters <- sim_complete[[2]]
   
    linked_sim <- as.numeric(apply(sim_data, 1, FIND_closest_microcluster, centers))
    assignment <- sp[linked_sim]
    performance[(t-sizeInit)/batchSize,] <- calc_vmeasure_purity_numClust(assigned = assignment, labels = sim_trueClusters)
    }
}

results <- data.frame(purity = performance[ ,1],
                      vmeasure = performance[ ,2],
                      batch_number = 1:nrow(performance),
                      stringsAsFactors = FALSE) 

file_name <- sprintf("clustream_unweighted_%s", dataset)
write.table(results, file = sprintf("results/%s.csv",file_name), sep = ",", row.names = FALSE)
}
