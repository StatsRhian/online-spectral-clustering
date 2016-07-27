run_clustream_unweighted <- function(dataset, nMicro){

source("code/global_settings.R")

############################## Parameter settings #############################################
nDim = 3            # Dimensionality of the data
rate = 1/200      # 1/rate = approximately the number of points needed for a class mean to complete 1 rotation
nClust = 2 # ALWAYS for Andy's Gaussians
N= 2000    # Number of data points
radius = 1         # Radius of hypersphere
ProbClassOne = 0.5 # Probaility of class twp = 1 - ProbClassOne
covar = diag(nDim)*0.005 # Noise covariance matrix


out <- generateData(dim = nDim, numPoints=N,rate=rate,radius=radius, ProbClassOne=ProbClassOne,covar=covar)
data <- out$data
trueClusters <- out$labels
modelCentres <- out$centres

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
    
    num_test_points = 200
    test_data = matrix(NA, num_test_points, nDim)
    #Decide how many points to generate from each cluster
    test_trueClusters = sample(c(1,2), simPoints, prob=c(ProbClassOne,1-ProbClassOne), replace = T)
    test_data[test_trueClusters==1] <- rmvn(sum(test_trueClusters==1), modelCentres$centres1[t,], covar)
    test_data[test_trueClusters==2] <- rmvn(sum(test_trueClusters==2), modelCentres$centres2[t,], covar)
    
    linked_test <- as.numeric(apply(test_data, 1, FIND_closest_microcluster, centers))
    assignment <- sp[linked_test]
    performance[(t-sizeInit)/batchSize,] <- calc_vmeasure_purity_numClust(assigned = assignment, labels = test_trueClusters)
    }
}

results <- data.frame(purity = performance[ ,1],
                      vmeasure = performance[ ,2],
                      batch_number = 1:nrow(performance),
                      stringsAsFactors = FALSE) 

file_name <- sprintf("clustream_unweighted_%s", dataset)
write.table(results, file = sprintf("results/%s.csv",file_name), sep = ",", row.names = FALSE)
}
