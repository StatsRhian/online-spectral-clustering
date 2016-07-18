run_ning <- function(dataset, tau, reclust){
  
source("code/global_settings.R")
  
data <- read.csv(sprintf("data/%s.csv",dataset), header = FALSE)
trueClusters <- data[,ncol(data)]
data <- data[ ,-ncol(data)]
nClust <- length(unique(trueClusters))
nDim <- ncol(data)
N <- nrow(data)

runs = N - sizeInit
  
performance <- array(NA, dim = c(floor(runs/batchSize),3))

# Initialisation of Ning
W <- similarity_all(data[1:sizeInit,])
D <- diag(rowSums(W))
L <- D - W
eigen <- eigs_sym(L, nClust,  sigma = 1e-10)

for(t in (sizeInit+1):(sizeInit+runs)){
  print(t)
  ning_update <- do_ning(t, L, D, W, eigen, data, tau = tau)
  L <- ning_update$L
  D <- ning_update$D
  W <- ning_update$W
  eigen <- ning_update$eigen
  
  if (t%%batchSize == 0){
    ning_assign <- kmeans(eigen$vectors, nClust, nstart = 20)$cluster
    performance[(t-sizeInit)/batchSize,] = calc_vmeasure_purity_numClust(ning_assign, trueClusters[1:length(ning_assign)])
   }
}

results <- data.frame(purity = performance[,1],
                      vmeasure = performance[,2],
                      batch_number = 1:nrow(performance),
                      stringsAsFactors = FALSE) 

file_name <- sprintf("ning_%s", dataset)
write.table(results, file = sprintf("results/%s.csv",file_name), sep = ",", row.names = FALSE)
}

