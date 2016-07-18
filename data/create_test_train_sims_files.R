# Creating data for sims

###Gaussian
if (dataset=="norm_original"){
  set.seed(101)
  nDim <- 2; nClust <-  3; N = 5000;
  pi_vec_true <- rep(1, nClust)/nClust
  mu_mat_true <- matrix(runif(nDim*nClust, 1, 20), nrow = nClust, ncol = nDim)
  sigma_list_true <- lapply(seq_len(nClust), function(X) diag(nDim))
  trainData <- simulateMMN_fixed(N, pi_vec_true, mu_mat_true, sigma_list_true)
  testData <- simulateMMN_fixed(N, pi_vec_true, mu_mat_true, sigma_list_true)
} 

if (dataset=="norm_jump"){
  simData <- NULL
  set.seed(101)
  nDim <- 2; nClust <-  3; N = 10000;
  pi_vec_true <- rep(1, nClust)/nClust
  mu_mat_true_a <-matrix(c(2,2,1,20,12,12), nrow = nClust, ncol = nDim, byrow = T)
  mu_mat_true_b <-matrix(c(20,20,1,20,12,12), nrow = nClust, ncol = nDim, byrow = T)
  sigma_list_true <- lapply(seq_len(nClust), function(X) diag(nDim))
  trainData_a <- simulateMMN_fixed(N, pi_vec_true, mu_mat_true_a, sigma_list_true)
  trainData_b <- simulateMMN_fixed(N, pi_vec_true, mu_mat_true_b, sigma_list_true)
  trainData[[1]] <- rbind(trainData_a[[1]], trainData_b[[1]])
  trainData[[2]] <- c(trainData_a[[2]], trainData_b[[2]])

  testData_a <- simulateMMN_fixed(N, pi_vec_true, mu_mat_true_a, sigma_list_true)
  testData_b <- simulateMMN_fixed(N, pi_vec_true, mu_mat_true_b, sigma_list_true)
  testData[[1]] <- rbind(testData_a[[1]], testData_b[[1]])
  testData[[2]] <- c(testData_a[[2]], testData_b[[2]])
  #N <- N*2
} 

###Gaussian
if (dataset=="norm_overlap_1"){
  set.seed(101)
  nDim <- 2; nClust <-  3; N = 5000;
  pi_vec_true <- rep(1, nClust)/nClust
  mu_mat_true <-matrix(c(2,2,3,5,12,12), nrow = nClust, ncol = nDim, byrow = T)
  sigma_list_true <- lapply(seq_len(nClust), function(X) diag(nDim))
  trainData <- simulateMMN_fixed(N, pi_vec_true, mu_mat_true, sigma_list_true)
  testData <- simulateMMN_fixed(N, pi_vec_true, mu_mat_true, sigma_list_true)
} 

###Gaussian - changepoint
if (dataset=="norm_overlap_jump"){
  set.seed(101)
  simData <- NULL
  nDim <- 2; nClust <-  3; N = 10000;
  pi_vec_true <- rep(1, nClust)/nClust
  mu_mat_true_a <-matrix(c(2,2,3,5,12,12), nrow = nClust, ncol = nDim, byrow = T)
  mu_mat_true_b <-matrix(c(7,7,3,5,12,12), nrow = nClust, ncol = nDim, byrow = T)
  sigma_list_true <- lapply(seq_len(nClust), function(X) diag(nDim))
  
  trainData_a <- simulateMMN_fixed(N, pi_vec_true, mu_mat_true_a, sigma_list_true)
  trianData_b <- simulateMMN_fixed(N, pi_vec_true, mu_mat_true_b, sigma_list_true)
  trainData[[1]] <- rbind(trainData_a[[1]], trainData_b[[1]])
  trainData[[2]] <- c(trainData_a[[2]], trainData_b[[2]])
  
  testData_a <- simulateMMN_fixed(N, pi_vec_true, mu_mat_true_a, sigma_list_true)
  testData_b <- simulateMMN_fixed(N, pi_vec_true, mu_mat_true_b, sigma_list_true)
  testData[[1]] <- rbind(testData_a[[1]], testData_b[[1]])
  testData[[2]] <- c(testData_a[[2]], testData_b[[2]])
  #N <- N*2
} 


if (dataset=="norm_overlap_2"){
  set.seed(101)
  nDim <- 2; nClust <-  3; N = 5000;
  pi_vec_true <- rep(1, nClust)/nClust
  mu_mat_true <-matrix(c(2,2,5,5,12,12), nrow = nClust, ncol = nDim, byrow = T)
  sigma_list_true <- lapply(seq_len(nClust), function(X) diag(nDim))
  trainData <- simulateMMN_fixed(N, pi_vec_true, mu_mat_true, sigma_list_true)
  testData <- simulateMMN_fixed(N, pi_vec_true, mu_mat_true, sigma_list_true)
} 


### Student T
if (dataset=="t_original"){
  set.seed(101)
  nDim <- 2; nClust <-  3; N = 5000
  pi_vec_true <- rep(1, nClust)/nClust
  mu_mat_true <- matrix(runif(nDim*nClust, 1, 20), nrow = nClust, ncol = nDim)
  sigma_list_true <- lapply(seq_len(nClust), function(X) diag(nDim))
  trainData <- simulateMMT_fixed(N, pi_vec_true, mu_mat_true, sigma_list_true, df = 4)
  testData <- simulateMMT_fixed(N, pi_vec_true, mu_mat_true, sigma_list_true, df = 4)
}  

if (dataset=="t_spaced_10df"){
  set.seed(161)
  nDim <- 2; nClust <-  3; N = 5000
  pi_vec_true <- rep(1, nClust)/nClust
  mu_mat_true <- matrix(runif(nDim*nClust, 1, 20), nrow = nClust, ncol = nDim)
  sigma_list_true <- lapply(seq_len(nClust), function(X) diag(nDim))
  trainData <- simulateMMT_fixed(N, pi_vec_true, mu_mat_true, sigma_list_true, df = 10)
  testData <- simulateMMT_fixed(N, pi_vec_true, mu_mat_true, sigma_list_true, df = 10)
} 

### Shapes
library(mlbench)
if (dataset=="shapes_spiral"){
  nDim = 2; nClust = 2; N = 5000;
  simData <- mlbench.spirals(N, 1.5, 0.05)
  shuffleVec <- sample(nrow(simData[[1]]))
  simData[[1]] <- simData[[1]][shuffleVec, ]
  simData[[2]] <- simData[[2]][shuffleVec]
}

if (dataset=="shapes_cassini"){
  nDim = 2; nClust = 3; N = 5000;
  simData <- mlbench.cassini(N)
  shuffleVec <- sample(nrow(simData[[1]]))
  simData[[1]] <- simData[[1]][shuffleVec, ]
  simData[[2]] <- simData[[2]][shuffleVec]
}

if (dataset=="sims_A"){
  nDim = 2; nClust = 3; N = 5000;
  simData = NULL
  simData[[1]] <- read.table("data/sims_feat_seed101_dim2_each10000_clust3.dat")
  simData[[2]] <- scan("data/sims_true_seed101_dim2_each10000_clust3.dat")
  shuffleVec <- sample(nrow(simData[[1]]),N)
  simData[[1]] <- simData[[1]][shuffleVec, ]
  simData[[2]] <- simData[[2]][shuffleVec]
}

if (dataset=="sims_B"){
  nDim = 3; nClust = 5; N = 5000;
  simData = NULL
  simData[[1]] <- read.table("data/sims_feat_seed901_dim3_each1000_clust5.dat")
  simData[[2]] <- scan("data/sims_true_seed901_dim3_each1000_clust5.dat")
  shuffleVec <- sample(nrow(simData[[1]]),N)
  simData[[1]] <- simData[[1]][shuffleVec, ]
  simData[[2]] <- simData[[2]][shuffleVec]
}

if (dataset=="sims_C"){
  nDim = 2; nClust = 2; N = 5000;
  simData = NULL
  simData[[1]] <- read.table("data/basic_sims_feat_seed901_dim2_each10000_clust2.dat")
  simData[[2]] <- scan("data/basic_sims_true_seed901_dim2_each10000_clust2.dat")
  shuffleVec <- sample(nrow(simData[[1]]),N)
  simData[[1]] <- simData[[1]][shuffleVec, ]
  simData[[2]] <- simData[[2]][shuffleVec]
}

if (dataset=="sims_D"){
  nDim = 3; nClust = 4; N = 5000;
  simData = NULL
  simData[[1]] <-  read.table("data/sims_feat_seed101_dim3_each10000_clust4.dat")
  simData[[2]] <- scan("data/sims_true_seed101_dim3_each10000_clust4.dat")
  shuffleVec <- sample(nrow(simData[[1]]),N)
  simData[[1]] <- simData[[1]][shuffleVec, ]
  simData[[2]] <- simData[[2]][shuffleVec]
}

if (dataset=="sims_E"){
  nDim = 3; nClust = 6; N = 5000;
  simData = NULL
  simData[[1]] <-  read.table("data/sims_feat_seed456_dim3_each10000_clust6.dat")
  simData[[2]] <- scan("data/sims_true_seed456_dim3_each10000_clust6.dat")
  shuffleVec <- sample(nrow(simData[[1]]),N)
  simData[[1]] <- simData[[1]][shuffleVec, ]
  simData[[2]] <- simData[[2]][shuffleVec]
}

if (dataset=="sims_F"){
  nDim = 5; nClust = 8; N = 5000;
  simData = NULL
  simData[[1]] <-  read.table("data/sims_feat_seed420_dim5_each10000_clust8.dat")
  simData[[2]] <- scan("data/sims_true_seed420_dim5_each10000_clust8.dat")
  shuffleVec <- sample(nrow(simData[[1]]), N)
  simData[[1]] <- simData[[1]][shuffleVec, ]
  simData[[2]] <- simData[[2]][shuffleVec]
}