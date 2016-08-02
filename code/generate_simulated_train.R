#################################
#Generate training simulations
#################################

######### Dataset one #########
# Static Multivariate Normal
if(dataset == "staticNormal"){
nDim = 2
nClust = 2
N <- 2000
pi_vec <- c(0.5,0.5)
mu_mat <- matrix(c(0,0,5,5), nrow = 2, ncol = 2, byrow = T)
sigma_list <- list(diag(2), diag(2))
data_complete <- simulateMMN_fixed(N, pi_vec, mu_mat, sigma_list)  
data <- data_complete[[1]]
trueClusters <- data_complete[[2]]
}
######### End Dataset one #########

######### Dataset two #########
# Hypersphere - Andy#s code
if(dataset == "hyperSphere"){
nDim = 3            # Dimensionality of the data
rate = 1/8000      # 1/rate = approximately the number of points needed for a class mean to complete 1 rotation
nClust = 2 # ALWAYS for Andy's Gaussians
N = 2000    # Number of data points
radius = 1         # Radius of hypersphere
ProbClassOne = 0.5 # Probaility of class twp = 1 - ProbClassOne
covar = diag(nDim)*0.005 # Noise covariance matrix

out <- generateData(dim = nDim, numPoints=N,rate=rate,radius=radius, ProbClassOne=ProbClassOne,covar=covar)
data <- out$data
trueClusters <- out$labels
modelCentres <- out$centres
}
######### End Dataset two #########

######### Dataset three #########
# Jump Multivariate Normal
if(dataset == "jumpNormal"){
  nDim = 2
  nClust = 2
  N <- 2000
  
  pi_vec1 <- c(0.5,0.5)
  mu_mat1 <- matrix(c(0,0,5,5), nrow = 2, ncol = 2, byrow = T)
  sigma_list1 <- list(diag(2), diag(2))
  
  pi_vec2 <- c(0.5,0.5)
  mu_mat2 <- matrix(c(15,0,5,5), nrow = 2, ncol = 2, byrow = T)
  sigma_list2 <- list(diag(2), diag(2))
  
  data_complete1 <- simulateMMN_fixed(N/2, pi_vec1, mu_mat1, sigma_list1)  
  data_complete2 <- simulateMMN_fixed(N/2, pi_vec2, mu_mat2, sigma_list2) 
  data <- rbind(data_complete1[[1]], data_complete2[[1]])
  trueClusters <- c(data_complete1[[2]], data_complete2[[2]])
}
######### End Dataset three #########