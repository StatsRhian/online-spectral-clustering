#################################
#Generate test simulations
#################################
num_test_points = 200

######### Dataset one #########
# Static Multivariate Normal
if(dataset == "staticNormal"){
test_complete <- simulateMMN_fixed(num_test_points, pi_vec, mu_mat, sigma_list)  
test_data <- test_complete[[1]]
test_trueClusters <-test_complete[[2]]
}
######### End Dataset one #########

######### Dataset two #########
# Hypersphere - Andy's code
if(dataset == "hyperSphere"){
test_data = matrix(NA, num_test_points, nDim)
test_trueClusters = sample(c(1,2), num_test_points, prob=c(ProbClassOne,1-ProbClassOne), replace = T)
test_data[test_trueClusters==1] <- rmvn(sum(test_trueClusters==1), modelCentres$centres1[t,], covar)
test_data[test_trueClusters==2] <- rmvn(sum(test_trueClusters==2), modelCentres$centres2[t,], covar)
}
######### End Dataset two #########

######### Dataset three #########
# Jump Multivariate Normal
if(dataset == "jumpNormal"){
  pi_vec1 <- c(0.5,0.5)
  mu_mat1 <- matrix(c(0,0,5,5), nrow = 2, ncol = 2, byrow = T)
  sigma_list1 <- list(diag(2), diag(2))
  
  pi_vec2 <- c(0.5,0.5)
  mu_mat2 <- matrix(c(15,0,5,5), nrow = 2, ncol = 2, byrow = T)
  sigma_list2 <- list(diag(2), diag(2))

  if(t<N/2){
    test_complete <- simulateMMN_fixed(num_test_points, pi_vec1, mu_mat1, sigma_list1)  
  }else{
    test_complete <- simulateMMN_fixed(num_test_points, pi_vec2, mu_mat2, sigma_list2)
  }
  test_data <- test_complete[[1]]
  test_trueClusters <- test_complete[[2]]
}
######### End Dataset one #########