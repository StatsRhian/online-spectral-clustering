#################################
#Generate test simulations
#################################

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