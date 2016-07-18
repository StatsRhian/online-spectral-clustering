ning_eigenVector_diff <- function(q1,q2){
  1 - ((t(q1)%*%q2)^2)/((t(q1)%*%q1)%*%(t(q2)%*%q2))
}
