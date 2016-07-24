#Similarity and affinity functions

CALC_euclidean_row <- function(datarow, point){return(as.numeric(dist(rbind(point, datarow))))}

FIND_closest_microcluster <- function(x, data){
  which.min(apply(data, 1, CALC_euclidean_row, x))
}

create_gaussian_affinity <- function(X, nn){
  ds <- as.matrix(dist(X))
  s <- unlist(apply(ds, 1, function(x) sort(x)[nn]))
  s[which(s==0)] <- min(s[which(s>0)])
  W <- exp(-ds^2/((s%*%t(s))))
  return(W)
}

similarity <- function(x1, x2, alpha=1) {
  exp(- alpha * norm(as.matrix(x1-x2), type="F"))
}


similarity_new_data <- function(data, x){
 dist <-  as.numeric(sqrt(rowSums(sweep(data, 2, x, FUN = "-")^2)))
 ans <- exp(-dist)
 return(ans)
}

similarity_all <- function(X){
  ds <- as.matrix(dist(X))
  ans <- exp(-ds)
  return(ans)
}
  
  
create_similarity <- function(my.data, similarity) {
  N <- nrow(my.data)
  S <- matrix(rep(NA,N^2), ncol=N)
  for(i in 1:N) {
    for(j in 1:N) {
      S[i,j] <- similarity(my.data[i,], my.data[j,])
    }
  }
  return(S)
}

create_affinity <- function(S, n.neighboors=2) {
  N <- length(S[,1])
  
  if (n.neighboors >= N) {  # fully connected
    A <- S
  } else {
    A <- matrix(rep(0,N^2), ncol=N)
    for(i in 1:N) { # for each line
      # only connect to those points with larger similarity 
      best.similarities <- sort(S[i,], decreasing=TRUE)[1:n.neighboors]
      for (s in best.similarities) {
        j <- which(S[i,] == s)
        A[i,j] <- S[i,j]
        A[j,i] <- S[i,j] # to make an undirected graph, ie, the matrix becomes symmetric
      }
    }
  }
  return(A)
}