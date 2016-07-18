##################################################################### 
# Code written by Dave Hofmeyr
# Outputs Purity, Vmeasure and Number of cluster percentage rate.
#####################################################################
calc_vmeasure_purity_numClust <-  function(assigned, labels){
	
  n <-  length(labels)
	T <-  table(assigned, labels)
	CK <-  - sum(apply(T, 1, function(x) return(sum(x[which(x>0)]*log(x[which(x>0)]/sum(x))))))/n
	KC <-  - sum(apply(T, 2, function(x) return(sum(x[which(x>0)]*log(x[which(x>0)]/sum(x))))))/n
	K <-  - sum(apply(T, 1, function(x) return(sum(x)*log(sum(x)/n))))/n
	C <-  - sum(apply(T, 2, function(x) return(sum(x)*log(sum(x)/n))))/n
	p <-  sum(apply(T, 1, function(x) return(max(x))))/n
	if(C!=0){
		h <-  1 - CK/C
	}
	else{
		h <-  0
	}
	if(K!=0){
		c <-  1 - KC/K
	}
	else{
		c <-  0
	}
	return(c(p, 2*h*c/(h+c+1e-100), length(table(assigned))/length(table(labels))))
}