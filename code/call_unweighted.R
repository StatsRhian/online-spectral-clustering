JOB <- as.numeric(Sys.getenv("PBS_ARRAYID"))
print(JOB)

invisible(sapply(X = list.files(path = "code/functions", pattern = "*.R$", full.names = TRUE, ignore.case = TRUE), source, .GlobalEnv))
dataset_list <- read.table("data/sim_data/dataset_list.txt", stringsAsFactors = F)
dataset_list <- dataset_list[[1]] 

dataset <- "norm_8clusters_3dim"

#dataset <- dataset_list[JOB]
print(dataset)
run_clustream_unweighted(dataset, 250)

