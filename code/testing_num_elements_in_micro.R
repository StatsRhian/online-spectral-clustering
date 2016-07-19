#Investigate number of elements in the microclusters over time

invisible(sapply(X = list.files(path = "code/functions", pattern = "*.R$", full.names = TRUE, ignore.case = TRUE), source, .GlobalEnv))
dataset <- "norm_original"#"norm_8clusters_3dim"
nMicro <- 150

run_clustream_unweighted(dataset, 150)
