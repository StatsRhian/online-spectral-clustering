#Investigate number of elements in the microclusters over time
rm(list = ls())
invisible(sapply(X = list.files(path = "code/functions", pattern = "*.R$", full.names = TRUE, ignore.case = TRUE), source, .GlobalEnv))
dataset <- "staticNormal" #"hyperSphere"
nMicro <- 150

run_clustream_weighted(dataset,  nMicro = 150, nRepeats = 2)
run_clustream_unweighted(dataset,  nMicro = 150, nRepeats = 2)
run_spectral_window (dataset, windowSize = 150, nRepeats = 2)
