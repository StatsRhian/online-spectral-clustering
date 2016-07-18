invisible(sapply(X = list.files(path = "code/functions", pattern = "*.R$", full.names = TRUE, ignore.case = TRUE), source, .GlobalEnv))

#run_clustream_weighted("norm", 50)
#run_clustream_unweighted("norm", 50)
run_ning("norm", 0.3)
#run_spectal_window("norm", 500)
