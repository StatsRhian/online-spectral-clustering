# README #

### What is this repository for? ###

* All the sims for Chapter 3
* Contains all data files and will run in STORM

### Example of job flow for unweighted clustering ###

submit_unweighted.sh
-> call_unweighted.R (Reads in all functions)
-> run_clustream_unweighted.R 
-> sources global_settings.R, generate_simulated_train.R, generate_simulated_test.R
-> calls initialise_microcluster.R, run_clustream.R