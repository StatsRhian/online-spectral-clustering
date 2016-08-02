#!/bin/bash
#PBS -N windowed         # change this
#PBS -M email       # change this or remove if you dont want emails

#PBS -l ncpus=1             # resource requests. See http://qcd.phys.cmu.edu/QCDcluster/pbs/run_serial.html for more options and a good explaination
#PBS -t 1-2%2               # m-n%c   submit the job array indexed from (m) to (n) (inclusive) but only allow c jobs to be running (c)oncurrently.
#PBS -m ae                   # email if the job: (a)borts, (b)egins, (e)nds. can be used in combinations. Delete this lone for no emails

#PBS -o outerr/
#PBS -j oe


echo Moving to project directory:
cd  online_spectral_clustering
echo Running R script in directory $PWD:                  # just prints the directory to the outerr dir to check that Storm cd'd to the right place
R CMD BATCH code/call_windowed.R rout/${PBS_JOBID}.Rout --no-save  # run algorithm.r (which can grab the array ID) and save R console output to file numbered by array Id i the rout directory
