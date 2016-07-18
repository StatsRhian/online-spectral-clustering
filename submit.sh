#!/bin/bash
#PBS -N spectral_clustering         # change this
#PBS -M r.davies3@lancaster.ac.uk       # change this or remove if you dont want emails

#PBS -l ncpus=1             # resource requests. See http://qcd.phys.cmu.edu/QCDcluster/pbs/run_serial.html for more options and a good explaination
#PBS -t 1               # m-n%c   submit the job array indexed from (m) to (n) (inclusive) but only allow c jobs to be running (c)oncurrently.
#PBS -m a                   # email if the job: (a)borts, (b)egins, (e)nds. can be used in combinations. Delete this lone for no emails

#PBS -j oe
#PBS -o outerr/

echo Moving to project directory: storm_sims
cd storm_sims
echo Running R script in directory $PWD:                  # just prints the directory to the outerr dir to check that Storm cd'd to the right place
R CMD BATCH code/run_all.R rout/${PBS_JOBID}.Rout --no-save  # run algorithm.r (which can grab the array ID) and save R console output to file numbered by array Id i the rout directory
