
#!/bin/bash

#PBS -N ZSEML
#PBS -l ncpus=4
#PBS -l mem=4GB
#PBS -J 1-16
#PBS -o experiments/logs
#PBS -j oe

cd ${PBS_O_WORKDIR}
apptainer run image.sif run_job.R 0

