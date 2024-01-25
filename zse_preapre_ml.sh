#!/bin/bash

#PBS -N ZSEPREPAREFOLDS
#PBS -l mem=50GB

cd ${PBS_O_WORKDIR}
apptainer run image.sif zse_preapre.R
