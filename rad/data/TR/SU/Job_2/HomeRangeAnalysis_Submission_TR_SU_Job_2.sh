#!/bin/bash
#SBATCH --mem=64000M
#SBATCH --time=48:00:00 # 48 hours
#SBATCH --job-name=HomeRangeAnalysis_TR_SU_JOB_2
#SBATCH --output=%x.out
#SBATCH --comment=Caribou_Quintette

module load r/4.2.2 udunits/2.2.28 gdal geos proj/9.0.1 protobuf

Rscript HomeRangeAnalysis_TR_SU_Job_2.R