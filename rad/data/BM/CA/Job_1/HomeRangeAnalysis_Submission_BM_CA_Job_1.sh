#!/bin/bash
#SBATCH --mem=64000M
#SBATCH --time=71:00:00 # 48 hours
#SBATCH --job-name=HomeRangeAnalysis_BM_CA_JOB_1
#SBATCH --output=%x.out
#SBATCH --comment=Caribou_Quintette
#SBATCH --nodes=1
#SBATCH --cpus-per-task=40

module load r/4.2.2 udunits/2.2.28 gdal geos proj/9.0.1 protobuf

Rscript HomeRangeAnalysis_BM_CA_Job_1.R