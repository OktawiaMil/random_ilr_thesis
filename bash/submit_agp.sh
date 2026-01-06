#!/bin/bash
set -euo pipefail

export SCRATCH_BASE="/dss/dsshome1/0E/$USER"
export DATA_DIR="$SCRATCH_BASE/random_ilr_thesis/data/data_agp"
export OUT_DIR="$SCRATCH_BASE/random_ilr_thesis/results/agp"
export LOG_DIR="$SCRATCH_BASE/random_ilr_thesis/logs"
mkdir -p "$DATA_DIR" "$OUT_DIR" "$LOG_DIR"

sbatch --export=ALL,SPLITS=20 agp_job.sh

