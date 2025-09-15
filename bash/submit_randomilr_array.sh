#!/bin/bash
set -euo pipefail

export SCRATCH_BASE="/dss/dsshome1/0E/$USER"
export DATA_DIR="$SCRATCH_BASE/random_ilr_thesis/data"
export OUT_DIR="$SCRATCH_BASE/random_ilr_thesis/results"
export LOG_DIR="$SCRATCH_BASE/random_ilr_thesis/logs"
mkdir -p "$DATA_DIR" "$OUT_DIR" "$LOG_DIR"

datasets=(1 2 3 4 5 6 7 8 9 10 11 12)
TOTAL=${#datasets[@]}

# Max number of array tasks that can run concurrently
PARALLEL=4

sbatch \
  --export=ALL,DATA_DIR="$DATA_DIR",OUT_DIR="$OUT_DIR",SPLITS=20,AUG_FACTOR=5 \
  --array=0-$((TOTAL-1))%${PARALLEL} \
  random_ilr_job.sh

