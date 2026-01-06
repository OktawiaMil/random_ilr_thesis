#!/bin/bash
#SBATCH -p lrz-v100x2
#SBATCH --nodes=1
#SBATCH --gres=gpu:1
#SBATCH --cpus-per-task=5
#SBATCH --time=22:00:00
#SBATCH -o /dss/dsshome1/0E/di97ceh/random_ilr_thesis/logs/%x_%j.out
#SBATCH -e /dss/dsshome1/0E/di97ceh/random_ilr_thesis/logs/%x_%j.err
#SBATCH --container-image=/dss/dsshome1/0E/di97ceh/containers/random_ilr_env_vimp.sqsh
#SBATCH --container-mounts=/dss/dsshome1/0E/di97ceh/random_ilr_thesis:/workspace,/dss/dsshome1/0E/di97ceh/random_ilr_thesis/data/data_agp:/data,/dss/dsshome1/0E/di97ceh/random_ilr_thesis/results/agp:/results

set -euo pipefail

# Avoid thread oversubscription inside each R process
export OMP_NUM_THREADS=1
export MKL_NUM_THREADS=1
export OPENBLAS_NUM_THREADS=1


densities=(0.1)

cd /workspace

# Build command list for all combinations then execute with bounded parallelism
cmds=()
for density in "${densities[@]}"; do
  for split in $(seq 1 "${SPLITS:-20}"); do
    echo "enqueue: density=$density split=$split"
    cmds+=("Rscript --vanilla scripts/03b_agp_analysis.R /data \"$split\" \"$density\" /results")
  done
done

concurrency="${SPLIT_WORKERS:-${SLURM_CPUS_PER_TASK:-1}}"
echo "Running with parallel splits: $concurrency concurrent Rscript processes"

# Execute commands in parallel; one command per process
printf '%s\n' "${cmds[@]}" | xargs -r -P "$concurrency" -I {} bash -lc "{}"
