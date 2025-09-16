#!/bin/bash
#SBATCH -p lrz-v100x2
#SBATCH --nodes=1
#SBATCH --gres=gpu:1
#SBATCH --cpus-per-task=9
#SBATCH --time=4:00:00
#SBATCH -o /dss/dsshome1/0E/di97ceh/random_ilr_thesis/logs/%x_%j.out
#SBATCH -e /dss/dsshome1/0E/di97ceh/random_ilr_thesis/logs/%x_%j.err
#SBATCH --container-image=/dss/dsshome1/0E/di97ceh/containers/random_ilr_env.sqsh
#SBATCH --container-mounts=/dss/dsshome1/0E/di97ceh/random_ilr_thesis:/workspace,/dss/dsshome1/0E/di97ceh/random_ilr_thesis/data/data_preproc:/data,/dss/dsshome1/0E/di97ceh/random_ilr_thesis/results/benchmark:/results

set -euo pipefail

# Avoid thread oversubscription inside each R process
export OMP_NUM_THREADS=1
export MKL_NUM_THREADS=1
export OPENBLAS_NUM_THREADS=1

datasets=(1 2 3 4 5 6 7 8 9 10 11 12)
transformations=(standard_ilr proportion)
pseudo_counts=(half max_lib_size)

dataset=${datasets[$SLURM_ARRAY_TASK_ID]}

cd /workspace

# Build command list for all combinations then execute with bounded parallelism
cmds=()
for p_count in "${pseudo_counts[@]}"; do
  for transform in "${transformations[@]}"; do
    for split in $(seq 1 "${SPLITS:-20}"); do
      echo "enqueue: d=$dataset pseudo_count=$p_count transform=$transform split=$split"
      cmds+=(
        "Rscript --vanilla scripts/01c_benchmark_models_pipeline.R \
                /data \"$dataset\" \"$transform\" \"$split\" \
                \"$p_count\" /results"
      )
    done
  done
done

concurrency="${SPLIT_WORKERS:-${SLURM_CPUS_PER_TASK:-1}}"
echo "Running with parallel splits: $concurrency concurrent Rscript processes"

# Execute commands in parallel; one command per process
printf '%s\n' "${cmds[@]}" | xargs -r -P "$concurrency" -I {} bash -lc "{}"

echo "All tasks completed for dataset=$dataset"
