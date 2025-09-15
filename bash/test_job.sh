#!/bin/bash
#SBATCH -p lrz-v100x2
#SBATCH --nodes=1
#SBATCH --gres=gpu:1
#SBATCH --cpus-per-task=9
#SBATCH --time=00:15:00
#SBATCH -o /dss/dsshome1/0E/di97ceh/random_ilr_thesis/logs/%x_%j.out
#SBATCH -e /dss/dsshome1/0E/di97ceh/random_ilr_thesis/logs/%x_%j.err
#SBATCH --container-image=/dss/dsshome1/0E/di97ceh/containers/randomilr_env_comp.sqsh
#SBATCH --container-mounts=/dss/dsshome1/0E/di97ceh/random_ilr_thesis:/workspace,/dss/dsshome1/0E/di97ceh/random_ilr_thesis/data/data_preproc:/data,/dss/dsshome1/0E/di97ceh/random_ilr_thesis/results/random_ilr:/results

set -euo pipefail

# Avoid thread oversubscription inside each R process
export OMP_NUM_THREADS=1
export MKL_NUM_THREADS=1
export OPENBLAS_NUM_THREADS=1


datasets=(1)
strategies=(aug_in_n aug_in_p)
densities=(NA 0.1)
pseudo_counts=(half)

dataset=${datasets[$SLURM_ARRAY_TASK_ID]}

cd /workspace

# Build command list for all combinations then execute with bounded parallelism
cmds=()
for p_count in "${pseudo_counts[@]}"; do
  for strategy in "${strategies[@]}"; do
    for density in "${densities[@]}"; do
      for split in $(seq 1 "${SPLITS:-2}"); do
        echo "enqueue: d=$dataset pseudo_count=$p_count strat=$strategy density=$density split=$split"
        cmds+=(
          "Rscript --vanilla scripts/01b_random_ilr_pipeline.R \
                  /data \"$dataset\" \"$strategy\" \"$split\" \
                  \"${AUG_FACTOR:-2}\" \"$density\" \"$p_count\" /results"
        )
      done
    done
  done
done

concurrency="${SPLIT_WORKERS:-${SLURM_CPUS_PER_TASK:-1}}"
echo "Running with parallel splits: $concurrency concurrent Rscript processes"

# Execute commands in parallel; one command per process
printf '%s\n' "${cmds[@]}" | xargs -n1 -P "$concurrency" -I CMD bash -lc "CMD"

echo "All tasks completed for dataset=$dataset"
