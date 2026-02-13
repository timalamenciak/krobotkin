#!/bin/bash
#SBATCH --job-name=pythia_cpt_sft
#SBATCH --account=def-talamenc
#SBATCH --nodes=1
#SBATCH --cpus-per-task=16
#SBATCH --mem=64G
#SBATCH --time=01:00:00
#SBATCH --array=0-1
#SBATCH --output=logs/%x_%A_%a.out
#SBATCH --error=logs/%x_%A_%a.err
#SBATCH --gpus-per-node=h100:4

set -euo pipefail
mkdir -p runs  # NOTE: logs/ must exist before sbatch (create it outside the job)

module --force purge || true
module load StdEnv/2023
module load python/3.11.5
module load arrow

VENV_ACTIVATE="$HOME/scratch/venvs/llm/bin/activate"
[[ -f "$VENV_ACTIVATE" ]] || { echo "ERROR: venv not found: $VENV_ACTIVATE"; exit 2; }
source "$VENV_ACTIVATE"

python -V
python -c "import accelerate; print('accelerate', accelerate.__version__)"
nvidia-smi || true

# FIX: Use HF_HOME only; drop TRANSFORMERS_CACHE to silence deprecation warning
export HF_HOME="${HF_HOME:-$HOME/.cache/huggingface}"
# Removed: export TRANSFORMERS_CACHE=...  (deprecated in transformers v5)
export TOKENIZERS_PARALLELISM=false

export CUDA_DEVICE_ORDER=PCI_BUS_ID
# FIX: Use the non-deprecated env var for NCCL async error handling
export TORCH_NCCL_ASYNC_ERROR_HANDLING=1
# Removed: export NCCL_ASYNC_ERROR_HANDLING=1  (deprecated)
export NCCL_DEBUG=WARN
echo "CUDA_VISIBLE_DEVICES=${CUDA_VISIBLE_DEVICES-}"

MIXED="bf16"
PREC_FLAGS="--bf16"
QLORA=""

MAX_SEQ_LEN=2048
CPT_EPOCHS=5
CPT_LR=1e-4
SFT_EPOCHS=3
SFT_LR=2e-4
PER_DEVICE_BATCH=1
GRAD_ACCUM=8

SYSTEM_PROMPT="You are Krobotkin, an assistant that answers in the voice and reasoning style of Peter Kropotkin while remaining grounded in the provided passages when applicable."

if [[ -n "${SLURM_GPUS_PER_NODE-}" ]]; then
  NUM_GPUS="$(python - <<'PY'
import os, re
raw = os.environ.get("SLURM_GPUS_PER_NODE","").strip()
total = 0
for part in raw.split(",") if raw else []:
    m = re.search(r":(\d+)\s*$", part.strip())
    if m: total += int(m.group(1))
    elif part.strip().isdigit(): total += int(part.strip())
print(total)
PY
)"
elif [[ -n "${SLURM_GPUS_ON_NODE-}" && "${SLURM_GPUS_ON_NODE}" =~ ^[0-9]+$ ]]; then
  NUM_GPUS="${SLURM_GPUS_ON_NODE}"
elif [[ -n "${CUDA_VISIBLE_DEVICES-}" ]]; then
  NUM_GPUS="$(python - <<'PY'
import os
c = os.environ.get("CUDA_VISIBLE_DEVICES","").strip()
print(len([x for x in c.split(",") if x.strip()]) if c else 0)
PY
)"
else
  NUM_GPUS=1
fi
[[ -n "${NUM_GPUS}" && "${NUM_GPUS}" -ge 1 ]] || NUM_GPUS=1
export NUM_GPUS

echo "SLURM_GPUS_PER_NODE=${SLURM_GPUS_PER_NODE-}"
echo "SLURM_GPUS_ON_NODE=${SLURM_GPUS_ON_NODE-}"
echo "SLURM_GPUS=${SLURM_GPUS-}"
echo "CUDA_VISIBLE_DEVICES=${CUDA_VISIBLE_DEVICES-}"
echo "NUM_GPUS=${NUM_GPUS}"

echo "============================================================"
echo "Model array id: ${SLURM_ARRAY_TASK_ID}"
echo "Node:           ${SLURM_JOB_NODELIST}"
echo "GPUs:           ${NUM_GPUS}"
echo "============================================================"
nvidia-smi || true

ACCEL_COMMON=( --num_processes "${NUM_GPUS}" --num_machines 1 --mixed_precision "${MIXED}" )

CPT_JSONL="data/kropotkin_cpt.jsonl"
SFT_JSONL="data/instructions.jsonl"
[[ -f "$CPT_JSONL" ]] || { echo "ERROR: Missing CPT dataset: $CPT_JSONL"; exit 1; }
[[ -f "$SFT_JSONL" ]] || { echo "ERROR: Missing SFT dataset: $SFT_JSONL"; exit 1; }

MODELS=("EleutherAI/pythia-2.8b" "EleutherAI/pythia-6.9b")
MODEL="${MODELS[$SLURM_ARRAY_TASK_ID]}"

STAMP="$(date +%Y%m%d_%H%M%S)"
SAFE_MODEL="$(echo "$MODEL" | tr '/' '_' )"
RUN_ROOT="runs/${SAFE_MODEL}_${STAMP}"
CPT_OUT="${RUN_ROOT}/cpt"
SFT_OUT="${RUN_ROOT}/sft"
mkdir -p "${CPT_OUT}" "${SFT_OUT}"

echo "Model: ${MODEL}"
echo "Run root: ${RUN_ROOT}"

echo "[Stage 1/2] CPT starting..."
accelerate launch "${ACCEL_COMMON[@]}" \
  train_cpt.py \
    --model "${MODEL}" \
    --data "${CPT_JSONL}" \
    --out "${CPT_OUT}" \
    --max_seq_len "${MAX_SEQ_LEN}" \
    --epochs "${CPT_EPOCHS}" \
    --lr "${CPT_LR}" \
    --per_device_batch "${PER_DEVICE_BATCH}" \
    --grad_accum "${GRAD_ACCUM}" \
    ${QLORA} \
    ${PREC_FLAGS} \
    --save_steps 500 \
    --logging_steps 25

echo "[Stage 1/2] CPT done. Output: ${CPT_OUT}"

echo "[Stage 2/2] SFT starting..."
accelerate launch "${ACCEL_COMMON[@]}" \
  train_sft.py \
    --model "${CPT_OUT}" \
    --data "${SFT_JSONL}" \
    --out "${SFT_OUT}" \
    --max_seq_len "${MAX_SEQ_LEN}" \
    --epochs "${SFT_EPOCHS}" \
    --lr "${SFT_LR}" \
    --per_device_batch "${PER_DEVICE_BATCH}" \
    --grad_accum "${GRAD_ACCUM}" \
    ${QLORA} \
    ${PREC_FLAGS} \
    --system "${SYSTEM_PROMPT}" \
    --save_steps 200 \
    --eval_steps 200 \
    --logging_steps 10

echo "[Stage 2/2] SFT done. Output: ${SFT_OUT}"
echo "All done: ${RUN_ROOT}"
