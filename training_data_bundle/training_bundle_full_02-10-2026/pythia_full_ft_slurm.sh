#!/bin/bash
#SBATCH --job-name=pythia_full_ft
#SBATCH --account=def-talamenc
#SBATCH --nodes=1
#SBATCH --cpus-per-task=16
#SBATCH --mem=128G
#SBATCH --time=24:00:00
#SBATCH --array=0-1
#SBATCH --output=logs/%x_%A_%a.out
#SBATCH --error=logs/%x_%A_%a.err
#SBATCH --gpus-per-node=h100:4

# ============================================================
# Full fine-tuning (CPT + SFT) with DeepSpeed ZeRO Stage 2
#
# Key differences from LoRA version:
#   - All parameters trainable (no PEFT)
#   - DeepSpeed ZeRO-2 for optimizer state sharding
#   - Lower learning rates (2e-5 CPT, 5e-6 SFT)
#   - Weight decay enabled (0.01)
#   - 128G RAM, 24hr wall time (full FT is slower)
#   - No merge step needed — output is a complete model
# ============================================================

set -euo pipefail
mkdir -p runs logs

module --force purge || true
module load StdEnv/2023
module load python/3.11.5
module load arrow
module load cuda/12.9

VENV_ACTIVATE="$HOME/scratch/venvs/llm/bin/activate"
[[ -f "$VENV_ACTIVATE" ]] || { echo "ERROR: venv not found: $VENV_ACTIVATE"; exit 2; }
source "$VENV_ACTIVATE"

python -V
python -c "import deepspeed; print('deepspeed', deepspeed.__version__)"
python -c "import accelerate; print('accelerate', accelerate.__version__)"
nvidia-smi || true

# ---- Environment ----
export HF_HOME="${HF_HOME:-$HOME/.cache/huggingface}"
export TOKENIZERS_PARALLELISM=false
export OMP_NUM_THREADS=16
# Force DeepSpeed to use CUDA (ROCm install on NIBI confuses auto-detection)
export DS_ACCELERATOR=cuda
unset ROCM_HOME
export CUDA_DEVICE_ORDER=PCI_BUS_ID
export NCCL_ASYNC_ERROR_HANDLING=1
export NCCL_DEBUG=WARN
echo "CUDA_VISIBLE_DEVICES=${CUDA_VISIBLE_DEVICES-}"

# ---- GPU detection ----
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

# ---- Hyperparameters ----
# Full fine-tuning requires lower learning rates than LoRA
MAX_SEQ_LEN=2048

CPT_EPOCHS=3
CPT_LR=2e-5              # was 1e-4 with LoRA
CPT_PER_DEVICE_BATCH=2   # can fit more without adapter overhead
CPT_GRAD_ACCUM=8         # effective batch = 2 * 4 GPUs * 8 = 64

SFT_EPOCHS=3
SFT_LR=5e-6              # very conservative for SFT on small dataset
SFT_PER_DEVICE_BATCH=2
SFT_GRAD_ACCUM=4          # effective batch = 2 * 4 GPUs * 4 = 32

WEIGHT_DECAY=0.01
WARMUP_RATIO=0.05

DEEPSPEED_CONFIG="ds_zero2.json"

SYSTEM_PROMPT="You are Krobotkin, an assistant that answers in the voice and reasoning style of Peter Kropotkin while remaining grounded in the provided passages when applicable."

# ---- Data paths ----
CPT_JSONL="data/kropotkin_cpt.jsonl"
SFT_JSONL="data/instructions_all.jsonl"   # merged dataset
[[ -f "$CPT_JSONL" ]] || { echo "ERROR: Missing CPT dataset: $CPT_JSONL"; exit 1; }
[[ -f "$SFT_JSONL" ]] || { echo "ERROR: Missing SFT dataset: $SFT_JSONL"; exit 1; }
[[ -f "$DEEPSPEED_CONFIG" ]] || { echo "ERROR: Missing DeepSpeed config: $DEEPSPEED_CONFIG"; exit 1; }

# ---- Model selection ----
MODELS=("EleutherAI/pythia-2.8b" "EleutherAI/pythia-6.9b")
MODEL="${MODELS[$SLURM_ARRAY_TASK_ID]}"

STAMP="$(date +%Y%m%d_%H%M%S)"
SAFE_MODEL="$(echo "$MODEL" | tr '/' '_')"
RUN_ROOT="runs/${SAFE_MODEL}_full_${STAMP}"
CPT_OUT="${RUN_ROOT}/cpt"
SFT_OUT="${RUN_ROOT}/sft"
mkdir -p "${CPT_OUT}" "${SFT_OUT}"

echo "============================================================"
echo "FULL FINE-TUNING RUN"
echo "Model:        ${MODEL}"
echo "Array ID:     ${SLURM_ARRAY_TASK_ID}"
echo "Node:         ${SLURM_JOB_NODELIST}"
echo "GPUs:         ${NUM_GPUS}"
echo "Run root:     ${RUN_ROOT}"
echo "DeepSpeed:    ${DEEPSPEED_CONFIG}"
echo "CPT LR:       ${CPT_LR}  (epochs: ${CPT_EPOCHS})"
echo "SFT LR:       ${SFT_LR}  (epochs: ${SFT_EPOCHS})"
echo "============================================================"

# ---- Stage 1: Continued Pretraining (full) ----
echo "[Stage 1/2] Full CPT starting..."
torchrun --nproc_per_node="${NUM_GPUS}" --master_port=29500 \
  train_cpt_full.py \
    --model "${MODEL}" \
    --data "${CPT_JSONL}" \
    --out "${CPT_OUT}" \
    --max_seq_len "${MAX_SEQ_LEN}" \
    --epochs "${CPT_EPOCHS}" \
    --lr "${CPT_LR}" \
    --per_device_batch "${CPT_PER_DEVICE_BATCH}" \
    --grad_accum "${CPT_GRAD_ACCUM}" \
    --weight_decay "${WEIGHT_DECAY}" \
    --warmup_ratio "${WARMUP_RATIO}" \
    --bf16 \
    --deepspeed "${DEEPSPEED_CONFIG}" \
    --save_steps 500 \
    --logging_steps 25

echo "[Stage 1/2] Full CPT done. Output: ${CPT_OUT}"

# ---- Stage 2: SFT (full) ----
echo "[Stage 2/2] Full SFT starting..."
torchrun --nproc_per_node="${NUM_GPUS}" --master_port=29500 \
  train_sft_full.py \
    --model "${CPT_OUT}" \
    --data "${SFT_JSONL}" \
    --out "${SFT_OUT}" \
    --max_seq_len "${MAX_SEQ_LEN}" \
    --epochs "${SFT_EPOCHS}" \
    --lr "${SFT_LR}" \
    --per_device_batch "${SFT_PER_DEVICE_BATCH}" \
    --grad_accum "${SFT_GRAD_ACCUM}" \
    --weight_decay "${WEIGHT_DECAY}" \
    --warmup_ratio "${WARMUP_RATIO}" \
    --bf16 \
    --deepspeed "${DEEPSPEED_CONFIG}" \
    --system "${SYSTEM_PROMPT}" \
    --save_steps 200 \
    --eval_steps 200 \
    --logging_steps 10

echo "[Stage 2/2] Full SFT done. Output: ${SFT_OUT}"
echo "============================================================"
echo "All done: ${RUN_ROOT}"
echo ""
echo "Output is a complete model — no LoRA merging needed."
echo "Convert directly to GGUF:"
echo "  python llama.cpp/convert_hf_to_gguf.py ${SFT_OUT} \\"
echo "    --outfile ${SAFE_MODEL}-krobotkin-full.Q8_0.gguf --outtype q8_0"
echo "============================================================"
