#!/bin/bash
#SBATCH --job-name=pythia_cpt_sft
#SBATCH --nodes=1
#SBATCH --gres=gpu:4
#SBATCH --cpus-per-task=16
#SBATCH --mem=64G
#SBATCH --time=24:00:00
#SBATCH --array=0-1
#SBATCH --output=logs/%x_%A_%a.out
#SBATCH --error=logs/%x_%A_%a.err

set -euo pipefail
mkdir -p logs runs

# ----------------- environment (customize) -----------------
# module load cuda/12.1
# source ~/miniconda3/etc/profile.d/conda.sh
# conda activate llm

# One-time installs:
# pip install -U "torch" "transformers>=4.41.0" "datasets>=2.19.0" "accelerate>=0.30.0" "trl>=0.9.0" "peft>=0.11.0" bitsandbytes deepspeed

export HF_HOME="${HF_HOME:-$HOME/.cache/huggingface}"
export TRANSFORMERS_CACHE="${TRANSFORMERS_CACHE:-$HF_HOME/transformers}"
export TOKENIZERS_PARALLELISM=false

# ----------------- inputs (YOU upload these) -----------------
CPT_JSONL="data/kropotkin_cpt.jsonl"
SFT_JSONL="data/instructions.jsonl"

if [[ ! -f "$CPT_JSONL" ]]; then
  echo "ERROR: Missing CPT dataset: $CPT_JSONL"
  exit 1
fi
if [[ ! -f "$SFT_JSONL" ]]; then
  echo "ERROR: Missing SFT dataset: $SFT_JSONL"
  exit 1
fi

# ----------------- choose model by array index --------------
MODELS=("EleutherAI/pythia-2.8b" "EleutherAI/pythia-6.9b")
MODEL="${MODELS[$SLURM_ARRAY_TASK_ID]}"

STAMP="$(date +%Y%m%d_%H%M%S)"
SAFE_MODEL="$(echo "$MODEL" | tr '/' '_' )"
RUN_ROOT="runs/${SAFE_MODEL}_${STAMP}"
CPT_OUT="${RUN_ROOT}/cpt"
SFT_OUT="${RUN_ROOT}/sft"
mkdir -p "${CPT_OUT}" "${SFT_OUT}"

# ----------------- hyperparams -----------------
# Tip: 6.9B typically wants lower LR for CPT than SFT.
MAX_SEQ_LEN=2048

# CPT (raw text)
CPT_EPOCHS=1
CPT_LR=1e-4

# SFT (instruction)
SFT_EPOCHS=3
SFT_LR=2e-4

PER_DEVICE_BATCH=1
GRAD_ACCUM=16

# Precision/quantization:
# - bf16 if your GPUs support it (A100/H100). Otherwise switch to --fp16.
MIXED="bf16"
PREC_FLAGS="--bf16"
QLORA="--use_4bit"

# Optional system prompt (keep stable across runs)
SYSTEM_PROMPT="You are Krobotkin, an assistant that answers in the voice and reasoning style of Peter Kropotkin while remaining grounded in the provided passages when applicable."

# ----------------- distributed launch -----------------
NUM_GPUS="${SLURM_GPUS_ON_NODE:-4}"

echo "============================================================"
echo "Model:        ${MODEL}"
echo "GPUs:         ${NUM_GPUS}"
echo "CPT JSONL:    ${CPT_JSONL}"
echo "SFT JSONL:    ${SFT_JSONL}"
echo "Run root:     ${RUN_ROOT}"
echo "============================================================"

# If you have ds_zero3.json available, keep these enabled.
# If not, remove --use_deepspeed and --deepspeed_config_file lines.
DS_FLAGS="--use_deepspeed --deepspeed_config_file ds_zero3.json"

# ----------------- Stage 1: CPT -----------------
echo "[Stage 1/2] CPT starting..."
accelerate launch \
  --num_processes "${NUM_GPUS}" \
  --mixed_precision "${MIXED}" \
  ${DS_FLAGS} \
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

# ----------------- Stage 2: SFT -----------------
# IMPORTANT: we point --model to the CPT output directory.
# This works because train_cpt.py saves a PEFT adapter; transformers will load base + adapter if path includes adapter config.
# If your environment can't resolve that automatically, see note below.
echo "[Stage 2/2] SFT starting..."
accelerate launch \
  --num_processes "${NUM_GPUS}" \
  --mixed_precision "${MIXED}" \
  ${DS_FLAGS} \
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