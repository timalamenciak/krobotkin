#!/usr/bin/env python3
"""
train_cpt_full.py — Full fine-tuning continued pretraining (CPT) with DeepSpeed.

Replaces LoRA with full parameter updates for maximum value absorption
from the Kropotkin corpus. Uses DeepSpeed ZeRO Stage 2 to shard optimizer
states across GPUs.

Input dataset: JSON/JSONL with {"text": "..."}.

Example (launched via SLURM script or manually):
  deepspeed --num_gpus=4 train_cpt_full.py \
    --model EleutherAI/pythia-2.8b \
    --data data/kropotkin_cpt.jsonl \
    --out runs/pythia28b_cpt_full \
    --bf16 \
    --deepspeed ds_zero2.json
"""

import argparse
import os
import random

import torch
from datasets import load_dataset
from transformers import (
    AutoTokenizer,
    AutoModelForCausalLM,
    TrainingArguments,
    Trainer,
    DataCollatorForLanguageModeling,
)


def seed_everything(seed: int) -> None:
    random.seed(seed)
    torch.manual_seed(seed)
    torch.cuda.manual_seed_all(seed)


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--model", required=True)
    ap.add_argument("--data", required=True, help="JSONL/JSON/Parquet with a 'text' column")
    ap.add_argument("--out", required=True)
    ap.add_argument("--max_seq_len", type=int, default=2048)
    ap.add_argument("--epochs", type=float, default=3.0)
    ap.add_argument("--lr", type=float, default=2e-5,
                    help="Learning rate — lower than LoRA (default: 2e-5)")
    ap.add_argument("--per_device_batch", type=int, default=2)
    ap.add_argument("--grad_accum", type=int, default=8)
    ap.add_argument("--warmup_ratio", type=float, default=0.05)
    ap.add_argument("--weight_decay", type=float, default=0.01,
                    help="Weight decay for full fine-tuning (default: 0.01)")
    ap.add_argument("--seed", type=int, default=42)
    ap.add_argument("--eval_ratio", type=float, default=0.01)

    ap.add_argument("--bf16", action="store_true")
    ap.add_argument("--fp16", action="store_true")

    ap.add_argument("--logging_steps", type=int, default=25)
    ap.add_argument("--save_steps", type=int, default=500)
    ap.add_argument("--save_total_limit", type=int, default=3)

    ap.add_argument("--smoke", action="store_true",
                    help="Run a tiny 1-step sanity train and exit.")
    ap.add_argument("--max_train_samples", type=int, default=None)
    ap.add_argument("--max_eval_samples", type=int, default=None)
    ap.add_argument("--max_steps", type=int, default=-1)
    ap.add_argument("--no_save", action="store_true")

    # DeepSpeed — passed through to TrainingArguments
    ap.add_argument("--deepspeed", type=str, default=None,
                    help="Path to DeepSpeed config JSON (e.g., ds_zero2.json)")
    # HuggingFace adds --local_rank for distributed; accept it silently
    ap.add_argument("--local_rank", type=int, default=-1)

    args = ap.parse_args()
    os.makedirs(args.out, exist_ok=True)
    seed_everything(args.seed)

    # ---- Tokenizer ----
    tokenizer = AutoTokenizer.from_pretrained(args.model, use_fast=True)
    if tokenizer.pad_token is None:
        tokenizer.pad_token = tokenizer.eos_token

    # ---- Model (full precision, no quantization) ----
    dtype = torch.bfloat16 if args.bf16 else (torch.float16 if args.fp16 else None)

    model = AutoModelForCausalLM.from_pretrained(
        args.model,
        dtype=dtype,
        trust_remote_code=False,
        # Do NOT set device_map with DeepSpeed — it handles placement
    )
    model.config.use_cache = False

    # All parameters are trainable for full fine-tuning
    trainable = sum(p.numel() for p in model.parameters() if p.requires_grad)
    total = sum(p.numel() for p in model.parameters())
    print(f"Full fine-tuning: {trainable:,} / {total:,} params trainable "
          f"({100 * trainable / total:.1f}%)")
    assert trainable == total, "Expected all parameters to be trainable"

    # ---- Dataset ----
    if os.path.exists(args.data):
        ext = os.path.splitext(args.data)[1].lower().lstrip(".")
        if ext in ("jsonl", "json"):
            ds = load_dataset("json", data_files=args.data)["train"]
        elif ext == "parquet":
            ds = load_dataset("parquet", data_files=args.data)["train"]
        elif ext == "csv":
            ds = load_dataset("csv", data_files=args.data)["train"]
        else:
            raise ValueError(f"Unsupported data extension: .{ext}")
    else:
        ds = load_dataset(args.data, split="train")

    if "text" not in ds.column_names:
        raise ValueError(f"Dataset must contain a 'text' column. Found: {ds.column_names}")

    # Train/eval split
    split = ds.train_test_split(test_size=args.eval_ratio, seed=args.seed)
    train_ds, eval_ds = split["train"], split["test"]

    if args.max_train_samples:
        train_ds = train_ds.select(range(min(len(train_ds), args.max_train_samples)))
    if args.max_eval_samples:
        eval_ds = eval_ds.select(range(min(len(eval_ds), args.max_eval_samples)))

    if args.smoke:
        train_ds = train_ds.select(range(min(len(train_ds), 64)))
        eval_ds = eval_ds.select(range(min(len(eval_ds), 16)))

    # ---- Tokenize ----
    def tokenize(batch):
        return tokenizer(batch["text"], truncation=False)

    train_ds = train_ds.map(tokenize, batched=True, remove_columns=train_ds.column_names)
    eval_ds = eval_ds.map(tokenize, batched=True, remove_columns=eval_ds.column_names)

    # ---- Group into fixed-size blocks ----
    block_size = args.max_seq_len

    def group_texts(examples):
        concatenated = {k: sum(examples[k], []) for k in examples.keys()}
        total_len = len(concatenated["input_ids"])
        if total_len >= block_size:
            total_len = (total_len // block_size) * block_size
        result = {
            k: [t[i: i + block_size] for i in range(0, total_len, block_size)]
            for k, t in concatenated.items()
        }
        result["labels"] = result["input_ids"].copy()
        return result

    train_ds = train_ds.map(group_texts, batched=True)
    eval_ds = eval_ds.map(group_texts, batched=True)

    print(f"Training blocks: {len(train_ds)}, Eval blocks: {len(eval_ds)}")

    # ---- Fix bf16 detection ----
    # On the NIBI cluster, ROCm is installed alongside CUDA, which confuses
    # both DeepSpeed's accelerator detection and HuggingFace's
    # is_torch_bf16_gpu_available() check. The H100s absolutely support bf16,
    # but the check fails because torch.version.cuda may not be detected
    # properly in the DeepSpeed-spawned processes. Monkey-patch the check.
    import transformers.utils.import_utils
    transformers.utils.import_utils.is_torch_bf16_gpu_available = lambda: True
    transformers.utils.import_utils.is_torch_bf16_available = lambda: True

    # ---- Training ----
    collator = DataCollatorForLanguageModeling(tokenizer=tokenizer, mlm=False)

    ta_kwargs = dict(
        output_dir=args.out,
        num_train_epochs=args.epochs,
        learning_rate=args.lr,
        weight_decay=args.weight_decay,
        warmup_ratio=args.warmup_ratio,
        per_device_train_batch_size=args.per_device_batch,
        per_device_eval_batch_size=1,
        gradient_accumulation_steps=args.grad_accum,
        eval_steps=args.save_steps,
        eval_strategy="steps",
        save_strategy="steps",
        save_steps=args.save_steps,
        save_total_limit=args.save_total_limit,
        logging_steps=args.logging_steps,
        bf16=args.bf16,
        fp16=args.fp16 and not args.bf16,
        gradient_checkpointing=True,
        gradient_checkpointing_kwargs={"use_reentrant": False},
        optim="adamw_torch",
        lr_scheduler_type="cosine",
        report_to="none",
        # DeepSpeed
        deepspeed=args.deepspeed,
        # Saving: let all ranks participate (needed for ZeRO)
        save_on_each_node=False,
    )

    if args.max_steps and args.max_steps > 0:
        ta_kwargs["max_steps"] = args.max_steps

    if args.smoke:
        ta_kwargs["max_steps"] = 1
        ta_kwargs["save_steps"] = 999999
        ta_kwargs["eval_steps"] = 999999
        ta_kwargs["logging_steps"] = 1

    training_args = TrainingArguments(**ta_kwargs)

    trainer = Trainer(
        model=model,
        args=training_args,
        train_dataset=train_ds,
        eval_dataset=eval_ds,
        data_collator=collator,
    )

    trainer.train()

    if not args.no_save and not args.smoke:
        # For DeepSpeed ZeRO: save consolidated model weights
        trainer.save_model(args.out)
        tokenizer.save_pretrained(args.out)
        print(f"Saved full CPT model + tokenizer to: {args.out}")


if __name__ == "__main__":
    main()
