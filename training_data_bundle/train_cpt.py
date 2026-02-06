#!/usr/bin/env python3
"""
Continued Pretraining (CPT) / language-modeling on raw text.

Input dataset: JSON/JSONL with {"text": "..."}.

Key difference vs SFT:
- no prompt/response formatting
- just next-token prediction on contiguous text blocks

Example:
  accelerate launch train_cpt.py \
    --model EleutherAI/pythia-2.8b \
    --data data/kropotkin_cpt.jsonl \
    --out runs/pythia28b_cpt \
    --use_4bit --bf16
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
    BitsAndBytesConfig,
)
from peft import LoraConfig, get_peft_model, prepare_model_for_kbit_training


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
    ap.add_argument("--epochs", type=float, default=1.0)
    ap.add_argument("--lr", type=float, default=1e-4)
    ap.add_argument("--per_device_batch", type=int, default=1)
    ap.add_argument("--grad_accum", type=int, default=16)
    ap.add_argument("--warmup_ratio", type=float, default=0.03)
    ap.add_argument("--weight_decay", type=float, default=0.0)
    ap.add_argument("--seed", type=int, default=42)
    ap.add_argument("--eval_ratio", type=float, default=0.01)

    ap.add_argument("--use_4bit", action="store_true")
    ap.add_argument("--bf16", action="store_true")
    ap.add_argument("--fp16", action="store_true")

    # LoRA knobs (CPT with LoRA is generally safest on limited compute)
    ap.add_argument("--lora_r", type=int, default=16)
    ap.add_argument("--lora_alpha", type=int, default=32)
    ap.add_argument("--lora_dropout", type=float, default=0.05)

    ap.add_argument("--logging_steps", type=int, default=25)
    ap.add_argument("--save_steps", type=int, default=500)
    ap.add_argument("--save_total_limit", type=int, default=3)

    args = ap.parse_args()
    os.makedirs(args.out, exist_ok=True)
    seed_everything(args.seed)

    # Tokenizer
    tokenizer = AutoTokenizer.from_pretrained(args.model, use_fast=True)
    if tokenizer.pad_token is None:
        tokenizer.pad_token = tokenizer.eos_token

    # Dtype / quant
    dtype = torch.bfloat16 if args.bf16 else (torch.float16 if args.fp16 else None)
    quant = None
    if args.use_4bit:
        quant = BitsAndBytesConfig(
            load_in_4bit=True,
            bnb_4bit_compute_dtype=dtype or torch.float16,
            bnb_4bit_quant_type="nf4",
            bnb_4bit_use_double_quant=True,
        )

    model = AutoModelForCausalLM.from_pretrained(
        args.model,
        torch_dtype=dtype,
        quantization_config=quant,
        device_map="auto" if args.use_4bit else None,
        trust_remote_code=False,
    )
    model.config.use_cache = False

    if args.use_4bit:
        model = prepare_model_for_kbit_training(model)

    # For GPT-NeoX/Pythia, this target is usually the right one.
    lora = LoraConfig(
        r=args.lora_r,
        lora_alpha=args.lora_alpha,
        lora_dropout=args.lora_dropout,
        bias="none",
        task_type="CAUSAL_LM",
        target_modules=["query_key_value"],  # neoX attention projection
    )
    model = get_peft_model(model, lora)

    # Load dataset
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

    # Tokenize
    def tokenize(batch):
        return tokenizer(batch["text"], truncation=False)

    train_ds = train_ds.map(tokenize, batched=True, remove_columns=train_ds.column_names)
    eval_ds = eval_ds.map(tokenize, batched=True, remove_columns=eval_ds.column_names)

    # Group into fixed-size blocks for LM training
    block_size = args.max_seq_len

    def group_texts(examples):
        # Concatenate
        concatenated = {k: sum(examples[k], []) for k in examples.keys()}
        total_len = len(concatenated["input_ids"])
        if total_len >= block_size:
            total_len = (total_len // block_size) * block_size
        result = {
            k: [t[i : i + block_size] for i in range(0, total_len, block_size)]
            for k, t in concatenated.items()
        }
        result["labels"] = result["input_ids"].copy()
        return result

    train_ds = train_ds.map(group_texts, batched=True)
    eval_ds = eval_ds.map(group_texts, batched=True)

    collator = DataCollatorForLanguageModeling(tokenizer=tokenizer, mlm=False)

    training_args = TrainingArguments(
        output_dir=args.out,
        num_train_epochs=args.epochs,
        learning_rate=args.lr,
        weight_decay=args.weight_decay,
        warmup_ratio=args.warmup_ratio,
        per_device_train_batch_size=args.per_device_batch,
        per_device_eval_batch_size=1,
        gradient_accumulation_steps=args.grad_accum,
        evaluation_strategy="steps",
        eval_steps=args.save_steps,
        save_strategy="steps",
        save_steps=args.save_steps,
        save_total_limit=args.save_total_limit,
        logging_steps=args.logging_steps,
        bf16=args.bf16,
        fp16=args.fp16 and not args.bf16,
        gradient_checkpointing=True,
        optim="paged_adamw_8bit" if args.use_4bit else "adamw_torch",
        lr_scheduler_type="cosine",
        report_to="none",
    )

    trainer = Trainer(
        model=model,
        args=training_args,
        train_dataset=train_ds,
        eval_dataset=eval_ds,
        data_collator=collator,
    )

    trainer.train()
    trainer.save_model(args.out)
    tokenizer.save_pretrained(args.out)
    print(f"Saved CPT LoRA adapters + tokenizer to: {args.out}")


if __name__ == "__main__":
    main()