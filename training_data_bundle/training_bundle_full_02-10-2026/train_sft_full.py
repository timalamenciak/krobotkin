#!/usr/bin/env python3
"""
train_sft_full.py — Full fine-tuning SFT / instruction tuning with DeepSpeed.

Takes the fully fine-tuned CPT model as base and runs full-parameter SFT
on the instruction dataset. Uses DeepSpeed ZeRO Stage 2.

Supports datasets with:
- {"instruction": "...", "response": "..."}   <-- your case
- {"instruction": "...", "input": "...", "output": "..."}
- {"prompt": "...", "response": "..."}
- {"messages": [...]}  (chat)
- {"text": "..."}      (already formatted)

Example:
  deepspeed --num_gpus=4 train_sft_full.py \
    --model runs/pythia28b_cpt_full \
    --data data/instructions_all.jsonl \
    --out runs/pythia28b_sft_full \
    --bf16 \
    --deepspeed ds_zero2.json \
    --system "You are Krobotkin..."
"""

import argparse
import os
import random
from typing import Any, Dict, Optional

import torch
from datasets import load_dataset
from transformers import (
    AutoModelForCausalLM,
    AutoTokenizer,
    TrainingArguments,
    DataCollatorForSeq2Seq,
    Trainer,
)


def seed_everything(seed: int) -> None:
    random.seed(seed)
    torch.manual_seed(seed)
    torch.cuda.manual_seed_all(seed)


def format_example(
    ex: Dict[str, Any],
    tokenizer: AutoTokenizer,
    system_default: Optional[str],
) -> Dict[str, str]:
    """Convert any supported schema into a single 'text' field."""

    # 1) already formatted
    if isinstance(ex.get("text"), str) and ex["text"].strip():
        return {"text": ex["text"].strip()}

    # 2) chat messages
    if isinstance(ex.get("messages"), list):
        msgs = ex["messages"]
        if system_default:
            has_system = any(
                isinstance(m, dict) and m.get("role") == "system" for m in msgs
            )
            if not has_system:
                msgs = [{"role": "system", "content": system_default}] + msgs
        try:
            rendered = tokenizer.apply_chat_template(
                msgs, tokenize=False, add_generation_prompt=False
            )
            return {"text": rendered.strip()}
        except Exception:
            parts = []
            for m in msgs:
                if not isinstance(m, dict):
                    continue
                role = m.get("role", "user").upper()
                content = (m.get("content") or "").strip()
                if content:
                    parts.append(f"{role}: {content}")
            return {"text": "\n".join(parts).strip()}

    # 3) instruction/response (your schema)
    instr = ex.get("instruction")
    resp = ex.get("response")

    # 4) other common variants
    if instr is None:
        instr = ex.get("prompt") or ex.get("query")
    if resp is None:
        resp = ex.get("output") or ex.get("answer")

    inp = ex.get("input") or ex.get("context") or ""

    if isinstance(instr, str) and isinstance(resp, str):
        instr = instr.strip()
        resp = resp.strip()
        inp = inp.strip() if isinstance(inp, str) else ""

        sys = (system_default.strip() + "\n\n") if system_default else ""
        if inp:
            text = (
                f"{sys}### Instruction:\n{instr}\n\n"
                f"### Input:\n{inp}\n\n"
                f"### Response:\n{resp}"
            )
        else:
            text = (
                f"{sys}### Instruction:\n{instr}\n\n"
                f"### Response:\n{resp}"
            )
        return {"text": text.strip()}

    raise ValueError(
        f"Could not infer example format. Keys present: {list(ex.keys())}. "
        "Need text, messages, or instruction/response."
    )


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--model", required=True,
                    help="Path to the CPT model (full fine-tuned) or HF model name")
    ap.add_argument("--data", required=True)
    ap.add_argument("--out", required=True)
    ap.add_argument("--max_seq_len", type=int, default=2048)

    ap.add_argument("--bf16", action="store_true")
    ap.add_argument("--fp16", action="store_true")

    ap.add_argument("--epochs", type=float, default=3.0)
    ap.add_argument("--lr", type=float, default=5e-6,
                    help="Learning rate — very low for full SFT (default: 5e-6)")
    ap.add_argument("--warmup_ratio", type=float, default=0.05)
    ap.add_argument("--weight_decay", type=float, default=0.01)

    ap.add_argument("--per_device_batch", type=int, default=2)
    ap.add_argument("--grad_accum", type=int, default=4)
    ap.add_argument("--eval_ratio", type=float, default=0.02)
    ap.add_argument("--seed", type=int, default=42)

    ap.add_argument("--system", default=None,
                    help="System prompt to prepend to each instruction")

    ap.add_argument("--logging_steps", type=int, default=10)
    ap.add_argument("--save_steps", type=int, default=200)
    ap.add_argument("--eval_steps", type=int, default=200)
    ap.add_argument("--save_total_limit", type=int, default=3)

    ap.add_argument("--smoke", action="store_true")
    ap.add_argument("--max_train_samples", type=int, default=None)
    ap.add_argument("--max_eval_samples", type=int, default=None)
    ap.add_argument("--max_steps", type=int, default=-1)
    ap.add_argument("--no_save", action="store_true")

    # DeepSpeed
    ap.add_argument("--deepspeed", type=str, default=None)
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
    )
    model.config.use_cache = False

    trainable = sum(p.numel() for p in model.parameters() if p.requires_grad)
    total = sum(p.numel() for p in model.parameters())
    print(f"Full SFT: {trainable:,} / {total:,} params trainable "
          f"({100 * trainable / total:.1f}%)")

    # ---- Dataset ----
    if os.path.exists(args.data):
        ext = os.path.splitext(args.data)[1].lower().lstrip(".")
        if ext in ("jsonl", "json"):
            ds = load_dataset("json", data_files=args.data)
        elif ext == "parquet":
            ds = load_dataset("parquet", data_files=args.data)
        elif ext == "csv":
            ds = load_dataset("csv", data_files=args.data)
        else:
            raise ValueError(f"Unsupported data extension: .{ext}")
    else:
        ds = load_dataset(args.data)

    if isinstance(ds, dict) and "train" in ds:
        base = ds["train"]
    else:
        base = ds if not isinstance(ds, dict) else ds[list(ds.keys())[0]]

    split = base.train_test_split(test_size=args.eval_ratio, seed=args.seed)
    train_ds, eval_ds = split["train"], split["test"]

    if args.max_train_samples:
        train_ds = train_ds.select(range(min(len(train_ds), args.max_train_samples)))
    if args.max_eval_samples:
        eval_ds = eval_ds.select(range(min(len(eval_ds), args.max_eval_samples)))

    if args.smoke:
        train_ds = train_ds.select(range(min(len(train_ds), 64)))
        eval_ds = eval_ds.select(range(min(len(eval_ds), 16)))

    # ---- Format ----
    def _map(ex):
        return format_example(ex, tokenizer=tokenizer, system_default=args.system)

    train_ds = train_ds.map(_map, remove_columns=train_ds.column_names)
    eval_ds = eval_ds.map(_map, remove_columns=eval_ds.column_names)

    # ---- Tokenize ----
    def tokenize(batch):
        return tokenizer(
            batch["text"],
            truncation=True,
            max_length=args.max_seq_len,
            padding=False,
        )

    train_ds = train_ds.map(tokenize, batched=True, remove_columns=["text"])
    eval_ds = eval_ds.map(tokenize, batched=True, remove_columns=["text"])

    # Add labels (identical to input_ids for causal LM)
    def add_labels(examples):
        examples["labels"] = examples["input_ids"].copy()
        return examples

    train_ds = train_ds.map(add_labels, batched=False)
    eval_ds = eval_ds.map(add_labels, batched=False)

    print(f"Training examples: {len(train_ds)}, Eval examples: {len(eval_ds)}")

    # ---- Fix bf16 detection ----
    # On the NIBI cluster, ROCm is installed alongside CUDA, which confuses
    # both DeepSpeed's accelerator detection and HuggingFace's
    # is_torch_bf16_gpu_available() check. The H100s absolutely support bf16,
    # but the check fails. Monkey-patch the check.
    import transformers.utils.import_utils
    transformers.utils.import_utils.is_torch_bf16_gpu_available = lambda: True
    transformers.utils.import_utils.is_torch_bf16_available = lambda: True

    # ---- Training ----
    collator = DataCollatorForSeq2Seq(
        tokenizer=tokenizer,
        padding=True,
        pad_to_multiple_of=8,  # efficient for tensor cores
    )

    ta_kwargs = dict(
        output_dir=args.out,
        num_train_epochs=args.epochs,
        learning_rate=args.lr,
        weight_decay=args.weight_decay,
        warmup_ratio=args.warmup_ratio,
        per_device_train_batch_size=args.per_device_batch,
        per_device_eval_batch_size=1,
        gradient_accumulation_steps=args.grad_accum,
        eval_strategy="steps",
        eval_steps=args.eval_steps,
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
        trainer.save_model(args.out)
        tokenizer.save_pretrained(args.out)
        print(f"Saved full SFT model + tokenizer to: {args.out}")


if __name__ == "__main__":
    main()
