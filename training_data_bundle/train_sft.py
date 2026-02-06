#!/usr/bin/env python3
"""
SFT / instruction fine-tuning using TRL SFTTrainer.

Supports datasets with any of:
- {"instruction": "...", "response": "..."}   <-- your case
- {"instruction": "...", "input": "...", "output": "..."}
- {"prompt": "...", "response": "..."}
- {"messages": [...]}  (chat)
- {"text": "..."}      (already formatted)

Example:
  accelerate launch train_sft.py \
    --model runs/pythia69b_cpt \
    --data data/instructions.jsonl \
    --out runs/pythia69b_sft \
    --use_4bit --bf16
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
    BitsAndBytesConfig,
    TrainingArguments,
)
from peft import LoraConfig, get_peft_model, prepare_model_for_kbit_training
from trl import SFTTrainer


def seed_everything(seed: int) -> None:
    random.seed(seed)
    torch.manual_seed(seed)
    torch.cuda.manual_seed_all(seed)


def format_example(ex: Dict[str, Any], tokenizer: AutoTokenizer, system_default: Optional[str]) -> Dict[str, str]:
    # 1) already formatted
    if isinstance(ex.get("text"), str) and ex["text"].strip():
        return {"text": ex["text"].strip()}

    # 2) chat
    if isinstance(ex.get("messages"), list):
        msgs = ex["messages"]
        if system_default:
            has_system = any(isinstance(m, dict) and m.get("role") == "system" for m in msgs)
            if not has_system:
                msgs = [{"role": "system", "content": system_default}] + msgs
        try:
            rendered = tokenizer.apply_chat_template(msgs, tokenize=False, add_generation_prompt=False)
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
            text = f"{sys}### Instruction:\n{instr}\n\n### Input:\n{inp}\n\n### Response:\n{resp}"
        else:
            text = f"{sys}### Instruction:\n{instr}\n\n### Response:\n{resp}"
        return {"text": text.strip()}

    raise ValueError(
        f"Could not infer example format. Keys present: {list(ex.keys())}. "
        "Need text, messages, or instruction/response."
    )


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--model", required=True)
    ap.add_argument("--data", required=True)
    ap.add_argument("--out", required=True)
    ap.add_argument("--max_seq_len", type=int, default=2048)

    ap.add_argument("--use_4bit", action="store_true")
    ap.add_argument("--bf16", action="store_true")
    ap.add_argument("--fp16", action="store_true")

    ap.add_argument("--epochs", type=float, default=3.0)
    ap.add_argument("--lr", type=float, default=2e-4)
    ap.add_argument("--warmup_ratio", type=float, default=0.03)
    ap.add_argument("--weight_decay", type=float, default=0.0)

    ap.add_argument("--per_device_batch", type=int, default=1)
    ap.add_argument("--grad_accum", type=int, default=16)
    ap.add_argument("--eval_ratio", type=float, default=0.02)
    ap.add_argument("--seed", type=int, default=42)

    ap.add_argument("--lora_r", type=int, default=16)
    ap.add_argument("--lora_alpha", type=int, default=32)
    ap.add_argument("--lora_dropout", type=float, default=0.05)

    ap.add_argument("--system", default=None)

    ap.add_argument("--logging_steps", type=int, default=10)
    ap.add_argument("--save_steps", type=int, default=200)
    ap.add_argument("--eval_steps", type=int, default=200)
    ap.add_argument("--save_total_limit", type=int, default=3)

    args = ap.parse_args()
    os.makedirs(args.out, exist_ok=True)
    seed_everything(args.seed)

    tokenizer = AutoTokenizer.from_pretrained(args.model, use_fast=True)
    if tokenizer.pad_token is None:
        tokenizer.pad_token = tokenizer.eos_token

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

    # Pythia/GPT-NeoX: query_key_value is the main attention projection
    lora_config = LoraConfig(
        r=args.lora_r,
        lora_alpha=args.lora_alpha,
        lora_dropout=args.lora_dropout,
        bias="none",
        task_type="CAUSAL_LM",
        target_modules=["query_key_value"],
    )
    model = get_peft_model(model, lora_config)

    # Load dataset
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

    # Choose split or create one
    if isinstance(ds, dict) and "train" in ds:
        base = ds["train"]
    else:
        base = ds if not isinstance(ds, dict) else ds[list(ds.keys())[0]]

    split = base.train_test_split(test_size=args.eval_ratio, seed=args.seed)
    train_ds, eval_ds = split["train"], split["test"]

    def _map(ex):
        return format_example(ex, tokenizer=tokenizer, system_default=args.system)

    train_ds = train_ds.map(_map, remove_columns=train_ds.column_names)
    eval_ds = eval_ds.map(_map, remove_columns=eval_ds.column_names)

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
        eval_steps=args.eval_steps,
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

    trainer = SFTTrainer(
        model=model,
        tokenizer=tokenizer,
        train_dataset=train_ds,
        eval_dataset=eval_ds,
        dataset_text_field="text",
        max_seq_length=args.max_seq_len,
        packing=True,
        args=training_args,
    )

    trainer.train()
    trainer.model.save_pretrained(args.out)
    tokenizer.save_pretrained(args.out)
    print(f"Saved SFT LoRA adapters + tokenizer to: {args.out}")


if __name__ == "__main__":
    main()