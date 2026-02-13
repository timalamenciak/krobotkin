#!/usr/bin/env python3
"""
Merge PEFT/LoRA adapters into the base model and save a full-weight checkpoint
that can then be converted to GGUF for Ollama.

Usage:
  python merge_and_export.py \
    --adapter_path runs/EleutherAI_pythia-2.8b_20260208_115700/sft \
    --base_model EleutherAI/pythia-2.8b \
    --out merged/pythia-2.8b-krobotkin
"""

import argparse
import torch
from transformers import AutoModelForCausalLM, AutoTokenizer
from peft import PeftModel


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--adapter_path", required=True, help="Path to the SFT LoRA adapter directory")
    ap.add_argument("--base_model", required=True, help="Base model name or path (e.g. EleutherAI/pythia-2.8b)")
    ap.add_argument("--out", required=True, help="Output directory for the merged model")
    ap.add_argument("--bf16", action="store_true", default=True, help="Save in bf16 (default)")
    ap.add_argument("--fp16", action="store_true", help="Save in fp16 instead")
    args = ap.parse_args()

    dtype = torch.float16 if args.fp16 else torch.bfloat16

    print(f"Loading base model: {args.base_model}")
    base_model = AutoModelForCausalLM.from_pretrained(
        args.base_model,
        torch_dtype=dtype,
        device_map="cpu",
        trust_remote_code=False,
    )
    # Try loading tokenizer from the adapter dir first (if CPT saved one there);
    # fall back to the base model if not found.
    import os
    tokenizer_candidates = [args.adapter_path, args.base_model]
    for src in tokenizer_candidates:
        if os.path.isdir(src) and os.path.exists(os.path.join(src, "tokenizer.json")):
            tokenizer = AutoTokenizer.from_pretrained(src, use_fast=True)
            print(f"Loaded tokenizer from: {src}")
            break
    else:
        tokenizer = AutoTokenizer.from_pretrained(args.base_model, use_fast=True)
        print(f"Loaded tokenizer from base model: {args.base_model}")

    print(f"Loading LoRA adapter: {args.adapter_path}")
    model = PeftModel.from_pretrained(base_model, args.adapter_path)

    print("Merging LoRA weights into base model...")
    model = model.merge_and_unload()

    print(f"Saving merged model to: {args.out}")
    model.save_pretrained(args.out, safe_serialization=True)
    tokenizer.save_pretrained(args.out)

    print("Done! You can now convert this to GGUF with llama.cpp")


if __name__ == "__main__":
    main()
