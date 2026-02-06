#!/usr/bin/env python3
"""
Build a Continued Pretraining (CPT) dataset JSONL from a manifest.csv.

Input:
  - manifest.csv with at least: out_path (and optionally title/year/etc.)
  - corpus root directory that contains the out_path files

Output:
  - JSONL with {"text": "...", "work_id": "...", ... optional metadata}

Example:
  python prep_corpus_from_manifest.py \
    --manifest kropotkin_corpus/manifest.csv \
    --corpus_root . \
    --out data/kropotkin_cpt.jsonl
"""

import argparse
import csv
import json
import os
from pathlib import Path
from typing import Dict, List, Optional


def read_manifest(manifest_path: Path) -> List[Dict[str, str]]:
    with manifest_path.open("r", encoding="utf-8") as f:
        reader = csv.DictReader(f)
        rows = [r for r in reader]
    if not rows:
        raise ValueError(f"Manifest appears empty: {manifest_path}")
    if "out_path" not in rows[0]:
        raise ValueError("manifest.csv missing required column: out_path")
    return rows


def resolve_text_path(corpus_root: Path, out_path: str) -> Path:
    # out_path may already be absolute or relative
    p = Path(out_path)
    if p.is_absolute():
        return p
    return (corpus_root / p).resolve()


def load_text(p: Path) -> str:
    # Try utf-8, fall back to latin-1 if needed (historic texts sometimes have weird bytes)
    try:
        return p.read_text(encoding="utf-8")
    except UnicodeDecodeError:
        return p.read_text(encoding="latin-1")


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--manifest", required=True, help="Path to manifest.csv")
    ap.add_argument("--corpus_root", default=".", help="Root dir for resolving out_path")
    ap.add_argument("--out", default="kropotkin_cpt.jsonl", help="Output JSONL path for CPT")
    ap.add_argument("--min_chars", type=int, default=200, help="Skip files shorter than this")
    ap.add_argument("--include_metadata", action="store_true", help="Include work metadata fields in JSONL rows")
    args = ap.parse_args()

    manifest_path = Path(args.manifest).resolve()
    corpus_root = Path(args.corpus_root).resolve()
    out_path = Path(args.out).resolve()
    out_path.parent.mkdir(parents=True, exist_ok=True)

    rows = read_manifest(manifest_path)

    written = 0
    missing_files = 0
    skipped_short = 0

    with out_path.open("w", encoding="utf-8") as out_f:
        for r in rows:
            text_path = resolve_text_path(corpus_root, r["out_path"])
            if not text_path.exists():
                missing_files += 1
                continue

            txt = load_text(text_path).strip()
            if len(txt) < args.min_chars:
                skipped_short += 1
                continue

            obj = {"text": txt}

            if args.include_metadata:
                # Include a small, stable subset of fields (avoid huge bloat)
                for k in ["work_id", "title", "year", "author", "genre", "source_url", "sha256", "n_chars"]:
                    if k in r and r[k] not in (None, ""):
                        obj[k] = r[k]

            out_f.write(json.dumps(obj, ensure_ascii=False) + "\n")
            written += 1

    print("CPT dataset build complete.")
    print(f"  manifest rows:   {len(rows)}")
    print(f"  written:         {written}")
    print(f"  missing files:   {missing_files}")
    print(f"  skipped (short): {skipped_short}")
    print(f"  output:          {out_path}")

    if written == 0:
        raise RuntimeError(
            "Wrote 0 examples. Check --corpus_root and that manifest out_path values exist on disk."
        )


if __name__ == "__main__":
    main()