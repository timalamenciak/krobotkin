# ⋈ krobotkin
A curated text corpus of writings by Pyotr Kropotkin, prepared for computational and language-model–based analysis.

This repository contains cleaned and compiled text from public-domain works by Kropotkin, organized as a machine-readable dataset.

## Contents
```
⋈ krobotkin
|   AL_Downloader.R #Downloads texts from Anarchist Library
|   compileCorpus.R #Compiles the corpus into chunks
|   goldInstructionJSON.r #Creates instruction training set for SFT
|   goldPassages.R #Extracts passages for the creation of instruction training set 
|   krobotkin.Rproj #R Project file
|   LICENSE
|   README.md #You are here
|
+---data
|   |   NoteworthyExclusions.txt #I left a couple things out - here's why
|   |   sources.csv #All sources
|   |   sources_back.csv #A backup of the original sourcelist
|   |
|   \---raw     #Raw texts are here
|
+---instruction_data
|       gold_manifest.csv
|       instructions.jsonl #Final instruction tuning data
|       instructions_metadata.csv
|
+---kropotkin_corpus
|   |   kropotkin_cpt.jsonl #Final corpus file
|   |   prep_corpus_from_manifest.py
|   \---Many subfolders of the corpus
|
\---training_data_bundle #Ready to go bundle for uploading to a data center
    |   pythia_cpt_sft.slurm.sh
    |   train_cpt.py
    |   train_sft.py
    |
    \---data
            instructions.jsonl
            kropotkin_cpt.jsonl
```

#### Important locations

**data/raw** – Original source texts (public domain)

**kropotkin_corpus** – Normalized plain-text corpus

**instruction_data** - Instruction dataset generated mainly with Claude

**training_data_bundle** - All the stuff to train

### Training data summary

#### Corpus

- Works: **88**
- Units (chapter/section files): **320**
- Total words (unit bodies): **1,411,187**
- Total characters (unit bodies): **8,318,162**
- Unit words (min/median/mean/max): **199 / 1431 / 4410 / 204074**
- Unit chars (min/median/mean/max): **1227 / 8679 / 25994.3 / 1205630**

#### Instructions

|Example type  | n  |
|--------------|----|
| contrastive | 150 |
| grounded    |1485| 
| modern_synthesis |30|

## Usage

The compiled corpus is suitable for:
* Text analysis
* NLP pipelines
* Training or fine-tuning language models
* Stylistic or comparative analyses

To regenerate the corpus from source files, run:

`source("compileCorpus.R")`

## Sources

Texts are drawn from public-domain works by Pyotr Kropotkin, including major books and essays. They have been sourced from [Project Gutenberg](https://www.gutenberg.org/), [Wikisource](https://en.wikisource.org/wiki/Main_Page) and the [Anarchist Library](https://theanarchistlibrary.org/).

## License

This repository is released under the MIT License.
Source texts are believed to be in the public domain.
