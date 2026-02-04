A curated text corpus of writings by Pyotr Kropotkin, prepared for computational and language-model–based analysis.

This repository contains cleaned and compiled text from public-domain works by Kropotkin, organized as a machine-readable dataset.

## Contents
```
/
├── data/
│   ├── raw/                  # Source text files
│   └── kropotkin_corpus/     # Cleaned and compiled corpus
├── compileCorpus.R           # Script used to build the corpus
├── krobotkin.Rproj
└── README.md
```

**data/raw** – Original source texts (public domain)

**data/kropotkin_corpus** – Normalized plain-text corpus

**compileCorpus.R** – R script to clean and compile the dataset

### Training data summary

- Works: **88**
- Units (chapter/section files): **320**
- Total words (unit bodies): **1,411,187**
- Total characters (unit bodies): **8,318,162**
- Unit words (min/median/mean/max): **199 / 1431 / 4410 / 204074**
- Unit chars (min/median/mean/max): **1227 / 8679 / 25994.3 / 1205630**

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
