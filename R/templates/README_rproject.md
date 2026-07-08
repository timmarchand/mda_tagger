# MDA Tagger — Exported R Project

Exported from [MDA Tagger](https://github.com/timmarchand/mda_tagger) by Tim Marchand.

## Project structure

```
├── data/
│   ├── tokenised_data.csv   # Raw input texts with doc_id and metadata
│   └── tagged_data.csv      # Fully tagged and scored results
└── r_docs/
    └── [script].R           # Analysis script (see below)
```

## Getting started

1. Open the `.Rproj` file in RStudio
2. Install any missing packages listed at the top of the script
3. Open the script in `r_docs/` and run it section by section

## Required packages

Install missing packages with:

```r
install.packages(c("tidyverse", "udpipe", "purrr", "stringr", "readr", "ggplot2"))
```

## Data

- `tokenised_data.csv` — one row per document with raw text, doc_id and metadata
- `tagged_data.csv` — one row per document with tagged text, dimension scores, and feature counts

## Script

**tagging.R** — loads raw texts, runs UDPipe POS tagging, extracts MDA features, and scores Biber dimensions. Includes examples for filtering and comparing groups.

**plotting.R** — produces 5 visualisations: dimension scores per text, mean scores by group, scatter plots, feature heatmap, and boxplots. Each plot can be saved with `ggsave()`.

**kwic.R** — KWIC concordance functions for token/phrase search and tag bundle search. Includes `print_kwic()` for console display and examples for saving results to CSV.

## Citation

If you use this in your research, please cite:

> Marchand, T. (2026). *MDA Tagger: A Multi-Dimensional Analysis toolkit for linguistic corpus analysis* [Software]. Retrieved from https://github.com/timmarchand/mda_tagger

> Biber, D. (1988). *Variation across speech and writing*. Cambridge University Press.
