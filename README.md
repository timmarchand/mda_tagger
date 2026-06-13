# MDA Tagger

Multi-Dimensional Analysis toolkit for linguistic corpus analysis based
on Biber (1988).

## 🌐 Live App

Try the app online: [MDA
Tagger](https://timmarchand-mda-tagger.share.connect.posit.cloud/)

## Quick Start

### Prerequisites

-   R ≥ 4.0.0
-   RStudio (recommended)

### New to R and RStudio?

If you have never used R before, follow these steps first:

1.  **Install R** from <https://cran.r-project.org> — choose the version
    for your operating system
2.  **Install RStudio** from <https://posit.co/download/rstudio-desktop>
    — this is the interface you will use to run R
3.  Once both are installed, open **RStudio** (not R directly)
4.  You are now ready to follow the installation steps below. Note that
    some steps use the **Console** panel (bottom left) and some use the
    **Terminal** panel (next to the Console tab) — these are marked
    separately

### Installation

1.  **Clone this repository** in the RStudio Terminal (not the Console —
    find it under Tools \> Terminal \> New Terminal):

``` bash
git clone https://github.com/timmarchand/mda_tagger.git
cd mda_tagger
```

Or download the ZIP from GitHub and extract it to a folder on your
computer.

2.  **Open in RStudio:**
    -   Open `mda_app.Rproj`
3.  **Restore package environment** in the RStudio Console:

``` r
renv::restore()
```

This will automatically install all required packages. It may take a few
minutes the first time.

4.  **Run the app** in the RStudio Console:

``` r
shiny::runApp()
```

The app will open in your browser. You do not need to install anything
else.

## Project Structure

mda_app/ ├── app.R \# Main Shiny app ├── global.R \# Setup &
configuration ├── R/ \# Core functions ├── modules/ \# Shiny modules ├──
data/ \# Reference data ├── www/ \# Web assets (CSS, images) └── tests/
\# Unit tests

## Development

### Building Incrementally

Files are being added incrementally. Track progress: - [x] Project
setup - [x] Folder structure - [x] Core utility functions - [x] Data
input module - [x] Processing module - [x] Visualization module - [x]
Export functionality - [x] KWIC concordancing of tags - [x] Export of R
code for plots

### Testing Locally

``` r
# Test individual functions
source("R/01_utils.R")
# Test modules
source("modules/ui_data_input.R")
source("modules/server_data_input.R")
```

## Features

-   Multi-format file upload (TXT, CSV, DOCX)
-   Corpus metadata management
-   POS tagging with UDPipe
-   67+ linguistic feature extraction
-   5-dimensional MDA scoring
-   Text type classification
-   Interactive visualizations

## Contributing

This is an active development project. Stay tuned for updates!

## License

MIT License - see LICENSE file

## Citation

If you use this app in your research, please cite it as follows:

> Marchand, T. (2026). *MDA Tagger: A Multi-Dimensional Analysis toolkit
> for linguistic corpus analysis* [Software]. Retrieved from
> <https://github.com/timmarchand/mda_tagger>

Please also cite the foundational work this app is based on:

> Biber, D. (1988). *Variation across speech and writing*. Cambridge
> University Press.

If you use the POS tagging functionality, please also cite UDPipe:

> Straka, M., & Straková, J. (2017). Tokenizing, POS tagging,
> lemmatizing and parsing UD 2.0 with UDPipe. In *Proceedings of the
> CoNLL 2017 Shared Task: Multilingual Parsing from Raw Text to
> Universal Dependencies* (pp. 88–99). Association for Computational
> Linguistics.
