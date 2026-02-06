# MDA Shiny App

Multi-Dimensional Analysis toolkit for linguistic corpus analysis based on Biber (1988).

## 🚀 Quick Start

### Prerequisites
- R ≥ 4.0.0
- RStudio (recommended)

### Installation

1. **Clone this repository:**
```bash
git clone https://github.com/YOUR_USERNAME/mda_app.git
cd mda_app
```

2. **Open in RStudio:**
   - Open `mda_app.Rproj`

3. **Restore package environment:**
```r
# In RStudio Console
renv::restore()
```

4. **Run the app:**
```r
shiny::runApp()
```

## 📁 Project Structure
```
mda_app/
├── app.R              # Main Shiny app
├── global.R           # Setup & configuration
├── R/                 # Core functions
├── modules/           # Shiny modules
├── data/              # Reference data
├── www/               # Web assets (CSS, images)
└── tests/             # Unit tests
```

## 🔧 Development

### Building Incrementally

Files are being added incrementally. Track progress:

- [x] Project setup
- [x] Folder structure
- [ ] Core utility functions
- [ ] Data input module
- [ ] Processing module
- [ ] Visualization module
- [ ] Export functionality

### Testing Locally
```r
# Test individual functions
source("R/01_utils.R")

# Test modules
source("modules/ui_data_input.R")
source("modules/server_data_input.R")
```

## 📊 Features

- Multi-format file upload (TXT, CSV, DOCX)
- Corpus metadata management
- POS tagging with UDPipe
- 67+ linguistic feature extraction
- 5-dimensional MDA scoring
- Text type classification
- Interactive visualizations

## 🤝 Contributing

This is an active development project. Stay tuned for updates!

## 📄 License

MIT License - see LICENSE file

## 📚 Citation

Based on:
> Biber, D. (1988). *Variation across speech and writing*. Cambridge University Press.
