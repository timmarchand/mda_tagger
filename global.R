#' Global Setup for MDA Shiny App
#' Loads packages, functions, and data

cat("═══════════════════════════════════════\n")
cat("  MDA Shiny App - Loading...\n")
cat("═══════════════════════════════════════\n\n")

# ==== Load Packages ====
cat("📦 Loading packages...\n")

library(shiny)
library(shinydashboard)
library(shinyjs)
library(DT)
library(plotly)
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(readr)
library(tibble)
library(data.table)

cat("   ✓ Core packages loaded\n")

# ==== App Configuration ====
options(shiny.maxRequestSize = 30*1024^2)  # 30 MB max upload

# ==== Helper Function ====
`%||%` <- function(a, b) if (is.null(a)) b else a

cat("\n✅ Global setup complete!\n")
cat("═══════════════════════════════════════\n\n")
# ==== Load Data ====
cat("📊 Loading reference data...\n")

# Load sh regex patterns
if (file.exists("data/sh.rds")) {
  sh <- readRDS("data/sh.rds")
  cat("   ✓ sh patterns loaded (", length(sh), " patterns)\n", sep = "")
} else {
  stop("❌ data/sh.rds not found. Run data/create_sh.R first.")
}

# Load biber_base statistics
if (file.exists("data/biber_base.rds")) {
  biber_base <- readRDS("data/biber_base.rds")
  cat("   ✓ biber_base loaded (", nrow(biber_base), " features)\n", sep = "")
} else {
  stop("❌ data/biber_base.rds not found. Run data/create_biber_base.R first.")
}

# ==== Source Functions ====
cat("🔧 Loading functions...\n")

source("R/05_file_helpers.R", local = TRUE)
cat("   ✓ File helpers loaded\n")

source("R/01_utils.R", local = TRUE)
cat("   ✓ Core tagging functions loaded\n")

source("R/02_pipeline.R", local = TRUE)
cat("   ✓ Pipeline functions loaded\n")

source("R/03_analysis.R", local = TRUE)
cat("   ✓ Analysis functions loaded\n")

# ==== Initialize UDPipe Model ====
cat("📦 Initializing UDPipe model...\n")

model_file <- "english-ewt-ud-2.5-191206.udpipe"

if (!file.exists(model_file)) {
  cat("   📥 Downloading model (first time only, ~20MB)...\n")
  udpipe::udpipe_download_model(language = "english-ewt", model_dir = ".")
  cat("   ✓ Downloaded\n")
}

cat("   Loading model...\n")
udmodel <- udpipe::udpipe_load_model(model_file)
cat("   ✓ Model ready\n")

# ==== Source Modules ====
cat("📦 Loading Shiny modules...\n")

source("modules/ui_data_input.R", local = TRUE)
source("modules/server_data_input.R", local = TRUE)
cat("   ✓ Data input module loaded\n")

source("modules/ui_processing.R", local = TRUE)
source("modules/server_processing.R", local = TRUE)
cat("   ✓ Processing module loaded\n")

cat("   ✓ Data input module loaded\n")
