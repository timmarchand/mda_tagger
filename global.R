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
library(writexl)
library(data.table)
library(zip)
library(withr)

cat("   ✓ Core packages loaded\n")

# ==== App Configuration ====
options(shiny.maxRequestSize = 30*1024^2)  # 30 MB max upload

# ==== Helper Function ====
`%||%` <- function(a, b) if (is.null(a)) b else a

cat("\n✅ Global setup complete!\n")
cat("═══════════════════════════════════════\n\n")
# ==== Load Data ====
cat("📊 Loading reference data...\n")

# Rebuild a cached .rds when it is missing or older than its create_*.R script,
# so edits to the pattern/norm definitions take effect without manual deletion
rds_stale <- function(rds, script) {
  !file.exists(rds) || file.mtime(script) > file.mtime(rds)
}

# Load sh regex patterns
if (rds_stale("data/sh.rds", "data/create_sh.R")) {
  cat("   ⚠️  data/sh.rds missing or out of date — generating...\n")
  source("data/create_sh.R")
}
sh <- readRDS("data/sh.rds")
cat("   ✓ sh patterns loaded (", length(sh), " patterns)\n", sep = "")

# Load biber_base statistics
if (rds_stale("data/biber_base.rds", "data/create_biber_base.R")) {
  cat("   ⚠️  data/biber_base.rds missing or out of date — generating...\n")
  source("data/create_biber_base.R")
}
biber_base <- readRDS("data/biber_base.rds")
cat("   ✓ biber_base loaded (", nrow(biber_base), " features)\n", sep = "")
# ==== Source Functions ====
cat("🔧 Loading functions...\n")
source("R/05_file_helpers.R")
cat("   ✓ File helpers loaded\n")
source("R/01_utils.R")
cat("   ✓ Core tagging functions loaded\n")
source("R/02_pipeline.R")
cat("   ✓ Pipeline functions loaded\n")
source("R/03_analysis.R")
cat("   ✓ Analysis functions loaded\n")
source("R/04_visualization.R")
cat("   ✓ Visualisation functions loaded\n")

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

source("modules/ui_data_input.R")
source("modules/server_data_input.R")
cat("   ✓ Data input module loaded\n")

source("modules/ui_tokencheck.R")
source("modules/server_tokencheck.R")
cat("   ✓ Tokenization check module loaded\n")

source("modules/ui_processing.R")
source("modules/server_processing.R")
cat("   ✓ Processing module loaded\n")

source("modules/ui_results.R")
source("modules/server_results.R")
cat("   ✓ Results module loaded\n")

source("modules/ui_export.R")
source("modules/server_export.R")
cat("   ✓ Export module loaded\n")

source("modules/ui_kwic.R")
source("modules/server_kwic.R")
cat("   ✓ KWIC module loaded\n")

source("modules/tag_guide.R")
cat("   ✓ Tag guide module loaded\n")
