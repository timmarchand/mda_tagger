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
