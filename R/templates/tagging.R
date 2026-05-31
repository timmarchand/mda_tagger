# =============================================================================
# tagging.R
# MDA Tagger - Full Tagging Pipeline
# Exported from MDA Tagger app (https://github.com/timmarchand/mda_tagger)
# =============================================================================
# This script runs the full MDA tagging pipeline on your text data:
#   1. Load text data from the data/ folder
#   2. Source pipeline functions from GitHub
#   3. Initialize UDPipe POS tagging model
#   4. Run full MDA analysis (POS tagging + feature extraction + dimension scoring)
#   5. Explore and export results
#
# REQUIREMENTS:
#   install.packages(c("udpipe", "tidyverse", "data.table"))
# =============================================================================

# ---- 1. Load packages ----
library(udpipe)
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(readr)
library(tibble)

# ---- 2. Load data ----
# Paths are relative to the project root — open the .Rproj file first
text_data <- read_csv("data/text_data.csv")
tagged    <- read_csv("data/tagged_data.csv")  # pre-tagged export from app

cat("Loaded", nrow(text_data), "documents\n")
cat("Columns:", paste(names(text_data), collapse = ", "), "\n")

# ---- 3. Source pipeline functions from GitHub ----
# Requires an internet connection.

cat("Sourcing pipeline functions from GitHub...\n")

base_url <- "https://raw.githubusercontent.com/timmarchand/mda_tagger/main/R"

source(paste0(base_url, "/01_utils.R"))
cat("   ✓ Utility functions loaded\n")

source(paste0(base_url, "/02_pipeline.R"))
cat("   ✓ Pipeline functions loaded\n")

source(paste0(base_url, "/03_analysis.R"))
cat("   ✓ Analysis functions loaded\n")

# ---- 4. Load reference data from GitHub ----
# sh regex patterns and Biber base statistics are generated
# by sourcing the creation scripts directly from GitHub

cat("Loading sh patterns...\n")
source("https://raw.githubusercontent.com/timmarchand/mda_tagger/main/data/create_sh.R")
cat("   ✓ sh patterns loaded (", length(sh), "patterns)\n")

cat("Loading Biber base statistics...\n")
source("https://raw.githubusercontent.com/timmarchand/mda_tagger/main/data/create_biber_base.R")
cat("   ✓ biber_base loaded (", nrow(biber_base), "features)\n")

# ---- 5. Initialize UDPipe model ----
# Downloads the model on first run (~20MB), then loads it.
# The model file will be saved in your current working directory.

init_udpipe_model()

# ---- 6. Run full MDA analysis ----
# This re-tags your texts from scratch using the full pipeline:
#   POS tagging → MDA feature extraction → dimension scoring

cat("\nRunning MDA analysis on", nrow(text_data), "texts...\n")

results <- mda_analysis(
  texts    = text_data$text,
  doc_ids  = text_data$doc_id,
  metadata = text_data$metadata
)

cat("\n✅ Analysis complete!\n")
cat("Results:", nrow(results), "documents,", ncol(results), "columns\n")
glimpse(results)

# ---- 7. Compare with app results ----
# Check how your re-tagged results compare to the app export

if (nrow(results) == nrow(tagged)) {
  cat("\nDimension score comparison (re-tagged vs app export):\n")
  for (dim in paste0("Dimension", 1:5)) {
    if (dim %in% names(results) && dim %in% names(tagged)) {
      cor_val <- cor(results[[dim]], tagged[[dim]], use = "complete.obs")
      cat("  ", dim, "correlation:", round(cor_val, 4), "\n")
    }
  }
}

# ---- 8. Example: filter by metadata group ----
group1 <- results |> filter(metadata == unique(results$metadata)[1])
cat("\nGroup 1 (", unique(results$metadata)[1], "):", nrow(group1), "documents\n")

# ---- 9. Example: compare dimension scores across groups ----
results |>
  group_by(metadata) |>
  summarise(
    across(starts_with("Dimension"), list(mean = mean, sd = sd),
           .names = "{.col}_{.fn}"),
    n = n()
  ) |>
  print()

# ---- 10. Example: explore feature columns ----
feature_cols <- setdiff(names(results),
                        c("doc_id", "metadata", "n_words", "tagged_text",
                          "closest_text_type", paste0("Dimension", 1:5)))

if (length(feature_cols) > 0) {
  cat("\nAvailable feature columns:\n")
  cat(paste(feature_cols, collapse = ", "), "\n")

  # Show top 10 documents by first feature
  results |>
    select(doc_id, metadata, all_of(feature_cols[1])) |>
    arrange(desc(.data[[feature_cols[1]]])) |>
    head(10) |>
    print()
}

# ---- 11. Save results ----
# Uncomment to save re-tagged results to CSV
# write_csv(results, "data/retagged_results.csv")
