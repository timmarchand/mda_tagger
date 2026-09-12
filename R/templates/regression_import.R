# 01_import.R - Load data for regression modelling of feature counts
#
# N-gram size used for this export: {{NGRAM_SIZE}}
#
# doc_lengths.csv includes any predictor columns from a metadata CSV
# uploaded at export time (the same file used for the Statistical
# Analysis export's factorial/mixed-effects options, if provided).

library(readr)
library(dplyr)

doc_lengths <- read_csv("data/doc_lengths.csv", show_col_types = FALSE)

standard_cols <- c("doc_id", "metadata", "n_words")
predictor_cols <- setdiff(names(doc_lengths), standard_cols)

cat("\nDocuments:", nrow(doc_lengths), "\n")
cat("Standard columns: doc_id, metadata, n_words\n")
if (length(predictor_cols) > 0) {
  cat("Additional predictor columns available:", paste(predictor_cols, collapse = ", "), "\n")
} else {
  cat("No additional predictor columns were uploaded - only metadata is available as a predictor.\n")
  cat("To add more, upload a metadata CSV when exporting from MDA Tagger.\n")
}

token_counts <- NULL
pos_counts   <- NULL
tag_counts   <- NULL

if (file.exists("data/token_counts.csv")) {
  token_counts <- read_csv("data/token_counts.csv", show_col_types = FALSE)
  cat("\nToken-level data loaded:", n_distinct(token_counts$feature), "unique features.\n")
}
if (file.exists("data/pos_counts.csv")) {
  pos_counts <- read_csv("data/pos_counts.csv", show_col_types = FALSE)
  cat("\nPOS-level data loaded:", n_distinct(pos_counts$feature), "unique features.\n")
}
if (file.exists("data/tag_counts.csv")) {
  tag_counts <- read_csv("data/tag_counts.csv", show_col_types = FALSE)
  cat("\nTag-level data loaded:", n_distinct(tag_counts$feature), "unique features.\n")
}

cat("\nSee 02_ppml.R, 03_glmm.R, and 04_zip.R to model the count of a single\n")
cat("chosen feature as a function of these predictors.\n")
