# 01_import.R - Load keyness data exported by MDA Tagger
#
# N-gram size used for this export: {{NGRAM_SIZE}}
#
# Objects created: doc_lengths, and whichever of token_counts / pos_counts /
# tag_counts were exported.

library(readr)
library(dplyr)

doc_lengths <- read_csv("data/doc_lengths.csv", show_col_types = FALSE)
doc_lengths$metadata <- as.factor(doc_lengths$metadata)

cat("\nDocuments and word counts by category:\n")
print(
  doc_lengths %>%
    group_by(metadata) %>%
    summarise(n_docs = n(), total_words = sum(n_words), .groups = "drop")
)

token_counts <- NULL
pos_counts   <- NULL
tag_counts   <- NULL

if (file.exists("data/token_counts.csv")) {
  token_counts <- read_csv("data/token_counts.csv", show_col_types = FALSE)
  cat("\nToken-level data loaded:", n_distinct(token_counts$feature), "unique features across",
      n_distinct(token_counts$doc_id), "documents.\n")
}

if (file.exists("data/pos_counts.csv")) {
  pos_counts <- read_csv("data/pos_counts.csv", show_col_types = FALSE)
  cat("\nPOS-level data loaded:", n_distinct(pos_counts$feature), "unique features across",
      n_distinct(pos_counts$doc_id), "documents.\n")
}

if (file.exists("data/tag_counts.csv")) {
  tag_counts <- read_csv("data/tag_counts.csv", show_col_types = FALSE)
  cat("\nTag-level data loaded:", n_distinct(tag_counts$feature), "unique features across",
      n_distinct(tag_counts$doc_id), "documents.\n")
}

cat("\nNote: these tables are long and sparse. A feature-document combination\n")
cat("with no row means a count of zero, not missing data. When computing\n")
cat("per-text statistics (e.g. for Key Feature Analysis), remember to fill\n")
cat("in these implicit zeros, for example with tidyr::complete().\n")
