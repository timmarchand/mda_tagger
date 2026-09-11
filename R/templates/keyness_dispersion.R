# 03_dispersion.R - Gries's Deviation of Proportions (DP)
#
# Gries, S. T. (2008). Dispersions and adjusted frequencies in corpora.
# International Journal of Corpus Linguistics, 13(4), 403-437.
#
# DP measures how evenly a feature is distributed across documents,
# independent of raw frequency. DP ranges from 0 (perfectly even,
# proportional to document length) to close to 1 (perfectly concentrated
# in as few documents as possible). A feature with high raw frequency but
# high DP is being driven by a small number of documents and its
# "keyness" from other measures should be treated with caution.

library(readr)
library(dplyr)
library(tidyr)

source("R/01_import.R")

dir.create("output", showWarnings = FALSE)

feature_level <- "tag"  # change to "token" to analyze word forms instead

counts <- switch(feature_level,
                 "token" = token_counts,
                 "pos"   = pos_counts,
                 "tag"   = tag_counts,
                 stop("feature_level must be 'token', 'pos', or 'tag'"))
if (is.null(counts)) {
  stop("No data found for feature_level = '", feature_level,
       "'. Check that this level was selected when exporting from MDA Tagger.")
}

# ---- DP for one feature within one category ----
dp_one_feature <- function(feature_counts, doc_words) {
  # feature_counts and doc_words must be aligned (same document order),
  # with feature_counts containing zeros for documents with no occurrences.
  total_count <- sum(feature_counts)
  total_words <- sum(doc_words)
  if (total_count == 0) return(NA_real_)

  observed_prop <- feature_counts / total_count
  expected_prop <- doc_words / total_words

  0.5 * sum(abs(observed_prop - expected_prop))
}

compute_dp_for_category <- function(freq_table, doc_lengths, category) {
  docs_in_cat <- doc_lengths %>% filter(metadata == category)
  all_features <- unique(freq_table$feature)

  complete_counts <- freq_table %>%
    filter(metadata == category) %>%
    select(doc_id, feature, count) %>%
    tidyr::complete(doc_id = docs_in_cat$doc_id, feature = all_features,
                     fill = list(count = 0)) %>%
    left_join(docs_in_cat %>% select(doc_id, n_words), by = "doc_id")

  complete_counts %>%
    group_by(feature) %>%
    summarise(
      total_count = sum(count),
      dp = dp_one_feature(count, n_words),
      .groups = "drop"
    ) %>%
    mutate(metadata = category) %>%
    filter(total_count > 0)
}

categories <- levels(droplevels(as.factor(doc_lengths$metadata)))

cat("\nComputing DP for each category...\n")
dp_results <- lapply(categories, function(cat_name) {
  compute_dp_for_category(counts, doc_lengths, cat_name)
})

dp_table <- dplyr::bind_rows(dp_results) %>%
  select(metadata, feature, total_count, dp) %>%
  arrange(metadata, dp)

# check dp_table
dp_table

# write to csv
readr::write_csv(dp_table, file.path("output", paste0("dispersion_dp_", feature_level, ".csv")))
cat("Saved dispersion_dp_", feature_level, ".csv\n", sep = "")
cat("\nLower DP = more evenly spread across documents (more reliable).\n")
cat("Higher DP = concentrated in fewer documents (treat keyness with caution).\n")
