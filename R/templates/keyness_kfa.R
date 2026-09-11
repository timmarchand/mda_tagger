# 02_kfa.R - Key Feature Analysis (Egbert & Biber, 2023)
#
# Egbert, J. and Biber, D. (2023). Key feature analysis: a simple, yet
# powerful method for comparing text varieties. Corpora, 18(1), 121-133.
#
# NOTE: the pooled SD formula below (an unweighted average of the two
# group variances, not weighted by sample size) is implemented exactly as
# specified in the notes this script was built from. Verify it against
# your own copy of Egbert & Biber (2023) before using results from this
# script in a publication.
#
# Set which feature-level table to analyze below.

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

# ---- Build a complete doc x feature matrix of per-1000-word frequencies,
#      filling in true zeros for features that do not occur in a document ----
all_features <- unique(counts$feature)

freq_table <- counts %>%
  select(doc_id, feature, count) %>%
  tidyr::complete(doc_id = doc_lengths$doc_id, feature = all_features,
                   fill = list(count = 0)) %>%
  left_join(doc_lengths, by = "doc_id") %>%
  mutate(per_1000 = (count / n_words) * 1000)

# ---- Core KFA function: text-level normalized frequencies in, effect
#      size table out. One category vs. one reference. ----
kfa <- function(freq_table, target_cat, reference_cat) {

  target <- freq_table %>% filter(metadata == target_cat)
  reference <- freq_table %>% filter(metadata == reference_cat)

  summary_fn <- function(df) {
    df %>%
      group_by(feature) %>%
      summarise(
        n = n(),
        mean = mean(per_1000, na.rm = TRUE),
        sd   = sd(per_1000, na.rm = TRUE),
        .groups = "drop"
      )
  }

  t_summary <- summary_fn(target)
  r_summary <- summary_fn(reference)

  merged <- t_summary %>%
    inner_join(r_summary, by = "feature", suffix = c("_target", "_reference"))

  merged %>%
    mutate(
      pooled_sd = sqrt((sd_target^2 + sd_reference^2) / 2),
      difference = mean_target - mean_reference,
      d = ifelse(pooled_sd == 0, NA_real_, difference / pooled_sd),
      abs_d = abs(d),
      direction = case_when(
        is.na(d) ~ "undefined (zero pooled SD)",
        d > 0    ~ paste("more frequent in", target_cat),
        d < 0    ~ paste("more frequent in", reference_cat),
        TRUE     ~ "no difference"
      ),
      target_category = target_cat,
      reference_category = reference_cat
    ) %>%
    select(
      target_category, reference_category, feature,
      n_target, mean_target = mean_target, sd_target,
      n_reference = n_reference, mean_reference, sd_reference,
      difference, pooled_sd, d, abs_d, direction
    ) %>%
    arrange(desc(abs_d))
}

categories <- levels(droplevels(as.factor(freq_table$metadata)))

if (length(categories) < 2) {
  stop("Need at least 2 metadata categories to run Key Feature Analysis.")
}

# ---- Comparison 1: each category vs. all others pooled ----
cat("\nRunning: each category vs. all others...\n")

vs_rest_results <- lapply(categories, function(cat_name) {
  ft <- freq_table %>%
    mutate(metadata = ifelse(metadata == cat_name, cat_name, "rest"))
  kfa(ft, cat_name, "rest")
})

vs_rest_table <- dplyr::bind_rows(vs_rest_results)

# check vs_rest_table
vs_rest_table

# write to csv

readr::write_csv(vs_rest_table, file.path("output", paste0("kfa_vs_rest_", feature_level, ".csv")))
cat("Saved kfa_vs_rest_", feature_level, ".csv\n", sep = "")

# ---- Comparison 2: every category vs. a chosen reference ----
# Set reference_category to one of the levels printed below, or leave as
# NULL to skip this comparison.
cat("\nAvailable categories:", paste(categories, collapse = ", "), "\n")
reference_category <- NULL  # e.g. "1" or "ENS" - set this to run comparison 2

if (!is.null(reference_category)) {
  targets <- setdiff(categories, reference_category)
  vs_ref_results <- lapply(targets, function(cat_name) {
    kfa(freq_table, cat_name, reference_category)
  })
  vs_ref_table <- dplyr::bind_rows(vs_ref_results)

  # check vs_ref_table
  vs_ref_table

  # write to csv
  readr::write_csv(vs_ref_table, file.path("output", paste0("kfa_vs_rest_", feature_level, ".csv")))

  cat("Saved kfa_vs_", reference_category, "_", feature_level, ".csv\n", sep = "")
} else {
  cat("\nSet reference_category above and re-run to compare every category against one reference.\n")
}

cat("\nInterpretation guide for |d| (Egbert & Biber and related literature",
    "have used varying thresholds - inspect the continuous values rather",
    "than relying on a single cutoff):\n",
    "  0.20 - small\n  0.50 - medium\n  0.80 - large\n")
