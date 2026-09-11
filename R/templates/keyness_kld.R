# 05_kld.R - Kullback-Leibler divergence and per-feature contribution
#
# Measures how much one category's feature distribution diverges from a
# reference distribution overall, and which individual features
# contribute most to that divergence. This is a different question from
# KFA or weighted log-odds: it asks about overall distributional
# difference, not which single features are most "key" in isolation.

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

# ---- KLD from P (target) to Q (reference), plus each feature's
#      contribution to the total divergence. A small epsilon avoids
#      undefined values when a feature is entirely absent from one side. ----
kld_with_contributions <- function(p_counts, q_counts, epsilon = 1e-10) {
  all_features <- union(names(p_counts), names(q_counts))

  p_counts <- setNames(p_counts[all_features], all_features)
  q_counts <- setNames(q_counts[all_features], all_features)
  p_counts[is.na(p_counts)] <- 0
  q_counts[is.na(q_counts)] <- 0

  p <- (p_counts + epsilon) / sum(p_counts + epsilon)
  q <- (q_counts + epsilon) / sum(q_counts + epsilon)

  contribution <- p * log(p / q)

  tibble(
    feature = all_features,
    p_target = as.numeric(p),
    p_reference = as.numeric(q),
    kld_contribution = as.numeric(contribution)
  ) %>%
    arrange(desc(kld_contribution))
}

get_feature_counts <- function(counts_table, category) {
  counts_table %>%
    filter(metadata == category) %>%
    group_by(feature) %>%
    summarise(total = sum(count), .groups = "drop") %>%
    { setNames(.$total, .$feature) }
}

categories <- levels(droplevels(as.factor(counts$metadata)))

if (length(categories) < 2) {
  stop("Need at least 2 metadata categories to run KL divergence.")
}

cat("\nRunning: each category vs. all others...\n")

vs_rest_results <- lapply(categories, function(cat_name) {
  p <- get_feature_counts(counts, cat_name)
  q <- get_feature_counts(counts %>% mutate(metadata = ifelse(metadata == cat_name, cat_name, "rest")), "rest")
  result <- kld_with_contributions(p, q)
  result$target_category <- cat_name
  result$total_kld <- sum(result$kld_contribution)
  result %>% select(target_category, total_kld, everything())
})

# check vs_rest_table
vs_rest_table

# write to csv
readr::write_csv(vs_rest_table, file.path("output", paste0("kld_vs_rest_", feature_level, ".csv")))
cat("Saved kld_vs_rest_", feature_level, ".csv\n", sep = "")

summary_table <- vs_rest_table %>%
  distinct(target_category, total_kld) %>%
  arrange(desc(total_kld))
cat("\nOverall divergence from the rest, by category (higher = more distinctive overall):\n")
print(summary_table)

cat("\nAvailable categories:", paste(categories, collapse = ", "), "\n")
reference_category <- NULL  # set this to run a vs-reference comparison instead

if (!is.null(reference_category)) {
  ref_counts <- get_feature_counts(counts, reference_category)
  targets <- setdiff(categories, reference_category)
  vs_ref_results <- lapply(targets, function(cat_name) {
    tgt_counts <- get_feature_counts(counts, cat_name)
    result <- kld_with_contributions(tgt_counts, ref_counts)
    result$target_category <- cat_name
    result$reference_category <- reference_category
    result$total_kld <- sum(result$kld_contribution)
    result %>% select(target_category, reference_category, total_kld, everything())
  })

   vs_ref_table <- dplyr::bind_rows(vs_ref_results)

   # check vs_ref_table
   vs_ref_table

   # write to csv
  readr::write_csv(vs_ref_table, file.path("output", paste0("kld_vs_", reference_category, "_", feature_level, ".csv")))
  cat("Saved kld_vs_", reference_category, "_", feature_level, ".csv\n", sep = "")
}
