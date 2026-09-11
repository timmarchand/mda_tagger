# 04_weighted_log_odds.R - Weighted log-odds with informative Dirichlet prior
#
# Monroe, B. L., Colaresi, M. P., and Quinn, K. M. (2008). Fightin' words:
# Lexical feature selection and evaluation for identifying the content of
# political conflict. Political Analysis, 16(4), 372-403.
#
# Unlike a raw log-odds ratio, this incorporates a prior derived from a
# background/reference corpus, which stabilizes estimates for
# low-frequency features and gives a z-score that accounts for
# uncertainty, not just a point estimate of association strength.

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

# ---- Weighted log-odds for two corpora, with a background prior ----
# a_counts, b_counts: named vectors of raw feature counts for each corpus
# prior_counts: named vector of raw feature counts for the background
#   (by default, the two corpora combined)
weighted_log_odds <- function(a_counts, b_counts, prior_counts = NULL) {
  all_features <- union(names(a_counts), names(b_counts))

  a_counts <- setNames(a_counts[all_features], all_features)
  b_counts <- setNames(b_counts[all_features], all_features)
  a_counts[is.na(a_counts)] <- 0
  b_counts[is.na(b_counts)] <- 0

  if (is.null(prior_counts)) {
    prior_counts <- a_counts + b_counts
  } else {
    prior_counts <- setNames(prior_counts[all_features], all_features)
    prior_counts[is.na(prior_counts)] <- 0
  }

  n_a <- sum(a_counts)
  n_b <- sum(b_counts)
  n_prior <- sum(prior_counts)

  # Posterior log-odds for each corpus, using the prior as pseudo-counts
  log_odds_a <- log((a_counts + prior_counts) / (n_a + n_prior - a_counts - prior_counts))
  log_odds_b <- log((b_counts + prior_counts) / (n_b + n_prior - b_counts - prior_counts))

  delta <- log_odds_a - log_odds_b
  variance <- 1 / (a_counts + prior_counts) + 1 / (b_counts + prior_counts)
  z <- delta / sqrt(variance)

  tibble(
    feature = all_features,
    count_a = as.numeric(a_counts),
    count_b = as.numeric(b_counts),
    log_odds_weighted = as.numeric(delta),
    z = as.numeric(z)
  ) %>%
    arrange(desc(abs(z)))
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
  stop("Need at least 2 metadata categories to run weighted log-odds.")
}

cat("\nRunning: each category vs. all others...\n")

vs_rest_results <- lapply(categories, function(cat_name) {
  a <- get_feature_counts(counts, cat_name)
  b <- get_feature_counts(counts %>% mutate(metadata = ifelse(metadata == cat_name, cat_name, "rest")), "rest")
  weighted_log_odds(a, b) %>%
    rename(count_target = count_a, count_rest = count_b) %>%
    mutate(target_category = cat_name, .before = 1)
})

vs_rest_table <- dplyr::bind_rows(vs_rest_results)

# check vs_rest_table
vs_rest_table

# write to csv

readr::write_csv(vs_rest_table, file.path("output", paste0("weighted_log_odds_vs_rest_", feature_level, ".csv")))
cat("Saved weighted_log_odds_vs_rest_", feature_level, ".csv\n", sep = "")

cat("\nAvailable categories:", paste(categories, collapse = ", "), "\n")
reference_category <- NULL  # set this to run a vs-reference comparison instead

if (!is.null(reference_category)) {
  ref_counts <- get_feature_counts(counts, reference_category)
  targets <- setdiff(categories, reference_category)
  vs_ref_results <- lapply(targets, function(cat_name) {
    tgt_counts <- get_feature_counts(counts, cat_name)
    weighted_log_odds(tgt_counts, ref_counts) %>%
      rename(count_target = count_a, count_reference = count_b) %>%
      mutate(target_category = cat_name, reference_category = reference_category, .before = 1)
  })
  vs_ref_table <- dplyr::bind_rows(vs_ref_results)


  # check vs_ref_table
  vs_ref_table

  # write to csv
  readr::write_csv(vs_ref_table, file.path("output", paste0("weighted_log_odds_vs_", reference_category, "_", feature_level, ".csv")))
  cat("Saved weighted_log_odds_vs_", reference_category, "_", feature_level, ".csv\n", sep = "")
}

cat("\nSort by |z| to see the most distinctive features in either direction.\n")
