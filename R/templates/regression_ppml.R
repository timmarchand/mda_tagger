# 02_ppml.R - Poisson pseudo-maximum-likelihood (PPML) regression
#
# Models the count of ONE chosen feature as a function of predictor
# variables, using document word count as an offset (so the model
# effectively estimates a rate, not a raw count).
#
# Uses fixest::fepois(), the standard implementation of PPML following
# Santos Silva and Tenreyro (2006), with heteroskedasticity-robust
# standard errors built in. To cluster standard errors instead (e.g. by
# author or text), change vcov below to vcov = ~your_grouping_var.
#
# install.packages("fixest")

library(dplyr)
library(fixest)
library(readr)

source("R/01_import.R")

dir.create("output", showWarnings = FALSE)

feature_level <- "tag"  # change to "token" or "pos" as needed

counts <- switch(feature_level,
                  "token" = token_counts,
                  "pos"   = pos_counts,
                  "tag"   = tag_counts,
                  stop("feature_level must be 'token', 'pos', or 'tag'"))
if (is.null(counts)) {
  stop("No data found for feature_level = '", feature_level,
       "'. Check that this level was selected when exporting from MDA Tagger.")
}

cat("\nMost frequent features at this level (pick one for target_feature below):\n")

top_features <- counts %>%
  group_by(feature) %>%
  summarise(total = sum(count), .groups = "drop") %>%
  arrange(desc(total)) %>%
  slice_head(n = 20)

# check top_features
top_features

target_feature <- NULL  # e.g. "{{NN}}" or "study" - set this to one of the
                         # features above, then re-run this script

if (is.null(target_feature)) {
  stop("Set target_feature above, then re-run.")
}

# ---- Predictor variables - edit to match the columns in doc_lengths ----
predictors <- c("metadata")
cat("\nAvailable columns in doc_lengths:", paste(names(doc_lengths), collapse = ", "), "\n")

feature_counts <- counts %>%
  filter(feature == target_feature) %>%
  select(doc_id, count)

model_data <- doc_lengths %>%
  mutate(doc_id = as.character(doc_id)) %>%
  left_join(feature_counts, by = "doc_id") %>%
  mutate(count = ifelse(is.na(count), 0, count))

for (p in predictors) {
  if (!is.numeric(model_data[[p]]) || length(unique(model_data[[p]])) < 15) {
    model_data[[p]] <- as.factor(model_data[[p]])
  }
}

f <- as.formula(paste("count ~", paste(predictors, collapse = " + "), "+ offset(log(n_words))"))
cat("\nModel formula:", deparse(f), "\n")
cat("Target feature:", target_feature, "\n")

model <- fixest::fepois(f, data = model_data, vcov = "hetero")

cat("\n--- PPML (fixest::fepois, heteroskedasticity-robust SEs) ---\n")
print(summary(model))

ppml_table <- as.data.frame(model$coeftable)
ppml_table$term <- rownames(model$coeftable)
ppml_table <- ppml_table %>%
  rename(estimate = Estimate, robust_se = `Std. Error`, z = `z value`, p_value = `Pr(>|z|)`) %>%
  select(term, estimate, robust_se, z, p_value)

# check ppml_table
ppml_table

safe_feature <- gsub("[^A-Za-z0-9]+", "_", target_feature)
readr::write_csv(ppml_table, file.path("output", paste0("ppml_", safe_feature, "_", feature_level, ".csv")))
cat("\nSaved PPML results for feature:", target_feature, "\n")

cat("\nTo cluster standard errors instead of using heteroskedasticity-robust\n")
cat("SEs (e.g. if multiple texts come from the same author), refit with:\n")
cat("  fixest::fepois(f, data = model_data, vcov = ~your_grouping_var)\n")
