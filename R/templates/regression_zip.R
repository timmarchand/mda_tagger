# 04_zip.R - Zero-inflated Poisson (ZIP) regression
#
# Use when the target feature has more zero counts than a plain Poisson
# model would predict, which is common for rare or register-specific
# features. ZIP models two processes at once: whether a document is a
# structural zero (would never use this feature) versus a Poisson
# process governing the count when the feature can occur.
#
# install.packages(c("pscl"))

library(dplyr)
library(pscl)
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

target_feature <- NULL  # e.g. "{{NN}}" or "study"
predictors     <- c("metadata")

cat("\nAvailable columns in doc_lengths:", paste(names(doc_lengths), collapse = ", "), "\n")

if (is.null(target_feature)) {
  stop("Set target_feature above, then re-run.")
}

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

pct_zero <- mean(model_data$count == 0) * 100
cat("\nPercentage of documents with zero occurrences of this feature:",
    round(pct_zero, 1), "%\n")

f <- as.formula(paste0(
  "count ~ ", paste(predictors, collapse = " + "), " + offset(log(n_words)) | ",
  paste(predictors, collapse = " + ")
))
cat("\nModel formula (count model | zero-inflation model):", deparse(f), "\n")
cat("Target feature:", target_feature, "\n")

model <- pscl::zeroinfl(f, data = model_data, dist = "poisson")

cat("\n--- Zero-inflated Poisson ---\n")
print(summary(model))

zip_count_table <- as.data.frame(summary(model)$coefficients$count)
zip_count_table$term <- rownames(zip_count_table)
zip_count_table$component <- "count"

zip_zero_table <- as.data.frame(summary(model)$coefficients$zero)
zip_zero_table$term <- rownames(zip_zero_table)
zip_zero_table$component <- "zero-inflation"

zip_full_table <- dplyr::bind_rows(zip_count_table, zip_zero_table) %>%
  select(component, term, everything())

# check zip_full_table
zip_full_table

safe_feature <- gsub("[^A-Za-z0-9]+", "_", target_feature)
readr::write_csv(zip_full_table, file.path("output", paste0("zip_", safe_feature, "_", feature_level, ".csv")))
cat("\nSaved ZIP results for feature:", target_feature, "\n")
