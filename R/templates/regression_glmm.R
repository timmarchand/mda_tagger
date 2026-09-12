# 03_glmm.R - Poisson generalized linear mixed model
#
# Same modelling question as 02_ppml.R, but adds a random intercept for a
# grouping variable (e.g. author or learner) to account for multiple
# texts coming from the same writer, rather than treating every text as
# fully independent.
#
# install.packages(c("lme4"))

library(dplyr)
library(lme4)
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

target_feature <- NULL   # e.g. "{{NN}}" or "study"
predictors     <- c("metadata")
grouping_var   <- NULL   # e.g. "author_id" - set this to add a random intercept

cat("\nAvailable columns in doc_lengths:", paste(names(doc_lengths), collapse = ", "), "\n")

if (is.null(target_feature)) {
  stop("Set target_feature above, then re-run.")
}
if (is.null(grouping_var)) {
  stop("Set grouping_var above to a column identifying repeated texts (e.g. an author or learner id), then re-run. Without a grouping structure, a mixed model is not meaningful - use 02_ppml.R instead.")
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
model_data[[grouping_var]] <- as.factor(model_data[[grouping_var]])

f <- as.formula(paste0(
  "count ~ ", paste(predictors, collapse = " + "),
  " + offset(log(n_words)) + (1 | ", grouping_var, ")"
))
cat("\nModel formula:", deparse(f), "\n")
cat("Target feature:", target_feature, "\n")

model <- lme4::glmer(f, data = model_data, family = poisson)

cat("\n--- Poisson GLMM ---\n")
print(summary(model))

glmm_table <- as.data.frame(summary(model)$coefficients)
glmm_table$term <- rownames(glmm_table)
glmm_table <- glmm_table %>% select(term, everything())

# check glmm_table
glmm_table

safe_feature <- gsub("[^A-Za-z0-9]+", "_", target_feature)
readr::write_csv(glmm_table, file.path("output", paste0("glmm_", safe_feature, "_", feature_level, ".csv")))
cat("\nSaved GLMM results for feature:", target_feature, "\n")
cat("\nCheck the output above for singular fit warnings - these indicate the\n")
cat("random effect variance is estimated at or near zero, meaning the\n")
cat("grouping structure may not be adding useful information here.\n")
