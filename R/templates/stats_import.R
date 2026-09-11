# =============================================================================
# 01_import.R - Load dimension scores exported by MDA Tagger
# =============================================================================
# Run this first. It just loads the data and gives you a quick look at its
# structure before moving on to descriptives / modelling.

library(readr)
library(dplyr)

scores <- read_csv("data/dimension_scores.csv", show_col_types = FALSE)

# Force metadata to be treated as a categorical grouping variable, not a
# number - this matters if your category labels happen to be digits (e.g.
# "1", "2", "3"), since read_csv() will otherwise guess it's numeric and
# every downstream model will fit a meaningless continuous slope instead
# of comparing distinct groups.
scores$metadata <- as.factor(scores$metadata)

# Quick structure check
glimpse(scores)

# metadata is the grouping/subcorpus column produced by MDA Tagger.
# If you have additional grouping variables (e.g. proficiency, task, cohort),
# add them here by joining a metadata table on doc_id, e.g.:
#
# extra_meta <- read_csv("data/my_extra_metadata.csv", show_col_types = FALSE)
# scores <- scores %>% left_join(extra_meta, by = "doc_id")

cat("\nSubcorpora (metadata) and text counts:\n")
print(table(scores$metadata))
