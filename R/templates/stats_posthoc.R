# =============================================================================
# 04_posthoc.R - Pairwise post-hoc comparisons and effect sizes
# =============================================================================
# Requires 03_group_comparisons.R to have been run first (uses models).
#
# install.packages(c("emmeans", "rstatix"))

library(dplyr)
library(emmeans)

source("R/03_group_comparisons.R")

dir.create("output", showWarnings = FALSE)

pairwise_rows <- list()

for (dim in names(models)) {
  emm <- emmeans(models[[dim]], ~ metadata)
  pw  <- pairs(emm, adjust = "tukey")
  pw_df <- as.data.frame(pw)
  pw_df$dimension <- dim
  pairwise_rows[[dim]] <- pw_df
  cat("\n---", dim, "---\n")
  print(pw)
}

pairwise_table <- dplyr::bind_rows(pairwise_rows) %>%
  select(dimension, contrast, estimate, SE, df, t.ratio, p.value)

readr::write_csv(pairwise_table, "output/pairwise_comparisons_tukey.csv")

# library(rstatix)
# dunn_rows <- lapply(names(models), function(dim) {
#   d <- rstatix::dunn_test(scores, as.formula(paste(dim, "~ metadata")),
#                            p.adjust.method = "holm")
#   d$dimension <- dim
#   d
# })
# dunn_table <- dplyr::bind_rows(dunn_rows)
# readr::write_csv(dunn_table, "output/pairwise_comparisons_dunn.csv")

cat("\nSaved pairwise_comparisons_tukey.csv to output/\n")
cat("Inspect p-values alongside the effect sizes and diagnostics from 03_group_comparisons.R -",
    "statistical significance alone is not sufficient evidence of a meaningful difference.\n")
