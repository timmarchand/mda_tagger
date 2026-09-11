# =============================================================================
# 02_descriptives.R - Descriptive statistics and boxplots by subcorpus
# =============================================================================

library(dplyr)
library(tidyr)
library(ggplot2)

source("R/01_import.R")

dir.create("output", showWarnings = FALSE)

dimension_cols <- names(scores)[grepl("^Dimension[0-9]+$", names(scores))]

# ---- Summary table: mean, SD, n per dimension per subcorpus ----
descriptives <- scores %>%
  select(metadata, all_of(dimension_cols)) %>%
  pivot_longer(all_of(dimension_cols), names_to = "dimension", values_to = "score") %>%
  group_by(metadata, dimension) %>%
  summarise(
    n    = n(),
    mean = round(mean(score, na.rm = TRUE), 2),
    sd   = round(sd(score, na.rm = TRUE), 2),
    .groups = "drop"
  )

print(descriptives)
write_csv(descriptives, "output/descriptive_statistics.csv")

# ---- Boxplots, one per dimension ----
for (dim in dimension_cols) {
  p <- ggplot(scores, aes(x = metadata, y = .data[[dim]], fill = metadata)) +
    geom_boxplot(alpha = 0.8) +
    geom_jitter(width = 0.15, alpha = 0.3) +
    labs(title = paste("Distribution of", dim, "by subcorpus"),
         x = "Subcorpus", y = dim) +
    theme_minimal() +
    theme(legend.position = "none")

  ggsave(file.path("output", paste0("boxplot_", dim, ".png")),
         plot = p, width = 7, height = 5, dpi = 300)
}

cat("\nSaved descriptive_statistics.csv and one boxplot per dimension to output/\n")
