# =============================================================================
# plotting.R
# MDA Tagger - Visualisation Script
# Exported from MDA Tagger app (https://github.com/timmarchand/mda_tagger)
# =============================================================================
# This script produces the 5 main MDA visualisations:
#   1. Dimension scores by text
#   2. Text type classification
#   3. Dimension scatter plot
#   4. Biber reference comparison
#   5. Aggregated dimensions by metadata group
# Comment out any plots you don't need.
# =============================================================================

# ---- 1. Install / load packages ----
# install.packages(c("tidyverse", "ggplot2", "plotly", "patchwork"))

library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(readr)
library(purrr)
library(tibble)

# ---- 2. Load data ----
tagged <- read_csv("data/tagged_data.csv")

cat("Loaded", nrow(tagged), "documents\n")
cat("Groups:", paste(unique(tagged$metadata), collapse = ", "), "\n")

# ---- 3. Plotting helpers ----

# Colour palette (one colour per metadata group)
group_colours <- setNames(
  scales::hue_pal()(length(unique(tagged$metadata))),
  unique(tagged$metadata)
)

# ---- 4. Plot 1: Dimension scores per text ----
# Shows all 5 dimension scores for each document, coloured by group

dim_long <- tagged |>
  select(doc_id, metadata, starts_with("dim")) |>
  pivot_longer(starts_with("dim"), names_to = "dimension", values_to = "score")

p1 <- ggplot(dim_long, aes(x = doc_id, y = score, colour = metadata, group = metadata)) +
  geom_point(size = 2, alpha = 0.7) +
  geom_line(alpha = 0.4) +
  facet_wrap(~dimension, scales = "free_y", ncol = 1) +
  scale_colour_manual(values = group_colours) +
  labs(
    title  = "MDA Dimension Scores by Document",
    x      = "Document",
    y      = "Dimension Score",
    colour = "Group"
  ) +
  theme_minimal(base_size = 11) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(p1)
# ggsave("plots/dimension_scores.png", p1, width = 10, height = 12, dpi = 300)

# ---- 5. Plot 2: Mean dimension scores by group ----
# Bar chart of mean dimension scores per metadata group

dim_means <- tagged |>
  group_by(metadata) |>
  summarise(across(starts_with("dim"), mean, .names = "{.col}"), .groups = "drop") |>
  pivot_longer(starts_with("dim"), names_to = "dimension", values_to = "mean_score")

p2 <- ggplot(dim_means, aes(x = dimension, y = mean_score, fill = metadata)) +
  geom_col(position = "dodge", alpha = 0.85) +
  scale_fill_manual(values = group_colours) +
  labs(
    title = "Mean Dimension Scores by Group",
    x     = "Dimension",
    y     = "Mean Score",
    fill  = "Group"
  ) +
  theme_minimal(base_size = 11)

print(p2)
# ggsave("plots/mean_dimensions.png", p2, width = 8, height = 5, dpi = 300)

# ---- 6. Plot 3: Dimension scatter (Dim1 vs Dim2) ----
# Change dim1/dim2 to any two dimension columns

p3 <- ggplot(tagged, aes(x = dim1, y = dim2, colour = metadata, label = doc_id)) +
  geom_point(size = 3, alpha = 0.8) +
  scale_colour_manual(values = group_colours) +
  labs(
    title  = "Dimension 1 vs Dimension 2",
    x      = "Dimension 1 (Involved vs Informational)",
    y      = "Dimension 2 (Narrative vs Non-narrative)",
    colour = "Group"
  ) +
  theme_minimal(base_size = 11)

print(p3)
# Uncomment to add document labels:
# p3 + ggrepel::geom_text_repel(size = 3)
# ggsave("plots/scatter_dim1_dim2.png", p3, width = 8, height = 6, dpi = 300)

# ---- 7. Plot 4: Feature heatmap ----
# Shows top N features across documents

top_n_features <- 20  # change as needed

# Select numeric feature columns (exclude dimension scores and metadata)
feature_cols <- tagged |>
  select(-doc_id, -metadata, -starts_with("dim"), -any_of("tagged_text")) |>
  select(where(is.numeric)) |>
  names()

# Pick top N by variance
top_features <- tagged |>
  select(all_of(feature_cols)) |>
  summarise(across(everything(), var)) |>
  pivot_longer(everything(), names_to = "feature", values_to = "variance") |>
  arrange(desc(variance)) |>
  slice_head(n = top_n_features) |>
  pull(feature)

heatmap_data <- tagged |>
  select(doc_id, all_of(top_features)) |>
  column_to_rownames("doc_id") |>
  as.matrix()

# Scale by feature for comparability
heatmap_scaled <- scale(heatmap_data)

# Base R heatmap — replace with pheatmap or ggplot2 if preferred
heatmap(
  heatmap_scaled,
  main   = paste("Top", top_n_features, "Features by Variance"),
  cexCol = 0.8,
  cexRow = 0.7
)

# ---- 8. Plot 5: Boxplots of dimension scores by group ----

p5 <- ggplot(dim_long, aes(x = metadata, y = score, fill = metadata)) +
  geom_boxplot(alpha = 0.7, outlier.size = 1) +
  geom_jitter(width = 0.15, alpha = 0.4, size = 1) +
  scale_fill_manual(values = group_colours) +
  facet_wrap(~dimension, scales = "free_y") +
  labs(
    title = "Distribution of Dimension Scores by Group",
    x     = "Group",
    y     = "Score",
    fill  = "Group"
  ) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "none")

print(p5)
# ggsave("plots/boxplots.png", p5, width = 10, height = 8, dpi = 300)
