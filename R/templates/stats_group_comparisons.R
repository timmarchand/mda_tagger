# =============================================================================
# 03_group_comparisons.R - Analysis 1: One-way group comparison
# =============================================================================
# Use when: you simply want to compare your subcorpora (the metadata
# column) on each dimension score.
#
# This fits a linear model per dimension and reports the omnibus (ANOVA)
# test. Post-hoc pairwise comparisons are in 04_posthoc.R - the omnibus
# test here tells you IF groups differ; post-hoc tells you WHICH ones.
#
# This script does NOT automatically choose between a parametric and
# non-parametric test based on a normality test. Inspect the diagnostic
# plots yourself and decide what's appropriate for your data.
#
# install.packages(c("emmeans"))  # needed in 04_posthoc.R

library(dplyr)

source("R/01_import.R")

dir.create("output", showWarnings = FALSE)

dimension_cols <- names(scores)[grepl("^Dimension[0-9]+$", names(scores))]

models       <- list()
omnibus_rows <- list()

for (dim in dimension_cols) {

  f <- as.formula(paste(dim, "~ metadata"))
  model <- lm(f, data = scores)
  models[[dim]] <- model

  a <- anova(model)
  omnibus_rows[[dim]] <- tibble::tibble(
    dimension = dim,
    df1       = a$Df[1],
    df2       = a$Df[2],
    F         = round(a$"F value"[1], 3),
    p         = round(a$"Pr(>F)"[1], 4)
  )

  png(file.path("output", paste0("diagnostics_", dim, ".png")), width = 800, height = 800)
  par(mfrow = c(2, 2))
  plot(model)
  dev.off()

  # kw <- kruskal.test(f, data = scores)
  # print(kw)
}

omnibus_table <- dplyr::bind_rows(omnibus_rows)

# check omnibus_table
omnibus_table

# write to csv
readr::write_csv(omnibus_table, "output/omnibus_tests.csv")

if (requireNamespace("effectsize", quietly = TRUE)) {
  eta_rows <- lapply(names(models), function(dim) {
    es <- effectsize::eta_squared(models[[dim]])
    tibble::tibble(dimension = dim, eta_squared = round(es$Eta2[1], 3))
  })

  eta_table <- dplyr::bind_rows(eta_rows)

  #check eta_table
  eta_table

  # write to csv
  readr::write_csv(eta_table, "output/effect_sizes_omnibus.csv")
} else {
  cat("\n(Install the 'effectsize' package for omnibus eta-squared effect sizes.)\n")
}

cat("\nSaved omnibus_tests.csv and one diagnostics plot per dimension to output/\n")
cat("Continue to 04_posthoc.R for pairwise comparisons.\n")
