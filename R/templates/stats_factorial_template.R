# =============================================================================
# 05_factorial.R - Analysis 2: Factorial analysis
# =============================================================================
# Generated for factors: {{FACTOR1}} and {{FACTOR2}}
# Use when your subcorpora represent combinations of two or more factors.
#
# install.packages(c("emmeans"))

library(dplyr)
library(emmeans)

source("R/01_import.R")

dir.create("output", showWarnings = FALSE)

dimension_cols <- names(scores)[grepl("^Dimension[0-9]+$", names(scores))]

scores${{FACTOR1}} <- as.factor(scores${{FACTOR1}})
scores${{FACTOR2}} <- as.factor(scores${{FACTOR2}})

factorial_models <- list()
factorial_omnibus_rows <- list()

for (dim in dimension_cols) {

  f <- as.formula(paste(dim, "~ {{FACTOR1}} * {{FACTOR2}}"))
  model <- lm(f, data = scores)
  factorial_models[[dim]] <- model

  a <- anova(model)
  a_df <- as.data.frame(a)
  a_df$term <- rownames(a_df)
  a_df$dimension <- dim
  rownames(a_df) <- NULL
  factorial_omnibus_rows[[dim]] <- a_df

  png(file.path("output", paste0("factorial_diagnostics_", dim, ".png")), width = 800, height = 800)
  par(mfrow = c(2, 2))
  plot(model)
  dev.off()
}

factorial_omnibus_table <- dplyr::bind_rows(factorial_omnibus_rows) %>%
  select(dimension, term, everything())

print(factorial_omnibus_table)
readr::write_csv(factorial_omnibus_table, "output/factorial_omnibus_tests.csv")

factorial_pairwise_rows <- list()

for (dim in names(factorial_models)) {
  emm <- emmeans(factorial_models[[dim]], ~ {{FACTOR1}} * {{FACTOR2}})
  pw <- pairs(emm, adjust = "holm")
  pw_df <- as.data.frame(pw)
  pw_df$dimension <- dim
  factorial_pairwise_rows[[dim]] <- pw_df
}

stat_col <- intersect(c("t.ratio", "z.ratio"), names(factorial_pairwise_rows[[1]]))
factorial_pairwise_table <- dplyr::bind_rows(factorial_pairwise_rows) %>%
  select(dimension, contrast, estimate, SE, df, all_of(stat_col), p.value)

readr::write_csv(factorial_pairwise_table, "output/factorial_pairwise_comparisons.csv")

cat("\nSaved factorial_omnibus_tests.csv, factorial_pairwise_comparisons.csv,",
    "and diagnostic plots to output/\n")
