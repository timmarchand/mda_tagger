# =============================================================================
# 06_mixed_effects.R - Analysis 3: Mixed-effects analysis
# =============================================================================
# Generated with grouping variable: {{GROUPING_VAR}}
# Use when multiple texts come from the same {{GROUPING_VAR}}, or there are
# other repeated/grouped observations that should not be treated as fully
# independent.
#
# install.packages(c("lme4", "lmerTest", "emmeans"))

library(dplyr)
library(lme4)
library(emmeans)

source("R/01_import.R")

dir.create("output", showWarnings = FALSE)

dimension_cols <- names(scores)[grepl("^Dimension[0-9]+$", names(scores))]

mixed_models <- list()

for (dim in dimension_cols) {

  f <- as.formula(paste(dim, "~ metadata + (1 | {{GROUPING_VAR}})"))
  model <- tryCatch(
    lme4::lmer(f, data = scores),
    error = function(e) {
      cat("\nCould not fit mixed model for", dim, ":", e$message, "\n")
      NULL
    }
  )
  if (is.null(model)) next
  mixed_models[[dim]] <- model

  print(summary(model))

  png(file.path("output", paste0("mixed_diagnostics_", dim, ".png")), width = 800, height = 800)
  plot(fitted(model), resid(model),
       xlab = "Fitted values", ylab = "Residuals",
       main = paste("Residuals vs Fitted -", dim))
  abline(h = 0, lty = 2)
  dev.off()
}

cat("\nFitted", length(mixed_models), "of", length(dimension_cols), "mixed-effects models.\n")

mixed_pairwise_rows <- list()

for (dim in names(mixed_models)) {
  emm <- emmeans(mixed_models[[dim]], ~ metadata)
  pw <- pairs(emm, adjust = "holm")
  pw_df <- as.data.frame(pw)
  pw_df$dimension <- dim
  mixed_pairwise_rows[[dim]] <- pw_df
}

if (length(mixed_pairwise_rows) > 0) {
  stat_col <- intersect(c("t.ratio", "z.ratio"), names(mixed_pairwise_rows[[1]]))
  mixed_pairwise_table <- dplyr::bind_rows(mixed_pairwise_rows) %>%
    select(dimension, contrast, estimate, SE, df, all_of(stat_col), p.value)
  readr::write_csv(mixed_pairwise_table, "output/mixed_pairwise_comparisons.csv")
  cat("\nSaved mixed_pairwise_comparisons.csv to output/\n")
}

cat("Note: mixed-effects p-values from lme4 can be approximate.",
    "Consider installing lmerTest for Satterthwaite-approximated p-values,",
    "or afex for a more complete workflow.\n")
