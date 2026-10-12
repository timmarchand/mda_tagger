# =============================================================================
# R/03_analysis.R
# Feature counting and dimension calculation functions
# =============================================================================

# Required packages
library(tibble)
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)

#' Count linguistic features in tagged text
#'
#' @param tagged_text Character vector with linguistic tags
#' @param per_n_words Normalize per this many words (default 1000)
#' @return Tibble with feature counts
#' @export
count_features <- function(tagged_text, per_n_words = 100,
                           ttr_window = 400, ttr_method = "first") {

  # Collapse into single string
  text_string <- paste(tagged_text, collapse = " ")

  # Count words (tokens with POS tags, excluding punctuation)
  word_tokens <- grepl("\\w+_[A-Z]", tagged_text) &
    !grepl("_[[:punct:]]", tagged_text)
  n_words <- sum(word_tokens)

  if (n_words == 0) {
    warning("No words found in text")
    n_words <- 1  # Avoid division by zero
  }

  # Define all features
  features <- c(
    "FPP1", "SPP2", "TPP3", "PIT", "DEMP", "DEMO", "INPR", "QUPR",
    "NN", "NOMZ", "GER",
    "VBD", "VPRT", "VBN", "PEAS", "PASS", "BYPA", "BEMA", "PROD",
    "JJ", "PRED", "RB", "AMP", "DWNT",
    "POMD", "NEMD", "PRMD",
    "PRIV", "PUBV", "SUAV", "SMP",
    "CAUS", "CONC", "COND", "OSUB", "ANDC", "PHC", "CONJ",
    "XX0", "SYNE", "CONT",
    "EMPH", "HDG", "DPAR", "HSTN",
    "QUAN", "EX", "PIN", "TIME", "PLACE",
    "TO", "SPIN", "SPAU", "STPR",
    "SERE", "THATD", "WHCL", "WHQU", "PIRE",
    "THAC", "THVC", "TOBJ", "TSUB", "WHOBJ", "WHSUB",
    "PASTP", "WZPAST", "PRESP", "WZPRES"
  )

  # Count each feature
  counts <- tibble(
    feature = features,
    count = map_dbl(features, ~str_count(text_string, paste0("<", .x, ">")))
  )

  # Normalize per n words
  counts <- counts %>%
    mutate(
      normed = (count / n_words) * per_n_words,
      n_words = n_words
    )

  # Add AWL and TTR
  awl_ttr <- add_awl_ttr(tagged_text, ttr_window = ttr_window, ttr_method = ttr_method)

  counts <- counts %>%
    bind_rows(
      tibble(
        feature = c("AWL", "TTR"),
        count = c(NA, NA),
        normed = c(awl_ttr$AWL, awl_ttr$TTR),
        n_words = n_words
      )
    )

  # TTR details for the results table (read with attr(counts, "ttr_info"))
  attr(counts, "ttr_info") <- list(
    TTR = awl_ttr$TTR,
    ttr_method = ttr_method,
    ttr_window = ttr_window,
    ttr_words = awl_ttr$ttr_words,
    ttr_short = awl_ttr$ttr_short
  )

  return(counts)
}

#' TTR columns for one text's row in the results table
#'
#' @param counts Output of count_features()
#' @return One-row tibble
ttr_columns <- function(counts) {
  info <- attr(counts, "ttr_info")
  if (is.null(info)) {
    return(tibble(TTR = NA_real_, ttr_method = NA_character_, ttr_window = NA_real_,
                  ttr_words = NA_real_, ttr_short = NA))
  }
  tibble(TTR = info$TTR, ttr_method = info$ttr_method, ttr_window = info$ttr_window,
         ttr_words = info$ttr_words, ttr_short = info$ttr_short)
}

#' Decide how texts shorter than the TTR window enter Dimension 1
#'
#' Applied to the whole results table after every text has been scored
#' (the "register_mean" option needs the other texts). Dimension 1 is
#' adjusted by swapping the TTR term only; nothing else changes.
#'
#'   "include"       TTR on all available words (no change; the default)
#'   "exclude"       TTR term removed from Dimension 1. Equivalent to giving
#'                   the text Biber's mean TTR, so D1 shifts by register.
#'   "blank_d1"      Dimension 1 set to NA for short texts
#'   "register_mean" the mean TTR of full-length texts with the same
#'                   metadata label (all full-length texts if the label has
#'                   none). Scores then depend on the rest of the batch and
#'                   carry the register difference into short texts' D1.
#'
#' Adds `ttr_used` (the TTR value that entered Dimension 1) and `ttr_note`.
#'
#' @param results Results table with TTR, ttr_short, metadata, Dimension1
#' @param short_policy One of the options above
#' @param base_stats biber_base
#' @return The results table, adjusted
apply_ttr_policy <- function(results,
                             short_policy = c("include", "exclude", "blank_d1", "register_mean"),
                             base_stats = biber_base) {

  short_policy <- match.arg(short_policy)
  if (!all(c("TTR", "ttr_short", "Dimension1") %in% names(results))) return(results)

  ttr_row <- base_stats[base_stats$feature == "TTR" & base_stats$dimension == "Dimension1", ]
  ttr_mean <- ttr_row$biber_mean[1]; ttr_sd <- ttr_row$biber_sd[1]
  # TTR loads negatively on D1: its term in the D1 sum is -(TTR - mean) / sd
  term <- function(t) -(t - ttr_mean) / ttr_sd

  short <- !is.na(results$ttr_short) & results$ttr_short
  results$ttr_used <- results$TTR
  results$ttr_note <- ifelse(is.na(results$ttr_short), NA_character_,
                             ifelse(short, "short: TTR on all words", "full window"))

  if (!any(short) || short_policy == "include") return(results)

  if (short_policy == "exclude") {
    results$Dimension1[short] <- results$Dimension1[short] - term(results$TTR[short])
    results$ttr_used[short] <- NA_real_
    results$ttr_note[short] <- "short: TTR left out of D1"
  }

  if (short_policy == "blank_d1") {
    results$Dimension1[short] <- NA_real_
    results$ttr_used[short] <- NA_real_
    results$ttr_note[short] <- "short: D1 left blank"
  }

  if (short_policy == "register_mean") {
    full <- !is.na(results$ttr_short) & !results$ttr_short
    overall <- if (any(full)) mean(results$TTR[full], na.rm = TRUE) else NA_real_
    for (i in which(short)) {
      same <- full & results$metadata == results$metadata[i]
      if (any(same, na.rm = TRUE)) {
        new <- mean(results$TTR[which(same)], na.rm = TRUE)
        note <- paste0("short: register mean TTR (", results$metadata[i], ")")
      } else if (!is.na(overall)) {
        new <- overall
        note <- "short: overall mean TTR (no full-length texts with this label)"
      } else {
        next  # nothing to borrow from: keep TTR on all words
      }
      results$Dimension1[i] <- results$Dimension1[i] - term(results$TTR[i]) + term(new)
      results$ttr_used[i] <- new
      results$ttr_note[i] <- note
    }
  }

  results
}



#' Calculate MDA dimension scores
#'
#' @param feature_counts Output from count_features()
#' @param base_stats The biber_base dataset with means, SDs, and loadings
#' @param deflated Remove low-frequency features (mean < 0.1) per Nini (2019)
#' @return Tibble with dimension scores
#' @export
calculate_dimensions <- function(feature_counts, base_stats = biber_base, deflated = TRUE) {

  # Merge counts with base statistics
  merged <- feature_counts %>%
    select(feature, normed) %>%
    left_join(base_stats, by = "feature") %>%
    filter(!is.na(dimension), dimension != "Others")

  # Apply deflation filter if requested
  if (deflated) {
    merged <- merged %>%
      filter(biber_mean >= 0.1)
  }

  # Calculate z-scores and weighted scores
  merged <- merged %>%
    mutate(
      z_score = (normed - biber_mean) / biber_sd,
      d_score = case_when(
        loading == "negative" | loading == -1 ~ -z_score,
        loading == "positive" | loading == 1 ~ z_score,
        is.numeric(loading) ~ z_score * loading,
        TRUE ~ z_score
      )
    )

  # Sum by dimension
  dimensions <- merged %>%
    group_by(dimension) %>%
    summarise(
      score = sum(d_score, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    pivot_wider(names_from = dimension, values_from = score)

  return(dimensions)
}

#' Classify text into Biber's text types
#'
#' @param dimension_scores Tibble with Dimension1-5 scores
#' @param by Column name to group by (default: "doc_id")
#' @return List with tibble containing closest_text_type added
#' @export
#' Classify text into Biber's text types
#'
#' @param dimension_scores Tibble with Dimension1-5 scores
#' @param by Column name to group by (default: "doc_id")
#' @return List with tibble containing closest_text_type added
#' @export
add_closest_text_type <- function(dimension_scores, by = "doc_id") {

  # Reference text type centroids from Biber (1988)
  text_types <- tibble(
    text_type = c(
      "Intimate interpersonal interaction",
      "Informational interaction",
      "Scientific exposition",
      "Learned exposition",
      "Imaginative narrative",
      "General narrative exposition",
      "Situated reportage",
      "Involved persuasion"
    ),
    D1 = c(45, 30, -15, -20, 5, -10, 0, 5),
    D2 = c(-1, -1, -2.5, -2, 7, 2, -3, -2),
    D3 = c(-6, -4, 4, 5, -4, 0, -13, 2),
    D4 = c(1, 1, -2, -3, 1, -1, -4.5, -4),
    D5 = c(-4, -3, 9, 2, -2, 0, -3, -1)
  )

  # Handle if input is a list (unwrap it)
  if (is.list(dimension_scores) && !is.data.frame(dimension_scores)) {
    dimension_scores <- dimension_scores[[1]]
  }

  # Make sure it's a data frame/tibble
  if (!is.data.frame(dimension_scores)) {
    warning("dimension_scores must be a data frame")
    return(list(dimension_scores))
  }

  # Check required columns exist
  required_cols <- c("Dimension1", "Dimension2", "Dimension3", "Dimension4", "Dimension5")
  if (!all(required_cols %in% names(dimension_scores))) {
    warning("Missing dimension columns")
    return(list(dimension_scores))
  }

  # For each document, find closest text type
  result <- dimension_scores %>%
    rowwise() %>%
    mutate(
      closest_text_type = {
        # Calculate Euclidean distance to each text type
        distances <- map_dbl(1:nrow(text_types), function(i) {
          sqrt(
            (Dimension1 - text_types$D1[i])^2 +
              (Dimension2 - text_types$D2[i])^2 +
              (Dimension3 - text_types$D3[i])^2 +
              (Dimension4 - text_types$D4[i])^2 +
              (Dimension5 - text_types$D5[i])^2
          )
        })

        # Return closest (NA if any dimension is missing, e.g. a blank D1)
        if (any(is.na(distances))) NA_character_ else text_types$text_type[which.min(distances)]
      }
    ) %>%
    ungroup()

  return(list(result))
}
