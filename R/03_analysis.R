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
count_features <- function(tagged_text, per_n_words = 1000) {

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
    "TO", "SPIN", "SPAU", "STPR"
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
  awl_ttr <- add_awl_ttr(tagged_text)

  counts <- counts %>%
    bind_rows(
      tibble(
        feature = c("AWL", "TTR"),
        count = c(NA, NA),
        normed = c(awl_ttr$AWL, awl_ttr$TTR),
        n_words = n_words
      )
    )

  return(counts)
}


#' Calculate MDA dimension scores
#'
#' @param feature_counts Output from count_features()
#' @param base_stats The biber_base dataset with means, SDs, and loadings
#' @return Tibble with dimension scores
#' @export
calculate_dimensions <- function(feature_counts, base_stats = biber_base) {

  # Merge counts with base statistics
  merged <- feature_counts %>%
    select(feature, normed) %>%
    left_join(base_stats, by = "feature") %>%
    filter(!is.na(dimension), dimension != "Others")

  # Calculate z-scores
  merged <- merged %>%
    mutate(
      z_score = (normed - biber_mean) / biber_sd,
      weighted_z = z_score * loading
    )

  # Sum by dimension
  dimensions <- merged %>%
    group_by(dimension) %>%
    summarise(
      score = sum(weighted_z, na.rm = TRUE),
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

        # Return closest
        text_types$text_type[which.min(distances)]
      }
    ) %>%
    ungroup()

  return(list(result))
}
