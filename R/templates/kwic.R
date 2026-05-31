# =============================================================================
# kwic.R
# MDA Tagger - KWIC Concordance Script
# Exported from MDA Tagger app (https://github.com/timmarchand/mda_tagger)
# =============================================================================
# This script provides KWIC (Key Word In Context) concordance functions for:
#   - Token/phrase search
#   - Tag bundle search (POS tags or full MDA tags)
# Data is loaded from the data/ folder exported alongside this script.
# =============================================================================

# ---- 1. Install / load packages ----
# install.packages(c("tidyverse", "purrr", "stringr"))

library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(readr)
library(tibble)

# ---- 2. Load data ----
tokenised <- read_csv("data/tokenised_data.csv")
tagged    <- read_csv("data/tagged_data.csv")

cat("Loaded", nrow(tagged), "documents\n")

# ---- 3. Helper: build vertical lines from tagged_text column ----
# Converts inline tagged_text (word_POS<MDA> format) to
# a named list of vertical lines per document, ready for KWIC search.

build_corpus <- function(tagged_df) {
  lapply(seq_len(nrow(tagged_df)), function(i) {
    text   <- tagged_df$tagged_text[i]
    text   <- str_replace_all(text, "(\\S+)\\s+(<)", "\\1\\2")
    tokens <- str_split(text, "\\s+")[[1]]
    tokens <- tokens[tokens != ""]
    list(file_id = tagged_df$doc_id[i], lines = tokens)
  })
}

corpus <- build_corpus(tagged)
cat("Corpus built:", length(corpus), "documents\n")

# ---- 4. Token / phrase KWIC ----

regex_escape <- function(x) {
  str_replace_all(x, "([.\\^$*+?()\\[\\]{}|\\\\])", "\\\\\\1")
}

kwic_token <- function(corpus, query, window = 5, case_sensitive = FALSE) {
  flag    <- if (case_sensitive) "" else "(?i)"
  words   <- str_split(trimws(query), "\\s+")[[1]]
  pattern <- paste0(flag, paste(map_chr(words, regex_escape), collapse = "\\s+"))
  n_words <- length(words)

  results <- map_dfr(corpus, function(doc) {
    tkns <- str_extract(doc$lines, "^.+?(?=_)")
    tkns <- ifelse(is.na(tkns), doc$lines, tkns)
    n    <- length(tkns)
    hit_positions <- c()
    for (i in seq_len(n - n_words + 1)) {
      chunk <- paste(tkns[i:(i + n_words - 1)], collapse = " ")
      if (grepl(pattern, chunk, perl = TRUE)) hit_positions <- c(hit_positions, i)
    }
    if (length(hit_positions) == 0) return(tibble())
    map_dfr(hit_positions, function(pos) {
      node_end  <- pos + n_words - 1
      left_str  <- paste(tkns[max(1, pos - window):(pos - 1)], collapse = " ")
      node_str  <- paste(tkns[pos:node_end], collapse = " ")
      right_str <- paste(tkns[(node_end + 1):min(n, node_end + window)], collapse = " ")
      tibble(file_id = doc$file_id, left = left_str, node = node_str, right = right_str)
    })
  })

  if (nrow(results) == 0) { message("No hits found for: ", query); return(invisible(tibble())) }

  node_counts <- count(results, node, name = "freq")
  results |>
    left_join(node_counts, by = "node") |>
    mutate(ttr = round(nrow(node_counts) / nrow(results), 3)) |>
    arrange(desc(freq))
}

# ---- 5. Tag bundle KWIC ----
# Search by POS tag (e.g. "{{DT}} {{JJ}}") or full MDA tag (e.g. "{{DT<QUAN>}} {{JJ<JJ>}}")
# POS-only patterns match any MDA variant ({{DT}} matches {{DT<QUAN>}}, {{DT<DEMP>}} etc.)

kwic_tag <- function(corpus, target_bundle, window = 5) {
  bundle_tags <- str_extract_all(target_bundle, "\\{\\{[^}]+\\}\\}")[[1]]
  n_bundle    <- length(bundle_tags)
  if (n_bundle == 0) stop("No valid {{tag}} patterns found. Use format: {{DT}} {{JJ}}")

  results <- map_dfr(corpus, function(doc) {
    raw  <- doc$lines
    tkns <- str_extract(raw, "^.+?(?=_\\{\\{)")
    tags <- str_extract(raw, "\\{\\{.+?\\}\\}")
    if (all(is.na(tags))) return(tibble())
    n <- length(tags)
    if (n < n_bundle) return(tibble())

    hit_positions <- keep(seq_len(n - n_bundle + 1), function(i) {
      wt <- tags[i:(i + n_bundle - 1)]
      all(map2_lgl(wt, bundle_tags, function(obs, pat) {
        if (is.na(obs)) return(FALSE)
        if (str_detect(pat, "<")) {
          obs == pat
        } else {
          obs_pos <- str_extract(obs, "(?<=\\{\\{)[A-Z$\\.]+")
          pat_pos <- str_extract(pat, "(?<=\\{\\{)[A-Z$\\.]+")
          !is.na(obs_pos) && !is.na(pat_pos) && obs_pos == pat_pos
        }
      }))
    })

    if (length(hit_positions) == 0) return(tibble())

    map_dfr(hit_positions, function(pos) {
      node_end  <- pos + n_bundle - 1
      left_str  <- paste(tkns[max(1, pos - window):(pos - 1)][!is.na(tkns[max(1, pos - window):(pos - 1)])], collapse = " ")
      node_str  <- paste(tkns[pos:node_end][!is.na(tkns[pos:node_end])], collapse = " ")
      right_str <- paste(tkns[(node_end + 1):min(n, node_end + window)][!is.na(tkns[(node_end + 1):min(n, node_end + window)])], collapse = " ")
      tibble(file_id = doc$file_id, left = left_str, node = node_str, right = right_str)
    })
  })

  if (nrow(results) == 0) { message("No hits for bundle: ", target_bundle); return(invisible(tibble())) }

  node_counts <- count(results, node, name = "freq")
  results |>
    left_join(node_counts, by = "node") |>
    mutate(ttr = round(nrow(node_counts) / nrow(results), 3)) |>
    arrange(desc(freq))
}

# ---- 6. Display helper ----
print_kwic <- function(results, n = 20) {
  if (nrow(results) == 0) { cat("No results.\n"); return(invisible(NULL)) }
  cat(sprintf("\n%d hits | %d types | TTR %.3f\n\n",
              nrow(results), n_distinct(results$node), results$ttr[1]))
  results |>
    slice_head(n = n) |>
    mutate(
      left  = str_pad(left,  30, side = "left"),
      node  = str_pad(node,  15, side = "both"),
      right = str_pad(right, 30, side = "right")
    ) |>
    with(cat(paste(file_id, left, node, right, sep = "  "), sep = "\n"))
}

# ---- 7. Example usage ----

# Token search — single word
results_token <- kwic_token(corpus, "the", window = 5)
print_kwic(results_token)

# Token search — phrase
results_phrase <- kwic_token(corpus, "in order to", window = 5)
print_kwic(results_phrase)

# Tag search — POS only (matches any MDA variant)
results_tag <- kwic_tag(corpus, "{{DT}} {{JJ}} {{NN}}", window = 5)
print_kwic(results_tag)

# Tag search — full MDA tag (exact match)
results_full <- kwic_tag(corpus, "{{DT<QUAN>}} {{JJ<JJ>}}", window = 5)
print_kwic(results_full)

# Save results to CSV
# write_csv(results_token, "kwic_the.csv")
# write_csv(results_tag,   "kwic_DT_JJ_NN.csv")
