# =============================================================================
# tagging.R
# MDA Tagger - Full Tagging Pipeline
# Exported from MDA Tagger app (https://github.com/timmarchand/mda_tagger)
# =============================================================================
# This script runs the full tagging pipeline:
#   1. Load raw tokenised data
#   2. POS tag with UDPipe
#   3. Extract MDA linguistic features
#   4. Score texts on Biber's 5 dimensions
# =============================================================================

# ---- 1. Install / load packages ----
# Run install.packages() for any packages you are missing:
# install.packages(c("udpipe", "tidyverse", "data.table"))

library(udpipe)
library(dplyr)
library(stringr)
library(readr)
library(purrr)
library(tibble)

# ---- 2. Load data ----
# Paths are relative to the project root — open the .Rproj file first
tokenised <- read_csv("data/tokenised_data.csv")
tagged    <- read_csv("data/tagged_data.csv")  # pre-tagged; use section 3 to re-tag

cat("Loaded", nrow(tokenised), "documents\n")
cat("Columns:", paste(names(tokenised), collapse = ", "), "\n")

# ---- 3. (Optional) Re-tag with UDPipe ----
# Skip this section if you want to work with the pre-tagged data above.

# Download the UDPipe model (first time only, ~20MB):
# udpipe_download_model(language = "english-ewt", model_dir = ".")

model_file <- list.files(".", pattern = "\\.udpipe$", full.names = TRUE)[1]

if (is.na(model_file)) {
  stop("No UDPipe model found. Run udpipe_download_model(language = 'english-ewt') first.")
}

udmodel <- udpipe_load_model(model_file)
cat("UDPipe model loaded:", model_file, "\n")

# Tag each document
pos_tag_text <- function(text, doc_id, model) {
  result <- udpipe_annotate(model, x = text, doc_id = doc_id)
  as_tibble(result)
}

tagged_raw <- map2_dfr(
  tokenised$text,
  tokenised$doc_id,
  ~ pos_tag_text(.x, .y, udmodel)
)

cat("POS tagging complete:", nrow(tagged_raw), "tokens\n")

# ---- 4. Inspect tagged data ----
# The tagged_data.csv exported from the app has these key columns:
#   doc_id       - document identifier
#   tagged_text  - full tagged text (word_POS<MDA> format)
#   metadata     - category/group label
#   dim1..dim5   - Biber dimension scores
#   [feature columns] - individual feature counts per 1000 words

glimpse(tagged)

# ---- 5. Example: filter by metadata group ----
# Replace "Academic" with your own category label
group1 <- tagged |> filter(metadata == unique(tagged$metadata)[1])
cat("Group 1 (", unique(tagged$metadata)[1], "):", nrow(group1), "documents\n")

# ---- 6. Example: compare dimension scores across groups ----
tagged |>
  group_by(metadata) |>
  summarise(
    across(starts_with("dim"), list(mean = mean, sd = sd), .names = "{.col}_{.fn}"),
    n = n()
  ) |>
  print()

# ---- 7. Example: extract a single feature across documents ----
# Replace "NN" with any feature column name from your data
if ("NN" %in% names(tagged)) {
  tagged |>
    select(doc_id, metadata, NN) |>
    arrange(desc(NN)) |>
    head(10) |>
    print()
}
