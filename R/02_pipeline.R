# =============================================================================
# R/02_pipeline.R
# MDA Analysis Pipeline Functions
# =============================================================================

#' Initialize UDPipe Model
#'
#' @param model_name Name of UDPipe model to download
#' @param model_dir Directory to store model
#' @param force_download Force re-download even if exists
#' @return Loaded UDPipe model (assigned to global environment)
#' @export
init_udpipe_model <- function(model_name = "english-ewt",
                              model_dir = ".",
                              force_download = FALSE) {

  model_file <- file.path(model_dir, paste0(model_name, "-ud-2.5-191206.udpipe"))

  # Download if needed
  if (!file.exists(model_file) || force_download) {
    cat("📥 Downloading UDPipe model (this may take a minute)...\n")
    udpipe::udpipe_download_model(
      language = model_name,
      model_dir = model_dir
    )
    cat("   ✓ Model downloaded\n")
  }

  # Load model
  cat("📦 Loading UDPipe model...\n")
  udmodel <- udpipe::udpipe_load_model(model_file)

  # Assign to global environment
  assign("udmodel", udmodel, envir = .GlobalEnv)

  cat("   ✓ Model loaded successfully!\n")
  invisible(udmodel)
}


#' Add Stanford-style POS tags to text
#'
#' @param x Character vector of input text
#' @param mdl UDPipe model to use (default: udmodel from global env)
#' @param st_hesitation Extract hesitation markers (default: FALSE)
#' @param flattened Is input already flattened (default: TRUE)
#' @param skip_parse Skip dependency parsing (default: TRUE)
#' @param ... Additional arguments passed to udpipe_annotate
#' @return Character vector of tokenized and tagged text
#' @export
add_st_tags <- function(x,
                        mdl = udmodel,
                        st_hesitation = FALSE,
                        flattened = TRUE,
                        skip_parse = TRUE,
                        ...) {

  if (!flattened) {
    x <- d_flatten_text(x)
  }

  # Check model is loaded
  if (!exists("udmodel", envir = .GlobalEnv)) {
    stop("UDPipe model not loaded. Run init_udpipe_model() first.")
  }

  # Tag text
  st_tagged <- udpipe::udpipe_annotate(
    mdl,
    x,
    tagger = "default",
    parser = if (skip_parse) "none" else "default",
    ...
  ) %>%
    as_tibble() %>%
    transmute(tagged = str_c(token, xpos, sep = "_")) %>%
    pull(tagged)

  return(st_tagged)
}


#' Complete MDA Analysis Pipeline
#'
#' @param texts Character vector of texts to analyze
#' @param doc_ids Character vector of document IDs
#' @param metadata Character vector of metadata labels
#' @param normalize_per Normalize feature counts per N words (default: 100;
#'   biber_base norms are per 100 words, so keep 100 for dimension scores)
#' @param deflated Drop features with Biber mean < 0.1 per 100 words
#' @param ttr_window Words per TTR sample (Biber: 400)
#' @param ttr_method "first" (Biber 1988) or "mattr"
#' @param ttr_short How texts shorter than ttr_window enter Dimension 1:
#'   "include", "exclude", "blank_d1" or "register_mean" (see apply_ttr_policy)
#' @param progress_callback Optional function to call with progress updates
#' @return Tibble with dimension scores, TTR details and metadata
#' @export
mda_analysis <- function(texts,
                         doc_ids = NULL,
                         metadata = NULL,
                         normalize_per = 100,
                         deflated = TRUE,
                         ttr_window = 400,
                         ttr_method = "first",
                         ttr_short = "include",
                         progress_callback = NULL) {

  n_texts <- length(texts)

  if (is.null(doc_ids)) {
    doc_ids <- paste0("doc_", sprintf("%03d", 1:n_texts))
  }
  if (is.null(metadata)) {
    metadata <- rep("unknown", n_texts)
  }

  if (!exists("udmodel", envir = .GlobalEnv)) {
    stop("UDPipe model not loaded. Run init_udpipe_model() first.")
  }

  results <- vector("list", n_texts)

  for (i in seq_len(n_texts)) {

    if (!is.null(progress_callback)) {
      progress_callback(i, n_texts, doc_ids[i])
    } else {
      cat("Processing", i, "/", n_texts, ":", doc_ids[i], "\n")
    }

    # tryCatch returns the row, so an error yields an error row instead of
    # being lost (the previous version assigned inside the handler, which
    # had no effect, and returned after the first text)
    results[[i]] <- tryCatch({

      tagged <- add_st_tags(texts[i])
      dtagged <- dtag_all(tagged)

      counts <- count_features(dtagged, per_n_words = normalize_per,
                               ttr_window = ttr_window, ttr_method = ttr_method)
      dims <- calculate_dimensions(counts, deflated = deflated)

      dims %>%
        mutate(doc_id = doc_ids[i],
               metadata = metadata[i],
               n_words = counts$n_words[1],
               tagged_text = paste(dtagged, collapse = " "),
               .before = 1) %>%
        bind_cols(ttr_columns(counts))

    }, error = function(e) {
      warning("Error processing ", doc_ids[i], ": ", e$message)
      tibble(doc_id = doc_ids[i], metadata = metadata[i], n_words = NA,
             tagged_text = NA, Dimension1 = NA, Dimension2 = NA, Dimension3 = NA,
             Dimension4 = NA, Dimension5 = NA, status = paste("error:", e$message))
    })
  }

  results_df <- bind_rows(results)
  results_df <- apply_ttr_policy(results_df, short_policy = ttr_short)

  if (nrow(results_df) > 0) {
    results_df <- add_closest_text_type(results_df)[[1]]
  }

  results_df
}

#' Batch process multiple documents with parallel support
#'
#' @param texts Character vector of texts
#' @param doc_ids Character vector of document IDs
#' @param metadata Character vector of metadata
#' @param parallel Use parallel processing (requires future/furrr packages)
#' @param ... Additional arguments passed to mda_analysis
#' @return Tibble with results for all documents
#' @export
mda_batch <- function(texts,
                      doc_ids = NULL,
                      metadata = NULL,
                      parallel = FALSE,
                      ...) {

  if (parallel) {
    if (!requireNamespace("furrr", quietly = TRUE)) {
      stop("Package 'furrr' required for parallel processing")
    }

    future::plan(future::multisession)
    cat("🚀 Using parallel processing\n")
  }

  result <- mda_analysis(
    texts = texts,
    doc_ids = doc_ids,
    metadata = metadata,
    ...
  )

  if (parallel) {
    future::plan(future::sequential)
  }

  return(result)
}
