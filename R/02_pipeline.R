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
#' @param normalize_per Normalize feature counts per N words (default: 100)
#' @param progress_callback Optional function to call with progress updates
#' @return Tibble with dimension scores and metadata
#' @export
mda_analysis <- function(texts,
                         doc_ids = NULL,
                         metadata = NULL,
                         normalize_per = 100,
                         progress_callback = NULL) {

  n_texts <- length(texts)

  # Generate doc_ids if not provided
  if (is.null(doc_ids)) {
    doc_ids <- paste0("doc_", sprintf("%03d", 1:n_texts))
  }

  # Generate metadata if not provided
  if (is.null(metadata)) {
    metadata <- rep("unknown", n_texts)
  }

  # Check model is loaded
  if (!exists("udmodel", envir = .GlobalEnv)) {
    stop("UDPipe model not loaded. Run init_udpipe_model() first.")
  }

  # Process each text
  results <- list()

  for (i in 1:n_texts) {

    if (!is.null(progress_callback)) {
      progress_callback(i, n_texts, doc_ids[i])
    } else {
      cat("Processing", i, "/", n_texts, ":", doc_ids[i], "\n")
    }

    tryCatch({

      # Step 1: POS tagging
      tagged <- add_st_tags(texts[i])

      # Step 2: Linguistic feature tagging
      dtagged <- dtag_all(tagged)

      # Step 3: Count features
      counts <- count_features(dtagged, per_n_words = normalize_per)
      counts$doc_id <- doc_ids[i]

      # Step 4: Calculate dimensions
      dims <- calculate_dimensions(counts)
      dims$doc_id <- doc_ids[i]
      dims$metadata = metadata[i]
      dims$n_words <- counts$n_words[1]

      results[[i]] <- dims

    }, error = function(e) {
      warning("Error processing ", doc_ids[i], ": ", e$message)
      results[[i]] <- tibble(
        doc_id = doc_ids[i],
        metadata = metadata[i],
        n_words = NA,
        Dimension1 = NA,
        Dimension2 = NA,
        Dimension3 = NA,
        Dimension4 = NA,
        Dimension5 = NA,
        status = paste("error:", e$message)
      )
    })
  }

  # Combine results
  results_df <- bind_rows(results)

  # Classify text types
  if (nrow(results_df) > 0) {
    results_df <- add_closest_text_type(results_df)[[1]]
  }

  return(results_df)
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
