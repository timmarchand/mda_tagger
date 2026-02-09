# =============================================================================
# R/05_file_helpers.R
# File Reading Utilities for MDA App
# =============================================================================

#' Process pasted text
#'
#' @param text Character string of pasted text
#' @return List with processed content, metadata, and doc_id
process_pasted_text <- function(text) {

  if (is.null(text) || nchar(trimws(text)) == 0) {
    return(NULL)
  }

  list(
    type = "paste",
    content = trimws(text),
    metadata = "pasted_text",
    doc_ids = "doc_001",
    n_texts = 1
  )
}


#' Read uploaded file (single file: CSV, TXT, or DOCX)
#'
#' @param file_info File input object from Shiny fileInput
#' @param skip_rows Number of rows to skip (for CSV only)
#' @return List with file content and metadata
read_uploaded_file <- function(file_info, skip_rows = 0) {

  if (is.null(file_info)) {
    return(NULL)
  }

  ext <- tools::file_ext(file_info$name)

  tryCatch({

    # Plain text file
    if (ext == "txt") {
      content <- readr::read_file(file_info$datapath)
      return(list(
        type = "txt",
        content = content,
        metadata = tools::file_path_sans_ext(file_info$name),
        doc_ids = "doc_001",
        n_texts = 1
      ))
    }

    # CSV file
    if (ext == "csv") {
      df <- readr::read_csv(
        file_info$datapath,
        skip = skip_rows,
        show_col_types = FALSE
      )

      # Check if CSV is empty
      if (nrow(df) == 0) {
        stop("CSV file is empty")
      }

      return(list(
        type = "csv",
        content = df,
        metadata = NULL,
        n_texts = nrow(df)
      ))
    }

    # Word document
    if (ext %in% c("docx", "doc")) {
      if (!requireNamespace("officer", quietly = TRUE)) {
        stop("Package 'officer' required for .docx files. Install with: install.packages('officer')")
      }

      doc <- officer::read_docx(file_info$datapath)
      doc_summary <- officer::docx_summary(doc)

      # Extract text content safely
      content <- ""
      if (nrow(doc_summary) > 0 && "text" %in% names(doc_summary)) {
        text_rows <- doc_summary[["text"]]
        text_rows <- text_rows[!is.na(text_rows)]
        content <- paste(text_rows, collapse = "\n")
      }

      if (nchar(trimws(content)) == 0) {
        stop("Word document appears to be empty or unreadable")
      }

      return(list(
        type = "txt",
        content = content,
        metadata = tools::file_path_sans_ext(file_info$name),
        doc_ids = "doc_001",
        n_texts = 1
      ))
    }

    # Unsupported format
    stop(paste("Unsupported file format:", ext))

  }, error = function(e) {
    message("Error reading file: ", e$message)
    return(NULL)
  })
}


#' Read multiple corpus files
#'
#' @param files_info Multiple file input object from Shiny
#' @param metadata_assignments Character vector of metadata labels (optional)
#' @return List with corpus content, metadata, and doc_ids
read_corpus_files <- function(files_info, metadata_assignments = NULL) {

  if (is.null(files_info)) {
    return(NULL)
  }

  n_files <- nrow(files_info)
  texts <- character(n_files)
  doc_ids <- paste0("doc_", sprintf("%03d", 1:n_files))

  # Default metadata from filenames if not provided
  if (is.null(metadata_assignments) || length(metadata_assignments) != n_files) {
    metadata_assignments <- tools::file_path_sans_ext(files_info$name)
  }

  # Read each file
  for (i in 1:n_files) {
    ext <- tools::file_ext(files_info$name[i])

    texts[i] <- tryCatch({

      if (ext == "txt") {
        readr::read_file(files_info$datapath[i])

      } else if (ext %in% c("docx", "doc")) {
        if (!requireNamespace("officer", quietly = TRUE)) {
          warning("Package 'officer' required for .docx files. Skipping: ", files_info$name[i])
          return("")
        }

        doc <- officer::read_docx(files_info$datapath[i])
        doc_summary <- officer::docx_summary(doc)

        # Extract text safely
        content <- ""
        if (nrow(doc_summary) > 0 && "text" %in% names(doc_summary)) {
          text_rows <- doc_summary[["text"]]
          text_rows <- text_rows[!is.na(text_rows)]
          content <- paste(text_rows, collapse = "\n")
        }
        content

      } else {
        warning("Unsupported file type for: ", files_info$name[i])
        ""
      }

    }, error = function(e) {
      warning("Error reading ", files_info$name[i], ": ", e$message)
      ""
    })
  }

  # Remove empty texts
  valid_indices <- nchar(trimws(texts)) > 0

  if (sum(valid_indices) == 0) {
    stop("No valid text content found in uploaded files")
  }

  list(
    type = "corpus",
    content = texts[valid_indices],
    metadata = metadata_assignments[valid_indices],
    doc_ids = doc_ids[valid_indices],
    filenames = files_info$name[valid_indices],
    n_texts = sum(valid_indices)
  )
}


#' Format file size for display
#'
#' @param bytes File size in bytes
#' @return Formatted string (e.g., "1.5 MB")
format_file_size <- function(bytes) {
  if (bytes < 1024) {
    return(paste(bytes, "B"))
  } else if (bytes < 1024^2) {
    return(paste(round(bytes/1024, 1), "KB"))
  } else if (bytes < 1024^3) {
    return(paste(round(bytes/1024^2, 1), "MB"))
  } else {
    return(paste(round(bytes/1024^3, 1), "GB"))
  }
}


#' Generate unique document ID
#'
#' @param existing_ids Vector of existing document IDs
#' @return New unique document ID
generate_doc_id <- function(existing_ids = character()) {
  n <- length(existing_ids) + 1
  new_id <- paste0("doc_", sprintf("%03d", n))

  # Ensure uniqueness
  while (new_id %in% existing_ids) {
    n <- n + 1
    new_id <- paste0("doc_", sprintf("%03d", n))
  }

  return(new_id)
}


#' Validate document IDs
#'
#' @param doc_ids Character vector of document IDs
#' @return List with validity status and message
validate_doc_ids <- function(doc_ids) {

  # Check for duplicates
  if (any(duplicated(doc_ids))) {
    return(list(
      valid = FALSE,
      message = paste("Duplicate document IDs found:",
                     paste(doc_ids[duplicated(doc_ids)], collapse = ", "))
    ))
  }

  # Check for valid characters
  invalid <- grepl("[^a-zA-Z0-9_-]", doc_ids)
  if (any(invalid)) {
    return(list(
      valid = FALSE,
      message = paste("Invalid characters in:",
                     paste(doc_ids[invalid], collapse = ", "))
    ))
  }

  # Check for empty IDs
  if (any(nchar(trimws(doc_ids)) == 0)) {
    return(list(
      valid = FALSE,
      message = "Some document IDs are empty"
    ))
  }

  return(list(valid = TRUE, message = "OK"))
}


#' Add timestamp to log message
#'
#' @param message Log message text
#' @return Formatted log entry with timestamp
log_message <- function(message) {
  paste0("[", format(Sys.time(), "%H:%M:%S"), "] ", message)
}


#' Create progress logger for processing
#'
#' @return Function to add and retrieve progress messages
create_progress_logger <- function() {
  messages <- character()

  function(message = NULL, reset = FALSE) {
    if (reset) {
      messages <<- character()
      return(invisible(NULL))
    }

    if (!is.null(message)) {
      messages <<- c(messages, log_message(message))
    }

    return(messages)
  }
}


#' Export data in various formats
#'
#' @param data List of data frames to export
#' @param format Export format ("csv", "xlsx", "rds")
#' @param filename Output filename path
export_data <- function(data, format, filename) {

  tryCatch({

    if (format == "csv") {
      if (length(data) == 1) {
        readr::write_csv(data[[1]], filename)
      } else {
        # Multiple datasets - create zip
        temp_dir <- tempdir()
        files <- character()

        for (name in names(data)) {
          file <- file.path(temp_dir, paste0(name, ".csv"))
          readr::write_csv(data[[name]], file)
          files <- c(files, file)
        }

        zip(filename, files, flags = "-j")
        unlink(files)
      }

    } else if (format == "xlsx") {
      if (!requireNamespace("writexl", quietly = TRUE)) {
        stop("Package 'writexl' required for Excel export")
      }
      writexl::write_xlsx(data, filename)

    } else if (format == "rds") {
      saveRDS(data, filename)

    } else {
      stop("Unknown format: ", format)
    }

    return(TRUE)

  }, error = function(e) {
    message("Error exporting data: ", e$message)
    return(FALSE)
  })
}


#' Count words in text
#'
#' @param text Character vector of texts
#' @return Integer vector of word counts
count_words <- function(text) {
  sapply(text, function(t) {
    length(stringr::str_split(t, "\\s+")[[1]])
  })
}


#' Get text summary statistics
#'
#' @param texts Character vector of texts
#' @param metadata Character vector of metadata labels
#' @return Data frame with summary statistics
summarize_texts <- function(texts, metadata = NULL) {

  n_texts <- length(texts)
  word_counts <- count_words(texts)
  char_counts <- nchar(texts)

  summary_df <- data.frame(
    n_texts = n_texts,
    total_words = sum(word_counts),
    mean_words = round(mean(word_counts), 1),
    median_words = median(word_counts),
    min_words = min(word_counts),
    max_words = max(word_counts),
    total_chars = sum(char_counts),
    stringsAsFactors = FALSE
  )

  # Add metadata summary if provided
  if (!is.null(metadata)) {
    meta_table <- table(metadata)
    summary_df$n_categories <- length(meta_table)
    summary_df$categories <- paste(names(meta_table), collapse = ", ")
  }

  return(summary_df)
}

#' Export tagged text in inline format
#'
#' @param tagged_text Character vector of tagged text
#' @param doc_id Document ID
#' @param output_file Output file path
#' @param bracket_tags Wrap tags in {{}} brackets (default: FALSE)
#' @export
#' Export tagged text in inline format
#'
#' @param tagged_text Character vector of tagged text
#' @param doc_id Document ID
#' @param output_file Output file path
#' @param bracket_tags Wrap tags in {{}} brackets (default: FALSE)
#' @export
export_tagged_inline <- function(tagged_text, doc_id, output_file, bracket_tags = FALSE) {

  # Remove spaces between tags (word_POS <TAG> -> word_POS<TAG>)
  tagged_text <- str_replace_all(tagged_text, "(\\S+)\\s+(<)", "\\1\\2")

  if (bracket_tags) {
    # Convert word_TAG<MDA> to word_{{TAG<MDA>}}
    # Keep the underscore, wrap everything after it
    tagged_text <- str_replace_all(tagged_text, "_(\\S+)", "_{{\\1}}")
  }

  writeLines(tagged_text, output_file)
}


#' Export tagged text in vertical format (one token per line)
#'
#' @param tagged_text Character vector of tagged text
#' @param doc_id Document ID
#' @param output_file Output file path
#' @param bracket_tags Wrap tags in {{}} brackets (default: FALSE)
#' @export
export_tagged_vertical <- function(tagged_text, doc_id, output_file, bracket_tags = FALSE) {

  # Remove spaces between tags (word_POS <TAG> -> word_POS<TAG>)
  tagged_text <- str_replace_all(tagged_text, "(\\S+)\\s+(<)", "\\1\\2")

  if (bracket_tags) {
    # Convert word_TAG<MDA> to word_{{TAG<MDA>}}
    # Keep the underscore, wrap everything after it
    tagged_text <- str_replace_all(tagged_text, "_(\\S+)", "_{{\\1}}")
  }

  # Split on spaces to get individual tokens
  tokens <- str_split(tagged_text, "\\s+")[[1]]

  # Write one token per line
  writeLines(tokens, output_file)
}

#' Export all tagged texts to a directory
#'
#' @param results_data Tibble with doc_id and tagged_text columns
#' @param output_dir Output directory path
#' @param format Format: "inline" or "vertical"
#' @param bracket_tags Wrap tags in {{}} brackets (default: FALSE)
#' @export
export_all_tagged_texts <- function(results_data, output_dir, format = "inline", bracket_tags = FALSE) {

  # Create output directory if it doesn't exist
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  # Export each text
  for (i in seq_len(nrow(results_data))) {
    doc_id <- results_data$doc_id[i]
    tagged_text <- results_data$tagged_text[i]

    # Clean filename
    safe_filename <- str_replace_all(doc_id, "[^A-Za-z0-9_-]", "_")
    output_file <- file.path(output_dir, paste0(safe_filename, ".txt"))

    if (format == "vertical") {
      export_tagged_vertical(tagged_text, doc_id, output_file, bracket_tags = bracket_tags)
    } else {
      export_tagged_inline(tagged_text, doc_id, output_file, bracket_tags = bracket_tags)
    }
  }

  return(output_dir)
}


