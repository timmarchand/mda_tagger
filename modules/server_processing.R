# =============================================================================
# modules/server_processing.R
# Processing Module Server
# =============================================================================

#' Processing Server Module
#'
#' @param id Module namespace ID
#' @param data_module Reactive data from data input module
#' @return List of reactive values with processed results
processingServer <- function(id, data_module, paren_session = NULL) {
  moduleServer(id, function(input, output, session) {

    # Reactive Values ----
    rv <- reactiveValues(
      processing = FALSE,
      processed_data = NULL,
      tagged_texts = list(),
      log_messages = character(),
      progress_pct = 0
    )

    # Value Boxes ----
    output$box_texts <- renderValueBox({
      data <- data_module$selected_text_and_meta()
      n <- if (!is.null(data)) length(data$text) else 0

      valueBox(
        n,
        "Texts to Process",
        icon = icon("file-text"),
        color = if (n > 0) "blue" else "red"
      )
    })

    output$box_words <- renderValueBox({
      data <- data_module$selected_text_and_meta()
      n_words <- if (!is.null(data)) {
        sum(count_words(data$text))
      } else 0

      valueBox(
        format(n_words, big.mark = ","),
        "Total Words",
        icon = icon("font"),
        color = "green"
      )
    })

    output$box_progress <- renderValueBox({
      pct <- rv$progress_pct

      valueBox(
        paste0(pct, "%"),
        "Progress",
        icon = icon("tasks"),
        color = if (pct == 100) "green" else if (pct > 0) "yellow" else "red"
      )
    })

    # Model Status ----
    output$model_status <- renderText({
      if (exists("udmodel", envir = .GlobalEnv)) {
        "✅ Model loaded"
      } else {
        "❌ Model not loaded"
      }
    })

    # Start Processing ----
    observeEvent(input$start_processing, {

      # Check data is available
      if (is.null(data_module$selected_text_and_meta())) {
        showNotification("No data loaded. Please upload data first.", type = "error")
        return()
      }

      # Check model is loaded
      if (!exists("udmodel", envir = .GlobalEnv)) {
        showNotification("Initializing UDPipe model...", type = "message", duration = NULL, id = "model_init")

        tryCatch({
          init_udpipe_model()
          removeNotification("model_init")
          showNotification("Model loaded successfully!", type = "message", duration = 3)
        }, error = function(e) {
          removeNotification("model_init")
          showNotification(paste("Error loading model:", e$message), type = "error", duration = 10)
          return()
        })
      }

      # Get data
      data <- data_module$selected_text_and_meta()
      texts <- data$text
      doc_ids <- data$doc_ids
      metadata <- data$meta
      n_texts <- length(texts)

      # Reset state
      rv$log_messages <- character()
      rv$progress_pct <- 0
      rv$processing <- TRUE
      rv$tagged_texts <- list()

      # Add initial log
      rv$log_messages <- c(rv$log_messages, log_message(paste("Starting processing of", n_texts, "texts")))

      # Disable button
      shinyjs::disable("start_processing")

      # Process with progress
      withProgress(message = "Processing texts", value = 0, {

        results <- list()

        for (i in 1:n_texts) {

          incProgress(1/n_texts, detail = paste("Text", i, "of", n_texts))

          rv$log_messages <- c(
            rv$log_messages,
            log_message(paste("Processing:", doc_ids[i]))
          )

          tryCatch({

            # Step 1: POS tagging
            tagged <- add_st_tags(texts[i])
            rv$log_messages <- c(rv$log_messages, log_message(paste("  ✓ POS tagged:", length(tagged), "tokens")))

            # Step 2: Linguistic tagging
            dtagged <- dtag_all(tagged)
            rv$tagged_texts[[doc_ids[i]]] <- dtagged
            rv$log_messages <- c(rv$log_messages, log_message(paste("  ✓ Feature tagged")))

            # Step 3: Count features
            counts <- count_features(dtagged, per_n_words = input$normalize_per)
            rv$log_messages <- c(rv$log_messages, log_message(paste("  ✓ Counted", nrow(counts), "features")))

            # Step 4: Calculate dimensions
            dims <- calculate_dimensions(counts)
            rv$log_messages <- c(rv$log_messages, log_message(paste("  ✓ Calculated dimension scores")))

            # Store result - ensure dims is a proper tibble
            if (is.data.frame(dims)) {
              results[[i]] <- dims %>%
                mutate(
                  doc_id = doc_ids[i],
                  metadata = metadata[i],
                  n_words = counts$n_words[1],
                  .before = 1
                )
            } else {
              # If dims is not a data frame, create one manually
              results[[i]] <- tibble(
                doc_id = doc_ids[i],
                metadata = metadata[i],
                n_words = counts$n_words[1],
                Dimension1 = NA,
                Dimension2 = NA,
                Dimension3 = NA,
                Dimension4 = NA,
                Dimension5 = NA
              )
            }

            rv$log_messages <- c(rv$log_messages, log_message(paste("  ✅ Complete:", doc_ids[i])))

          }, error = function(e) {
            rv$log_messages <- c(
              rv$log_messages,
              log_message(paste("  ❌ Error:", e$message))
            )

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

          # Update progress
          rv$progress_pct <- round(100 * i / n_texts)
        }

        # Combine results
        if (length(results) > 0) {
          rv$processed_data <- bind_rows(results)

          # Add text type classification
          if (nrow(rv$processed_data) > 0 &&
              all(c("Dimension1", "Dimension2", "Dimension3", "Dimension4", "Dimension5") %in% names(rv$processed_data))) {

            tryCatch({
              classified <- add_closest_text_type(rv$processed_data)
              if (is.list(classified)) {
                rv$processed_data <- classified[[1]]
              }
            }, error = function(e) {
              cat("Warning: Could not classify text types:", e$message, "\n")
              rv$processed_data$closest_text_type <- "unknown"
            })
          } else {
            rv$processed_data$closest_text_type <- "unknown"
          }

          cat("\n✅ Stored", nrow(rv$processed_data), "processed texts\n")
        } else {
          cat("\n⚠️ No results to store\n")
        }

        rv$log_messages <- c(rv$log_messages, log_message("═══ Processing Complete ═══"))

      })

      # Re-enable button
      shinyjs::enable("start_processing")
      rv$processing <- FALSE

      # Update sample selector
      updateSelectInput(session, "sample_doc_select",
                        choices = setNames(doc_ids, doc_ids))

      showNotification("Processing complete!", type = "message", duration = 5)
    })

    # Processing Log Output ----
    output$processing_log <- renderText({
      paste(rev(rv$log_messages), collapse = "\n")
    })

    # Processing Status ----
    output$processing_status <- renderUI({
      if (rv$processing) {
        div(
          style = "text-align: center; padding: 20px;",
          icon("spinner", class = "fa-spin fa-3x text-primary"),
          h4("Processing in progress...", style = "margin-top: 10px;")
        )
      } else if (!is.null(rv$processed_data)) {
        div(
          style = "padding: 15px; background-color: #d4edda; border-radius: 4px;",
          h4(icon("check-circle", class = "text-success"), " Processing Complete!"),
          p("Successfully processed", nrow(rv$processed_data), "texts"),
          actionButton(
            session$ns("view_results"),
            "View Results →",
            icon = icon("arrow-right"),
            class = "btn-primary"
          )
        )
      } else {
        div(
          style = "text-align: center; padding: 20px; color: #999;",
          p("Click 'Start Processing' to begin")
        )
      }
    })

    # Sample Output ----
    output$sample_output <- renderText({
      req(input$sample_doc_select, rv$tagged_texts)

      tagged <- rv$tagged_texts[[input$sample_doc_select]]
      if (is.null(tagged)) return("No data available")

      n <- min(input$sample_n_tokens, length(tagged))
      paste(tagged[1:n], collapse = "\n")
    })

    # Navigate to Results ----

    observeEvent(input$view_results, {
      # Use shinyjs to click the results tab
      shinyjs::runjs("$('a[data-value=\"results\"]').tab('show');")
    })

    # Module Returns ----
    return(reactive({
      list(
        processed_data = rv$processed_data,
        tagged_texts = rv$tagged_texts,
        is_complete = !is.null(rv$processed_data)
      )
    }))
  })
}
