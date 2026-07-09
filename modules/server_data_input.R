# =============================================================================
# modules/server_data_input.R
# Data Input Server Module
# =============================================================================

#' Data Input Server Module
#'
#' @param id Module namespace ID
#' @return List of reactive values
dataInputServer <- function(id) {
  moduleServer(id, function(input, output, session) {

    # Reactive Values ----
    corpus_metadata <- reactiveVal(NULL)
    data_confirmed <- reactiveVal(FALSE)

    # Data Ingestion ----
    uploaded_data <- reactive({

      # Paste text
      if (input$input_method == "paste") {
        req(input$pasted_text)
        return(process_pasted_text(input$pasted_text))
      }

      # Single file upload
      if (input$input_method == "single") {
        req(input$upload_csv)
        result <- read_uploaded_file(
          input$upload_csv,
          skip_rows = input$skip_rows %||% 0
        )
        return(result)
      }

      # Corpus upload
      if (input$input_method == "corpus") {
        req(input$upload_corpus)
        metadata_assignments <- corpus_metadata()
        return(read_corpus_files(input$upload_corpus, metadata_assignments))
      }

      return(NULL)
    })

    # CSV Column Selection UI ----
    output$column_selection_ui <- renderUI({
      req(input$input_method)
      if (input$input_method != "single") return(NULL)
      if (is.null(uploaded_data())) return(NULL)
      if (uploaded_data()$type != "csv") return(NULL)

      columns <- names(uploaded_data()$content)
      df <- uploaded_data()$content

      # Only check character/factor columns excluding the text column for
      # cardinality warning — the text column will almost always have high
      # cardinality and isn't a meaningful metadata candidate.
      candidate_meta_cols <- setdiff(columns, columns[1])
      candidate_meta_cols <- candidate_meta_cols[sapply(candidate_meta_cols, function(col) {
        is.character(df[[col]]) || is.factor(df[[col]])
      })]
      column_info <- sapply(candidate_meta_cols, function(col) length(unique(df[[col]])))
      high_cardinality_cols <- names(column_info)[column_info > 20]

      tagList(
        hr(),
        h5("📋 Column Selection"),
        p("Choose which columns contain your text, document IDs, and metadata:"),

        # 1. Text column
        selectInput(
          session$ns("text_column"),
          "Text Column:",
          choices = columns,
          selected = columns[1]
        ),

        # 2. Document ID column
        selectInput(
          session$ns("doc_id_column"),
          "Document ID Column (optional):",
          choices = c("Auto-generate" = "", columns),
          selected = ""
        ),

        # 3. Metadata column
        selectInput(
          session$ns("meta_column"),
          "Metadata Column(s) (optional):",
          choices = NULL,
          multiple = TRUE,
          selected = NULL
        ),

        # Warning only for high-cardinality metadata candidates
        if (length(high_cardinality_cols) > 0) {
          div(
            style = "margin-top: 10px; padding: 10px; background-color: #fff3cd; border-radius: 4px;",
            HTML(paste0(
              "<strong>⚠️ Metadata Warning:</strong><br/>",
              "These columns have >20 unique values and may not be suitable as metadata categories: ",
              "<strong>", paste(high_cardinality_cols, collapse = ", "), "</strong><br/>",
              "Metadata works best with a small number of distinct category labels."
            ))
          )
        },

        # Metadata value filter
        conditionalPanel(
          condition = paste0("input['", session$ns("meta_column"), "'] != null && input['", session$ns("meta_column"), "'].length > 0"),
          br(),
          uiOutput(session$ns("meta_value_filter_ui"))
        )
      )
    })

    # Update meta column choices based on text column selection
    observe({
      req(input$input_method)
      if (input$input_method != "single") return()
      if (is.null(uploaded_data())) return()
      if (uploaded_data()$type != "csv") return()
      if (is.null(input$text_column)) return()

      all_columns <- names(uploaded_data()$content)
      available_meta_columns <- setdiff(all_columns, input$text_column)

      if (length(available_meta_columns) > 0) {
        df <- uploaded_data()$content
        meta_column_info <- sapply(available_meta_columns, function(col) {
          length(unique(df[[col]]))
        })

        meta_choices <- setNames(
          available_meta_columns,
          paste0(available_meta_columns, " (", meta_column_info, " unique)")
        )

        updateSelectInput(session, "meta_column", choices = meta_choices)
      } else {
        updateSelectInput(session, "meta_column", choices = NULL)
      }
    })

    # Metadata value filter UI ----
    output$meta_value_filter_ui <- renderUI({
      req(input$meta_column, uploaded_data())

      # Get unique values from the selected metadata column(s)
      df <- uploaded_data()$content

      # If multiple meta columns selected, combine them
      if (length(input$meta_column) > 1) {
        combined_values <- apply(df[, input$meta_column, drop = FALSE], 1, paste, collapse = " | ")
      } else {
        combined_values <- df[[input$meta_column]]
      }


      # Sort and get counts
      value_counts <- table(combined_values)
      value_counts <- value_counts[order(names(value_counts))]

      # Create choices with counts — guard against no values to filter on
      if (length(value_counts) == 0) {
        return(div(
          class = "alert alert-warning",
          "Selected metadata column has no values to filter."
        ))
      }

      choices_with_counts <- setNames(
        names(value_counts),
        paste0(names(value_counts), " (n=", value_counts, ")")
      )

      tagList(
        h5("🎯 Filter Metadata Values"),
        p("Select which categories to include in analysis:"),

        div(
          style = "margin-bottom: 10px;",
          actionButton(
            session$ns("select_all_meta"),
            "Select All",
            class = "btn-sm btn-info",
            style = "margin-right: 5px;"
          ),
          actionButton(
            session$ns("deselect_all_meta"),
            "Deselect All",
            class = "btn-sm btn-secondary"
          )
        ),

        checkboxGroupInput(
          session$ns("meta_values_selected"),
          label = NULL,
          choices = choices_with_counts,
          selected = names(value_counts)  # All selected by default
        ),

        textOutput(session$ns("filtered_text_count"))
      )
    })

    # Select all metadata values
    observeEvent(input$select_all_meta, {
      req(input$meta_column, uploaded_data())

      df <- uploaded_data()$content
      if (length(input$meta_column) > 1) {
        combined_values <- apply(df[, input$meta_column, drop = FALSE], 1, paste, collapse = " | ")
        all_values <- unique(combined_values)
      } else {
        all_values <- unique(df[[input$meta_column]])
      }

      updateCheckboxGroupInput(session, "meta_values_selected", selected = all_values)
    })

    # Deselect all metadata values
    observeEvent(input$deselect_all_meta, {
      updateCheckboxGroupInput(session, "meta_values_selected", selected = character(0))
    })

    # Show count of filtered texts
    output$filtered_text_count <- renderText({
      req(input$meta_values_selected)
      paste("Selected:", length(input$meta_values_selected), "categories")
    })

    # Corpus Metadata UI ----
    output$corpus_metadata_container <- renderUI({
      req(input$input_method)
      if (input$input_method != "corpus") return(NULL)
      if (is.null(input$upload_corpus)) return(NULL)
      if (nrow(input$upload_corpus) == 0) return(NULL)

      files <- input$upload_corpus

      tagList(
        br(),
        div(
          style = "border: 1px solid #ddd; padding: 15px; border-radius: 5px; background-color: #f9f9f9;",

          h5("📝 Metadata Assignment"),
          p("Assign category labels to your files for corpus analysis:"),

          tags$ul(
            tags$li("Use the same label for multiple files (e.g., 'Academic', 'News')"),
            tags$li("Create different categories for register comparison"),
            tags$li("Labels will be used for filtering and grouping")
          ),

          # Quick assignment tools
          div(
            style = "margin-bottom: 15px; padding: 10px; background-color: #e8f4f8; border-radius: 3px;",
            h6("🚀 Quick Assignment Tools:"),

            fluidRow(
              column(4,
                     textInput(session$ns("bulk_meta_label"),
                               "Bulk Label:",
                               placeholder = "e.g., Academic, News")
              ),
              column(4,
                     numericInput(session$ns("bulk_start"),
                                  "From file #:",
                                  value = 1, min = 1, step = 1)
              ),
              column(4,
                     numericInput(session$ns("bulk_end"),
                                  "To file #:",
                                  value = nrow(files), min = 1, step = 1)
              )
            ),

            div(style = "text-align: center; margin-top: 10px;",
                actionButton(session$ns("apply_bulk"),
                             "Apply to Range",
                             class = "btn-info btn-sm"),
                actionButton(session$ns("apply_all"),
                             "Apply to All",
                             class = "btn-warning btn-sm")
            )
          ),

          # Individual file assignments
          h6("📁 Individual File Assignments:"),
          div(
            style = "max-height: 300px; overflow-y: auto; border: 1px solid #ddd; padding: 10px; border-radius: 3px; background-color: white;",

            lapply(1:nrow(files), function(i) {
              filename_base <- tools::file_path_sans_ext(files$name[i])

              div(
                style = "margin-bottom: 8px; padding: 8px; border: 1px solid #e0e0e0; border-radius: 3px;",
                fluidRow(
                  column(1,
                         div(style = "text-align: center; font-weight: bold; color: #666; padding-top: 5px;",
                             paste0("#", i))
                  ),
                  column(5,
                         div(style = "font-size: 13px; color: #333; padding-top: 5px; word-break: break-all;",
                             files$name[i])
                  ),
                  column(6,
                         textInput(
                           session$ns(paste0("meta_", i)),
                           label = NULL,
                           value = filename_base,
                           placeholder = "Enter category"
                         )
                  )
                )
              )
            })
          ),

          br(),
          div(style = "text-align: center;",
              actionButton(session$ns("update_metadata"),
                           "✅ Confirm Metadata",
                           class = "btn-success"),
              tags$span(style = "margin: 0 10px;"),
              actionButton(session$ns("reset_metadata"),
                           "🔄 Reset to Filenames",
                           class = "btn-outline-secondary btn-sm")
          )
        )
      )
    })

    # Metadata Assignment Observers ----
    observeEvent(input$apply_bulk, {
      req(input$upload_corpus, input$bulk_meta_label, input$bulk_start, input$bulk_end)

      if (nchar(trimws(input$bulk_meta_label)) == 0) {
        showNotification("Please enter a label", type = "warning")
        return()
      }

      files <- input$upload_corpus
      start_idx <- max(1, min(input$bulk_start, nrow(files)))
      end_idx <- max(start_idx, min(input$bulk_end, nrow(files)))

      for (i in start_idx:end_idx) {
        updateTextInput(session, paste0("meta_", i),
                        value = trimws(input$bulk_meta_label))
      }

      showNotification(
        paste("Applied to files", start_idx, "-", end_idx),
        type = "message"
      )
    })

    observeEvent(input$apply_all, {
      req(input$upload_corpus, input$bulk_meta_label)

      if (nchar(trimws(input$bulk_meta_label)) == 0) {
        showNotification("Please enter a label", type = "warning")
        return()
      }

      files <- input$upload_corpus
      for (i in 1:nrow(files)) {
        updateTextInput(session, paste0("meta_", i),
                        value = trimws(input$bulk_meta_label))
      }

      showNotification(
        paste("Applied to all", nrow(files), "files"),
        type = "message"
      )
    })

    observeEvent(input$reset_metadata, {
      req(input$upload_corpus)
      files <- input$upload_corpus
      for (i in 1:nrow(files)) {
        filename_base <- tools::file_path_sans_ext(files$name[i])
        updateTextInput(session, paste0("meta_", i), value = filename_base)
      }
      showNotification("Reset to filenames", type = "message")
    })

    observeEvent(input$update_metadata, {
      req(input$upload_corpus)
      files <- input$upload_corpus
      assignments <- character(nrow(files))

      for (i in 1:nrow(files)) {
        meta_value <- input[[paste0("meta_", i)]]
        if (!is.null(meta_value) && nchar(trimws(meta_value)) > 0) {
          assignments[i] <- trimws(meta_value)
        } else {
          assignments[i] <- tools::file_path_sans_ext(files$name[i])
        }
      }

      corpus_metadata(assignments)

      assignment_summary <- table(assignments)
      summary_text <- paste(
        "Metadata confirmed!",
        paste(names(assignment_summary), "(", assignment_summary, ")", collapse = ", ")
      )
      showNotification(summary_text, type = "message", duration = 5)
    })

    # Final Data Assembly ----
    selected_text_and_meta <- reactive({

      # Safety check for input_type
      req(input$input_method)

      # Handle pre-tagged data
      if (input$input_method == "pretagged") {
        req(pretagged_data())
        return(list(
          type = "pretagged",
          text = pretagged_data()$tagged_text,
          meta = pretagged_data()$metadata,
          doc_ids = pretagged_data()$doc_id,
          tagged_text = pretagged_data()$tagged_text
        ))
      }

      req(uploaded_data())

      # For paste, txt, corpus - already have everything
      if (uploaded_data()$type %in% c("paste", "txt", "corpus")) {
        return(list(
          text = uploaded_data()$content,
          meta = uploaded_data()$metadata,
          doc_ids = uploaded_data()$doc_ids
        ))
      }

      # For CSV - need column selection
      if (uploaded_data()$type == "csv") {
        req(input$text_column)
        df <- uploaded_data()$content

        # Generate text data first
        text_data <- as.character(df[[input$text_column]])

        # Define keep_rows upfront — default to keeping all rows
        keep_rows <- rep(TRUE, length(text_data))

        # Get metadata
        if (!is.null(input$meta_column) && length(input$meta_column) > 0) {
          if (length(input$meta_column) > 1) {
            meta_data <- apply(df[, input$meta_column, drop = FALSE], 1, paste, collapse = " | ")
          } else {
            meta_data <- as.character(df[[input$meta_column]])
          }

          # Filter by selected metadata values if specified
          if (!is.null(input$meta_values_selected) && length(input$meta_values_selected) > 0) {
            keep_rows <- meta_data %in% input$meta_values_selected

            # Apply filter to BOTH text and metadata
            text_data <- text_data[keep_rows]
            meta_data <- meta_data[keep_rows]

            # Show notification about filtering
            n_filtered <- sum(keep_rows)
            n_total <- length(keep_rows)
            if (n_filtered < n_total) {
              showNotification(
                paste("Filtered:", n_filtered, "of", n_total, "texts selected"),
                type = "message",
                duration = 3
              )
            }
          }
        } else {
          meta_data <- rep("unknown", length(text_data))
        }

        # Generate doc_ids AFTER filtering
        if (!is.null(input$doc_id_column) && nchar(input$doc_id_column) > 0) {
          doc_ids <- as.character(df[[input$doc_id_column]])[keep_rows]
          doc_ids <- str_replace_all(doc_ids, "[^A-Za-z0-9_-]", "_")
        } else {
          doc_ids <- paste0("doc_", sprintf("%03d", seq_along(text_data)))
        }

        return(list(
          text = text_data,
          meta = meta_data,
          doc_ids = doc_ids
        ))
      }

      # Default return NULL if nothing matches
      return(NULL)
    })

    # Data Summary Output ----
    output$data_available <- reactive({
      !is.null(selected_text_and_meta())
    })
    outputOptions(output, "data_available", suspendWhenHidden = FALSE)

    output$data_summary <- renderText({
      req(selected_text_and_meta())

      data <- selected_text_and_meta()
      summary <- summarize_texts(data$text, data$meta)

      paste0(
        "📊 DATA SUMMARY\n",
        "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n",
        "Documents:      ", summary$n_texts, "\n",
        "Total words:    ", format(summary$total_words, big.mark = ","), "\n",
        "Mean words:     ", summary$mean_words, "\n",
        "Range:          ", summary$min_words, " - ", summary$max_words, " words\n",
        if (!is.null(data$meta) && length(unique(data$meta)) > 1) {
          paste0("Categories:     ", summary$n_categories, " (", summary$categories, ")\n")
        } else "",
        "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n",
        "✅ Data ready for processing"
      )
    })

    # Confirm Data Button ----
    observeEvent(input$confirm_data, {
      req(selected_text_and_meta())
      data_confirmed(TRUE)
      showNotification(
        "Data confirmed! You can now proceed to processing.",
        type = "message",
        duration = 3
      )
    })

    # Pre-tagged file upload ----
    pretagged_data <- reactive({
      req(input$pretagged_file)

      tryCatch({
        df <- readr::read_csv(input$pretagged_file$datapath, show_col_types = FALSE)

        # Validate required columns
        if (!"doc_id" %in% names(df)) {
          showNotification("CSV must have 'doc_id' column", type = "error", duration = 5)
          return(NULL)
        }

        if (!"tagged_text" %in% names(df)) {
          showNotification("CSV must have 'tagged_text' column", type = "error", duration = 5)
          return(NULL)
        }

        # Add metadata column if missing
        if (!"metadata" %in% names(df)) {
          df$metadata <- "unknown"
        }

        # Validate tagged_text format (basic check)
        sample_text <- df$tagged_text[1]
        if (!grepl("_[A-Z]+", sample_text)) {
          showNotification(
            "Warning: tagged_text doesn't appear to have POS tags (word_TAG format)",
            type = "warning",
            duration = 8
          )
        }

        return(df)

      }, error = function(e) {
        showNotification(paste("Error reading CSV:", e$message), type = "error", duration = 10)
        return(NULL)
      })
    })

    # Preview for pre-tagged data
    output$pretagged_preview_available <- reactive({
      !is.null(pretagged_data())
    })
    outputOptions(output, "pretagged_preview_available", suspendWhenHidden = FALSE)

    output$pretagged_preview <- DT::renderDataTable({
      req(pretagged_data())

      pretagged_data() %>%
        mutate(tagged_text = str_trunc(tagged_text, 100)) %>%
        DT::datatable(
          options = list(pageLength = 5, scrollX = TRUE),
          rownames = FALSE
        )
    })

    # Confirm pre-tagged data
    observeEvent(input$confirm_pretagged, {
      req(pretagged_data())

      data_confirmed(TRUE)

      showNotification(
        paste("Pre-tagged data confirmed:", nrow(pretagged_data()), "texts"),
        type = "message",
        duration = 3
      )
    })

    # Download pre-tagged template
    output$download_template <- downloadHandler(
      filename = "pretagged_template.csv",
      content = function(file) {
        template <- tibble(
          doc_id = c("text1", "text2"),
          tagged_text = c(
            "the_DT<DEMP> cat_NN sat_VBD<PASTP> on_IN the_DT mat_NN ._SENT",
            "this_DT<DEMP> is_VBZ a_DT test_NN<NN> ._SENT"
          ),
          metadata = c("example", "example")
        )
        readr::write_csv(template, file)
      }
    )

    # Module Returns ----
    return(list(
      uploaded_data = uploaded_data,
      selected_text_and_meta = selected_text_and_meta,
      data_confirmed = data_confirmed
    ))
  })
}
