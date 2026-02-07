# =============================================================================
# modules/server_results.R
# Results Module Server
# =============================================================================

#' Results Server Module
#'
#' @param id Module namespace ID
#' @param processing_module Reactive from processing module
#' @return NULL
resultsServer <- function(id, processing_module) {
  moduleServer(id, function(input, output, session) {

    # Get processed data
    results_data <- reactive({
      proc <- processing_module()
      cat("\n=== DEBUG results_data ===\n")
      cat("Processing module result:", !is.null(proc), "\n")

      if (!is.null(proc)) {
        cat("Is complete:", proc$is_complete, "\n")

        if (proc$is_complete) {
          cat("Processed data exists:", !is.null(proc$processed_data), "\n")
          if (!is.null(proc$processed_data)) {
            cat("Data rows:", nrow(proc$processed_data), "\n")
            cat("Data cols:", ncol(proc$processed_data), "\n")
            cat("Column names:", paste(names(proc$processed_data), collapse = ", "), "\n")
          }
          return(proc$processed_data)
        }
      }

      cat("Returning NULL\n")
      return(NULL)
    })

    # # Get processed data
    # results_data <- reactive({
    #   proc <- processing_module()
    #   if (!is.null(proc) && proc$is_complete) {
    #     return(proc$processed_data)
    #   }
    #   return(NULL)
    # })

    # Value Boxes ----
    output$box_texts_processed <- renderValueBox({
      data <- results_data()
      n <- if (!is.null(data)) nrow(data) else 0

      valueBox(
        n,
        "Texts Processed",
        icon = icon("file-alt"),
        color = "blue"
      )
    })

    output$box_total_words <- renderValueBox({
      data <- results_data()
      n_words <- if (!is.null(data)) sum(data$n_words, na.rm = TRUE) else 0

      valueBox(
        format(n_words, big.mark = ","),
        "Total Words",
        icon = icon("font"),
        color = "green"
      )
    })

    output$box_categories <- renderValueBox({
      data <- results_data()
      n_cat <- if (!is.null(data)) length(unique(data$metadata)) else 0

      valueBox(
        n_cat,
        "Categories",
        icon = icon("tags"),
        color = "yellow"
      )
    })

    output$box_text_types <- renderValueBox({
      data <- results_data()
      n_types <- if (!is.null(data) && "closest_text_type" %in% names(data)) {
        length(unique(data$closest_text_type))
      } else 0

      valueBox(
        n_types,
        "Text Types",
        icon = icon("shapes"),
        color = "purple"
      )
    })

    # Results Table ----
    output$results_table <- DT::renderDataTable({
      req(results_data())

      results_data() %>%
        select(doc_id, metadata, n_words,
               Dimension1, Dimension2, Dimension3, Dimension4, Dimension5,
               closest_text_type) %>%
        DT::datatable(
          options = list(
            pageLength = 25,
            scrollX = TRUE,
            dom = 'Bfrtip',
            buttons = c('copy', 'csv', 'excel')
          ),
          rownames = FALSE,
          filter = "top"
        ) %>%
        DT::formatRound(columns = c("Dimension1", "Dimension2", "Dimension3",
                                    "Dimension4", "Dimension5"), digits = 2)
    })

    # Dimension Plot ----
    output$dimension_plot <- renderPlotly({
      req(results_data())

      plot_dimensions(
        results_data(),
        color_by = input$plot_color_by,
        interactive = TRUE
      )
    })

    # Text Type Plot ----
    output$text_type_plot <- renderPlotly({
      req(results_data())

      plot_text_types(results_data(), interactive = TRUE)
    })

    # Scatter Plot ----
    output$scatter_plot <- renderPlotly({
      req(results_data())

      plot_dimension_scatter(
        results_data(),
        dim_x = input$scatter_dim_x,
        dim_y = input$scatter_dim_y,
        color_by = input$scatter_color_by,
        interactive = TRUE
      )
    })

    # Summary Table ----
    output$summary_table <- DT::renderDataTable({
      req(results_data())

      summarize_dimensions(
        results_data(),
        group_by = input$summary_group_by
      ) %>%
        DT::datatable(
          options = list(
            pageLength = 10,
            scrollX = TRUE
          ),
          rownames = FALSE
        )
    })

    # Update document selector for Biber comparison
    observe({
      req(results_data())
      all_docs <- results_data()$doc_id
      updateSelectInput(session, "biber_doc_select",
                        choices = all_docs,
                        selected = all_docs[1])
    })

    # Biber Comparison Plot (multiple documents)
    output$biber_comparison_plot <- renderPlot({
      req(results_data(), input$biber_doc_select)

      # Limit to 10 documents
      selected_docs <- head(input$biber_doc_select, 10)

      if (length(selected_docs) == 0) return(NULL)

      plot_biber_comparison_multiple(
        results_data(),
        doc_ids_selected = selected_docs
      )
    })

    # Biber Comparison Plot (all documents grid)
    output$biber_comparison_all_plot <- renderPlot({
      req(results_data())

      plot_biber_comparison_all(results_data(), max_texts = 12)
    })

    # Aggregated data reactive ----
    aggregated_data <- reactive({
      req(results_data())
      aggregate_by_metadata(results_data())
    })

    # Aggregated Table ----
    output$aggregated_table <- DT::renderDataTable({
      req(aggregated_data())

      aggregated_data() %>%
        mutate(across(starts_with("Dimension"), ~round(.x, 2))) %>%
        DT::datatable(
          options = list(
            pageLength = 10,
            scrollX = TRUE
          ),
          rownames = FALSE,
          colnames = c(
            "Category",
            "N Texts",
            "Avg Words",
            "D1",
            "D2",
            "D3",
            "D4",
            "D5",
            "Most Common Type"
          )
        )
    })

    # Update category selectors ----
    # Update category selectors ----
    observe({
      req(aggregated_data())
      all_categories <- aggregated_data()$metadata

      # For category comparison (multiple)
      updateSelectInput(session, "categories_to_compare",
                        choices = all_categories,
                        selected = all_categories)

      # For category+docs view (single)
      updateSelectInput(session, "category_with_docs",
                        choices = all_categories,
                        selected = all_categories[1])

      updateSelectInput(session, "biber_categories_select",
                        choices = all_categories,
                        selected = all_categories[1])
    })

    # Aggregated Dimension Plot ----
    output$aggregated_dimension_plot <- renderPlotly({
      req(aggregated_data(), input$aggregated_plot_type)

      if (input$aggregated_plot_type == "categories") {
        # Compare multiple categories
        req(input$categories_to_compare)

        plot_data_agg <- aggregated_data() %>%
          filter(metadata %in% input$categories_to_compare)

        plot_aggregated_dimensions(plot_data_agg, interactive = TRUE)

      } else {
        # Show single category with individual docs
        req(input$category_with_docs)

        plot_data_agg <- aggregated_data() %>%
          filter(metadata == input$category_with_docs)

        plot_data_ind <- results_data() %>%
          filter(metadata == input$category_with_docs)

        plot_aggregated_dimensions_with_docs(
          plot_data_agg,
          plot_data_ind,
          interactive = TRUE
        )
      }
    })

    # Aggregated Dimension Plot ----
    output$aggregated_dimension_plot <- renderPlotly({
      req(aggregated_data(), input$categories_to_compare)

      # Filter to selected categories
      plot_data_agg <- aggregated_data() %>%
        filter(metadata %in% input$categories_to_compare)

      if (input$show_individual_docs) {
        # Show aggregated lines + individual points
        plot_data_ind <- results_data() %>%
          filter(metadata %in% input$categories_to_compare)

        plot_aggregated_dimensions_with_docs(
          plot_data_agg,
          plot_data_ind,
          interactive = TRUE
        )
      } else {
        # Show only aggregated lines
        plot_aggregated_dimensions(plot_data_agg, interactive = TRUE)
      }
    })
    # Biber comparison for categories ----
    output$biber_category_plot <- renderPlot({
      req(aggregated_data(), input$biber_categories_select)

      # Limit to 10 categories
      selected_cats <- head(input$biber_categories_select, 10)

      if (length(selected_cats) == 0) return(NULL)

      # Check if we need individual docs
      show_docs <- input$biber_category_mode == "with_docs"

      ind_data <- if (show_docs) {
        results_data() %>% filter(metadata %in% selected_cats)
      } else {
        NULL
      }

      plot_biber_comparison_categories(
        aggregated_data(),
        individual_data = ind_data,
        categories_selected = selected_cats,
        show_docs = show_docs
      )
    })
  })
}
