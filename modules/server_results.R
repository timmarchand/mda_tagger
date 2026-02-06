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
      if (!is.null(proc) && proc$is_complete) {
        return(proc$processed_data)
      }
      return(NULL)
    })

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
      choices <- setNames(results_data()$doc_id, results_data()$doc_id)
      updateSelectInput(session, "biber_doc_select", choices = choices)
    })

    # Biber Comparison Plot (single document)
    output$biber_comparison_plot <- renderPlot({
      req(results_data(), input$biber_doc_select)

      plot_biber_comparison(
        results_data(),
        doc_id_selected = input$biber_doc_select
      )
    })

    # Biber Comparison Plot (all documents)
    output$biber_comparison_all_plot <- renderPlot({
      req(results_data())

      plot_biber_comparison_all(results_data(), max_texts = 12)
    })

    # Download Handler ----
    output$download_results <- downloadHandler(
      filename = function() {
        paste0("mda_results_", format(Sys.Date(), "%Y%m%d"), ".csv")
      },
      content = function(file) {
        readr::write_csv(results_data(), file)
      }
    )

  })
}
