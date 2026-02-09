# =============================================================================
# modules/server_export.R
# Export Module Server
# =============================================================================

#' Export Server Module
#'
#' @param id Module namespace ID
#' @param processing_module Reactive from processing module
#' @return NULL
exportServer <- function(id, processing_module) {
  moduleServer(id, function(input, output, session) {

    # Get processed data
    results_data <- reactive({
      proc <- processing_module()
      if (!is.null(proc) && proc$is_complete) {
        return(proc$processed_data)
      }
      return(NULL)
    })

    # Download Tagged Texts ZIP ----
    output$download_tagged_zip <- downloadHandler(
      filename = function() {
        paste0("tagged_texts_", format(Sys.Date(), "%Y%m%d"), ".zip")
      },
      content = function(file) {
        req(results_data())

        # DEBUG: Check what columns exist
        cat("\n=== Available columns ===\n")
        print(names(results_data()))
        cat("========================\n")

        # Check if tagged_text column exists
        if (!"tagged_text" %in% names(results_data())) {
          showNotification(
            "Error: Tagged text not available. Please reprocess your data.",
            type = "error",
            duration = 10
          )
          return(NULL)
        }

        # Show progress
        withProgress(message = 'Creating ZIP file...', value = 0, {

          incProgress(0.3, detail = "Exporting texts...")

          # Create ZIP
          zip_path <- create_tagged_texts_zip(
            results_data(),
            format = input$tagged_format,
            bracket_tags = input$bracket_tags  # ← Add this parameter
          )

          incProgress(0.9, detail = "Finalizing...")

          # Copy to download location
          file.copy(zip_path, file, overwrite = TRUE)

          # Clean up temp file
          unlink(zip_path)
        })
      },
      contentType = "application/zip"
    )

    # Download Tables (Excel) ----
    output$download_tables <- downloadHandler(
      filename = function() {
        paste0("mda_tables_", format(Sys.Date(), "%Y%m%d"), ".xlsx")
      },
      content = function(file) {
        req(results_data(), input$tables_to_export)

        # Create workbook
        wb <- openxlsx::createWorkbook()

        # Add full results if selected
        if ("full" %in% input$tables_to_export) {
          openxlsx::addWorksheet(wb, "Full Results")
          openxlsx::writeData(wb, "Full Results", results_data())
        }

        # Add aggregated results if selected
        if ("aggregated" %in% input$tables_to_export) {
          agg_data <- aggregate_by_metadata(results_data())
          openxlsx::addWorksheet(wb, "Aggregated")
          openxlsx::writeData(wb, "Aggregated", agg_data)
        }

        # Add summary statistics if selected
        if ("summary" %in% input$tables_to_export) {
          summary_data <- summarize_dimensions(results_data(), group_by = "metadata")
          openxlsx::addWorksheet(wb, "Summary Statistics")
          openxlsx::writeData(wb, "Summary Statistics", summary_data)
        }

        # Save workbook
        openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
      }
    )

    # Download Plot ----
    output$download_plot <- downloadHandler(
      filename = function() {
        ext <- tolower(input$plot_format)
        paste0("mda_plot_", input$plot_to_export, "_", format(Sys.Date(), "%Y%m%d"), ".", ext)
      },
      content = function(file) {
        req(results_data())

        # Generate plot based on selection
        p <- switch(input$plot_to_export,
                    "dimensions" = plot_dimensions(results_data(), interactive = FALSE),
                    "text_types" = plot_text_types(results_data(), interactive = FALSE),
                    "scatter" = plot_dimension_scatter(results_data(), interactive = FALSE),
                    "biber" = plot_biber_comparison(results_data()),
                    "aggregated" = plot_aggregated_dimensions(aggregate_by_metadata(results_data()), interactive = FALSE)
        )

        # Save plot
        ggsave(
          filename = file,
          plot = p,
          width = input$plot_width,
          height = input$plot_height,
          dpi = input$plot_dpi,
          device = tolower(input$plot_format)
        )
      }
    )

  })
}


