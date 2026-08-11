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

    output$data_ready <- reactive({
      !is.null(results_data())
    })
    outputOptions(output, "data_ready", suspendWhenHidden = FALSE)
    # ---- Download Tagged ZIP ----
    output$download_tagged_zip <- downloadHandler(
      filename = function() {
        paste0("tagged_texts_", format(Sys.Date(), "%Y%m%d"), ".zip")
      },
      content = function(file) {
        req(results_data())
        validate(
          need("tagged_text" %in% names(results_data()),
               "Tagged text not available. Please reprocess your data.")
        )
        withProgress(message = 'Creating ZIP file...', value = 0, {
          incProgress(0.2, detail = "Exporting texts...")
          tmp_dir <- file.path(tempdir(), paste0("tagged_export_", Sys.getpid()))
          export_all_tagged_texts(
            results_data(),
            output_dir   = tmp_dir,
            format       = input$tagged_format,
            bracket_tags = input$bracket_tags
          )
          incProgress(0.7, detail = "Zipping...")
          txt_files <- list.files(tmp_dir, full.names = TRUE)
          zip::zip(zipfile = file, files = txt_files, mode = "cherry-pick")
          incProgress(1, detail = "Done")
          unlink(tmp_dir, recursive = TRUE)
        })
      },
      contentType = "application/zip"
    )
    # ---- Download Pre-tagged CSV (matches import template) ----
    output$download_pretagged_csv <- downloadHandler(
      filename = function() {
        paste0("pretagged_data_", format(Sys.Date(), "%Y%m%d"), ".csv")
      },
      content = function(file) {
        req(results_data())
        validate(
          need("tagged_text" %in% names(results_data()),
               "Tagged text not available. Please reprocess your data.")
        )

        data <- results_data()
        has_meta <- "metadata" %in% names(data)

        export_df <- tibble(
          doc_id      = data$doc_id,
          tagged_text = data$tagged_text,
          metadata    = if (has_meta) data$metadata else "unknown"
        )

        readr::write_csv(export_df, file)
      },
      contentType = "text/csv"
    )

    # ---- Download Tables (Excel) ----
    output$download_tables <- downloadHandler(
      filename = function() {
        paste0("mda_tables_", format(Sys.Date(), "%Y%m%d"), ".xlsx")
      },
      content = function(file) {
  req(results_data(), input$tables_to_export)

  sheets <- list()

  if ("full" %in% input$tables_to_export) {
    sheets[["Full Results"]] <- results_data()
  }
  if ("aggregated" %in% input$tables_to_export) {
    sheets[["Aggregated"]] <- aggregate_by_metadata(results_data())
  }
  if ("summary" %in% input$tables_to_export) {
    sheets[["Summary Statistics"]] <- summarize_dimensions(results_data(), group_by = "metadata")
  }

  writexl::write_xlsx(sheets, path = file)
}
    )

    # ---- Download Plot ----
    output$download_plot <- downloadHandler(
      filename = function() {
        ext <- tolower(input$plot_format)
        paste0("mda_plot_", input$plot_to_export, "_", format(Sys.Date(), "%Y%m%d"), ".", ext)
      },
      content = function(file) {
        req(results_data())
        p <- switch(input$plot_to_export,
                    "dimensions" = plot_dimensions(results_data(), interactive = FALSE),
                    "text_types" = plot_text_types(results_data(), interactive = FALSE),
                    "scatter"    = plot_dimension_scatter(results_data(), interactive = FALSE),
                    "biber"      = plot_biber_comparison(results_data()),
                    "aggregated" = plot_aggregated_dimensions(aggregate_by_metadata(results_data()), interactive = FALSE)
        )
        ggsave(
          filename = file,
          plot     = p,
          width    = input$plot_width,
          height   = input$plot_height,
          dpi      = input$plot_dpi,
          device   = tolower(input$plot_format)
        )
      }
    )

    # ---- Helper: build and zip an R project folder ----
    build_rproject_zip <- function(results_data, script_name, script_path,
                                   readme_path, zip_dest) {

      tmp_root  <- file.path(tempdir(), paste0("mda_", script_name, "_", Sys.getpid()))
      data_dir  <- file.path(tmp_root, "data")
      rdocs_dir <- file.path(tmp_root, "r_docs")
      dir.create(data_dir,  recursive = TRUE, showWarnings = FALSE)
      dir.create(rdocs_dir, recursive = TRUE, showWarnings = FALSE)

      # Write tagged data
      tagged <- results_data |> select(-any_of("text"))
      readr::write_csv(tagged, file.path(data_dir, "tagged_data.csv"))

      # Reconstruct plain text by stripping tags from tagged_text
      plain_text <- results_data |>
        select(doc_id, any_of("metadata")) |>
        mutate(
          text = results_data$tagged_text |>
            (\(x) str_replace_all(x, "<[A-Z][A-Z0-9]*>", ""))() |>
            (\(x) str_replace_all(x, "_[A-Z][A-Z0-9$\\.]*", " "))() |>
            (\(x) str_replace_all(x, "\\s+", " "))() |>
            trimws()
        )
      readr::write_csv(plain_text, file.path(data_dir, "text_data.csv"))

      # Copy script and README
      file.copy(script_path, file.path(rdocs_dir, paste0(script_name, ".R")))
      file.copy(readme_path, file.path(tmp_root,  "README.md"))

      # Write .Rproj file
      writeLines(paste0(
        "Version: 1.0\n\n",
        "RestoreWorkspace: No\n",
        "SaveWorkspace: No\n",
        "AlwaysSaveHistory: No\n\n",
        "EnableCodeIndexing: Yes\n",
        "UseSpacesForTab: Yes\n",
        "NumSpacesForTab: 2\n",
        "Encoding: UTF-8\n"
      ), file.path(tmp_root, paste0("mda_", script_name, ".Rproj")))

      # Explicit file list — preserves data/ and r_docs/ folder structure
      all_files <- c(
        file.path("data",   "tagged_data.csv"),
        file.path("data",   "text_data.csv"),
        file.path("r_docs", paste0(script_name, ".R")),
        "README.md",
        paste0("mda_", script_name, ".Rproj")
      )

      old_wd <- setwd(tmp_root)
      on.exit({
        setwd(old_wd)
        unlink(tmp_root, recursive = TRUE)
      }, add = TRUE)

      zip::zip(zipfile = zip_dest, files = all_files, mode = "mirror")
      zip_dest
    }  # closes build_rproject_zip

    # ---- Paths to script templates ----
    tagging_script_path  <- "R/templates/tagging.R"
    plotting_script_path <- "R/templates/plotting.R"
    kwic_script_path     <- "R/templates/kwic.R"
    readme_path          <- "R/templates/README_rproject.md"

    # ---- Download: Tagging R project ----
    output$download_rcode_tagging <- downloadHandler(
      filename = function() {
        paste0("mda_tagging_", format(Sys.Date(), "%Y%m%d"), ".zip")
      },
      content = function(file) {
        req(results_data())
        withProgress(message = "Building tagging R project...", value = 0, {
          incProgress(0.5)
          build_rproject_zip(
            results_data = results_data(),
            script_name  = "tagging",
            script_path  = tagging_script_path,
            readme_path  = readme_path,
            zip_dest     = file
          )
          incProgress(1)
        })
      },
      contentType = "application/zip"
    )

    # ---- Download: Plotting R project ----
    output$download_rcode_plotting <- downloadHandler(
      filename = function() {
        paste0("mda_plotting_", format(Sys.Date(), "%Y%m%d"), ".zip")
      },
      content = function(file) {
        req(results_data())
        withProgress(message = "Building plotting R project...", value = 0, {
          incProgress(0.5)
          build_rproject_zip(
            results_data = results_data(),
            script_name  = "plotting",
            script_path  = plotting_script_path,
            readme_path  = readme_path,
            zip_dest     = file
          )
          incProgress(1)
        })
      },
      contentType = "application/zip"
    )

    # ---- Download: KWIC R project ----
    output$download_rcode_kwic <- downloadHandler(
      filename = function() {
        paste0("mda_kwic_", format(Sys.Date(), "%Y%m%d"), ".zip")
      },
      content = function(file) {
        req(results_data())
        withProgress(message = "Building KWIC R project...", value = 0, {
          incProgress(0.5)
          build_rproject_zip(
            results_data = results_data(),
            script_name  = "kwic",
            script_path  = kwic_script_path,
            readme_path  = readme_path,
            zip_dest     = file
          )
          incProgress(1)
        })
      },
      contentType = "application/zip"
    )

  })  # closes moduleServer
}     # closes exportServer
