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

    ns <- session$ns

    # Get processed data
    results_data <- reactive({
      proc <- processing_module()
      if (!is.null(proc) && proc$is_complete) {
        return(proc$processed_data)
      }
      return(NULL)
    })

    # ---- Optional extra metadata for factorial / mixed-effects designs ----
    extra_metadata <- reactive({
      req(input$extra_metadata_csv)
      df <- tryCatch(
        readr::read_csv(input$extra_metadata_csv$datapath, show_col_types = FALSE),
        error = function(e) NULL
      )
      validate(need(!is.null(df), "Could not read the metadata CSV."))
      validate(need("doc_id" %in% names(df), "Metadata CSV must have a doc_id column."))
      df
    })

    output$factor_selectors <- renderUI({
      req(extra_metadata())
      extra_cols <- setdiff(names(extra_metadata()), "doc_id")
      validate(need(length(extra_cols) > 0,
                    "No additional columns found besides doc_id."))

      tagList(
        selectizeInput(
          ns("factorial_factors"),
          "Factors for factorial analysis (choose exactly 2 to enable):",
          choices = extra_cols, multiple = TRUE,
          options = list(maxItems = 2)
        ),
        selectInput(
          ns("mixed_grouping_var"),
          "Grouping variable for mixed-effects (e.g. author_id):",
          choices = c("None" = "", extra_cols)
        )
      )
    })

    results_data_for_stats <- reactive({
      base <- results_data()
      req(base)
      if (is.null(input$extra_metadata_csv)) return(base)
      em <- extra_metadata()
      if (is.null(em)) return(base)

      base$doc_id <- as.character(base$doc_id)
      em$doc_id   <- as.character(em$doc_id)

      dplyr::left_join(base, em, by = "doc_id")
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

    # ---- Helper: fill a template file's {{PLACEHOLDER}} tokens ----
    fill_template <- function(template_path, replacements) {
      txt <- readLines(template_path, warn = FALSE)
      txt <- paste(txt, collapse = "\n")
      for (key in names(replacements)) {
        txt <- gsub(paste0("\\{\\{", key, "\\}\\}"), replacements[[key]], txt)
      }
      txt
    }

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

    # ---- Helper: build and zip the Statistical Analysis project ----
    build_stats_rproject_zip <- function(processed_data, zip_dest,
                                         factorial_factors = NULL,
                                         mixed_grouping_var = NULL) {

      tmp_root <- file.path(tempdir(), paste0("mda_stats_", Sys.getpid()))
      data_dir <- file.path(tmp_root, "data")
      r_dir    <- file.path(tmp_root, "R")
      dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)
      dir.create(r_dir,    recursive = TRUE, showWarnings = FALSE)

      dim_cols <- intersect(
        c("Dimension1", "Dimension2", "Dimension3", "Dimension4", "Dimension5"),
        names(processed_data)
      )

      extra_cols <- unique(c(factorial_factors, mixed_grouping_var))
      extra_cols <- extra_cols[!is.na(extra_cols) & extra_cols != ""]

      scores <- processed_data %>%
        select(doc_id, any_of("metadata"), any_of("n_words"), all_of(dim_cols), any_of(extra_cols))
      readr::write_csv(scores, file.path(data_dir, "dimension_scores.csv"))

      file.copy(stats_import_script_path,            file.path(r_dir, "01_import.R"))
      file.copy(stats_descriptives_script_path,       file.path(r_dir, "02_descriptives.R"))
      file.copy(stats_group_comparisons_script_path,  file.path(r_dir, "03_group_comparisons.R"))
      file.copy(stats_posthoc_script_path,            file.path(r_dir, "04_posthoc.R"))
      file.copy(stats_readme_path,                    file.path(tmp_root, "README.md"))

      all_files <- c(
        file.path("data", "dimension_scores.csv"),
        file.path("R", "01_import.R"),
        file.path("R", "02_descriptives.R"),
        file.path("R", "03_group_comparisons.R"),
        file.path("R", "04_posthoc.R"),
        "README.md",
        "MDA_analysis.Rproj"
      )

      extra_notes <- character(0)

      if (!is.null(factorial_factors) && length(factorial_factors) == 2) {
        factorial_txt <- fill_template(
          stats_factorial_template_path,
          list(FACTOR1 = factorial_factors[1], FACTOR2 = factorial_factors[2])
        )
        writeLines(factorial_txt, file.path(r_dir, "05_factorial.R"))
        all_files <- c(all_files, file.path("R", "05_factorial.R"))
        extra_notes <- c(extra_notes, paste0(
          "- R/05_factorial.R - factorial analysis using ",
          factorial_factors[1], " and ", factorial_factors[2], "."
        ))
      }

      if (!is.null(mixed_grouping_var) && nchar(mixed_grouping_var) > 0) {
        mixed_txt <- fill_template(
          stats_mixed_template_path,
          list(GROUPING_VAR = mixed_grouping_var)
        )
        writeLines(mixed_txt, file.path(r_dir, "06_mixed_effects.R"))
        all_files <- c(all_files, file.path("R", "06_mixed_effects.R"))
        extra_notes <- c(extra_notes, paste0(
          "- R/06_mixed_effects.R - mixed-effects analysis with (1 | ", mixed_grouping_var, ")."
        ))
      }

      if (length(extra_notes) > 0) {
        cat(
          "\n\n## Additional analyses included in this export\n\n",
          paste(extra_notes, collapse = "\n"), "\n",
          file = file.path(tmp_root, "README.md"), append = TRUE
        )
      }

      writeLines(paste0(
        "Version: 1.0\n\n",
        "RestoreWorkspace: No\n",
        "SaveWorkspace: No\n",
        "AlwaysSaveHistory: No\n\n",
        "EnableCodeIndexing: Yes\n",
        "UseSpacesForTab: Yes\n",
        "NumSpacesForTab: 2\n",
        "Encoding: UTF-8\n"
      ), file.path(tmp_root, "MDA_analysis.Rproj"))

      old_wd <- setwd(tmp_root)
      on.exit({
        setwd(old_wd)
        unlink(tmp_root, recursive = TRUE)
      }, add = TRUE)

      zip::zip(zipfile = zip_dest, files = all_files, mode = "mirror")
      zip_dest
    }

    # ---- Helper: build long-format feature count tables plus doc lengths ----
    build_keyness_tables <- function(processed_data, feature_types, ngram_size = 1) {
      has_meta <- "metadata" %in% names(processed_data)

      make_ngrams <- function(x, n) {
        if (length(x) < n) return(character(0))
        if (n == 1) return(x)
        cols <- lapply(0:(n - 1), function(k) x[(1 + k):(length(x) - n + 1 + k)])
        do.call(paste, c(cols, list(sep = " ")))
      }

      parsed_docs <- lapply(processed_data$tagged_text, function(tt) {
        tt <- str_replace_all(tt, "(\\S+)\\s+(<)", "\\1\\2")
        tokens <- str_split(tt, "\\s+")[[1]]
        tokens <- tokens[tokens != ""]

        if (length(tokens) == 0) {
          return(list(word = character(0), pos = character(0), tag = character(0), n_words = 0))
        }

        has_us    <- str_detect(tokens, "_")
        word      <- tolower(ifelse(has_us, str_extract(tokens, "^.+?(?=_)"), tokens))
        full_tag  <- ifelse(has_us, str_extract(tokens, "(?<=_).+$"), "UNTAGGED")
        base_pos  <- ifelse(full_tag == "UNTAGGED", "UNTAGGED", str_extract(full_tag, "^[^<]+"))

        list(
          word    = word,
          pos     = paste0("{{", base_pos, "}}"),
          tag     = paste0("{{", full_tag, "}}"),
          n_words = length(tokens)
        )
      })

      doc_lengths <- tibble(
        doc_id   = processed_data$doc_id,
        metadata = if (has_meta) processed_data$metadata else "unknown",
        n_words  = map_int(parsed_docs, ~ .x$n_words)
      )

      level_field <- c(token = "word", pos = "pos", tag = "tag")
      counts <- list()

      for (ft in feature_types) {
        field <- level_field[[ft]]
        counts[[ft]] <- map_dfr(seq_along(parsed_docs), function(i) {
          units <- parsed_docs[[i]][[field]]
          feats <- make_ngrams(units, ngram_size)
          if (length(feats) == 0) return(tibble())
          tibble(feature = feats) %>%
            count(feature, name = "count") %>%
            mutate(
              doc_id   = processed_data$doc_id[i],
              metadata = if (has_meta) processed_data$metadata[i] else "unknown"
            ) %>%
            select(doc_id, metadata, feature, count)
        })
      }

      list(counts = counts, doc_lengths = doc_lengths)
    }

    # ---- Helper: build and zip the Keyness Analysis project ----
    build_keyness_rproject_zip <- function(processed_data, feature_types, ngram_size, zip_dest) {

      tmp_root <- file.path(tempdir(), paste0("mda_keyness_", Sys.getpid()))
      data_dir <- file.path(tmp_root, "data")
      r_dir    <- file.path(tmp_root, "R")
      dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)
      dir.create(r_dir,    recursive = TRUE, showWarnings = FALSE)

      tables <- build_keyness_tables(processed_data, feature_types, ngram_size)

      readr::write_csv(tables$doc_lengths, file.path(data_dir, "doc_lengths.csv"))
      all_files <- c(file.path("data", "doc_lengths.csv"))

      filenames <- c(token = "token_counts.csv", pos = "pos_counts.csv", tag = "tag_counts.csv")
      for (ft in feature_types) {
        readr::write_csv(tables$counts[[ft]], file.path(data_dir, filenames[[ft]]))
        all_files <- c(all_files, file.path("data", filenames[[ft]]))
      }

      file.copy(keyness_kfa_script_path, file.path(r_dir, "02_kfa.R"))
      file.copy(keyness_dispersion_script_path, file.path(r_dir, "03_dispersion.R"))
      file.copy(keyness_wlo_script_path, file.path(r_dir, "04_weighted_log_odds.R"))
      file.copy(keyness_kld_script_path, file.path(r_dir, "05_kld.R"))

      import_txt <- fill_template(keyness_import_script_path, list(NGRAM_SIZE = ngram_size))
      writeLines(import_txt, file.path(r_dir, "01_import.R"))

      type_labels <- c(token = "Token (word forms)", pos = "POS tag only", tag = "Full tag (POS + MDA subtags)")
      readme_txt <- fill_template(keyness_readme_path, list(
        NGRAM_SIZE     = ngram_size,
        FEATURE_TYPES  = paste(type_labels[feature_types], collapse = ", ")
      ))
      writeLines(readme_txt, file.path(tmp_root, "README.md"))

      all_files <- c(all_files,
                     file.path("R", "01_import.R"),
                     file.path("R", "02_kfa.R"),
                     file.path("R", "03_dispersion.R"),
                     file.path("R", "04_weighted_log_odds.R"),
                     file.path("R", "05_kld.R"),
                     "README.md", "MDA_keyness.Rproj")

      writeLines(paste0(
        "Version: 1.0\n\n",
        "RestoreWorkspace: No\n",
        "SaveWorkspace: No\n",
        "AlwaysSaveHistory: No\n\n",
        "EnableCodeIndexing: Yes\n",
        "UseSpacesForTab: Yes\n",
        "NumSpacesForTab: 2\n",
        "Encoding: UTF-8\n"
      ), file.path(tmp_root, "MDA_keyness.Rproj"))

      old_wd <- setwd(tmp_root)
      on.exit({
        setwd(old_wd)
        unlink(tmp_root, recursive = TRUE)
      }, add = TRUE)

      zip::zip(zipfile = zip_dest, files = all_files, mode = "mirror")
      zip_dest
    }

    # ---- Paths to script templates ----
    tagging_script_path  <- "R/templates/tagging.R"
    plotting_script_path <- "R/templates/plotting.R"
    kwic_script_path     <- "R/templates/kwic.R"
    readme_path          <- "R/templates/README_rproject.md"

    stats_import_script_path            <- "R/templates/stats_import.R"
    stats_descriptives_script_path      <- "R/templates/stats_descriptives.R"
    stats_group_comparisons_script_path <- "R/templates/stats_group_comparisons.R"
    stats_posthoc_script_path           <- "R/templates/stats_posthoc.R"
    stats_readme_path                   <- "R/templates/README_stats_rproject.md"
    stats_factorial_template_path       <- "R/templates/stats_factorial_template.R"
    stats_mixed_template_path           <- "R/templates/stats_mixed_template.R"

    keyness_import_script_path        <- "R/templates/keyness_import.R"
    keyness_kfa_script_path           <- "R/templates/keyness_kfa.R"
    keyness_dispersion_script_path    <- "R/templates/keyness_dispersion.R"
    keyness_wlo_script_path           <- "R/templates/keyness_weighted_log_odds.R"
    keyness_kld_script_path           <- "R/templates/keyness_kld.R"
    keyness_readme_path               <- "R/templates/README_keyness_rproject.md"

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

    # ---- Download: Statistical Analysis R project ----
    output$download_stats_project <- downloadHandler(
      filename = function() {
        paste0("mda_stats_analysis_", format(Sys.Date(), "%Y%m%d"), ".zip")
      },
      content = function(file) {
        req(results_data_for_stats())
        validate(
          need(any(c("Dimension1","Dimension2","Dimension3","Dimension4","Dimension5") %in% names(results_data_for_stats())),
               "Dimension scores not available. Please reprocess your data.")
        )
        withProgress(message = "Building statistical analysis R project...", value = 0, {
          incProgress(0.5)
          build_stats_rproject_zip(
            results_data_for_stats(),
            zip_dest           = file,
            factorial_factors  = input$factorial_factors,
            mixed_grouping_var = input$mixed_grouping_var
          )
          incProgress(1)
        })
      },
      contentType = "application/zip"
    )

    # ---- Download: Keyness Analysis data project ----
    output$download_keyness_project <- downloadHandler(
      filename = function() {
        paste0("mda_keyness_", format(Sys.Date(), "%Y%m%d"), ".zip")
      },
      content = function(file) {
        req(results_data())
        validate(
          need("tagged_text" %in% names(results_data()),
               "Tagged text not available. Please reprocess your data.")
        )
        validate(
          need(length(input$keyness_feature_types) > 0,
               "Select at least one feature type.")
        )
        withProgress(message = "Building keyness analysis data...", value = 0, {
          incProgress(0.5)
          build_keyness_rproject_zip(
            results_data(),
            feature_types = input$keyness_feature_types,
            ngram_size    = as.integer(input$keyness_ngram_size),
            zip_dest      = file
          )
          incProgress(1)
        })
      },
      contentType = "application/zip"
    )
  })  # closes moduleServer
}     # closes exportServer
