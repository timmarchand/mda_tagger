# =============================================================================
# modules/ui_export.R
# Export Module UI
# =============================================================================

#' Export UI Module
#'
#' @param id Module namespace ID
#' @return Shiny UI elements
exportUI <- function(id) {
  ns <- NS(id)

  tagList(

    conditionalPanel(
      condition = paste0("!output['", ns("data_ready"), "']"),
      div(
        class = "alert alert-warning",
        icon("exclamation-triangle"),
        " Export is only available after processing your texts. Please go to the ",
        tags$strong("Processing"), " tab first."
      )
    ),

    # Shared: optional metadata upload, used by Statistical Analysis,
    # Regression Modelling (and, if selected, Keyness) exports below
    fluidRow(
      box(
        title       = "\U0001F4CE Optional: Additional Metadata for Downloads",
        width       = 12,
        status      = "info",
        solidHeader = TRUE,
        collapsible = TRUE,

        p("Upload a CSV with a doc_id column plus any additional variables (e.g.
           proficiency, task, author_id). These are joined to your data by doc_id and
           made available as predictors in the Statistical Analysis and Regression
           Modelling downloads below."),

        fileInput(ns("extra_metadata_csv"), "Additional metadata CSV (optional):", accept = ".csv")
      )
    ),

    # Row 1: Tagged texts + Results tables ----
    fluidRow(

      box(
        title = "📝 Tagged Texts",
        width = 6,
        status = "primary",
        solidHeader = TRUE,

        p("Download your texts with POS tags and MDA tags."),

        radioButtons(
          ns("tagged_format"),
          "Format:",
          choices = c(
            "Inline (original format)"      = "inline",
            "Vertical (one token per line)" = "vertical"
          ),
          selected = "inline"
        ),

        checkboxInput(
          ns("bracket_tags"),
          "Wrap tags in {{}} brackets",
          value = FALSE
        ),

        p(class = "text-muted", style = "font-size: 11px;",
          "Example: 'the_DT<DEMP>' becomes 'the{{DT<DEMP>}}'"),

        br(),

        downloadButton(
          ns("download_tagged_zip"),
          "Download All Tagged Texts (ZIP)",
          class = "btn-primary btn-block"
        ),

        br(), br(),

        p(class = "text-muted",
          "ZIP file will contain one .txt file per document with tagged text."),

        hr(),

        downloadButton(
          ns("download_pretagged_csv"),
          "Download as Pre-tagged CSV",
          class = "btn-outline-primary btn-block"
        ),

        br(), br(),

        p(class = "text-muted",
          "CSV with doc_id, tagged_text, metadata columns \u2014 matches the format
           expected by the Pre-tagged Data upload option, so you can re-import
           this file directly.")
      ),

      box(
        title = "📊 Results Tables",
        width = 6,
        status = "success",
        solidHeader = TRUE,

        p("Download dimension scores and statistics."),

        checkboxGroupInput(
          ns("tables_to_export"),
          "Select tables:",
          choices = c(
            "Full results table"     = "full",
            "Aggregated by category" = "aggregated",
            "Summary statistics"     = "summary"
          ),
          selected = c("full", "aggregated")
        ),

        br(),

        downloadButton(
          ns("download_tables"),
          "Download Tables (Excel)",
          class = "btn-success btn-block"
        ),

        br(), br(),

        p(class = "text-muted",
          "Excel file will contain multiple sheets with selected tables.")
      )
    ),

    # Row 2: Plots ----
    fluidRow(

      box(
        title = "📈 Plots & Visualizations",
        width = 12,
        status = "info",
        solidHeader = TRUE,

        p("Download high-resolution plots from the Results tab."),

        fluidRow(
          column(4,
                 selectInput(
                   ns("plot_to_export"),
                   "Select plot:",
                   choices = c(
                     "Dimension scores"       = "dimensions",
                     "Text types"             = "text_types",
                     "2D comparison"          = "scatter",
                     "Biber comparison"       = "biber",
                     "Aggregated by category" = "aggregated"
                   )
                 )
          ),
          column(4,
                 selectInput(
                   ns("plot_format"),
                   "Format:",
                   choices  = c("PNG", "PDF", "SVG"),
                   selected = "PNG"
                 )
          ),
          column(4,
                 numericInput(
                   ns("plot_width"),
                   "Width (inches):",
                   value = 10, min = 4, max = 20, step = 1
                 )
          )
        ),

        fluidRow(
          column(4,
                 numericInput(
                   ns("plot_height"),
                   "Height (inches):",
                   value = 6, min = 4, max = 20, step = 1
                 )
          ),
          column(4,
                 numericInput(
                   ns("plot_dpi"),
                   "DPI (resolution):",
                   value = 300, min = 72, max = 600, step = 50
                 )
          )
        ),

        br(),

        downloadButton(
          ns("download_plot"),
          "Download Plot",
          class = "btn-info"
        )
      )
    ),

    # Row 3: R Code Export ----
    fluidRow(

      box(
        title       = "💾 Export R Code",
        width       = 12,
        status      = "success",
        solidHeader = TRUE,
     #   collapsible = TRUE,

        p("Download a self-contained R project with your data and ready-to-run analysis scripts.
           Open the ", tags$code(".Rproj"), " file in RStudio to get started."),

        fluidRow(

          column(4,
                 div(class = "well", style = "min-height: 160px;",
                     h5("🏷️ Tagging Pipeline"),
                     p(class = "text-muted", style = "font-size: 12px;",
                       "Full UDPipe POS tagging, MDA feature extraction, and Biber dimension scoring.
                 Includes examples for filtering and comparing groups."),
                     downloadButton(
                       ns("download_rcode_tagging"),
                       "Download Tagging Project",
                       class = "btn-success btn-sm btn-block"
                     )
                 )
          ),

          column(4,
                 div(class = "well", style = "min-height: 160px;",
                     h5("📊 Plotting"),
                     p(class = "text-muted", style = "font-size: 12px;",
                       "Five ggplot2 visualisations: dimension scores, group comparisons,
                 scatter plots, feature heatmap, and boxplots. Each plot can be saved."),
                     downloadButton(
                       ns("download_rcode_plotting"),
                       "Download Plotting Project",
                       class = "btn-success btn-sm btn-block"
                     )
                 )
          ),

          column(4,
                 div(class = "well", style = "min-height: 160px;",
                     h5("🔍 KWIC Concordance"),
                     p(class = "text-muted", style = "font-size: 12px;",
                       "Token/phrase and tag bundle KWIC functions with console display
                 and CSV export. Supports POS-only and full MDA tag matching."),
                     downloadButton(
                       ns("download_rcode_kwic"),
                       "Download KWIC Project",
                       class = "btn-success btn-sm btn-block"
                     )
                 )
          )
        ),

        div(
          class = "alert alert-info",
          style = "margin-top: 10px; margin-bottom: 0;",
          icon("info-circle"),
          " Each download includes: your ", tags$strong("data CSVs"),
          ", an ", tags$strong("R script"),
          ", a ", tags$strong("README"),
          ", and an ", tags$strong(".Rproj"),
          " file. Open the project in RStudio and run the script section by section."
        )
      )
    ),
    # Row 4: Statistical Analysis Export ----
    fluidRow(
      box(
        title       = "📈 Statistical Analysis Export",
        width       = 12,
        status      = "success",
        solidHeader = TRUE,

        p("Download a ready-to-run R project for comparing dimension scores across your metadata categories: ",
          tags$strong("one-way ANOVA, Tukey post-hoc tests, diagnostics, and effect sizes"),
          ". Includes a README documenting how to extend to factorial or mixed-effects designs
           if you add more metadata columns."),
        hr(),
        h5("Factorial / mixed-effects designs"),
        p(class = "text-muted", style = "font-size: 12px;",
          "If you uploaded a metadata CSV above, choose factors below to generate
           additional analysis scripts."),

        uiOutput(ns("factor_selectors")),


        downloadButton(
          ns("download_stats_project"),
          "Download Statistical Analysis Project",
          class = "btn-success btn-block"
        ),

        br(), br(),

        p(class = "text-muted",
          "ZIP contains dimension_scores.csv, four R scripts (import, descriptives, group
           comparisons, post-hoc), and a README explaining the analysis choices.")
      )
    ),
    # Row 5: Keyness / Key Feature Analysis Export ----
    fluidRow(
      box(
        title       = "🔑 Keyness / Key Feature Analysis Export",
        width       = 12,
        status      = "warning",
        solidHeader = TRUE,

        p("Download data for comparing metadata categories on individual features (words or tags),
           for use with Key Feature Analysis, weighted log-odds, KL divergence, and dispersion measures."),

        checkboxGroupInput(
          ns("keyness_feature_types"),
          "Feature type(s) to export:",
          choices  = c("Token (word forms)" = "token",
                       "POS tag only" = "pos",
                       "Full tag (POS + MDA subtags)" = "tag"),
          selected = "tag"
        ),

        selectInput(
          ns("keyness_ngram_size"),
          "N-gram size:",
          choices  = c("1 (single feature)" = 1, "2 (bigram)" = 2, "3 (trigram)" = 3, "4" = 4),
          selected = 1
        ),

        downloadButton(
          ns("download_keyness_project"),
          "Download Keyness Data Project",
          class = "btn-warning btn-block"
        ),

        br(), br(),

        p(class = "text-muted",
          "ZIP contains doc_lengths.csv plus token/tag count tables, and R scripts for
           Key Feature Analysis, Gries's dispersion (DP), weighted log-odds, and KL divergence.")
      )
    ),
    # Row 6: Regression Modelling Export ----
    fluidRow(
      box(
        title       = "\U0001F4C9 Regression Modelling Export",
        width       = 12,
        status      = "danger",
        solidHeader = TRUE,

        p("Download feature count data and R scripts for modelling how the frequency of a
           single chosen feature varies with predictor variables (PPML, GLMM, and
           zero-inflated Poisson)."),

        p(class = "text-muted", style = "font-size: 12px;",
          "Uses the metadata CSV uploaded above, if any, so its columns are available as
           predictors alongside metadata."),

        checkboxGroupInput(
          ns("regression_feature_types"),
          "Feature type(s) to export:",
          choices  = c("Token (word forms)" = "token",
                       "POS tag only" = "pos",
                       "Full tag (POS + MDA subtags)" = "tag"),
          selected = "tag"
        ),

        selectInput(
          ns("regression_ngram_size"),
          "N-gram size:",
          choices  = c("1 (single feature)" = 1, "2 (bigram)" = 2, "3 (trigram)" = 3, "4" = 4),
          selected = 1
        ),

        downloadButton(
          ns("download_regression_project"),
          "Download Regression Modelling Project",
          class = "btn-danger btn-block"
        ),

        br(), br(),

        p(class = "text-muted",
          "ZIP contains doc_lengths.csv (with any uploaded predictor columns joined in),
           the relevant count table(s), and PPML/GLMM/ZIP model scripts.")
      )
    )
  )  # end tagList
}    # end function
