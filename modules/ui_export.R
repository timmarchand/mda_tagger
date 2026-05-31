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

    # Row 1: Tagged texts + Results tables
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
          "ZIP file will contain one .txt file per document with tagged text.")
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

    # Row 2: Plots
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

    # Row 3: R Code Export
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
    )

  )  # end tagList
}    # end function
