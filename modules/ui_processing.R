# =============================================================================
# modules/ui_processing.R
# Processing Module UI
# =============================================================================

#' Processing UI Module
#'
#' @param id Module namespace ID
#' @return Shiny UI elements
processingUI <- function(id) {
  ns <- NS(id)

  tagList(

    # Status Summary Boxes
    fluidRow(
      valueBoxOutput(ns("box_texts"), width = 4),
      valueBoxOutput(ns("box_words"), width = 4),
      valueBoxOutput(ns("box_progress"), width = 4)
    ),

    # Processing Controls
    fluidRow(
      box(
        title = "⚙️ Processing Pipeline",
        width = 8,
        status = "primary",
        solidHeader = TRUE,

        h4("Pipeline Steps:"),
        tags$ol(
          tags$li(
            icon("check-circle", class = "text-success"),
            "POS Tagging with UDPipe"
          ),
          tags$li(
            icon("check-circle", class = "text-success"),
            "Linguistic Feature Extraction (67+ features)"
          ),
          tags$li(
            icon("check-circle", class = "text-success"),
            "Feature Counting & Normalization"
          ),
          tags$li(
            icon("check-circle", class = "text-success"),
            "Dimension Score Calculation"
          ),
          tags$li(
            icon("check-circle", class = "text-success"),
            "Text Type Classification"
          )
        ),

        hr(),

        actionButton(
          ns("start_processing"),
          "▶️ Start Processing",
          icon = icon("play"),
          class = "btn-success btn-lg",
          width = "100%"
        ),

        br(), br(),

        uiOutput(ns("processing_status"))
      ),

      box(
        title = "⚙️ Options",
        width = 4,
        status = "info",

        numericInput(
          ns("normalize_per"),
          "Normalize per N words:",
          value = 1000,
          min = 100,
          max = 10000,
          step = 100
        ),

        checkboxInput(
          ns("extract_hesitations"),
          "Extract hesitation markers",
          value = FALSE
        ),

        hr(),

        h5("Model Status:"),
        verbatimTextOutput(ns("model_status"))
      )
    ),

    # Processing Log
    fluidRow(
      box(
        title = "📋 Processing Log",
        width = 12,
        status = "warning",
        collapsible = TRUE,

        verbatimTextOutput(ns("processing_log"))
      )
    ),

    # Sample Output
    fluidRow(
      box(
        title = "👁️ Sample Tagged Output",
        width = 12,
        status = "info",
        collapsible = TRUE,
        collapsed = TRUE,

        selectInput(
          ns("sample_doc_select"),
          "Select Document:",
          choices = NULL
        ),

        numericInput(
          ns("sample_n_tokens"),
          "Number of tokens to display:",
          value = 50,
          min = 10,
          max = 500
        ),

        verbatimTextOutput(ns("sample_output"))
      )
    )
  )
}
