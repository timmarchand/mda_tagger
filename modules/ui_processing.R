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

        h4("Pipeline Steps:"), ####
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
          value = 100,
          min = 100,
          max = 10000,
          step = 100
        ),

        checkboxInput(
          ns("use_deflated"),
          "Use deflated scores (recommended)",
          value = TRUE
        ),

        p(class = "text-muted", style = "font-size: 11px;",
          "Deflated scores exclude rare features (mean < 0.1) following Nini (2019)"),

        hr(),
        h5("Type-token ratio (Dimension 1)"),

        selectInput(
          ns("ttr_method"),
          "TTR method:",
          choices = c("First N words (Biber 1988)" = "first",
                      "Moving-average TTR (MATTR)" = "mattr"),
          selected = "first"
        ),

        numericInput(
          ns("ttr_window"),
          "TTR window (words):",
          value = 400, min = 50, max = 5000, step = 50
        ),
        conditionalPanel(
          condition = paste0("input['", ns("ttr_window"), "'] != 400"),
          p(class = "text-warning", style = "font-size: 11px;",
            "Biber's TTR norm (mean 51.1, SD 5.2) is based on 400 words. ",
            "Other window sizes shift TTR, and so Dimension 1, away from that norm.")
        ),

        radioButtons(
          ns("ttr_short"),
          "Texts shorter than the window:",
          choices = c("Include: TTR on all available words" = "include",
                      "Leave TTR out of Dimension 1" = "exclude",
                      "Leave Dimension 1 blank" = "blank_d1",
                      "Use the register average (by metadata)" = "register_mean"),
          selected = "include"
        ),
        p(class = "text-muted", style = "font-size: 11px;",
          "Include: short texts get a higher TTR, so a lower D1 (about 2 points at 200 words). ",
          "Leave out: same as giving them Biber's mean TTR, which shifts D1 by register. ",
          "Register average: borrows the mean TTR of full-length texts with the same label; ",
          "scores then depend on the rest of the batch and carry register differences into ",
          "short texts, so avoid it when the labels are the groups you are comparing. ",
          "The results table records TTR, the method, the words used and how each text was handled."),

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
