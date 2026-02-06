# =============================================================================
# modules/ui_data_input.R
# Data Input UI Module
# =============================================================================

#' Data Input UI Module
#'
#' @param id Module namespace ID
#' @return Shiny UI elements
dataInputUI <- function(id) {
  ns <- NS(id)

  tagList(
    fluidRow(
      box(
        title = "📁 Data Input Options",
        width = 12,
        status = "primary",
        solidHeader = TRUE,

        radioButtons(
          ns("input_type"),
          "Choose input method:",
          choices = c(
            "Paste Text" = "paste",
            "Upload File (CSV/TXT/DOCX)" = "file",
            "Upload Multiple Files (Corpus)" = "corpus"
          ),
          selected = "paste",
          inline = FALSE
        ),

        hr(),

        # Paste Text Panel
        conditionalPanel(
          condition = paste0("input['", ns("input_type"), "'] == 'paste'"),
          h4("📝 Paste Your Text"),
          p("Paste text directly for quick analysis of a single document."),
          textAreaInput(
            ns("pasted_text"),
            label = NULL,
            placeholder = "Paste or type your text here...",
            rows = 12,
            width = "100%"
          ),
          actionButton(
            ns("process_paste"),
            "Process Text",
            icon = icon("play"),
            class = "btn-success"
          ),
          helpText("Best for: Quick tests, single documents, short texts")
        ),

        # File Upload Panel
        conditionalPanel(
          condition = paste0("input['", ns("input_type"), "'] == 'file'"),
          h4("📄 Upload Single File"),
          p("Upload a CSV with multiple texts, or a single TXT/DOCX file."),
          fileInput(
            ns("upload_csv"),
            "Choose file:",
            accept = c(".txt", ".csv", ".docx", ".doc"),
            multiple = FALSE,
            buttonLabel = "Browse...",
            placeholder = "No file selected"
          ),

          conditionalPanel(
            condition = paste0("input['", ns("upload_csv"), "'] != null"),
            numericInput(
              ns("skip_rows"),
              "Skip rows (for CSV headers):",
              value = 0,
              min = 0,
              step = 1,
              width = "200px"
            )
          ),

          uiOutput(ns("column_selection_ui")),

          helpText("Supported formats: .txt, .csv, .docx")
        ),

        # Corpus Upload Panel
        conditionalPanel(
          condition = paste0("input['", ns("input_type"), "'] == 'corpus'"),
          h4("📚 Upload Corpus (Multiple Files)"),
          p("Upload multiple text files to create a corpus for comparison."),
          fileInput(
            ns("upload_corpus"),
            "Choose multiple files:",
            multiple = TRUE,
            accept = c(".txt", ".doc", ".docx"),
            buttonLabel = "Browse...",
            placeholder = "No files selected"
          ),

          uiOutput(ns("corpus_metadata_container")),

          helpText("Upload multiple .txt or .docx files. Each file = one document.")
        )
      )
    ),

    fluidRow(
      box(
        title = "📊 Data Summary",
        width = 12,
        status = "info",

        conditionalPanel(
          condition = paste0("output['", ns("data_available"), "'] == true"),
          verbatimTextOutput(ns("data_summary")),
          hr(),
          actionButton(
            ns("confirm_data"),
            "✅ Confirm & Proceed to Processing",
            icon = icon("arrow-right"),
            class = "btn-primary btn-lg",
            width = "100%"
          )
        ),

        conditionalPanel(
          condition = paste0("output['", ns("data_available"), "'] == false"),
          p("No data loaded yet. Please select an input method above.",
            class = "text-muted",
            style = "text-align: center; padding: 20px;")
        )
      )
    )
  )
}
