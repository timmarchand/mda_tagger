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
          ns("input_method"),
          "Choose input method:",
          choices = c(
            "Paste text" = "paste",
            "Upload single file (TXT/DOCX/CSV)" = "single",
            "Upload multiple corpus files" = "corpus",
            "Upload pre-tagged texts (CSV)" = "pretagged"
          ),
          selected = "paste"
        ),

        hr(),

        # Paste Text Panel ----
        conditionalPanel(
          condition = paste0("input['", ns("input_method"), "'] == 'paste'"),
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

        # File Upload Panel ----
        conditionalPanel(
          condition = paste0("input['", ns("input_method"), "'] == 'single'"),  # ← Changed from 'file'
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

        # Corpus Upload Panel ----
        conditionalPanel(
          condition = paste0("input['", ns("input_method"), "'] == 'corpus'"),
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
        ),

        # Pre-tagged text upload ----
        conditionalPanel(
          condition = paste0("input['", ns("input_method"), "'] == 'pretagged'"),

          h4("📄 Upload Pre-Tagged Texts"),
          p("Upload a CSV with pre-tagged texts. Required columns:"),
          tags$ul(
            tags$li(tags$code("doc_id"), " - Document identifier"),
            tags$li(tags$code("tagged_text"), " - Text with POS and MDA tags (e.g., the_DT<DEMP> cat_NN)"),
            tags$li(tags$code("metadata"), " - Category/group label (optional)")
          ),

          p(class = "text-muted",
            "Example format:",
            tags$br(),
            tags$code("doc_id,tagged_text,metadata"),
            tags$br(),
            tags$code('text1,"the_DT<DEMP> cat_NN sat_VBD",fiction')
          ),

          downloadLink(ns("download_template"), "Download CSV template"),

          br(), br(),

          fileInput(
            ns("pretagged_file"),
            "Choose CSV file:",
            accept = ".csv"
          ),

          # Preview
          conditionalPanel(
            condition = paste0("output['", ns("pretagged_preview_available"), "']"),
            h5("Preview:"),
            DT::dataTableOutput(ns("pretagged_preview"), height = "300px"),
            br()
          )
        )  # End pretagged panel
      )    # End box
    ),     # End first fluidRow

    # Data Summary box ----
    fluidRow(
      box(
        title = "📊 Data Summary",
        width = 12,
        status = "info",

        # Data Summary (shown when data is loaded) ----
        conditionalPanel(
          condition = paste0("output['", ns("data_available"), "']"),
          br(),
          verbatimTextOutput(ns("data_summary")),
          br(),
          actionButton(ns("confirm_data"), "✅ Confirm & Proceed",
                       class = "btn-success btn-lg btn-block")
        ),

        conditionalPanel(
          condition = paste0("!output['", ns("data_available"), "']"),  # ← Fixed condition
          p("No data loaded yet. Please select an input method above.",
            class = "text-muted",
            style = "text-align: center; padding: 20px;")
        )
      )
    )
  )  # End tagList
}    # End function
