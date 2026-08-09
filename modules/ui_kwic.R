# =============================================================================
# modules/ui_kwic.R
# KWIC Module UI
# =============================================================================

#' KWIC UI Module
#'
#' @param id Module namespace ID
#' @return Shiny UI elements
kwicUI <- function(id) {
  ns <- NS(id)

  tagList(

    fluidRow(

      # Search Controls
      box(
        title = "🔍 KWIC Search",
        width = 4,
        status = "primary",
        solidHeader = TRUE,

        radioButtons(
          ns("data_source"),
          "Data source:",
          choices = c(
            "In-memory (processed data)" = "memory",
            "Upload vertical .txt files"  = "upload",
            "Upload tagged CSV"           = "csv"
          ),
          selected = "memory"
        ),
        conditionalPanel(
          condition = paste0("input['", ns("data_source"), "'] == 'upload'"),
          fileInput(
            ns("txt_files"),
            "Upload vertical tagged .txt files:",
            multiple = TRUE,
            accept   = ".txt"
          )
        ),
        conditionalPanel(
          condition = paste0("input['", ns("data_source"), "'] == 'csv'"),
          fileInput(
            ns("tagged_csv"),
            "Upload tagged CSV:",
            multiple = FALSE,
            accept   = ".csv"
          ),
          p(class = "text-muted", style = "font-size: 11px;",
            "CSV must have doc_id and tagged_text columns; metadata column optional.")
        ),

        hr(),

        # Search mode
        radioButtons(
          ns("search_mode"),
          "Search mode:",
          choices = c(
            "Token / Phrase" = "token",
            "Tag Bundle"     = "tag"
          ),
          selected = "token",
          inline   = TRUE
        ),

        # Token search input
        conditionalPanel(
          condition = paste0("input['", ns("search_mode"), "'] == 'token'"),
          textInput(
            ns("token_query"),
            "Word or phrase:",
            placeholder = "e.g.  in order to"
          ),
          checkboxInput(
            ns("token_case"),
            "Case sensitive",
            value = FALSE
          )
        ),

        # Tag bundle search input
        conditionalPanel(
          condition = paste0("input['", ns("search_mode"), "'] == 'tag'"),
          textInput(
            ns("bundle_query"),
            "Tag bundle:",
            placeholder = "e.g.  {{DT}} {{JJ}}  or  {{DT<QUAN>}} {{JJ<JJ>}}"
          ),
          p(class = "text-muted", style = "font-size: 11px;",
            "Enter tags in {{}} brackets as they appear in the vertical output.")
        ),

        hr(),

        # Window and display options
        sliderInput(
          ns("window"),
          "Context window (tokens):",
          min   = 2,
          max   = 15,
          value = 5,
          step  = 1
        ),

        conditionalPanel(
          condition = paste0("input['", ns("mode"), "'] != 'summary'"),
          numericInput(
            ns("lines"),
            "Max lines to display:",
            value = 20,
            min   = 5,
            max   = 200,
            step  = 5
          )
        ),

        radioButtons(
          ns("mode"),
          "Line selection:",
          choices = c(
            "Top frequent types first" = "top",
            "Random sample"            = "random",
            "Summary (one per type)"   = "summary"
          ),
          selected = "top",
          inline   = TRUE
        ),

        conditionalPanel(
          condition = paste0("input['", ns("mode"), "'] == 'top'"),
          numericInput(
            ns("top_n"),
            "Number of top types:",
            value = 5,
            min   = 1,
            max   = 20,
            step  = 1
          )
        ),

        br(),

        actionButton(
          ns("run_kwic"),
          "🔍 Run KWIC",
          class = "btn-primary btn-block"
        )
      ),

      # Results panel
      # Results panel
      box(
        title = "📋 Results",
        width = 8,
        status = "info",
        solidHeader = TRUE,
        # Summary line
        uiOutput(ns("results_summary")),
        br(),
        # Download button — only shown when results exist
        conditionalPanel(
          condition = paste0("output['", ns("has_results"), "']"),
          downloadButton(
            ns("download_csv"),
            "Download Results (CSV)",
            class = "btn-success btn-sm"
          ),
          br(), br()
        ),
        # Results table
        DT::dataTableOutput(ns("kwic_table")),
        # TTR by category — shown only when metadata is present
        conditionalPanel(
          condition = paste0("output['", ns("has_meta"), "']"),
          hr(),
          h5("📊 TTR by Category"),
          DT::dataTableOutput(ns("meta_ttr_table"))
        )
      )
    )
  )
}
