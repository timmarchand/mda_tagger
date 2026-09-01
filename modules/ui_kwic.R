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
          selectInput(
            ns("lines"),
            "Max lines to display:",
            choices  = c("5" = "5", "10" = "10", "20" = "20", "50" = "50",
                         "100" = "100", "200" = "200", "All" = "all"),
            selected = "20"
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
          downloadButton(
            ns("download_meta_ttr"),
            "Download TTR/Entropy (CSV)",
            class = "btn-success btn-sm"
          ),
          br(), br(),
          DT::dataTableOutput(ns("meta_ttr_table"))
        )
      )
    ),

    # Tag Inspector
    fluidRow(
      box(
        title = "🏷️ Tag Inspector",
        width = 12,
        status = "success",
        solidHeader = TRUE,

        p(class = "text-muted", style = "font-size: 12px;",
          "Look up a word or phrase and see exactly how the tagger tagged it. ",
          "Each example shows a concordance line with a parallel line of tags underneath, ",
          "aligned token by token. Tags are shown in the same {{TAG}} format as the ",
          "Tag Bundle box above, so you can copy a tag line and paste it there to search ",
          "for that exact pattern."),

        fluidRow(
          column(
            width = 5,
            textInput(
              ns("inspect_query"),
              "Word or phrase to inspect:",
              placeholder = "e.g.  and so on"
            )
          ),
          column(
            width = 3,
            checkboxInput(ns("inspect_case"), "Case sensitive", value = FALSE)
          ),
          column(
            width = 4,
            div(style = "margin-top: 25px;",
                actionButton(ns("run_inspect"), "🏷️ Show Tags", class = "btn-success btn-block"))
          )
        ),

        fluidRow(
          column(
            width = 5,
            sliderInput(
              ns("inspect_window"),
              "Context window (tokens):",
              min   = 2,
              max   = 10,
              value = 5,
              step  = 1
            )
          ),
          column(
            width = 3,
            selectInput(
              ns("inspect_n"),
              "Examples to show:",
              choices  = c("5" = "5", "10" = "10", "20" = "20", "50" = "50", "All" = "all"),
              selected = "5"
            )
          ),
          column(
            width = 4,
            conditionalPanel(
              condition = paste0("output['", ns("inspect_has_results"), "']"),
              div(style = "margin-top: 25px;",
                  downloadButton(
                    ns("download_inspect_csv"),
                    "Download (CSV)",
                    class = "btn-success btn-sm btn-block"
                  ))
            )
          )
        ),

        uiOutput(ns("inspect_output"))      )
    )
  )
}
