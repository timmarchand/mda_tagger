# =============================================================================
# modules/ui_tokencheck.R
# Tokenization Check UI Module
# =============================================================================

#' Tokenization Check UI
#'
#' @param id Module namespace ID
#' @return Shiny UI elements
tokenCheckUI <- function(id) {
  ns <- NS(id)

  tagList(
    fluidRow(
      box(
        title = "🔎 Tokenization Check",
        width = 12,
        status = "warning",
        solidHeader = TRUE,

        p("Before tagging, check your raw texts for a common data problem: punctuation that ",
          "runs straight into the next word with no space — e.g. ",
          tags$code("...and so on.Should I..."), " instead of ",
          tags$code("...and so on. Should I..."), ". Left uncorrected, the tokenizer fuses the ",
          "two words into one run-on token, which distorts POS tagging and every MDA feature ",
          "count that depends on it."),

        checkboxGroupInput(
          ns("issue_types"),
          "Check for:",
          choices  = issue_type_choices,
          selected = DEFAULT_ISSUE_TYPES
        ),

        p(class = "text-muted", style = "font-size: 11px;",
          "Every row below starts selected. If a match is a false positive you don't want ",
          "changed — e.g. ", tags$code("e.g."), " or ", tags$code("a.m."),
          " under period + lowercase letter — click that row to deselect it, then use ",
          "\"Fix Selected\" to insert a space everywhere still checked."),

        fluidRow(
          column(
            width = 4,
            actionButton(ns("scan"), "🔎 Scan for Issues", class = "btn-warning btn-block")
          ),
          column(
            width = 4,
            conditionalPanel(
              condition = paste0("output['", ns("has_issues"), "']"),
              actionButton(ns("fix_selected"), "🛠️ Fix Selected",
                           class = "btn-success btn-block")
            )
          ),
          column(
            width = 4,
            conditionalPanel(
              condition = paste0("output['", ns("is_fixed"), "']"),
              actionButton(ns("reset_fix"), "↩️ Revert to Original Text",
                           class = "btn-outline-secondary btn-block")
            )
          )
        ),

        br(),
        uiOutput(ns("status_banner")),

        conditionalPanel(
          condition = paste0("output['", ns("has_issues"), "']"),
          br(),
          downloadButton(ns("download_report"), "Download Issue Report (CSV)",
                         class = "btn-info btn-sm"),
          br(), br(),
          DT::dataTableOutput(ns("issues_table"))
        ),

        hr(),

        # Before a scan has been run, this is a genuine skip (no check
        # happens at all). Once a scan has been run, clicking through is no
        # longer "skipping" anything -- it's just moving on -- so the button
        # relabels itself to "Continue to Processing" instead of staying on
        # "Skip Check" as if nothing had happened.
        conditionalPanel(
          condition = paste0("!output['", ns("has_scanned"), "']"),
          p(class = "text-muted", style = "font-size: 12px;",
            "Not interested in checking this? You can skip straight to processing — your texts ",
            "will be tagged exactly as uploaded."),
          actionButton(ns("skip_check"), "⏭️ Skip Check — Go to Processing",
                       class = "btn-outline-secondary")
        ),
        conditionalPanel(
          condition = paste0("output['", ns("has_scanned"), "']"),
          p(class = "text-muted", style = "font-size: 12px;",
            "Done reviewing? Continue to processing with the text as it currently stands ",
            "(including any fixes you applied above)."),
          actionButton(ns("continue_processing"), "➡️ Continue to Processing",
                       class = "btn-success")
        )
      )
    )
  )
}
