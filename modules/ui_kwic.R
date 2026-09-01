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

    # Tagger-accuracy audit guide — collapsed by default so it stays out of
    # the way day to day, but gives the full step-by-step workflow for both
    # halves of the audit (Section 3.6) in one place instead of scattered
    # one-line hints next to the relevant controls.
    fluidRow(
      box(
        title = "📖 How to Test Tagger Precision & Recall",
        width = 12,
        status = "warning",
        solidHeader = TRUE,
        collapsible = TRUE,
        collapsed = TRUE,

        h5("Precision check"),
        p(class = "text-muted", style = "font-size: 12px;",
          "For a given pattern, draw a random sample of its tagged hits and hand-check each ",
          "one for whether the full tag sequence is correct."),
        tags$ol(
          style = "font-size: 12px;",
          tags$li("Below, set ", strong("Search mode"), " to \"Tag Bundle\"."),
          tags$li("Enter the pattern's tag in ", tags$code("{{TAG}}"), " form, e.g. ",
                  tags$code("{{PHC}}"), " — or a fuller bundle like ",
                  tags$code("{{DT<QUAN>}} {{JJ<JJ>}}"), "."),
          tags$li("Set ", strong("Line selection"), " to \"Random sample\"."),
          tags$li("Set ", strong("Max lines to display"), " to 50 (or your target sample ",
                  "size) — if fewer hits exist, you'll get all of them instead."),
          tags$li("Optional: use ", strong("Filter to category"), " above the search mode to ",
                  "scope the sample to one corpus at a time."),
          tags$li("Click ", strong("Run KWIC"), ", then ", strong("Download Results (CSV)"),
                  ". The file includes a blank ", tags$code("tag_correct"), " column — fill ",
                  "in your judgment per hit."),
          tags$li("Repeat once per pattern, and once per corpus if you're scoping by category.")
        ),

        hr(),

        h5("Recall proxy"),
        p(class = "text-muted", style = "font-size: 12px;",
          "For a pattern's most frequent lexical realisations, check the raw text for any ",
          "occurrence that wasn't tagged as the pattern."),
        tags$ol(
          style = "font-size: 12px;",
          tags$li("Identify the pattern's top 5 lexical realisations (e.g. from your ",
                  "lexical-realisation profiling)."),
          tags$li("In the ", strong("🏷️ Tag Inspector"), " box below, type one realisation ",
                  "into \"Word or phrase to inspect\"."),
          tags$li("Enter the pattern's tag into ", strong("Expected tag"), " — same ",
                  tags$code("{{TAG}}"), " format, braces optional. You can paste it straight ",
                  "from the Tag Bundle box above, or from the tag line under a result below."),
          tags$li("Pasting more than one ", tags$code("{{TAG}}"), " (e.g. the whole tag line ",
                  "for a multi-word query) requires ", strong("all"), " of them to be present ",
                  "for a match — useful for checking a whole expected sequence, not just one tag."),
          tags$li("Optional: use ", strong("Filter to category"), " above to scope to one corpus."),
          tags$li("Click ", strong("🏷️ Show Tags"), ". Each hit is flagged ",
                  HTML("<span style='color:#1a7a1a;font-weight:bold;'>✅ MATCH</span>"), " or ",
                  HTML("<span style='color:#a71d1d;font-weight:bold;'>❌ MISS</span>"),
                  ", with a running tally at the top — any miss is a candidate recall miss."),
          tags$li("Download (CSV) for a ", tags$code("match"), " column in your records."),
          tags$li("Repeat per lexical realisation, per pattern, per corpus.")
        )
      )
    ),

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

        # Category filter — scopes BOTH the KWIC search below and the Tag
        # Inspector box further down the page. Populated dynamically from
        # whatever values exist in the metadata column, so it works whether
        # that column holds a corpus label (e.g. HYSOC/JUSOC), a genre, or
        # anything else; if there's no metadata at all, this has no effect.
        selectInput(
          ns("category_filter"),
          "Filter to category (optional):",
          choices  = c("All" = "all"),
          selected = "all"
        ),
        p(class = "text-muted", style = "font-size: 10px; margin-top: -8px;",
          "Scopes both the search below and the Tag Inspector box further down the page."),

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
          "for that exact pattern. See \"How to Test Tagger Precision & Recall\" above for ",
          "the full recall-check workflow using Expected tag below."),

        fluidRow(
          column(
            width = 4,
            textInput(
              ns("inspect_query"),
              "Word or phrase to inspect:",
              placeholder = "e.g.  and so on"
            )
          ),
          column(
            width = 3,
            textInput(
              ns("inspect_expected_tag"),
              "Expected tag (optional):",
              placeholder = "e.g.  {{PHC}}"
            )
          ),
          column(
            width = 2,
            div(style = "margin-top: 25px;",
                checkboxInput(ns("inspect_case"), "Case sensitive", value = FALSE))
          ),
          column(
            width = 3,
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

        uiOutput(ns("inspect_output"))
      )
    )
  )
}
