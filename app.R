# =============================================================================
# app.R - Main MDA Shiny App
# =============================================================================

source("global.R")

# UI ----
ui <- dashboardPage(
  dashboardHeader(
    title = "MDA Toolkit",
    titleWidth = 300
  ),

  dashboardSidebar(
    width = 300,
    sidebarMenu(
      id = "tabs",
      menuItem("1. Upload Data", tabName = "upload", icon = icon("upload")),
      menuItem("2. Check Tokenization", tabName = "tokencheck", icon = icon("magnifying-glass")),
      menuItem("3. Process & Tag", tabName = "process", icon = icon("cogs")),
      menuItem("4. Results", tabName = "results", icon = icon("chart-bar")),
      menuItem("5. KWIC", tabName = "kwic", icon = icon("search")),
      menuItem("6. Export", tabName = "export", icon = icon("download"))
    ),
    hr(),
    div(
      style = "padding: 15px;",
      h5("About MDA"),
      p("Multi-Dimensional Analysis based on Biber (1988)",
        style = "font-size: 12px;"),
      p("Upload texts, extract 67+ linguistic features, calculate 5 dimension scores.",
        style = "font-size: 11px; color: #999;")
    )
  ),

  dashboardBody(

    # Load custom CSS
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),

    useShinyjs(),

    tabItems(

      # Tab 1: Upload ----
      tabItem(
        tabName = "upload",
        h2("📁 Data Upload"),
        p("Choose your input method and upload your texts for analysis."),
        br(),
        dataInputUI("data_input")
      ),

      # Tab 2: Tokenization Check ----
      tabItem(
        tabName = "tokencheck",
        h2("🔎 Tokenization Check"),
        p("Catch a common data problem before tagging: sentences that run together ",
          "with no space after the period."),
        br(),
        tokenCheckUI("token_check")
      ),

      # Tab 3: Process ----
      tabItem(
        tabName = "process",
        h2("⚙️ Processing & Tagging"),
        p("POS tag your texts and extract linguistic features for MDA."),
        br(),
        processingUI("processing")
      ),

      # Tab 4: Results ----
      tabItem(
        tabName = "results",
        h2("📊 Results & Visualization"),
        p("View dimension scores and analyze your corpus."),
        br(),
        resultsUI("results")
      ),

      # Tab 5: KWIC ----
      tabItem(tabName = "kwic", kwicUI("kwic")),

      # Tab 6: Export ----
      tabItem(
        tabName = "export",
        h2("💾 Export Results"),
        p("Download your results in various formats."),
        br(),
        exportUI("export")
      )

    )  # End tabItems
  )    # End dashboardBody
)      # End dashboardPage


# Server ----
server <- function(input, output, session) {

  # Call data input module
  data_module <- dataInputServer("data_input")

  # Tokenization check sits between data input and processing: it passes
  # data_module's text straight through unless the user has clicked
  # "Fix All", in which case it substitutes the cleaned version. Same
  # return shape as dataInputServer(), so it's a drop-in replacement below.
  token_check_module <- tokenCheckServer("token_check", data_module, session)

  # Call processing module (pass session for tab switching)
  processing_module <- processingServer("processing", token_check_module, session)

  # Call results module
  resultsServer("results", processing_module)

  # Call KWIC module
  kwicServer("kwic", processing_module = processing_module)

  # Call tag guide
  tag_guide <- tagGuideServer("tag_guide", biber_base)   # <- add here

  observeEvent(tag_guide$clicked_tag(), {                # <- and this right after
    req(tag_guide$clicked_tag())
    updateRadioButtons(session, "kwic-search_mode", selected = "tag")
    updateTextInput(session, "kwic-bundle_query",
                    value = paste0("{{", tag_guide$clicked_tag(), "}}"))
  })


  # Call export module
  exportServer("export", processing_module)

  # Monitor when data is confirmed
  prev_confirmed <- reactiveVal(FALSE)

  observeEvent(data_module$data_confirmed(), {
    confirmed_now <- isTRUE(data_module$data_confirmed())

    if (confirmed_now && !isolate(prev_confirmed())) {
      data <- isolate(data_module$selected_text_and_meta())
      cat("\n✅ Data confirmed!\n")
      cat("  Texts:", length(data$text), "\n")
      cat("  Doc IDs:", paste(head(data$doc_ids, 3), collapse = ", "), "...\n")
      cat("  Metadata:", paste(head(unique(data$meta), 3), collapse = ", "), "\n\n")
      # Auto-switch to the tokenization check tab (before processing)
      updateTabItems(session, "tabs", "tokencheck")
      showNotification(
        "Data loaded! Switched to the Tokenization Check tab.",
        type = "message",
        duration = 3
      )
    }

    prev_confirmed(confirmed_now)
  }, ignoreInit = TRUE)

  # Monitor processing completion
  observe({
    result <- processing_module()
    if (!is.null(result) && result$is_complete) {
      cat("\n✅ Processing complete!\n")
      cat("  Processed:", nrow(result$processed_data), "texts\n")
    }
  })
}

# Run ----
shinyApp(ui, server)
