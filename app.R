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
      menuItem("2. Process & Tag", tabName = "process", icon = icon("cogs")),
      menuItem("3. Results", tabName = "results", icon = icon("chart-bar")),
      menuItem("4. Export", tabName = "export", icon = icon("download"))
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

      # Tab 2: Process ----
      tabItem(
        tabName = "process",
        h2("⚙️ Processing & Tagging"),
        p("Process uploaded texts and extract linguistic features."),
        br(),
        box(
          title = "Processing",
          width = 12,
          status = "warning",
          p("Processing module coming soon...",
            class = "text-muted",
            style = "text-align: center; padding: 40px;")
        )
      ),

      # Tab 3: Results ----
      tabItem(
        tabName = "results",
        h2("📊 Results & Visualization"),
        p("View dimension scores and visualize your corpus."),
        br(),
        box(
          title = "Results",
          width = 12,
          status = "info",
          p("Results module coming soon...",
            class = "text-muted",
            style = "text-align: center; padding: 40px;")
        )
      ),

      # Tab 4: Export ----
      tabItem(
        tabName = "export",
        h2("💾 Export Results"),
        p("Download your results in various formats."),
        br(),
        box(
          title = "Export",
          width = 12,
          status = "success",
          p("Export module coming soon...",
            class = "text-muted",
            style = "text-align: center; padding: 40px;")
        )
      )
    )
  )
)

# Server ----
server <- function(input, output, session) {

  # Call data input module
  data_module <- dataInputServer("data_input")

  # Monitor when data is confirmed
  observe({
    if (!is.null(data_module$data_confirmed()) && data_module$data_confirmed()) {
      data <- data_module$selected_text_and_meta()
      cat("\n✅ Data confirmed!\n")
      cat("  Texts:", length(data$text), "\n")
      cat("  Doc IDs:", paste(head(data$doc_ids, 3), collapse = ", "), "...\n")
      cat("  Metadata:", paste(head(unique(data$meta), 3), collapse = ", "), "\n\n")

      # Auto-switch to processing tab
      updateTabItems(session, "tabs", "process")

      showNotification(
        "Data loaded! Switched to Processing tab.",
        type = "message",
        duration = 3
      )
    }
  })

  # Future: Processing module will go here
  # Future: Results module will go here
  # Future: Export module will go here

}

# Run ----
shinyApp(ui, server)
