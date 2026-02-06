# =============================================================================
# app.R - Main MDA Shiny App
# =============================================================================

source("global.R")

# UI ----
ui <- dashboardPage(

  dashboardHeader(title = "MDA Toolkit", titleWidth = 300),

  dashboardSidebar(
    width = 300,
    sidebarMenu(
      id = "tabs",
      menuItem("Upload Data", tabName = "upload", icon = icon("upload")),
      menuItem("Process", tabName = "process", icon = icon("cogs")),
      menuItem("Results", tabName = "results", icon = icon("chart-bar"))
    )
  ),

  dashboardBody(
    useShinyjs(),

    tabItems(
      tabItem(
        tabName = "upload",
        h2("Data Upload"),
        p("Module coming soon...")
      ),

      tabItem(
        tabName = "process",
        h2("Processing"),
        p("Module coming soon...")
      ),

      tabItem(
        tabName = "results",
        h2("Results"),
        p("Module coming soon...")
      )
    )
  )
)

# Server ----
server <- function(input, output, session) {

  # Server logic coming soon

}

# Run ----
shinyApp(ui, server)
