# =============================================================================
# modules/tag_guide.R
# Tag Guide Reference Panel Module
# =============================================================================

#' Tag Guide UI
#'
#' @param id Module namespace ID
#' @return UI elements for the tag guide toggle panel
tagGuideUI <- function(id) {
  ns <- NS(id)

  tagList(
    actionButton(ns("toggle_tag_guide"), "📖 Show Tag Guide", class = "btn-outline-primary btn-sm"),
    br(), br(),
    shinyjs::hidden(
      div(id = ns("tag_guide_panel"),
          selectInput(ns("dimension_filter"), "Filter by dimension:",
                      choices = NULL, selected = "All"),
          p(style = "font-size: 12px; color: #888; margin-top: -5px;",
            "Click any row to copy its tag into the KWIC search box."),
          DT::DTOutput(ns("tag_guide_table"))
      )
    )
  )
}

#' Tag Guide Server
#'
#' @param id Module namespace ID
#' @param biber_base Reference tibble with columns: dimension, feature, detail,
#'   biber_mean, biber_sd, loading
#' @return A reactive giving the feature code of the last-clicked row (or NULL
#'   if nothing has been clicked yet)
tagGuideServer <- function(id, biber_base) {
  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    # Populate dimension filter choices once biber_base is available ----
    observe({
      updateSelectInput(session, "dimension_filter",
                        choices = c("All", unique(biber_base$dimension)),
                        selected = "All")
    })

    # Toggle panel visibility ----
    observeEvent(input$toggle_tag_guide, {
      shinyjs::toggle("tag_guide_panel")
      updateActionButton(
        session, "toggle_tag_guide",
        label = if (input$toggle_tag_guide %% 2 == 1) "📖 Hide Tag Guide" else "📖 Show Tag Guide"
      )
    })

    # Filtered reference data ----
    guide_data <- reactive({
      data <- biber_base
      if (!is.null(input$dimension_filter) && input$dimension_filter != "All") {
        data <- data %>% filter(dimension == input$dimension_filter)
      }
      data %>%
        mutate(Direction = case_when(
          loading == 1  ~ "▲ Positive",
          loading == -1 ~ "▼ Negative",
          TRUE          ~ "—"
        )) %>%
        select(Dimension = dimension, Tag = feature, Description = detail, Direction)
    })

    # Render table ----
    output$tag_guide_table <- DT::renderDT({
      guide_data() %>%
        DT::datatable(
          options = list(pageLength = 15, dom = "ftip"),
          rownames = FALSE,
          selection = "none"
        ) %>%
        DT::formatStyle(
          "Direction",
          color = DT::styleEqual(
            c("▲ Positive", "▼ Negative", "—"),
            c("#2c7bb6", "#d7191c", "#999")
          )
        )
    })

    # Capture cell clicks and expose the clicked tag ----
    clicked_tag <- reactiveVal(NULL)

    observeEvent(input$tag_guide_table_cell_clicked, {
      info <- input$tag_guide_table_cell_clicked
      req(info$value, nrow(guide_data()) > 0)

      # Only respond to clicks in the "Tag" column (column index 1, 0-based
      # since Dimension=0, Tag=1, Description=2, Direction=3)
      if (info$col == 1) {
        clicked_tag(info$value)
      }
    })

    # Module Returns ----
    return(list(
      clicked_tag = clicked_tag
    ))
  })
}
