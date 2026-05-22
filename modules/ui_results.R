# =============================================================================
# modules/ui_results.R
# Results Module UI
# =============================================================================

#' Results UI Module
#'
#' @param id Module namespace ID
#' @return Shiny UI elements
resultsUI <- function(id) {
  ns <- NS(id)

  tagList(
    # Summary boxes
    fluidRow(
      valueBoxOutput(ns("box_texts_processed"), width = 3),
      valueBoxOutput(ns("box_total_words"), width = 3),
      valueBoxOutput(ns("box_categories"), width = 3),
      valueBoxOutput(ns("box_text_types"), width = 3)
    ),

    # Main results tabs
    fluidRow(
      box(
        title = "📊 Analysis Results",
        width = 12,
        status = "primary",
        solidHeader = TRUE,

        tabsetPanel(
          id = ns("results_tabs"),

          # Tab 1: Summary Statistics ----
          tabPanel(
            "Summary Statistics",
            br(),

            # Summary table
            h4("Summary by Category"),
            selectInput(
              ns("summary_group_by"),
              "Group by:",
              choices = c("metadata", "closest_text_type"),
              selected = "metadata"
            ),
            DT::dataTableOutput(ns("summary_table")),

            br(),
            hr(),

            # Heatmap
            h4("Mean Dimension Scores Heatmap"),
            plotOutput(ns("dimension_heatmap"), height = "500px"),
            p(class = "text-muted", style = "margin-top: 10px;",
              "Rows are clustered by similarity. Positive scores (red) vs negative scores (blue).")
          ),

          # Tab 2: Document Dimensions (Overview Table) ----
          tabPanel(
            "Document Dimensions",
            br(),
            div(
              style = "margin-bottom: 10px;",
              downloadButton(ns("download_results"), "Download Table", class = "btn-sm")
            ),
            DT::dataTableOutput(ns("results_table"))
          ),

          # Tab 3: Dimension Scores ----
          tabPanel(
            "Dimension Scores",
            br(),
            fluidRow(
              column(3,
                     radioButtons(
                       ns("dimension_plot_type"),
                       "Plot type:",
                       choices = c(
                         "Boxplots (all dimensions)" = "boxplot",
                         "Single dimension view" = "single"
                       ),
                       selected = "boxplot"
                     )
              ),
              column(3,
                     conditionalPanel(
                       condition = paste0("input['", ns("dimension_plot_type"), "'] == 'boxplot'"),
                       selectInput(
                         ns("plot_color_by"),
                         "Color by:",
                         choices = c("metadata", "closest_text_type"),
                         selected = "metadata"
                       )
                     ),
                     conditionalPanel(
                       condition = paste0("input['", ns("dimension_plot_type"), "'] == 'single'"),
                       selectInput(
                         ns("single_dimension"),
                         "Select dimension:",
                         choices = paste0("Dimension", 1:5),
                         selected = "Dimension1"
                       )
                     )
              ),
              column(3,
                     conditionalPanel(
                       condition = paste0("input['", ns("dimension_plot_type"), "'] == 'single'"),
                       selectInput(
                         ns("single_color_by"),
                         "Group by:",
                         choices = c("metadata", "closest_text_type"),
                         selected = "metadata"
                       )
                     )
              )
            ),
            plotlyOutput(ns("dimension_plot"), height = "500px")
          ),

          # Tab 4: 2D Comparison ----
          tabPanel(
            "2D Comparison",
            br(),
            fluidRow(
              column(3,
                     selectInput(
                       ns("scatter_dim_x"),
                       "X-axis:",
                       choices = paste0("Dimension", 1:5),
                       selected = "Dimension1"
                     )
              ),
              column(3,
                     selectInput(
                       ns("scatter_dim_y"),
                       "Y-axis:",
                       choices = paste0("Dimension", 1:5),
                       selected = "Dimension2"
                     )
              ),
              column(3,
                     selectInput(
                       ns("scatter_color_by"),
                       "Color by:",
                       choices = c("metadata", "closest_text_type"),
                       selected = "metadata"
                     )
              ),
              column(3,
                     checkboxInput(
                       ns("show_ellipse"),
                       "Show confidence ellipses",
                       value = FALSE
                     )
              )
            ),
            plotlyOutput(ns("scatter_plot"), height = "500px")
          ),

          # Tab 5: Biber vs Meta ----
          tabPanel(
            "Biber vs Meta",
            br(),
            h4("📊 Compare Categories to Biber Reference Genres"),

            fluidRow(
              column(6,
                     selectInput(
                       ns("biber_categories_select"),
                       "Select categories to compare (up to 10):",
                       choices = NULL,
                       multiple = TRUE
                     )
              ),
              column(6,
                     radioButtons(
                       ns("biber_category_mode"),
                       "Comparison mode:",
                       choices = c(
                         "Category averages only" = "avg_only",
                         "Category + individual docs" = "with_docs"
                       ),
                       selected = "avg_only"
                     )
              )
            ),

            plotOutput(ns("biber_category_plot"), height = "600px")
          ),

          # Tab 6: Biber vs All Docs (Biber Comparison) ----
          tabPanel(
            "Biber vs All Docs",
            br(),
            fluidRow(
              column(6,
                     selectInput(
                       ns("biber_doc_select"),
                       "Select documents to compare (up to 10):",
                       choices = NULL,
                       multiple = TRUE
                     )
              ),
              column(6,
                     checkboxInput(
                       ns("show_all_biber"),
                       "Show all texts (grid view)",
                       value = FALSE
                     )
              )
            ),
            conditionalPanel(
              condition = paste0("!input['", ns("show_all_biber"), "']"),
              plotOutput(ns("biber_comparison_plot"), height = "600px")
            ),
            conditionalPanel(
              condition = paste0("input['", ns("show_all_biber"), "']"),
              plotOutput(ns("biber_comparison_all_plot"), height = "800px")
            )
          ),

          # Tab 7: Text Types ----
          tabPanel(
            "Text Types",
            br(),
            fluidRow(
              column(6,
                     plotlyOutput(ns("text_type_plot"), height = "400px")
              ),
              column(6,
                     h4("Text Type Descriptions"),
                     div(
                       style = "max-height: 400px; overflow-y: auto; padding: 10px;",
                       tags$dl(
                         tags$dt("Intimate interpersonal interaction"),
                         tags$dd("Private conversations, high involvement, immediate context"),
                         tags$dt("Informational interaction"),
                         tags$dd("Public conversations, informational focus"),
                         tags$dt("Scientific exposition"),
                         tags$dd("Academic prose, technical descriptions, impersonal"),
                         tags$dt("Learned exposition"),
                         tags$dd("Academic prose, abstract concepts, formal"),
                         tags$dt("Imaginative narrative"),
                         tags$dd("Fiction, creative writing, narrative focus"),
                         tags$dt("General narrative exposition"),
                         tags$dd("Non-fiction narrative, reportage style"),
                         tags$dt("Situated reportage"),
                         tags$dd("News reporting, immediate context"),
                         tags$dt("Involved persuasion"),
                         tags$dd("Editorials, persuasive essays, personal stance")
                       )
                     )
              )
            )
          )
        ) # End tabsetPanel
      )   # End box
    ),    # End fluidRow (Main results tabs)

    # Interpretation guide
    fluidRow(
      box(
        title = "📖 Dimension Interpretations",
        width = 12,
        status = "info",
        collapsible = TRUE,
        collapsed = TRUE,

        tags$dl(
          tags$dt("Dimension 1: Involved vs. Informational Production"),
          tags$dd("Positive: Interactive, personal, involved (e.g., conversation)"),
          tags$dd("Negative: Informational, detached, formal (e.g., academic writing)"),

          tags$dt("Dimension 2: Narrative vs. Non-narrative Concerns"),
          tags$dd("Positive: Narrative focus, past tense, third person"),
          tags$dd("Negative: Non-narrative, present tense, descriptive"),

          tags$dt("Dimension 3: Explicit vs. Situation-Dependent Reference"),
          tags$dd("Positive: Explicit elaboration, precise reference"),
          tags$dd("Negative: Situation-dependent, immediate context assumed"),

          tags$dt("Dimension 4: Overt Expression of Persuasion"),
          tags$dd("Positive: Persuasive, argumentative stance"),
          tags$dd("Negative: Neutral, objective presentation"),

          tags$dt("Dimension 5: Abstract vs. Non-abstract Information"),
          tags$dd("Positive: Abstract, technical, formal style"),
          tags$dd("Negative: Concrete, non-technical information")
        )
      )
    )

  )  # End tagList
}    # End function
