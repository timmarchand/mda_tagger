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

          # Tab 1: Overview Table
          tabPanel(
            "Overview Table",
            br(),
            div(
              style = "margin-bottom: 10px;",
              downloadButton(ns("download_results"), "Download Table", class = "btn-sm")
            ),
            DT::dataTableOutput(ns("results_table"))
          ),

          # Tab 2: Dimension Plots
          tabPanel(
            "Dimension Scores",
            br(),
            fluidRow(
              column(3,
                     selectInput(
                       ns("plot_color_by"),
                       "Color by:",
                       choices = c("metadata", "closest_text_type"),
                       selected = "metadata"
                     )
              ),
              column(3,
                     checkboxInput(
                       ns("show_individual_lines"),
                       "Show individual lines",
                       value = TRUE
                     )
              )
            ),
            plotlyOutput(ns("dimension_plot"), height = "500px")
          ),

          # Tab 3: Text Types
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
          ),

          # Tab 4: Scatter Plots
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
              )
            ),
            plotlyOutput(ns("scatter_plot"), height = "500px")
          ),

          # Tab 5: Summary Statistics
          tabPanel(
            "Summary Statistics",
            br(),
            selectInput(
              ns("summary_group_by"),
              "Group by:",
              choices = c("metadata", "closest_text_type"),
              selected = "metadata"
            ),
            DT::dataTableOutput(ns("summary_table"))
          ),

          # Tab 6: Biber Comparison
          tabPanel(
            "Biber Comparison",
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

          # Tab 7: By Category (Aggregated)
          tabPanel(
            "By Category",
            br(),
            h4("📊 Aggregated Results by Metadata Category"),

            # Category Summary Table
            fluidRow(
              column(12,
                     h5("Category Summary Table"),
                     DT::dataTableOutput(ns("aggregated_table")),
                     br()
              )
            ),

            hr(),

            # Average Dimension Scores
            h5("Average Dimension Scores by Category"),
            fluidRow(
              column(9,
                     plotlyOutput(ns("aggregated_dimension_plot"), height = "450px")
              ),
              column(3,
                     selectInput(
                       ns("categories_to_compare"),
                       "Select categories:",
                       choices = NULL,
                       multiple = TRUE
                     ),
                     checkboxInput(
                       ns("show_individual_docs"),
                       "Show individual documents",
                       value = FALSE
                     )
              )
            ),

            hr(),

            # Biber Comparison Section
            h5("Compare Categories to Biber Reference Genres"),
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

            fluidRow(
              column(12,
                     plotOutput(ns("biber_category_plot"), height = "600px")
              )
            )
          )

        )  # End tabsetPanel
      )    # End box
    ),     # End fluidRow (Main results tabs)

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
