# =============================================================================
# R/04_visualization.R
# Visualization functions for MDA results
# =============================================================================

library(ggplot2)
library(plotly)
library(dplyr)
library(tidyr)

#' Plot dimension scores
#'
#' @param results_data Tibble with dimension scores
#' @param color_by Column to color by (default: "metadata")
#' @param interactive Return interactive plotly plot (default: TRUE)
#' @return ggplot2 or plotly object
#' @export
plot_dimensions <- function(results_data, color_by = "metadata", interactive = TRUE) {

  # Reshape to long format
  plot_data <- results_data %>%
    select(doc_id, metadata, Dimension1, Dimension2, Dimension3, Dimension4, Dimension5) %>%
    pivot_longer(
      cols = starts_with("Dimension"),
      names_to = "dimension",
      values_to = "score"
    ) %>%
    mutate(dimension = gsub("Dimension", "D", dimension))

  # Create plot
  p <- ggplot(plot_data, aes(x = dimension, y = score, color = .data[[color_by]], group = doc_id)) +
    geom_line(alpha = 0.6) +
    geom_point(size = 2) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    theme_minimal() +
    labs(
      title = "Multi-Dimensional Analysis Scores",
      x = "Dimension",
      y = "Score",
      color = str_to_title(color_by)
    ) +
    theme(
      plot.title = element_text(face = "bold", size = 14),
      legend.position = "right"
    )

  if (interactive) {
    p <- ggplotly(p, tooltip = c("doc_id", "dimension", "score"))
  }

  return(p)
}


#' Plot dimension distributions
#'
#' @param results_data Tibble with dimension scores
#' @param dimension Which dimension to plot (default: "Dimension1")
#' @param group_by Column to group by (default: "metadata")
#' @param interactive Return interactive plotly plot (default: TRUE)
#' @return ggplot2 or plotly object
#' @export
plot_dimension_distribution <- function(results_data,
                                        dimension = "Dimension1",
                                        group_by = "metadata",
                                        interactive = TRUE) {

  p <- ggplot(results_data, aes(x = .data[[dimension]], fill = .data[[group_by]])) +
    geom_density(alpha = 0.6) +
    theme_minimal() +
    labs(
      title = paste(dimension, "Distribution"),
      x = "Score",
      y = "Density",
      fill = str_to_title(group_by)
    ) +
    theme(plot.title = element_text(face = "bold", size = 14))

  if (interactive) {
    p <- ggplotly(p)
  }

  return(p)
}


#' Plot text type classification
#'
#' @param results_data Tibble with closest_text_type column
#' @param interactive Return interactive plotly plot (default: TRUE)
#' @return ggplot2 or plotly object
#' @export
plot_text_types <- function(results_data, interactive = TRUE) {

  # Count texts by type
  type_counts <- results_data %>%
    count(closest_text_type, name = "n_texts") %>%
    arrange(desc(n_texts))

  p <- ggplot(type_counts, aes(x = reorder(closest_text_type, n_texts), y = n_texts)) +
    geom_col(fill = "steelblue", alpha = 0.8) +
    coord_flip() +
    theme_minimal() +
    labs(
      title = "Text Type Classification",
      x = NULL,
      y = "Number of Texts"
    ) +
    theme(
      plot.title = element_text(face = "bold", size = 14),
      axis.text.y = element_text(size = 10)
    )

  if (interactive) {
    p <- ggplotly(p)
  }

  return(p)
}


#' Create 2D scatter plot of two dimensions
#'
#' @param results_data Tibble with dimension scores
#' @param dim_x X-axis dimension (default: "Dimension1")
#' @param dim_y Y-axis dimension (default: "Dimension2")
#' @param color_by Column to color by (default: "metadata")
#' @param label_by Column to label points (default: "doc_id")
#' @param interactive Return interactive plotly plot (default: TRUE)
#' @return ggplot2 or plotly object
#' @export
plot_dimension_scatter <- function(results_data,
                                   dim_x = "Dimension1",
                                   dim_y = "Dimension2",
                                   color_by = "metadata",
                                   label_by = "doc_id",
                                   interactive = TRUE) {

  p <- ggplot(results_data, aes(x = .data[[dim_x]], y = .data[[dim_y]],
                                color = .data[[color_by]],
                                text = .data[[label_by]])) +
    geom_point(size = 3, alpha = 0.7) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
    theme_minimal() +
    labs(
      title = paste(dim_x, "vs", dim_y),
      x = dim_x,
      y = dim_y,
      color = str_to_title(color_by)
    ) +
    theme(plot.title = element_text(face = "bold", size = 14))

  if (interactive) {
    p <- ggplotly(p, tooltip = c("text", "x", "y", "colour"))
  }

  return(p)
}


#' Create summary statistics table
#'
#' @param results_data Tibble with dimension scores
#' @param group_by Column to group by (default: "metadata")
#' @return Tibble with summary statistics
#' @export
summarize_dimensions <- function(results_data, group_by = "metadata") {

  results_data %>%
    group_by(.data[[group_by]]) %>%
    summarise(
      n_texts = n(),
      D1_mean = round(mean(Dimension1, na.rm = TRUE), 2),
      D1_sd = round(sd(Dimension1, na.rm = TRUE), 2),
      D2_mean = round(mean(Dimension2, na.rm = TRUE), 2),
      D2_sd = round(sd(Dimension2, na.rm = TRUE), 2),
      D3_mean = round(mean(Dimension3, na.rm = TRUE), 2),
      D3_sd = round(sd(Dimension3, na.rm = TRUE), 2),
      D4_mean = round(mean(Dimension4, na.rm = TRUE), 2),
      D4_sd = round(sd(Dimension4, na.rm = TRUE), 2),
      D5_mean = round(mean(Dimension5, na.rm = TRUE), 2),
      D5_sd = round(sd(Dimension5, na.rm = TRUE), 2),
      .groups = "drop"
    )
}


#' Create Biber reference data for comparison plots
#'
#' @return Tibble with Biber's 8 text types and their dimension scores
#' @export
get_biber_reference <- function() {
  tibble(
    genre = c(
      "Intimate interpersonal interaction",
      "Informational interaction",
      "Scientific exposition",
      "Learned exposition",
      "Imaginative narrative",
      "General narrative exposition",
      "Situated reportage",
      "Involved persuasion"
    ),
    Dimension1 = c(45, 30, -15, -20, 5, -10, 0, 5),
    Dimension2 = c(-1, -1, -2.5, -2, 7, 2, -3, -2),
    Dimension3 = c(-6, -4, 4, 5, -4, 0, -13, 2),
    Dimension4 = c(1, 1, -2, -3, 1, -1, -4.5, -4),
    Dimension5 = c(-4, -3, 9, 2, -2, 0, -3, -1)
  ) %>%
    pivot_longer(
      cols = starts_with("Dimension"),
      names_to = "dimension",
      values_to = "value"
    ) %>%
    mutate(corpus = "Biber")
}


#' Plot individual text vs Biber reference genres
#'
#' @param results_data Tibble with dimension scores
#' @param doc_id_selected Which document to compare (default: first doc)
#' @return ggplot object
#' @export
plot_biber_comparison <- function(results_data, doc_id_selected = NULL) {

  # Select document
  if (is.null(doc_id_selected)) {
    doc_id_selected <- results_data$doc_id[1]
  }

  # Get Biber reference data
  biber_ref <- get_biber_reference()

  # Prepare student data
  student_data <- results_data %>%
    filter(doc_id == doc_id_selected) %>%
    select(doc_id, Dimension1, Dimension2, Dimension3, Dimension4, Dimension5) %>%
    pivot_longer(
      cols = starts_with("Dimension"),
      names_to = "dimension",
      values_to = "value"
    ) %>%
    mutate(
      corpus = "Your Text",
      genre = doc_id
    )

  # Combine
  combined <- bind_rows(
    biber_ref %>% select(corpus, genre, dimension, value),
    student_data %>% select(corpus, genre, dimension, value)
  )

  # Create plot
  p <- combined %>%
    ggplot(aes(x = 0, y = value)) +
    geom_text(
      data = filter(combined, corpus == "Biber"),
      aes(label = genre),
      hjust = 1.1,
      size = 2.5,
      color = "gray30"
    ) +
    geom_text(
      data = filter(combined, corpus == "Your Text"),
      aes(label = genre),
      hjust = -0.1,
      size = 3.5,
      color = "#E74C3C",
      fontface = "bold"
    ) +
    geom_vline(xintercept = 0, linewidth = 1, color = "gray50") +
    facet_wrap(~ dimension, scales = "free_y", ncol = 3) +
    labs(
      title = paste("Dimension Scores: Biber Reference Genres vs", doc_id_selected),
      y = "Score",
      x = NULL
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      strip.background = element_rect(fill = "gray70"),
      strip.text = element_text(face = "bold", size = 11)
    )

  return(p)
}


#' Plot all texts vs Biber reference (faceted by text)
#'
#' @param results_data Tibble with dimension scores
#' @param max_texts Maximum number of texts to show (default: 12)
#' @return ggplot object
#' @export
plot_biber_comparison_all <- function(results_data, max_texts = 12) {

  # Limit number of texts
  if (nrow(results_data) > max_texts) {
    results_data <- results_data %>% slice(1:max_texts)
    warning(paste("Showing first", max_texts, "texts only"))
  }

  # Get Biber reference data
  biber_ref <- get_biber_reference()

  # Prepare all student data
  student_data <- results_data %>%
    select(doc_id, Dimension1, Dimension2, Dimension3, Dimension4, Dimension5) %>%
    pivot_longer(
      cols = starts_with("Dimension"),
      names_to = "dimension",
      values_to = "value"
    ) %>%
    mutate(
      corpus = "Your Text",
      genre = doc_id
    )

  # For each document, combine with Biber
  all_plots_data <- student_data %>%
    group_by(doc_id) %>%
    group_split() %>%
    map_dfr(function(doc_data) {
      bind_rows(
        biber_ref %>% mutate(doc_id = doc_data$doc_id[1]),
        doc_data
      )
    })

  # Create faceted plot
  p <- all_plots_data %>%
    ggplot(aes(x = 0, y = value)) +
    geom_text(
      data = filter(all_plots_data, corpus == "Biber"),
      aes(label = genre),
      hjust = 1.1,
      size = 2,
      color = "gray30"
    ) +
    geom_text(
      data = filter(all_plots_data, corpus == "Your Text"),
      aes(label = genre),
      hjust = -0.1,
      size = 2.5,
      color = "#E74C3C",
      fontface = "bold"
    ) +
    geom_vline(xintercept = 0, linewidth = 0.5, color = "gray50") +
    facet_grid(doc_id ~ dimension, scales = "free_y") +
    labs(
      title = "All Texts vs Biber Reference Genres",
      y = "Score",
      x = NULL
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      strip.background = element_rect(fill = "gray70"),
      strip.text = element_text(face = "bold", size = 9)
    )

  return(p)
}


#' Aggregate results by metadata category
#'
#' @param results_data Tibble with dimension scores and metadata
#' @return Tibble with mean dimension scores per metadata category
#' @export
aggregate_by_metadata <- function(results_data) {

  results_data %>%
    group_by(metadata) %>%
    summarise(
      n_texts = n(),
      avg_words = round(mean(n_words, na.rm = TRUE), 0),
      Dimension1 = round(mean(Dimension1, na.rm = TRUE), 2),
      Dimension2 = round(mean(Dimension2, na.rm = TRUE), 2),
      Dimension3 = round(mean(Dimension3, na.rm = TRUE), 2),
      Dimension4 = round(mean(Dimension4, na.rm = TRUE), 2),
      Dimension5 = round(mean(Dimension5, na.rm = TRUE), 2),
      most_common_type = names(sort(table(closest_text_type), decreasing = TRUE))[1],
      .groups = "drop"
    )
}


#' Plot aggregated dimension scores by metadata
#'
#' @param aggregated_data Output from aggregate_by_metadata()
#' @param interactive Return interactive plotly plot (default: TRUE)
#' @return ggplot2 or plotly object
#' @export
plot_aggregated_dimensions <- function(aggregated_data, interactive = TRUE) {

  # Reshape to long format
  plot_data <- aggregated_data %>%
    select(metadata, Dimension1, Dimension2, Dimension3, Dimension4, Dimension5) %>%
    pivot_longer(
      cols = starts_with("Dimension"),
      names_to = "dimension",
      values_to = "score"
    ) %>%
    mutate(dimension = gsub("Dimension", "D", dimension))

  # Create plot
  p <- ggplot(plot_data, aes(x = dimension, y = score, color = metadata, group = metadata)) +
    geom_line(linewidth = 1.2) +
    geom_point(size = 3) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    theme_minimal() +
    labs(
      title = "Average Dimension Scores by Category",
      x = "Dimension",
      y = "Average Score",
      color = "Category"
    ) +
    theme(
      plot.title = element_text(face = "bold", size = 14),
      legend.position = "right"
    )

  if (interactive) {
    p <- ggplotly(p, tooltip = c("metadata", "dimension", "score"))
  }

  return(p)
}


#' Plot Biber comparison for aggregated metadata
#'
#' @param aggregated_data Output from aggregate_by_metadata()
#' @param category_selected Which category to compare (default: first)
#' @return ggplot object
#' @export
plot_biber_comparison_aggregated <- function(aggregated_data, category_selected = NULL) {

  # Select category
  if (is.null(category_selected)) {
    category_selected <- aggregated_data$metadata[1]
  }

  # Get Biber reference data
  biber_ref <- get_biber_reference()

  # Prepare category data
  category_data <- aggregated_data %>%
    filter(metadata == category_selected) %>%
    select(metadata, Dimension1, Dimension2, Dimension3, Dimension4, Dimension5) %>%
    pivot_longer(
      cols = starts_with("Dimension"),
      names_to = "dimension",
      values_to = "value"
    ) %>%
    mutate(
      corpus = "Your Category",
      genre = paste(metadata, "(avg)")
    )

  # Combine
  combined <- bind_rows(
    biber_ref %>% select(corpus, genre, dimension, value),
    category_data %>% select(corpus, genre, dimension, value)
  )

  # Create plot
  p <- combined %>%
    ggplot(aes(x = 0, y = value)) +
    geom_text(
      data = filter(combined, corpus == "Biber"),
      aes(label = genre),
      hjust = 1.1,
      size = 2.5,
      color = "gray30"
    ) +
    geom_text(
      data = filter(combined, corpus == "Your Category"),
      aes(label = genre),
      hjust = -0.1,
      size = 3.5,
      color = "#3498DB",
      fontface = "bold"
    ) +
    geom_vline(xintercept = 0, linewidth = 1, color = "gray50") +
    facet_wrap(~ dimension, scales = "free_y", ncol = 3) +
    labs(
      title = paste("Biber Genres vs", category_selected, "(Average)"),
      y = "Score",
      x = NULL
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      strip.background = element_rect(fill = "gray70"),
      strip.text = element_text(face = "bold", size = 11)
    )

  return(p)

  #' Plot aggregated dimensions with individual documents
  #'
  #' @param aggregated_data Aggregated data by metadata
  #' @param individual_data Full results data with individual docs
  #' @param interactive Return interactive plotly plot
  #' @return ggplot2 or plotly object
  #' @export
  plot_aggregated_dimensions_with_docs <- function(aggregated_data,
                                                   individual_data,
                                                   interactive = TRUE) {

    # Prepare aggregated data (lines)
    agg_long <- aggregated_data %>%
      select(metadata, Dimension1, Dimension2, Dimension3, Dimension4, Dimension5) %>%
      pivot_longer(
        cols = starts_with("Dimension"),
        names_to = "dimension",
        values_to = "score"
      ) %>%
      mutate(
        dimension = gsub("Dimension", "D", dimension),
        type = "average"
      )

    # Prepare individual data (points)
    ind_long <- individual_data %>%
      select(doc_id, metadata, Dimension1, Dimension2, Dimension3, Dimension4, Dimension5) %>%
      pivot_longer(
        cols = starts_with("Dimension"),
        names_to = "dimension",
        values_to = "score"
      ) %>%
      mutate(
        dimension = gsub("Dimension", "D", dimension),
        type = "individual"
      )

    # Create plot
    p <- ggplot() +
      # Individual points
      geom_point(
        data = ind_long,
        aes(x = dimension, y = score, color = metadata, text = doc_id),
        alpha = 0.3,
        size = 2
      ) +
      # Average lines
      geom_line(
        data = agg_long,
        aes(x = dimension, y = score, color = metadata, group = metadata),
        linewidth = 1.5
      ) +
      geom_point(
        data = agg_long,
        aes(x = dimension, y = score, color = metadata),
        size = 4
      ) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
      theme_minimal() +
      labs(
        title = "Dimension Scores: Category Averages with Individual Documents",
        x = "Dimension",
        y = "Score",
        color = "Category"
      ) +
      theme(
        plot.title = element_text(face = "bold", size = 14),
        legend.position = "right"
      )

    if (interactive) {
      p <- ggplotly(p, tooltip = c("metadata", "dimension", "score", "text"))
    }

    return(p)
  }


  #' Plot multiple documents vs Biber (side by side)
  #'
  #' @param results_data Tibble with dimension scores
  #' @param doc_ids_selected Vector of document IDs to compare
  #' @return ggplot object
  #' @export
  plot_biber_comparison_multiple <- function(results_data, doc_ids_selected) {

    # Limit to 10
    doc_ids_selected <- head(doc_ids_selected, 10)

    # Get Biber reference data
    biber_ref <- get_biber_reference()

    # Prepare selected docs data
    docs_data <- results_data %>%
      filter(doc_id %in% doc_ids_selected) %>%
      select(doc_id, Dimension1, Dimension2, Dimension3, Dimension4, Dimension5) %>%
      pivot_longer(
        cols = starts_with("Dimension"),
        names_to = "dimension",
        values_to = "value"
      ) %>%
      mutate(
        corpus = "Your Texts",
        genre = doc_id
      )

    # Replicate Biber for each doc
    all_data <- map_dfr(doc_ids_selected, function(doc) {
      bind_rows(
        biber_ref %>% mutate(doc_id = doc),
        docs_data %>% filter(doc_id == doc)
      )
    })

    # Create plot
    p <- all_data %>%
      ggplot(aes(x = 0, y = value)) +
      geom_text(
        data = filter(all_data, corpus == "Biber"),
        aes(label = genre),
        hjust = 1.1,
        size = 2,
        color = "gray30"
      ) +
      geom_text(
        data = filter(all_data, corpus == "Your Texts"),
        aes(label = genre),
        hjust = -0.1,
        size = 2.5,
        color = "#E74C3C",
        fontface = "bold"
      ) +
      geom_vline(xintercept = 0, linewidth = 0.5, color = "gray50") +
      facet_grid(doc_id ~ dimension, scales = "free_y") +
      labs(
        title = "Selected Documents vs Biber Reference Genres",
        y = "Score",
        x = NULL
      ) +
      theme_minimal() +
      theme(
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        strip.background = element_rect(fill = "gray70"),
        strip.text = element_text(face = "bold", size = 9)
      )

    return(p)
  }


  #' Plot multiple categories vs Biber
  #'
  #' @param aggregated_data Aggregated data by metadata
  #' @param categories_selected Vector of categories to compare
  #' @return ggplot object
  #' @export
  plot_biber_comparison_aggregated_multiple <- function(aggregated_data, categories_selected) {

    # Limit to 10
    categories_selected <- head(categories_selected, 10)

    # Get Biber reference data
    biber_ref <- get_biber_reference()

    # Prepare category data
    cats_data <- aggregated_data %>%
      filter(metadata %in% categories_selected) %>%
      select(metadata, Dimension1, Dimension2, Dimension3, Dimension4, Dimension5) %>%
      pivot_longer(
        cols = starts_with("Dimension"),
        names_to = "dimension",
        values_to = "value"
      ) %>%
      mutate(
        corpus = "Your Categories",
        genre = paste(metadata, "(avg)")
      )

    # Replicate Biber for each category
    all_data <- map_dfr(categories_selected, function(cat) {
      bind_rows(
        biber_ref %>% mutate(category = cat),
        cats_data %>% filter(metadata == cat) %>% mutate(category = cat)
      )
    })

    # Create plot
    p <- all_data %>%
      ggplot(aes(x = 0, y = value)) +
      geom_text(
        data = filter(all_data, corpus == "Biber"),
        aes(label = genre),
        hjust = 1.1,
        size = 2,
        color = "gray30"
      ) +
      geom_text(
        data = filter(all_data, corpus == "Your Categories"),
        aes(label = genre),
        hjust = -0.1,
        size = 2.5,
        color = "#3498DB",
        fontface = "bold"
      ) +
      geom_vline(xintercept = 0, linewidth = 0.5, color = "gray50") +
      facet_grid(category ~ dimension, scales = "free_y") +
      labs(
        title = "Selected Categories (Averages) vs Biber Reference Genres",
        y = "Score",
        x = NULL
      ) +
      theme_minimal() +
      theme(
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        strip.background = element_rect(fill = "gray70"),
        strip.text = element_text(face = "bold", size = 9)
      )

    return(p)
  }
}
