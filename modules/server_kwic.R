# =============================================================================
# modules/server_kwic.R
# KWIC Module Server
# =============================================================================

#' KWIC Server Module
#'
#' @param id Module namespace ID
#' @param processing_module Reactive from processing module
#' @return NULL
kwicServer <- function(id, processing_module) {
  moduleServer(id, function(input, output, session) {

    # ---- Helper: get vertical lines from in-memory results_data ----
    # Converts the tagged_text column (inline format) to vertical lines
    # so both data sources share the same search functions downstream.
    memory_lines <- reactive({
      proc <- processing_module()
      if (is.null(proc) || !proc$is_complete) return(NULL)
      rd <- proc$processed_data
      if (!"tagged_text" %in% names(rd)) return(NULL)

      # Build a named list: doc_id -> character vector of vertical lines
      lapply(seq_len(nrow(rd)), function(i) {
        text <- rd$tagged_text[i]
        # Convert inline "word_TAG<MDA> ..." to one-token-per-line
        # First collapse any space between POS and MDA tag
        text <- str_replace_all(text, "(\\S+)\\s+(<)", "\\1\\2")
        # Split on whitespace to get tokens
        tokens <- str_split(text, "\\s+")[[1]]
        tokens <- tokens[tokens != ""]
        list(
          file_id = rd$doc_id[i],
          lines   = tokens
        )
      })
    })

    # ---- Helper: get vertical lines from uploaded .txt files ----
    upload_lines <- reactive({
      req(input$txt_files)
      lapply(seq_len(nrow(input$txt_files)), function(i) {
        raw <- readLines(input$txt_files$datapath[i], warn = FALSE)
        raw <- raw[raw != ""]
        list(
          file_id = tools::file_path_sans_ext(input$txt_files$name[i]),
          lines   = raw
        )
      })
    })

    # ---- Active corpus (list of file_id + lines) ----
    active_corpus <- reactive({
      if (input$data_source == "memory") {
        memory_lines()
      } else {
        upload_lines()
      }
    })

    # ---- Token/phrase KWIC ----
    kwic_token <- function(corpus, query, window, lines, mode, top_n,
                           case_sensitive, seed = 42) {

      flag <- if (case_sensitive) "" else "(?i)"
      # Build regex: escape the query and allow flexible internal whitespace
      words   <- str_split(trimws(query), "\\s+")[[1]]
      pattern <- paste0(flag, paste(map_chr(words, regex_escape), collapse = "\\s+"))

      results <- map_dfr(corpus, function(doc) {
        tkns <- str_extract(doc$lines, "^.+?(?=_)")
        # Replace NA (lines without underscore) with raw line
        tkns <- ifelse(is.na(tkns), doc$lines, tkns)

        n <- length(tkns)
        sentence <- paste(tkns, collapse = " ")

        # Find all match positions in the token sequence
        hit_positions <- c()
        # Slide word-by-word
        for (i in seq_len(n - length(words) + 1)) {
          chunk <- paste(tkns[i:(i + length(words) - 1)], collapse = " ")
          if (grepl(pattern, chunk, perl = TRUE)) {
            hit_positions <- c(hit_positions, i)
          }
        }

        if (length(hit_positions) == 0) return(tibble())

        map_dfr(hit_positions, function(pos) {
          left_start  <- max(1, pos - window)
          left_str    <- paste(tkns[left_start:(pos - 1)], collapse = " ")
          node_end    <- pos + length(words) - 1
          node_str    <- paste(tkns[pos:node_end], collapse = " ")
          right_end   <- min(n, node_end + window)
          right_str   <- paste(tkns[(node_end + 1):right_end], collapse = " ")
          tibble(
            file_id = doc$file_id,
            pos     = pos,
            left    = left_str,
            node    = node_str,
            right   = right_str
          )
        })
      })

      results
    }

    # ---- Tag bundle KWIC (adapted from kwic_bundle) ----
    kwic_tag <- function(corpus, target_bundle, window, lines, mode, top_n,
                         seed = 42) {

      bundle_tags <- str_extract_all(target_bundle, "\\{\\{[^}]+\\}\\}")[[1]]
      n_bundle    <- length(bundle_tags)
      if (n_bundle == 0) stop("No valid {{tag}} patterns found in bundle.")

      results <- map_dfr(corpus, function(doc) {
        raw   <- doc$lines
        tkns  <- str_extract(raw, "^.+?(?=_\\{\\{)")
        tags  <- str_extract(raw, "\\{\\{.+?\\}\\}")
        if (all(is.na(tags))) return(tibble())
        n <- length(tags)
        if (n < n_bundle) return(tibble())

        hit_positions <- keep(
          seq_len(n - n_bundle + 1),
          function(i) {
            wt <- tags[i:(i + n_bundle - 1)]
            all(map2_lgl(wt, bundle_tags, function(obs, pat) {
              if (is.na(obs)) return(FALSE)
              # If pattern contains MDA tags (has <), match full tag exactly
              # If pattern is POS only, match just the POS portion of observed
              if (str_detect(pat, "<")) {
                obs == pat
              } else {
                obs_pos <- str_extract(obs, "(?<=\\{\\{)[A-Z$\\.]+")
                pat_pos <- str_extract(pat, "(?<=\\{\\{)[A-Z$\\.]+")
                !is.na(obs_pos) && !is.na(pat_pos) && obs_pos == pat_pos
              }
            }))
          }
        )
        if (length(hit_positions) == 0) return(tibble())

        map_dfr(hit_positions, function(pos) {
          left_start  <- max(1, pos - window)
          left_str    <- paste(tkns[left_start:(pos - 1)][!is.na(tkns[left_start:(pos - 1)])], collapse = " ")
          node_end    <- pos + n_bundle - 1
          node_str    <- paste(tkns[pos:node_end][!is.na(tkns[pos:node_end])], collapse = " ")
          right_end   <- min(n, node_end + window)
          right_str   <- paste(tkns[(node_end + 1):right_end][!is.na(tkns[(node_end + 1):right_end])], collapse = " ")
          tibble(
            file_id = doc$file_id,
            pos     = pos,
            left    = left_str,
            node    = node_str,
            right   = right_str
          )
        })
      })

      results
    }

    # ---- Shared post-processing (freq, TTR, sampling) ----
    postprocess_kwic <- function(results, lines, mode, top_n, seed = 42) {
      if (nrow(results) == 0) return(results)

      node_counts <- results |>
        count(node, name = "freq") |>
        arrange(desc(freq))

      total_hits  <- nrow(results)
      total_types <- nrow(node_counts)
      overall_ttr <- round(total_types / total_hits, 3)

      results <- results |>
        left_join(node_counts, by = "node") |>
        mutate(ttr = overall_ttr)

      set.seed(seed)

      if (mode == "random") {
        idx <- sample(nrow(results), min(lines, nrow(results)))
        results[idx, ] |> arrange(desc(freq)) |> mutate(section = "random")
      } else {
        top_n_actual      <- min(top_n, nrow(node_counts))
        top_nodes         <- node_counts |> slice_head(n = top_n_actual) |> pull(node)
        examples_per_node <- floor(lines / top_n_actual)
        extra             <- lines - examples_per_node * top_n_actual

        top_rows <- map_dfr(seq_along(top_nodes), function(i) {
          nd        <- top_nodes[i]
          n_to_take <- if (i == 1) examples_per_node + extra else examples_per_node
          n_to_take <- min(n_to_take, sum(results$node == nd))
          if (n_to_take == 0L) return(tibble())
          pool <- results[results$node == nd, ]
          pool[sample(nrow(pool), n_to_take), ] |> mutate(section = "top")
        })

        remaining_needed <- lines - nrow(top_rows)
        random_rows <- if (remaining_needed > 0) {
          pool <- results[!results$node %in% top_nodes, ]
          if (nrow(pool) > 0) {
            idx <- sample(nrow(pool), min(remaining_needed, nrow(pool)))
            pool[idx, ] |> mutate(section = "random")
          } else tibble()
        } else tibble()

        bind_rows(top_rows, random_rows)
      }
    }

    # ---- Regex escape helper ----
    regex_escape <- function(x) {
      str_replace_all(x, "([.\\^$*+?()\\[\\]{}|\\\\])", "\\\\\\1")
    }

    # ---- Reactive: run KWIC on button click ----
    kwic_results <- eventReactive(input$run_kwic, {
      corpus <- active_corpus()
      validate(need(!is.null(corpus) && length(corpus) > 0,
                    "No data available. Please process texts or upload .txt files."))

      if (input$search_mode == "token") {
        validate(need(nchar(trimws(input$token_query)) > 0,
                      "Please enter a word or phrase to search for."))
        raw <- kwic_token(
          corpus         = corpus,
          query          = input$token_query,
          window         = input$window,
          lines          = input$lines,
          mode           = input$mode,
          top_n          = input$top_n,
          case_sensitive = input$token_case
        )
      } else {
        validate(need(nchar(trimws(input$bundle_query)) > 0,
                      "Please enter a tag bundle to search for."))
        raw <- kwic_tag(
          corpus        = corpus,
          target_bundle = input$bundle_query,
          window        = input$window,
          lines         = input$lines,
          mode          = input$mode,
          top_n         = input$top_n
        )
      }

      validate(need(nrow(raw) > 0, "No hits found. Try a different query or data source."))

      postprocess_kwic(raw, input$lines, input$mode, input$top_n)
    })

    # ---- Summary bar ----
    output$results_summary <- renderUI({
      req(kwic_results())
      res <- kwic_results()
      total   <- nrow(res)
      types   <- n_distinct(res$node)
      files   <- n_distinct(res$file_id)
      ttr_val <- round(types / nrow(res), 3)
      div(
        class = "alert alert-info",
        style = "padding: 8px 12px; margin-bottom: 0;",
        HTML(paste0(
          "<strong>", total, "</strong> lines shown &nbsp;|&nbsp; ",
          "<strong>", types, "</strong> node types &nbsp;|&nbsp; ",
          "<strong>", files, "</strong> files &nbsp;|&nbsp; ",
          "TTR <strong>", ttr_val, "</strong>"
        ))
      )
    })

    # ---- has_results flag for conditional panel ----
    output$has_results <- reactive({ !is.null(kwic_results()) && nrow(kwic_results()) > 0 })
    outputOptions(output, "has_results", suspendWhenHidden = FALSE)

    # ---- DT table ----
    output$kwic_table <- DT::renderDataTable({
      req(kwic_results())
      res <- kwic_results() |>
        mutate(file_id = str_replace_all(file_id, "_", " ")) |>
        select(file_id, left, node, right, freq, ttr)

      DT::datatable(
        res,
        rownames  = FALSE,
        colnames  = c("File", "Left", "Node", "Right", "Freq", "TTR"),
        options   = list(
          pageLength  = 20,
          scrollX     = TRUE,
          ordering    = TRUE,
          dom         = "tip",
          columnDefs  = list(
            list(className = "dt-right",  targets = 1),  # Left context
            list(className = "dt-center", targets = 2),  # Node
            list(className = "dt-left",   targets = 3),  # Right context
            list(className = "dt-center", targets = c(4, 5))
          )
        )
      ) |>
        DT::formatStyle(
          "node",
          fontWeight = "bold",
          color      = "#2c7bb6"
        ) |>
        DT::formatStyle(
          "file_id",
          color = "grey40"
        )
    })

    # ---- CSV download ----
    output$download_csv <- downloadHandler(
      filename = function() {
        query <- if (input$search_mode == "token") input$token_query else input$bundle_query
        query <- str_replace_all(trimws(query), "[^A-Za-z0-9_-]", "_")
        paste0("kwic_", query, "_", format(Sys.Date(), "%Y%m%d"), ".csv")
      },
      content = function(file) {
        req(kwic_results())
        kwic_results() |>
          select(file_id, left, node, right, freq, ttr, section) |>
          readr::write_csv(file)
      },
      contentType = "text/csv"
    )

  })
}
