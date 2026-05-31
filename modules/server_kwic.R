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
    memory_lines <- reactive({
      proc <- processing_module()
      if (is.null(proc) || !proc$is_complete) return(NULL)
      rd <- proc$processed_data
      if (!"tagged_text" %in% names(rd)) return(NULL)
      lapply(seq_len(nrow(rd)), function(i) {
        text   <- rd$tagged_text[i]
        text   <- str_replace_all(text, "(\\S+)\\s+(<)", "\\1\\2")
        tokens <- str_split(text, "\\s+")[[1]]
        tokens <- tokens[tokens != ""]
        tokens <- str_replace(tokens, "^(.+?)_([A-Z\\$\\.].*)$", "\\1_{{\\2}}")
        list(file_id = rd$doc_id[i], lines = tokens)
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

    # ---- Active corpus ----
    active_corpus <- reactive({
      if (input$data_source == "memory") memory_lines() else upload_lines()
    })

    # ---- Regex escape helper ----
    regex_escape <- function(x) {
      str_replace_all(x, "([.\\^$*+?()\\[\\]{}|\\\\])", "\\\\\\1")
    }

    # ---- Token/phrase KWIC ----
    kwic_token <- function(corpus, query, window, case_sensitive) {
      flag    <- if (case_sensitive) "" else "(?i)"
      words   <- str_split(trimws(query), "\\s+")[[1]]
      # Add word boundaries to prevent partial matches
      pattern <- paste0(flag, "\\b", paste(map_chr(words, regex_escape), collapse = "\\s+"), "\\b")
      n_words <- length(words)

      map_dfr(corpus, function(doc) {
        tkns <- str_extract(doc$lines, "^.+?(?=_)")
        tkns <- ifelse(is.na(tkns), doc$lines, tkns)
        n    <- length(tkns)
        hit_positions <- c()
        for (i in seq_len(n - n_words + 1)) {
          chunk <- paste(tkns[i:(i + n_words - 1)], collapse = " ")
          if (grepl(pattern, chunk, perl = TRUE)) hit_positions <- c(hit_positions, i)
        }
        if (length(hit_positions) == 0) return(tibble())
        map_dfr(hit_positions, function(pos) {
          node_end  <- pos + n_words - 1
          left_str  <- paste(tkns[max(1, pos - window):(pos - 1)], collapse = " ")
          node_str  <- paste(tkns[pos:node_end], collapse = " ")
          right_str <- paste(tkns[(node_end + 1):min(n, node_end + window)], collapse = " ")
          tibble(file_id = doc$file_id, pos = pos,
                 left = left_str, node = node_str, right = right_str)
        })
      })
    }

    # ---- Tag bundle KWIC ----
    kwic_tag <- function(corpus, target_bundle, window) {
      bundle_tags <- str_extract_all(target_bundle, "\\{\\{[^}]+\\}\\}")[[1]]
      n_bundle    <- length(bundle_tags)
      if (n_bundle == 0) stop("No valid {{tag}} patterns found in bundle.")

      map_dfr(corpus, function(doc) {
        raw  <- doc$lines
        tkns <- str_extract(raw, "^.+?(?=_\\{\\{)")
        tags <- str_extract(raw, "\\{\\{.+?\\}\\}")
        if (all(is.na(tags))) return(tibble())
        n <- length(tags)
        if (n < n_bundle) return(tibble())

        hit_positions <- keep(seq_len(n - n_bundle + 1), function(i) {
          wt <- tags[i:(i + n_bundle - 1)]
          all(map2_lgl(wt, bundle_tags, function(obs, pat) {
            if (is.na(obs)) return(FALSE)
            if (str_detect(pat, "<")) {
              obs == pat
            } else {
              obs_pos <- str_extract(obs, "(?<=\\{\\{)[A-Z$\\.]+")
              pat_pos <- str_extract(pat, "(?<=\\{\\{)[A-Z$\\.]+")
              !is.na(obs_pos) && !is.na(pat_pos) && obs_pos == pat_pos
            }
          }))
        })

        if (length(hit_positions) == 0) return(tibble())

        map_dfr(hit_positions, function(pos) {
          node_end  <- pos + n_bundle - 1
          left_str  <- paste(tkns[max(1, pos - window):(pos - 1)][!is.na(tkns[max(1, pos - window):(pos - 1)])], collapse = " ")
          node_str  <- paste(tkns[pos:node_end][!is.na(tkns[pos:node_end])], collapse = " ")
          right_str <- paste(tkns[(node_end + 1):min(n, node_end + window)][!is.na(tkns[(node_end + 1):min(n, node_end + window)])], collapse = " ")
          tibble(file_id = doc$file_id, pos = pos,
                 left = left_str, node = node_str, right = right_str)
        })
      })
    }

    # ---- Post-processing: add freq/TTR, then apply mode ----
    postprocess_kwic <- function(results, mode, lines = 20, top_n = 5,
                                 seed = 42, is_token = FALSE) {
      if (nrow(results) == 0) return(results)

      node_counts <- results |>
        count(node, name = "freq") |>
        arrange(desc(freq))

      total_hits  <- nrow(results)
      total_types <- nrow(node_counts)
      # TTR is meaningless for token search (node is always identical)
      overall_ttr <- if (is_token) NA_real_ else round(total_types / total_hits, 3)

      results <- results |>
        left_join(node_counts, by = "node") |>
        mutate(ttr = overall_ttr)

      set.seed(seed)

      if (mode == "summary") {
        results |>
          group_by(node) |>
          slice_sample(n = 1) |>
          ungroup() |>
          arrange(desc(freq)) |>
          select(node, left, right, freq, ttr, file_id)

      } else if (mode == "random") {
        idx <- sample(nrow(results), min(lines, nrow(results)))
        results[idx, ] |>
          arrange(desc(freq)) |>
          mutate(section = "random") |>
          select(file_id, left, node, right, freq, ttr, section)

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

        bind_rows(top_rows, random_rows) |>
          select(file_id, left, node, right, freq, ttr, section)
      }
    }

    # ---- Reactive: run KWIC on button click ----
    kwic_results <- eventReactive(input$run_kwic, {
      corpus <- active_corpus()
      validate(need(!is.null(corpus) && length(corpus) > 0,
                    "No data available. Please process texts or upload .txt files."))

      is_token <- input$search_mode == "token"

      if (is_token) {
        validate(need(nchar(trimws(input$token_query)) > 0,
                      "Please enter a word or phrase to search for."))
        raw <- kwic_token(
          corpus         = corpus,
          query          = input$token_query,
          window         = input$window,
          case_sensitive = input$token_case
        )
      } else {
        validate(need(nchar(trimws(input$bundle_query)) > 0,
                      "Please enter a tag bundle to search for."))
        raw <- kwic_tag(
          corpus        = corpus,
          target_bundle = input$bundle_query,
          window        = input$window
        )
      }

      validate(need(nrow(raw) > 0, "No hits found. Try a different query or data source."))

      postprocess_kwic(
        results  = raw,
        mode     = input$mode,
        lines    = if (input$mode != "summary") input$lines else 200,
        top_n    = if (!is.null(input$top_n)) input$top_n else 5,
        is_token = is_token
      )
    })

    # ---- Summary bar ----
    output$results_summary <- renderUI({
      req(kwic_results())
      res     <- kwic_results()
      is_summ <- input$mode == "summary"
      total   <- if (is_summ) sum(res$freq) else nrow(res)
      types   <- n_distinct(res$node)
      files   <- n_distinct(res$file_id)
      ttr_val <- if (is.na(res$ttr[1])) "N/A" else res$ttr[1]

      div(
        class = "alert alert-info",
        style = "padding: 8px 12px; margin-bottom: 0;",
        HTML(paste0(
          if (is_summ) paste0("<strong>", types, "</strong> types &nbsp;|&nbsp; ")
          else paste0("<strong>", nrow(res), "</strong> lines shown &nbsp;|&nbsp; "),
          "<strong>", total, "</strong> total hits &nbsp;|&nbsp; ",
          "<strong>", files, "</strong> files &nbsp;|&nbsp; ",
          "TTR <strong>", ttr_val, "</strong>"
        ))
      )
    })

    # ---- has_results flag ----
    output$has_results <- reactive({
      !is.null(kwic_results()) && nrow(kwic_results()) > 0
    })
    outputOptions(output, "has_results", suspendWhenHidden = FALSE)

    # ---- DT table ----
    output$kwic_table <- DT::renderDataTable({
      req(kwic_results())
      res     <- kwic_results()
      is_summ <- input$mode == "summary"

      if (is_summ) {
        display <- res |>
          mutate(file_id = str_replace_all(file_id, "_", " ")) |>
          select(node, left, right, freq, ttr, file_id)

        DT::datatable(
          display,
          rownames = FALSE,
          colnames = c("Node", "Example left", "Example right", "Freq", "TTR", "Example file"),
          options  = list(
            pageLength = 50,
            scrollX    = TRUE,
            ordering   = TRUE,
            dom        = "tip",
            columnDefs = list(
              list(className = "dt-center", targets = 0),
              list(className = "dt-right",  targets = 1),
              list(className = "dt-left",   targets = 2),
              list(className = "dt-center", targets = c(3, 4, 5))
            )
          )
        ) |>
          DT::formatStyle("node", fontWeight = "bold", color = "#2c7bb6") |>
          DT::formatStyle("file_id", color = "grey40")

      } else {
        display <- res |>
          mutate(file_id = str_replace_all(file_id, "_", " ")) |>
          select(file_id, left, node, right, freq, ttr)

        DT::datatable(
          display,
          rownames = FALSE,
          colnames = c("File", "Left", "Node", "Right", "Freq", "TTR"),
          options  = list(
            pageLength = 20,
            scrollX    = TRUE,
            ordering   = TRUE,
            dom        = "tip",
            columnDefs = list(
              list(className = "dt-right",  targets = 1),
              list(className = "dt-center", targets = 2),
              list(className = "dt-left",   targets = 3),
              list(className = "dt-center", targets = c(4, 5))
            )
          )
        ) |>
          DT::formatStyle("node", fontWeight = "bold", color = "#2c7bb6") |>
          DT::formatStyle("file_id", color = "grey40")
      }
    })

    # ---- CSV download ----
    output$download_csv <- downloadHandler(
      filename = function() {
        query <- if (input$search_mode == "token") input$token_query else input$bundle_query
        query <- str_replace_all(trimws(query), "[^A-Za-z0-9_-]", "_")
        paste0("kwic_", input$mode, "_", query, "_", format(Sys.Date(), "%Y%m%d"), ".csv")
      },
      content = function(file) {
        req(kwic_results())
        readr::write_csv(kwic_results(), file)
      },
      contentType = "text/csv"
    )

  })
}
