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

    # ---- Shared helper: convert one tagged_text string into token/tag list ----
    tagged_text_to_tokens <- function(text) {
      text   <- str_replace_all(text, "(\\S+)\\s+(<)", "\\1\\2")
      tokens <- str_split(text, "\\s+")[[1]]
      tokens <- tokens[tokens != ""]
      str_replace(tokens, "^(.+?)_([A-Z\\$\\.].*)$", "\\1_{{\\2}}")
    }

    # ---- Helper: get vertical lines from in-memory results_data ----
    memory_lines <- reactive({
      proc <- processing_module()
      if (is.null(proc) || !proc$is_complete) return(NULL)
      rd <- proc$processed_data
      if (!"tagged_text" %in% names(rd)) return(NULL)
      has_meta <- "metadata" %in% names(rd)
      lapply(seq_len(nrow(rd)), function(i) {
        list(
          file_id = rd$doc_id[i],
          meta    = if (has_meta) rd$metadata[i] else NA_character_,
          lines   = tagged_text_to_tokens(rd$tagged_text[i])
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
          meta    = NA_character_,
          lines   = raw
        )
      })
    })

    # ---- Helper: get vertical lines from uploaded tagged CSV ----
    csv_lines <- reactive({
      req(input$tagged_csv)

      df <- tryCatch(
        readr::read_csv(input$tagged_csv$datapath, show_col_types = FALSE),
        error = function(e) NULL
      )

      validate(need(!is.null(df), "Could not read CSV file."))
      validate(need("doc_id" %in% names(df), "CSV must have a 'doc_id' column."))
      validate(need("tagged_text" %in% names(df), "CSV must have a 'tagged_text' column."))

      has_meta <- "metadata" %in% names(df)

      lapply(seq_len(nrow(df)), function(i) {
        raw_lines <- str_split(df$tagged_text[i], "\\r?\\n")[[1]]
        raw_lines <- raw_lines[raw_lines != ""]
        list(
          file_id = as.character(df$doc_id[i]),
          meta    = if (has_meta) as.character(df$metadata[i]) else NA_character_,
          lines   = raw_lines
        )
      })
    })


    # ---- Active corpus ----
    active_corpus <- reactive({
      switch(input$data_source,
             memory = memory_lines(),
             upload = upload_lines(),
             csv    = csv_lines())
    })

    # ---- Regex escape helper ----
    regex_escape <- function(x) {
      str_replace_all(x, "([.\\^$*+?()\\[\\]{}|\\\\])", "\\\\\\1")
    }

    # ---- Context boundary helpers ----
    get_left_context <- function(tkns, pos, window, boundary = ">>") {
      if (pos <= 1) return(boundary)
      from <- max(1, pos - window)
      to   <- pos - 1
      toks <- tkns[from:to]
      toks <- toks[!is.na(toks)]
      if (length(toks) == 0) return(boundary)
      paste(toks, collapse = " ")
    }

    get_right_context <- function(tkns, node_end, window, n, boundary = "<<") {
      if (node_end >= n) return(boundary)
      from <- node_end + 1
      to   <- min(n, node_end + window)
      toks <- tkns[from:to]
      toks <- toks[!is.na(toks)]
      if (length(toks) == 0) return(boundary)
      paste(toks, collapse = " ")
    }

    # ---- Shannon's normalized entropy (0-1 scale, 1 = perfectly even distribution) ----
    shannon_entropy_normalized <- function(counts) {
      counts <- counts[counts > 0]
      n <- length(counts)
      if (n <= 1) return(NA_real_)
      p <- counts / sum(counts)
      h <- -sum(p * log(p))
      round(h / log(n), 3)
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
          left_str  <- get_left_context(tkns, pos, window)
          node_str  <- paste(tkns[pos:node_end], collapse = " ")
          right_str <- get_right_context(tkns, node_end, window, n)
          tibble(file_id = doc$file_id, meta = doc$meta, pos = pos,
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
              obs_pos <- str_extract(obs, "(?<=\\{\\{)[^<}]+")
              pat_pos <- str_extract(pat, "(?<=\\{\\{)[^<}]+")
              !is.na(obs_pos) && !is.na(pat_pos) && obs_pos == pat_pos
            }
          }))
        })

        if (length(hit_positions) == 0) return(tibble())

        map_dfr(hit_positions, function(pos) {
          node_end  <- pos + n_bundle - 1
          left_str  <- get_left_context(tkns, pos, window)
          node_str  <- paste(tkns[pos:node_end][!is.na(tkns[pos:node_end])], collapse = " ")
          right_str <- get_right_context(tkns, node_end, window, n)
          tibble(file_id = doc$file_id, meta = doc$meta, pos = pos,
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
      # TTR and entropy are meaningless for token search (node is always identical)
      overall_ttr     <- if (is_token) NA_real_ else round(total_types / total_hits, 3)
      overall_entropy <- if (is_token) NA_real_ else shannon_entropy_normalized(node_counts$freq)

      results <- results |>
        left_join(node_counts, by = "node") |>
        mutate(ttr = overall_ttr, entropy = overall_entropy)

      set.seed(seed)

      if (mode == "summary") {
        results |>
          group_by(node) |>
          slice_sample(n = 1) |>
          ungroup() |>
          arrange(desc(freq)) |>
          select(file_id, meta, left, node, right, freq, ttr, entropy)

      } else if (mode == "random") {
        idx <- sample(nrow(results), min(lines, nrow(results)))
        results[idx, ] |>
          arrange(desc(freq)) |>
          mutate(section = "random") |>
          select(file_id, meta, left, node, right, freq, ttr, entropy, section)

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
          select(file_id, meta, left, node, right, freq, ttr, entropy, section)
      }
    }

    # ---- Reactive: run KWIC on button click ----
    kwic_raw_results <- eventReactive(input$run_kwic, {
      showNotification("🔍 Running KWIC search...", id = "kwic_running",
                       type = "message", duration = NULL)
      on.exit(removeNotification("kwic_running"), add = TRUE)

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

      raw
    })
    kwic_results <- reactive({
      req(kwic_raw_results())
      lines_val <- if (input$mode == "summary") {
        200
      } else if (input$lines == "all") {
        nrow(kwic_raw_results())
      } else {
        as.numeric(input$lines)
      }
      postprocess_kwic(
        results  = kwic_raw_results(),
        mode     = input$mode,
        lines    = lines_val,
        top_n    = if (!is.null(input$top_n)) input$top_n else 5,
        is_token = input$search_mode == "token"
      )
    })

    # ---- Meta/TTR breakdown ----
    meta_ttr_table <- reactive({
      req(kwic_raw_results())
      raw <- kwic_raw_results()
      if (!"meta" %in% names(raw) || all(is.na(raw$meta))) return(NULL)
      is_token <- input$search_mode == "token"

      raw |>
        filter(!is.na(meta)) |>
        group_by(meta) |>
        summarise(
          hits    = n(),
          types   = n_distinct(node),
          ttr     = if (is_token) NA_real_ else round(types / hits, 3),
          entropy = if (is_token) NA_real_ else shannon_entropy_normalized(table(node)),
          .groups = "drop"
        ) |>
        arrange(desc(hits))
    })

    output$meta_ttr_table <- DT::renderDataTable({
      req(meta_ttr_table())
      DT::datatable(
        meta_ttr_table(), rownames = FALSE,
        colnames = c("Category", "Hits", "Types", "TTR", "Entropy"),
        options = list(pageLength = 10, dom = "tip")
      )
    })

    output$has_meta <- reactive({ !is.null(meta_ttr_table()) })
    outputOptions(output, "has_meta", suspendWhenHidden = FALSE)

    # ---- Summary bar ----
    output$results_summary <- renderUI({
      req(kwic_results())
      res     <- kwic_results()
      is_summ <- input$mode == "summary"
      total   <- if (is_summ) sum(res$freq) else nrow(res)
      types   <- n_distinct(res$node)
      files   <- n_distinct(res$file_id)
      ttr_val     <- if (is.na(res$ttr[1])) "N/A" else res$ttr[1]
      entropy_val <- if (is.na(res$entropy[1])) "N/A" else res$entropy[1]

      div(
        class = "alert alert-info",
        style = "padding: 8px 12px; margin-bottom: 0;",
        HTML(paste0(
          if (is_summ) paste0("<strong>", types, "</strong> types &nbsp;|&nbsp; ")
          else paste0("<strong>", nrow(res), "</strong> lines shown &nbsp;|&nbsp; "),
          "<strong>", total, "</strong> total hits &nbsp;|&nbsp; ",
          "<strong>", files, "</strong> files &nbsp;|&nbsp; ",
          "TTR <strong>", ttr_val, "</strong> &nbsp;|&nbsp; ",
          "Entropy <strong>", entropy_val, "</strong>"
        ))
      )
    })

    # ---- has_results flag ----
    output$has_results <- reactive({
      !is.null(kwic_results()) && nrow(kwic_results()) > 0
    })
    outputOptions(output, "has_results", suspendWhenHidden = FALSE)

    # ---- DT table ----
    # ---- DT table ----
    output$kwic_table <- DT::renderDataTable({
      req(kwic_results())
      res <- kwic_results()

      has_meta_col <- "meta" %in% names(res) && !all(is.na(res$meta))

      display <- res |>
        mutate(file_id = str_replace_all(file_id, "_", " "))

      if (has_meta_col) {
        display   <- display |> select(file_id, meta, left, node, right, freq, ttr)
        col_names <- c("File", "Category", "Left", "Node", "Right", "Freq", "TTR")
        col_defs  <- list(
          list(className = "dt-right",  targets = 2),
          list(className = "dt-center", targets = 3),
          list(className = "dt-left",   targets = 4),
          list(className = "dt-center", targets = c(5, 6))
        )
      } else {
        display   <- display |> select(file_id, left, node, right, freq, ttr)
        col_names <- c("File", "Left", "Node", "Right", "Freq", "TTR")
        col_defs  <- list(
          list(className = "dt-right",  targets = 1),
          list(className = "dt-center", targets = 2),
          list(className = "dt-left",   targets = 3),
          list(className = "dt-center", targets = c(4, 5))
        )
      }

      DT::datatable(
        display,
        rownames = FALSE,
        colnames = col_names,
        options  = list(
          pageLength = if (input$mode == "summary") 50 else 20,
          scrollX    = TRUE,
          ordering   = TRUE,
          dom        = "tip",
          columnDefs = col_defs
        )
      ) |>
        DT::formatStyle("node", fontWeight = "bold", color = "#2c7bb6") |>
        DT::formatStyle("file_id", color = "grey40")
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
    # ---- TTR/Entropy CSV download ----
    output$download_meta_ttr <- downloadHandler(
      filename = function() {
        query <- if (input$search_mode == "token") input$token_query else input$bundle_query
        query <- str_replace_all(trimws(query), "[^A-Za-z0-9_-]", "_")
        paste0("kwic_ttr_entropy_", query, "_", format(Sys.Date(), "%Y%m%d"), ".csv")
      },
      content = function(file) {
        req(meta_ttr_table())
        readr::write_csv(meta_ttr_table(), file)
      },
      contentType = "text/csv"
    )
  })
}
