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

    # ---- Category filter: repopulate the dropdown from whatever values
    #      actually exist in the metadata column of the active corpus. This
    #      makes no assumption about what "meta" holds (a corpus label like
    #      HYSOC/JUSOC, a genre, or nothing at all) -- it just reflects
    #      reality, and degrades to a single "All" choice with no effect
    #      when there's no metadata to filter on. ----
    observe({
      corpus <- active_corpus()
      metas  <- if (is.null(corpus) || length(corpus) == 0) {
        character(0)
      } else {
        vapply(corpus, function(d) {
          if (is.null(d$meta) || is.na(d$meta)) "" else as.character(d$meta)
        }, character(1))
      }
      cats <- sort(unique(metas[metas != ""]))
      choices <- c("All" = "all", setNames(cats, cats))
      updateSelectInput(session, "category_filter", choices = choices, selected = "all")
    })

    # ---- Restrict a corpus list to documents whose meta matches the
    #      selected category. "all" (or anything falsy) is a no-op. Shared
    #      by both the main KWIC search and the Tag Inspector below, so a
    #      single dropdown scopes both to one corpus/category at a time. ----
    apply_category_filter <- function(corpus, category) {
      if (is.null(corpus) || length(corpus) == 0) return(corpus)
      if (is.null(category) || category == "all") return(corpus)
      Filter(function(d) {
        !is.null(d$meta) && !is.na(d$meta) && as.character(d$meta) == category
      }, corpus)
    }

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

    # =========================================================================
    # Tag Inspector: look up a word/phrase and show it aligned with its tags
    # =========================================================================

    # ---- Wrap a raw tag in the {{ }} bundle format (or a placeholder if untagged) ----
    wrap_tag <- function(raw_tag) {
      paste0("{{", ifelse(nchar(raw_tag) == 0, "—", raw_tag), "}}")
    }

    # ---- Recall-proxy helper: does a hit's tag window carry the expected
    #      pattern tag anywhere in it? Used to auto-flag each Tag Inspector
    #      hit as a match or a (candidate) recall miss, per the "any
    #      occurrence not tagged as the pattern" rule -- a fixed substring
    #      check against each token's raw tag, since Biber tags stack
    #      (e.g. "CC<PHC><CONJ>") and the target may be one of several. ----
    tag_hits_expected <- function(raw_tags, expected_tag) {
      if (length(raw_tags) == 0) return(FALSE)
      expected_tag <- trimws(expected_tag)
      if (!nzchar(expected_tag)) return(FALSE)

      # The tag line under each Tag Inspector hit shows one {{TAG}} per
      # token, so for a multi-word query like "I think" it reads
      # "{{PRP<FPP1>}} {{VBP<PRIV><VPRT>}}" -- and that whole line is
      # exactly what a user will naturally copy and paste in here, not just
      # a single tag. Pull out every {{...}} group present; if there isn't
      # one (a bare tag like "PRIV"), treat the whole string as one target.
      targets <- regmatches(expected_tag, gregexpr("\\{\\{[^}]+\\}\\}", expected_tag))[[1]]
      targets <- if (length(targets) > 0) gsub("^\\{\\{|\\}\\}$", "", targets) else expected_tag

      # Require EVERY pasted target tag to appear somewhere in the hit's
      # tag window, not just any one of them -- this is what makes pasting
      # the full multi-tag line a genuine check ("was this whole sequence
      # reproduced for this occurrence too?") rather than a trivially-true
      # one (which an OR over the same tags the line came from would be).
      all(vapply(targets, function(target) {
        any(vapply(raw_tags, function(t) grepl(target, t, fixed = TRUE), logical(1)))
      }, logical(1)))
    }

    # ---- Find occurrences of a query n-gram and pair each token with its tag ----
    # max_examples = Inf shows every occurrence found ("All")
    tag_inspect <- function(corpus, query, window, case_sensitive, max_examples = Inf) {
      flag    <- if (case_sensitive) "" else "(?i)"
      words   <- str_split(trimws(query), "\\s+")[[1]]
      pattern <- paste0(flag, "\\b", paste(map_chr(words, regex_escape), collapse = "\\s+"), "\\b")
      n_words <- length(words)

      hits <- map_dfr(corpus, function(doc) {
        raw <- doc$lines
        n   <- length(raw)
        if (n < n_words) return(tibble())

        # token = everything before the first "_"; tag = everything after it
        # (works whether tags are bracketed "{{POS<SUB>}}" or plain "POS<SUB>")
        tkns <- str_extract(raw, "^.+?(?=_)")
        tkns <- ifelse(is.na(tkns), raw, tkns)
        tags <- str_extract(raw, "(?<=_).*$")
        tags <- str_remove_all(ifelse(is.na(tags), "", tags), "[{}]")

        hit_positions <- c()
        for (i in seq_len(n - n_words + 1)) {
          chunk <- paste(tkns[i:(i + n_words - 1)], collapse = " ")
          if (grepl(pattern, chunk, perl = TRUE)) hit_positions <- c(hit_positions, i)
        }
        if (length(hit_positions) == 0) return(tibble())

        map_dfr(hit_positions, function(pos) {
          node_end <- pos + n_words - 1
          tibble(
            file_id    = doc$file_id,
            meta       = doc$meta,
            left       = get_left_context(tkns, pos, window),
            right      = get_right_context(tkns, node_end, window, n),
            node_words = list(tkns[pos:node_end]),
            node_tags  = list(tags[pos:node_end])
          )
        })
      })

      if (nrow(hits) > 0 && is.finite(max_examples) && nrow(hits) > max_examples) {
        hits <- hits[seq_len(max_examples), ]
      }
      hits
    }

    # ---- Render one hit as an aligned "words above / tags below" HTML block ----
    render_inspect_block <- function(row) {
      esc <- htmltools::htmlEscape

      words <- row$node_words[[1]]
      raw_tags <- row$node_tags[[1]]

      # Wrap each tag in {{ }} -- the exact format the Tag Bundle search box
      # above expects (see placeholder "e.g. {{DT}} {{JJ}}") -- so the whole
      # tag line can be copied and pasted straight into it. An untagged token
      # still gets a bracketed placeholder so the position lines up with its
      # word; it just won't match anything if pasted (no token is tagged "—").
      disp_tags <- wrap_tag(raw_tags)

      widths <- pmax(nchar(words), nchar(disp_tags), 1)

      # pad the RAW text first so column widths are correct, then escape —
      # escaping can lengthen the string (e.g. "<" -> "&lt;") without changing
      # its rendered width, so padding must happen before escaping.
      pad_token <- function(x, w) formatC(x, width = w, flag = "-")

      word_cells <- mapply(function(w, wd) {
        paste0("<span style='color:#2c7bb6;font-weight:bold;'>",
               esc(pad_token(w, wd)), "</span>")
      }, words, widths)

      tag_cells <- mapply(function(t, wd) {
        paste0("<span style='color:#d7191c;'>", esc(pad_token(t, wd)), "</span>")
      }, disp_tags, widths)

      left_str  <- if (nchar(row$left)  > 0) paste0(esc(row$left),  " ") else ""
      right_str <- if (nchar(row$right) > 0) paste0(" ", esc(row$right)) else ""
      pad       <- strrep(" ", nchar(row$left) + (if (nchar(row$left) > 0) 1 else 0))

      line1 <- paste0(left_str, paste(word_cells, collapse = " "), right_str)
      line2 <- paste0(pad, paste(tag_cells, collapse = " "))

      # Match/miss badge -- only shown when an expected tag was supplied
      # (row$match is NA otherwise, meaning "not being checked").
      match_badge <- if (is.null(row$match) || is.na(row$match)) {
        ""
      } else if (isTRUE(row$match)) {
        "<span style='background:#d4f7d4;color:#1a7a1a;padding:1px 6px;border-radius:3px;font-size:11px;font-weight:bold;margin-left:8px;'>✅ MATCH</span>"
      } else {
        "<span style='background:#ffd6d6;color:#a71d1d;padding:1px 6px;border-radius:3px;font-size:11px;font-weight:bold;margin-left:8px;'>❌ MISS</span>"
      }

      header <- paste0(
        "<div style='color:#888;font-size:11px;margin-top:10px;'>",
        esc(row$file_id),
        if (!is.na(row$meta)) paste0(" &nbsp;|&nbsp; ", esc(row$meta)) else "",
        match_badge,
        "</div>"
      )

      paste0(
        header,
        "<pre style='margin:2px 0 6px 0;white-space:pre-wrap;word-break:break-word;",
        "background:#f7f7f9;padding:6px 8px;border-radius:4px;'>",
        line1, "\n", line2,
        "</pre>"
      )
    }

    # ---- Reactive: run tag inspector on button click ----
    inspect_results <- eventReactive(input$run_inspect, {
      corpus <- apply_category_filter(active_corpus(), input$category_filter)
      validate(need(!is.null(corpus) && length(corpus) > 0,
                    "No data available for this category. Please process texts, upload .txt files, or choose a different category filter."))
      validate(need(nchar(trimws(input$inspect_query)) > 0,
                    "Please enter a word or phrase to inspect."))

      max_ex <- if (is.null(input$inspect_n) || input$inspect_n == "all") {
        Inf
      } else {
        as.numeric(input$inspect_n)
      }

      hits <- tag_inspect(
        corpus         = corpus,
        query          = input$inspect_query,
        window         = if (!is.null(input$inspect_window)) input$inspect_window else 5,
        case_sensitive = input$inspect_case,
        max_examples   = max_ex
      )

      validate(need(nrow(hits) > 0,
                    paste0('No occurrences of "', input$inspect_query, '" found.')))

      # Recall-proxy flag: if an expected tag was given, mark every hit as a
      # match or a miss; otherwise leave it NA (unflagged) for every row.
      expected_tag <- trimws(input$inspect_expected_tag %||% "")
      hits$match <- if (nzchar(expected_tag)) {
        vapply(hits$node_tags, tag_hits_expected, logical(1), expected_tag = expected_tag)
      } else {
        NA
      }

      hits
    })

    output$inspect_output <- renderUI({
      hits   <- inspect_results()
      blocks <- vapply(seq_len(nrow(hits)), function(i) render_inspect_block(hits[i, ]),
                       character(1))

      # When an expected tag was set, add a match/miss tally next to the
      # count -- the at-a-glance recall-proxy summary for this lexical form.
      match_summary <- if (!all(is.na(hits$match))) {
        n_match <- sum(hits$match, na.rm = TRUE)
        n_miss  <- sum(!hits$match, na.rm = TRUE)
        paste0(
          " &nbsp;|&nbsp; <strong style='color:#1a7a1a;'>", n_match, " match</strong>",
          " &nbsp;<strong style='color:#a71d1d;'>", n_miss, " miss</strong>"
        )
      } else {
        ""
      }

      HTML(paste0(
        "<div style='font-size:11px;color:#888;margin-bottom:2px;'>Showing ", nrow(hits),
        " example", if (nrow(hits) != 1) "s" else "", match_summary, "</div>",
        paste(blocks, collapse = "")
      ))
    })

    # ---- has_results flag, for showing the download button ----
    output$inspect_has_results <- reactive({
      !is.null(inspect_results()) && nrow(inspect_results()) > 0
    })
    outputOptions(output, "inspect_has_results", suspendWhenHidden = FALSE)

    # ---- CSV download: one row per example, tags in the same {{TAG}} format ----
    output$download_inspect_csv <- downloadHandler(
      filename = function() {
        query <- str_replace_all(trimws(input$inspect_query), "[^A-Za-z0-9_-]", "_")
        paste0("tag_inspector_", query, "_", format(Sys.Date(), "%Y%m%d"), ".csv")
      },
      content = function(file) {
        hits <- inspect_results()
        req(hits, nrow(hits) > 0)

        out <- map_dfr(seq_len(nrow(hits)), function(i) {
          row <- hits[i, ]
          tibble(
            file_id = row$file_id,
            meta    = row$meta,
            left    = row$left,
            node    = paste(row$node_words[[1]], collapse = " "),
            tags    = paste(wrap_tag(row$node_tags[[1]]), collapse = " "),
            right   = row$right,
            match   = row$match  # NA unless an expected tag was set above
          )
        })
        readr::write_csv(out, file)
      },
      contentType = "text/csv"
    )

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

      corpus <- apply_category_filter(active_corpus(), input$category_filter)
      validate(need(!is.null(corpus) && length(corpus) > 0,
                    "No data available for this category. Please process texts, upload .txt files, or choose a different category filter."))

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
        out <- kwic_results()

        # Precision-check workflow: Tag Bundle search + Random sample mode
        # produces exactly "a random sample of hits for this pattern" (see
        # the help text above the Tag Bundle box) -- add a blank column here
        # for the hand-coded correctness judgment, so the download is ready
        # to annotate rather than needing a column added in Excel first.
        if (isTRUE(input$search_mode == "tag") && isTRUE(input$mode == "random")) {
          out$tag_correct <- NA
        }

        readr::write_csv(out, file)
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
