# =============================================================================
# modules/server_tokencheck.R
# Tokenization Check Server Module
# =============================================================================

# ---- Punctuation-adjacency patterns available in the check ----
# "adjacency" kind: a single "before" character, the punctuation mark
# itself, and a single "after" character, with no space between any of
# them - e.g. "on.Should". Fixed with a single space insertion.
#
# "span" kind: a variable-length match (e.g. an HTML tag) that gets
# replaced entirely with a single space, since deleting it outright would
# glue the surrounding words together into a new run-on.
issue_defs <- list(
  list(id = "html_residue", short = "HTML", kind = "span",
       label = "HTML residue (tags/entities) - e.g. <br/>, &nbsp; - replaced with a single space",
       pattern = "</?[a-zA-Z][a-zA-Z0-9]*\\s*/?>|&[a-zA-Z#][a-zA-Z0-9]*;"),
  list(id = "period_upper", short = "Period+Cap", kind = "adjacency",
       label = "Period + capital letter, no space (default) - e.g. \"on.Should\"",
       pattern = "[a-z]\\.[A-Z]"),
  list(id = "period_lower", short = "Per+low", kind = "adjacency", subtype = "exclude_common",
       label = "Period + lowercase letter, no space, excluding common abbreviations/URLs - e.g. \"on.should\"",
       pattern = "[a-z]\\.[a-z]"),
  list(id = "period_lower_common", short = "Per+low (common)", kind = "adjacency", subtype = "common_only",
       label = "Period + lowercase - common abbreviations/URLs only, for separate review - e.g. \"a.m.\", \"google.com\"",
       pattern = "[a-z]\\.[a-z]"),
  list(id = "comma_word", short = "Comma", kind = "adjacency",
       label = "Comma + letter, no space - e.g. \"cats,dogs\"",
       pattern = "[a-zA-Z],[a-zA-Z]"),
  list(id = "question_excl", short = "?/!", kind = "adjacency",
       label = "Question mark or exclamation point + letter, no space - e.g. \"now?Then\"",
       pattern = "[a-zA-Z][?!][A-Za-z]"),
  list(id = "semicolon_colon", short = ";/:", kind = "adjacency",
       label = "Semicolon or colon + letter, no space - e.g. \"first;second\"",
       pattern = "[a-zA-Z][;:][A-Za-z]")
)

DEFAULT_ISSUE_TYPES <- c("html_residue", "period_upper")

# Named vector for checkboxGroupInput: names are the long labels shown to
# the user, values are the ids used internally.
issue_type_choices <- setNames(
  vapply(issue_defs, function(d) d$id, character(1)),
  vapply(issue_defs, function(d) d$label, character(1))
)

# ---- Common abbreviations and domain suffixes treated as "safe" period +
#      lowercase adjacencies (e.g. "e.g.", "8a.m.", "xxx@site.com",
#      "www.site.com"), scanned separately via period_lower_common above
#      rather than mixed in with genuine run-on candidates. ----
common_period_tlds <- c("com", "org", "net", "edu", "gov", "co", "io", "jp", "uk",
                        "de", "fr", "cn", "info", "biz", "us", "ca", "au")

# Matches a.m / p.m / e.g / i.e as a bounded unit, regardless of what
# digits, colons, or punctuation are glued directly onto them (e.g. "8a.m",
# "11:30a.m.", ".i.e."). The lookaround requires the letter on each side
# NOT be part of a longer lowercase word, so "via.middle" is correctly left
# as a genuine run-on rather than misread as containing "i.e" or similar.
abbrev_regex <- "(?<![a-z])(a\\.m|p\\.m|e\\.g|i\\.e)(?![a-z])"

is_common_period_context <- function(full_text, match_pos) {
  n <- nchar(full_text)

  # ---- Abbreviation check: small local window, case-insensitive ----
  local_from <- max(1, match_pos - 6)
  local_to   <- min(n, match_pos + 6)
  local_window <- tolower(substr(full_text, local_from, local_to))
  if (grepl(abbrev_regex, local_window, perl = TRUE)) return(TRUE)

  # ---- Email / URL / domain check: whole whitespace-delimited token ----
  left <- match_pos
  while (left > 1 && !grepl("\\s", substr(full_text, left - 1, left - 1))) left <- left - 1
  right <- match_pos
  while (right < n && !grepl("\\s", substr(full_text, right + 1, right + 1))) right <- right + 1
  word <- tolower(substr(full_text, left, right))

  # Trim leading/trailing quote marks, parentheses, etc. that aren't part
  # of the email/URL itself but often sit right next to it in prose.
  word <- str_replace(word, "^[^a-z0-9]+", "")
  word <- str_replace(word, "[^a-z0-9]+$", "")

  if (nchar(word) == 0) return(FALSE)

  domain_pattern <- paste0(
    "^(https?://)?(www\\.)?([a-z0-9_.+-]+@)?[a-z0-9-]+(\\.[a-z0-9-]+)*\\.(",
    paste(common_period_tlds, collapse = "|"),
    ")([/:.,;].*)?$"
  )

  grepl(domain_pattern, word)
}

#' Tokenization Check Server Module
#'
#' Scans the raw texts coming out of the data input module for common
#' pre-tagging hazards: punctuation immediately adjoining the next word
#' with no space (e.g. "...and so on.Should I..."), and residual HTML
#' markup (e.g. "<br/>") that would otherwise be counted as an untagged
#' token and inflate word counts downstream.
#'
#' This module sits between the data input module and the processing
#' module. It returns the SAME shape of list that dataInputServer() does
#' (uploaded_data / selected_text_and_meta / data_confirmed), so it can be
#' passed to processingServer() as a drop-in replacement.
#'
#' @param id Module namespace ID
#' @param data_module Return value of dataInputServer()
#' @param paren_session The top-level app session, used only so the "Skip
#'   Check" button can switch the sidebar to the Processing tab. Pass NULL
#'   to disable that navigation.
#' @return List with the same shape as dataInputServer()'s return value
tokenCheckServer <- function(id, data_module, paren_session = NULL) {
  moduleServer(id, function(input, output, session) {

    cleaned_text   <- reactiveVal(NULL)   # NULL = using the original text as-is
    scan_triggered <- reactiveVal(FALSE)

    observeEvent(input$scan, { scan_triggered(TRUE) })

    # ---- Wrapped text: the cleaned version if a fix has been applied,
    #      otherwise the original data untouched ----
    patched_selected_text_and_meta <- reactive({
      data <- data_module$selected_text_and_meta()
      if (is.null(data)) return(NULL)
      override <- cleaned_text()
      if (!is.null(override) && length(override) == length(data$text)) {
        data$text <- override
      }
      data
    })

    # ---- Scan whichever text is currently active, for every currently
    #      selected pattern (adjacency or span kind) ----
    scan_results <- reactive({
      req(scan_triggered())
      data <- patched_selected_text_and_meta()
      validate(need(!is.null(data) && length(data$text) > 0,
                    "No text data available. Please upload and confirm data on the Upload tab first."))

      selected_ids <- input$issue_types
      validate(need(length(selected_ids) > 0,
                    "Select at least one pattern to check."))

      active_defs <- Filter(function(d) d$id %in% selected_ids, issue_defs)

      texts   <- data$text
      doc_ids <- data$doc_ids

      hits <- map_dfr(active_defs, function(def) {
        kind <- if (!is.null(def$kind)) def$kind else "adjacency"

        map_dfr(seq_along(texts), function(i) {
          txt <- texts[i]
          m <- gregexpr(def$pattern, txt, perl = TRUE)[[1]]
          if (m[1] == -1) return(tibble())
          lens <- attr(m, "match.length")

          map_dfr(seq_along(m), function(j) {
            if (kind == "span") {
              match_start <- m[j]
              match_end   <- m[j] + lens[j] - 1
              ctx_from <- max(1, match_start - 35)
              ctx_to   <- min(nchar(txt), match_end + 35)
              tibble(
                doc_id = doc_ids[i], doc_idx = i, type = def$id, type_short = def$short,
                kind = kind, position = match_start, end = match_end,
                before     = substr(txt, ctx_from, match_start - 1),
                match_text = substr(txt, match_start, match_end),
                after      = substr(txt, match_end + 1, ctx_to)
              )
            } else {
              # m[j] is the START of the 3-char match (before-char, punct,
              # after-char) - i.e. the "before" character, NOT the
              # punctuation mark itself.
              before_pos <- m[j]
              punct_pos  <- before_pos + 1
              after_pos  <- before_pos + 2

              if (!is.null(def$subtype)) {
                is_common <- is_common_period_context(txt, punct_pos)
                if (def$subtype == "exclude_common" && is_common) return(tibble())
                if (def$subtype == "common_only" && !is_common) return(tibble())
              }

              ctx_from   <- max(1, before_pos - 34)
              ctx_to     <- min(nchar(txt), after_pos + 35)
              tibble(
                doc_id = doc_ids[i], doc_idx = i, type = def$id, type_short = def$short,
                kind = kind, position = punct_pos, end = punct_pos,
                before     = substr(txt, ctx_from, punct_pos),
                match_text = substr(txt, after_pos, after_pos),
                after      = substr(txt, after_pos + 1, ctx_to)
              )
            }
          })
        })
      })

      if (nrow(hits) > 0) {
        hits <- hits %>% arrange(doc_idx, position)
      }
      hits
    })

    output$has_issues <- reactive({
      tryCatch(scan_triggered() && nrow(scan_results()) > 0, error = function(e) FALSE)
    })
    outputOptions(output, "has_issues", suspendWhenHidden = FALSE)

    output$is_fixed <- reactive({ !is.null(cleaned_text()) })
    outputOptions(output, "is_fixed", suspendWhenHidden = FALSE)

    output$has_scanned <- reactive({ scan_triggered() })
    outputOptions(output, "has_scanned", suspendWhenHidden = FALSE)

    # ---- Status banner ----
    output$status_banner <- renderUI({
      if (!scan_triggered()) {
        return(div(class = "text-muted", style = "font-size: 12px;",
                   'Click "Scan for Issues" to check your texts.'))
      }

      hits <- tryCatch(scan_results(), error = function(e) NULL)
      fixed_note <- if (!is.null(cleaned_text())) " (checking the cleaned text)" else ""

      if (is.null(hits)) {
        return(div(class = "alert alert-warning", style = "padding: 8px 12px;",
                   "Select at least one pattern to check, then scan again."))
      }
      if (nrow(hits) == 0) {
        return(div(class = "alert alert-success", style = "padding: 8px 12px;",
                   paste0("No instances found for the selected pattern(s)", fixed_note, ".")))
      }

      n_docs <- n_distinct(hits$doc_id)
      div(
        class = "alert alert-warning", style = "padding: 8px 12px;",
        HTML(paste0(
          "Found <strong>", nrow(hits), "</strong> instance",
          if (nrow(hits) != 1) "s" else "",
          " across <strong>", n_docs, "</strong> document",
          if (n_docs != 1) "s" else "", fixed_note, "."
        ))
      )
    })

    # ---- Table of flagged instances, with the matched text highlighted.
    #      Rows are selectable (all selected by default) so a user can
    #      deselect known false positives before running "Fix Selected". ----
    output$issues_table <- DT::renderDataTable({
      hits <- scan_results()
      req(nrow(hits) > 0)

      esc <- htmltools::htmlEscape

      display <- hits %>%
        mutate(
          context = paste0(
            esc(before),
            "<span style='background:#ffe08a;font-weight:bold;'>",
            esc(match_text),
            "</span>",
            esc(after)
          )
        ) %>%
        select(doc_id, type_short, context)

      DT::datatable(
        display, rownames = FALSE, escape = FALSE,
        colnames = c("File", "Type", "Context (highlighted = matched text)"),
        selection = list(mode = "multiple", selected = seq_len(nrow(display)), target = "row"),
        options = list(
          pageLength = 25,
          lengthMenu = list(c(10, 25, 50, 100, -1), c("10", "25", "50", "100", "All")),
          dom = "ltip", scrollX = TRUE,
          columnDefs = list(list(className = "dt-center", targets = 1, width = "90px"))
        )
      )
    })

    # ---- CSV report of every flagged instance ----
    output$download_report <- downloadHandler(
      filename = function() paste0("tokenization_issues_", format(Sys.Date(), "%Y%m%d"), ".csv"),
      content = function(file) {
        hits <- scan_results()
        req(nrow(hits) > 0)
        out <- hits %>%
          transmute(doc_id, type = type_short, position, context = paste0(before, match_text, after))
        readr::write_csv(out, file)
      },
      contentType = "text/csv"
    )

    # ---- Shared primitives for applying a fix ----
    insert_space_after <- function(txt, pos) {
      paste0(substr(txt, 1, pos), " ", substr(txt, pos + 1, nchar(txt)))
    }

    replace_span_with_space <- function(txt, start, end) {
      paste0(substr(txt, 1, start - 1), " ", substr(txt, end + 1, nchar(txt)))
    }

    apply_fix <- function(txt, kind, position, end) {
      if (identical(kind, "span")) {
        replace_span_with_space(txt, position, end)
      } else {
        insert_space_after(txt, position)
      }
    }

    # ---- Fix only the currently SELECTED rows in issues_table. All rows
    #      start selected, so the default behavior is "fix everything" -
    #      but a user can deselect a false positive first. ----
    observeEvent(input$fix_selected, {
      hits <- tryCatch(scan_results(), error = function(e) NULL)
      if (is.null(hits) || nrow(hits) == 0) {
        showNotification("No issues to fix for the selected pattern(s).", type = "warning")
        return()
      }

      selected_rows <- input$issues_table_rows_selected
      if (is.null(selected_rows) || length(selected_rows) == 0) {
        showNotification("No rows selected - nothing to fix.", type = "warning")
        return()
      }

      to_fix <- hits[selected_rows, ]

      data  <- patched_selected_text_and_meta()
      texts <- data$text

      # Apply fixes within each document from right to left, so an
      # earlier fix doesn't shift the position of a later one that
      # hasn't been processed yet - regardless of whether the fixes are
      # a mix of adjacency insertions and span replacements.
      for (di in unique(to_fix$doc_idx)) {
        doc_rows <- to_fix[to_fix$doc_idx == di, ]
        doc_rows <- doc_rows[order(doc_rows$position, decreasing = TRUE), ]
        txt <- texts[di]
        for (r in seq_len(nrow(doc_rows))) {
          txt <- apply_fix(txt, doc_rows$kind[r], doc_rows$position[r], doc_rows$end[r])
        }
        texts[di] <- txt
      }

      cleaned_text(texts)
      skipped <- nrow(hits) - nrow(to_fix)
      showNotification(
        paste0(
          "Fixed ", nrow(to_fix), " instance", if (nrow(to_fix) != 1) "s" else "", ".",
          if (skipped > 0) paste0(" Left ", skipped, " deselected instance",
                                  if (skipped != 1) "s" else "", " untouched.") else ""
        ),
        type = "message", duration = 5
      )
    })

    observeEvent(input$reset_fix, {
      cleaned_text(NULL)
      scan_triggered(TRUE)
      showNotification("Reverted to the original, unmodified text.", type = "message", duration = 3)
    })

    go_to_processing <- function(msg) {
      if (!is.null(paren_session)) {
        updateTabItems(paren_session, "tabs", "process")
      }
      showNotification(msg, type = "warning", duration = 3)
    }

    observeEvent(input$skip_check, {
      go_to_processing("Skipped tokenization check.")
    })

    observeEvent(input$continue_processing, {
      go_to_processing("Continuing to processing.")
    })

    # ---- Module Returns: same shape as dataInputServer(), text patched ----
    return(list(
      uploaded_data          = data_module$uploaded_data,
      selected_text_and_meta = patched_selected_text_and_meta,
      data_confirmed         = data_module$data_confirmed
    ))
  })
}
