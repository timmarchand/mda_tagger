# =============================================================================
# modules/server_tokencheck.R
# Tokenization Check Server Module
# =============================================================================

# ---- Punctuation-adjacency patterns available in the check ----
# Each is structurally identical: a single "before" character, the
# punctuation mark itself, and a single "after" character, with no space
# between any of them. That uniform 3-character shape is what lets the scan,
# the highlighting, and the space-insertion fix all share the same position
# arithmetic regardless of which pattern matched.
#
# `period_upper` is the original, default-on check (a lowercase letter, a
# period, an uppercase letter -- e.g. "on.Should"). The others are opt-in.
issue_defs <- list(
  list(id = "period_upper", short = "Period+Cap",
       label = "Period + capital letter, no space (default) — e.g. \"on.Should\"",
       pattern = "[a-z]\\.[A-Z]"),
  list(id = "period_lower", short = "Period+low",
       label = "Period + lowercase letter, no space — e.g. \"on.should\"",
       pattern = "[a-z]\\.[a-z]"),
  list(id = "comma_word", short = "Comma",
       label = "Comma + letter, no space — e.g. \"cats,dogs\"",
       pattern = "[a-zA-Z],[a-zA-Z]"),
  list(id = "question_excl", short = "?/!",
       label = "Question mark or exclamation point + letter, no space — e.g. \"now?Then\"",
       pattern = "[a-zA-Z][?!][A-Za-z]"),
  list(id = "semicolon_colon", short = ";/:",
       label = "Semicolon or colon + letter, no space — e.g. \"first;second\"",
       pattern = "[a-zA-Z][;:][A-Za-z]")
)

DEFAULT_ISSUE_TYPES <- "period_upper"

# Named vector for checkboxGroupInput: names are the long labels shown to
# the user, values are the ids used internally.
issue_type_choices <- setNames(
  vapply(issue_defs, function(d) d$id, character(1)),
  vapply(issue_defs, function(d) d$label, character(1))
)

#' Tokenization Check Server Module
#'
#' Scans the raw texts coming out of the data input module for common
#' pre-tagging hazards -- punctuation immediately adjoining the next word
#' with no space (e.g. "...and so on.Should I..."). UDPipe's tokenizer has
#' no other cue to split on, so it fuses the two words into one run-on
#' token, which then distorts POS tagging and every downstream MDA count.
#'
#' This module sits between the data input module and the processing
#' module. It returns the SAME shape of list that dataInputServer() does
#' (uploaded_data / selected_text_and_meta / data_confirmed), so it can be
#' passed to processingServer() as a drop-in replacement: if the user has
#' clicked "Fix Selected", selected_text_and_meta() transparently returns
#' the cleaned text; otherwise it passes the original through unchanged.
#'
#' @param id Module namespace ID
#' @param data_module Return value of dataInputServer()
#' @param paren_session The top-level app session, used only so the "Skip
#'   Check" button can switch the sidebar to the Processing tab. Pass NULL
#'   to disable that navigation (the button still hides the check results,
#'   it just won't change tabs).
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
    #      selected punctuation pattern ----
    scan_results <- reactive({
      req(scan_triggered())
      data <- patched_selected_text_and_meta()
      validate(need(!is.null(data) && length(data$text) > 0,
                    "No text data available. Please upload and confirm data on the Upload tab first."))

      selected_ids <- input$issue_types
      validate(need(length(selected_ids) > 0,
                    "Select at least one punctuation pattern to check."))

      active_defs <- Filter(function(d) d$id %in% selected_ids, issue_defs)

      texts   <- data$text
      doc_ids <- data$doc_ids

      hits <- map_dfr(active_defs, function(def) {
        map_dfr(seq_along(texts), function(i) {
          txt <- texts[i]
          m <- gregexpr(def$pattern, txt, perl = TRUE)[[1]]
          if (m[1] == -1) return(tibble())

          map_dfr(seq_along(m), function(j) {
            # m[j] is the START of the 3-char match (before-char, punct,
            # after-char) -- i.e. the "before" character, NOT the
            # punctuation mark itself. The punctuation is one character
            # further in, and the run-on character one further still.
            before_pos <- m[j]
            punct_pos  <- before_pos + 1
            after_pos  <- before_pos + 2
            ctx_from   <- max(1, before_pos - 34)
            ctx_to     <- min(nchar(txt), after_pos + 35)
            tibble(
              doc_id   = doc_ids[i],
              doc_idx  = i,
              type     = def$id,
              type_short = def$short,
              position = punct_pos,
              before   = substr(txt, ctx_from, punct_pos),  # ends at (and includes) the punctuation
              after    = substr(txt, after_pos, ctx_to)     # starts at the run-on character
            )
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

    # Drives which button shows at the bottom of the box: "Skip Check" before
    # a scan has ever been run, "Continue to Processing" afterward (since at
    # that point the user has actually done the check, not skipped it).
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
                   "⚠️ Select at least one punctuation pattern to check, then scan again."))
      }
      if (nrow(hits) == 0) {
        return(div(class = "alert alert-success", style = "padding: 8px 12px;",
                   paste0("✅ No instances found for the selected pattern(s)", fixed_note, ".")))
      }

      n_docs <- n_distinct(hits$doc_id)
      div(
        class = "alert alert-warning", style = "padding: 8px 12px;",
        HTML(paste0(
          "⚠️ Found <strong>", nrow(hits), "</strong> instance",
          if (nrow(hits) != 1) "s" else "",
          " across <strong>", n_docs, "</strong> document",
          if (n_docs != 1) "s" else "", fixed_note, "."
        ))
      )
    })

    # ---- Table of flagged instances, with the hazard highlighted. Rows are
    #      selectable (all selected by default) so a user can deselect known
    #      false positives -- e.g. "e.g." or "a.m." under period+lowercase --
    #      before running "Fix Selected" below. ----
    output$issues_table <- DT::renderDataTable({
      hits <- scan_results()
      req(nrow(hits) > 0)

      esc <- htmltools::htmlEscape

      display <- hits %>%
        mutate(
          context = paste0(
            esc(before),
            "<span style='background:#ffe08a;font-weight:bold;'>",
            esc(substr(after, 1, 1)),
            "</span>",
            esc(substr(after, 2, nchar(after)))
          )
        ) %>%
        select(doc_id, type_short, context)

      DT::datatable(
        display, rownames = FALSE, escape = FALSE,
        colnames = c("File", "Type", "Context (highlighted = run-on character)"),
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
          transmute(doc_id, type = type_short, position, context = paste0(before, after))
        readr::write_csv(out, file)
      },
      contentType = "text/csv"
    )

    # ---- Shared primitive: insert a single space right after `pos` in one
    #      document's text. ----
    insert_space_after <- function(txt, pos) {
      paste0(substr(txt, 1, pos), " ", substr(txt, pos + 1, nchar(txt)))
    }

    # ---- Fix only the currently SELECTED rows in issues_table. All rows
    #      start selected (see the `selection` argument in issues_table
    #      above), so the default behavior is "fix everything" -- but a user
    #      can click a row to deselect a false positive (e.g. "e.g." or
    #      "a.m." under period+lowercase) before applying the fix. ----
    observeEvent(input$fix_selected, {
      hits <- tryCatch(scan_results(), error = function(e) NULL)
      if (is.null(hits) || nrow(hits) == 0) {
        showNotification("No issues to fix for the selected pattern(s).", type = "warning")
        return()
      }

      selected_rows <- input$issues_table_rows_selected
      if (is.null(selected_rows) || length(selected_rows) == 0) {
        showNotification("No rows selected — nothing to fix.", type = "warning")
        return()
      }

      to_fix <- hits[selected_rows, ]

      data  <- patched_selected_text_and_meta()
      texts <- data$text

      # Apply positions within each document from right to left, so
      # inserting a space earlier in the string doesn't shift the
      # positions of matches later in the same string that haven't been
      # processed yet.
      for (di in unique(to_fix$doc_idx)) {
        positions <- sort(to_fix$position[to_fix$doc_idx == di], decreasing = TRUE)
        txt <- texts[di]
        for (pos in positions) txt <- insert_space_after(txt, pos)
        texts[di] <- txt
      }

      cleaned_text(texts)
      skipped <- nrow(hits) - nrow(to_fix)
      showNotification(
        paste0(
          "Inserted a space at ", nrow(to_fix), " instance",
          if (nrow(to_fix) != 1) "s" else "", ".",
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

    # ---- Move on to Processing, either by skipping the check altogether
    #      (before a scan) or by continuing after reviewing/fixing (after a
    #      scan). Both buttons do the same tab switch; only the label and
    #      notification differ, since which one is visible already tells the
    #      user which case they're in (see has_scanned above). ----
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
