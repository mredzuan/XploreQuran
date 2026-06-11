# ==============================================================================
# modules/mod_custom_import.R
# Quran Translation Explorer - Custom Translation Import Module
#
# A small cloud-download icon button in the sidebar triggers a modal dialog
# where the user can import up to 4 translations from tanzil.net URLs.
# On success, the shared custom_trans_rv reactiveVal is updated, which
# automatically propagates to all translation selectors in the app.
# ==============================================================================

# --- UI -----------------------------------------------------------------------
# Note: The trigger button (btn_open_custom_import) is defined in ui_sidebar.R
# so it sits naturally inline beside the Translation label.
# This UI function only provides the modal structure placeholder (no visible element).
mod_custom_import_ui <- function(id) {
  ns <- NS(id)
  # Invisible placeholder — all UI is rendered inside the modal on demand
  tagList()
}

# --- Server -------------------------------------------------------------------
mod_custom_import_server <- function(id, custom_trans_rv, open_trigger) {
  # custom_trans_rv : reactiveVal() from app.R — named list of loaded custom translations
  # open_trigger    : reactiveVal() from app.R — incremented when sidebar icon is clicked

  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ------------------------------------------------------------------
    # Open custom import modal when the trigger fires
    # ------------------------------------------------------------------
    observeEvent(open_trigger(), ignoreInit = TRUE, {

      showModal(
        modalDialog(
          title     = tagList(bsicons::bs_icon("cloud-download"), " Import Custom Translation"),
          size      = "m",
          easyClose = TRUE,
          fade      = TRUE,

          footer = tagList(
            actionButton(
              ns("btn_do_load"),
              label = tagList(bsicons::bs_icon("cloud-arrow-down"), " Load"),
              class = "btn btn-primary"
            ),
            modalButton("Cancel")
          ),

          # Instructions
          tags$p(
            class = "text-muted small mb-3",
            "Enter up to 4 translation URLs from ",
            tags$a("tanzil.net/trans", href = "https://tanzil.net/trans/",
                   target = "_blank", style = "color:#58a6ff;"),
            " and provide a display name for each."
          ),

          # 4 URL + Name input rows
          purrr::map(1:4, function(i) {
            div(
              class = "mb-3 p-2",
              style = "background:#1c2128; border-radius:0.5rem; border:1px solid #30363d;",

              tags$label(
                class = "form-label text-muted small fw-bold mb-2",
                paste0(bsicons::bs_icon("link-45deg"), " Custom Translation #", i)
              ),

              div(
                class = "mb-1",
                tags$small(class = "text-muted", "Tanzil URL"),
                textInput(
                  inputId     = ns(paste0("url_", i)),
                  label       = NULL,
                  placeholder = "https://tanzil.net/trans/xx.translatorname",
                  width       = "100%"
                )
              ),

              div(
                tags$small(class = "text-muted", "Display Name"),
                textInput(
                  inputId     = ns(paste0("name_", i)),
                  label       = NULL,
                  placeholder = paste0("e.g. Urdu - Jawadi"),
                  width       = "100%"
                )
              )
            )
          }),

          # Status area (shows per-row results after loading)
          uiOutput(ns("import_status"))
        )
      )
    })

    # ------------------------------------------------------------------
    # Execute import when "Load" button is clicked inside the modal
    # ------------------------------------------------------------------
    observeEvent(input$btn_do_load, {

      store    <- isolate(custom_trans_rv())  # keep existing entries
      messages <- list()
      errors   <- list()

      for (i in seq_len(4)) {
        url  <- input[[paste0("url_",  i)]]
        name <- input[[paste0("name_", i)]]

        # Skip blank rows
        if (is.null(url) || trimws(url) == "") next

        result <- tryCatch(
          load_custom_translation(url, name),
          error = function(e) {
            errors[[length(errors) + 1]] <<- paste0("Row #", i, ": ", conditionMessage(e))
            NULL
          }
        )

        if (!is.null(result)) {
          store[[result$key]] <- result

          # Warn if language was defaulted to English
          if (isTRUE(result$lang_defaulted)) {
            showNotification(
              tagList(
                tags$strong(paste0("\u2139\ufe0f  Import #", i, " \u2014 Language Notice")),
                tags$br(),
                paste0(
                  "Language code for \u2018", result$name, "\u2019 could not be detected ",
                  "from the URL prefix. Stop word removal will default to English (en). ",
                  "This may affect analysis quality for non-English text."
                )
              ),
              type     = "warning",
              duration = 12
            )
          }

          messages[[length(messages) + 1]] <- list(
            name = result$name,
            lang = toupper(result$lang)
          )
        }
      }

      # Update the shared store
      custom_trans_rv(store)

      # Render status inside the modal
      output$import_status <- renderUI({

        status_rows <- list()

        if (length(messages) > 0) {
          for (m in messages) {
            status_rows[[length(status_rows) + 1]] <- tags$p(
              class = "text-success small mb-1",
              bsicons::bs_icon("check-circle-fill"),
              paste0(" Loaded: ", m$name, " (", m$lang, ")")
            )
          }
        }

        if (length(errors) > 0) {
          for (e in errors) {
            status_rows[[length(status_rows) + 1]] <- tags$p(
              class = "text-danger small mb-1",
              bsicons::bs_icon("exclamation-triangle-fill"),
              paste0(" ", e)
            )
          }
        }

        if (length(status_rows) == 0) {
          return(div(
            class = "text-muted small mt-2",
            bsicons::bs_icon("info-circle"),
            " No rows were processed. Please enter at least one URL."
          ))
        }

        div(
          class = "mt-3 p-2",
          style = "background:#161b22; border-radius:0.4rem; border:1px solid #30363d;",
          tags$p(class = "text-muted small fw-bold mb-2", "Import Results:"),
          do.call(tagList, status_rows),
          if (length(messages) > 0)
            tags$p(
              class = "text-muted small mt-2 mb-0",
              bsicons::bs_icon("arrow-left"),
              " The translation dropdown has been updated."
            )
        )
      })

      # Notify user
      if (length(messages) > 0) {
        showNotification(
          paste0(length(messages), " custom translation(s) imported successfully."),
          type = "message", duration = 5
        )
      }
    })

  })
}
