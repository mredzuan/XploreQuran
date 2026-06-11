# ==============================================================================
# modules/mod_quran_viewer.R
# Quran Translation Explorer - Floating Quran Viewer Module
#
# Fixes (Update 3):
#   - Double-click bug: renderUI now uses tryCatch(viewer_data()) instead of
#     req(viewer_data()), so the first click always renders correctly.
#   - Ayah number shown as a single centred header row spanning both columns,
#     so Arabic and translation numbers are always at the same vertical position.
#   - Filter bar: Load button vertically centred inline with the dropdowns.
# ==============================================================================

# --- UI -----------------------------------------------------------------------
mod_quran_viewer_ui <- function(id) {
  ns <- NS(id)

  tagList(
    tags$button(
      id           = "fab-quran-viewer",
      title        = "Open Quran Viewer",
      `aria-label` = "Open Quran Viewer",
      HTML("&#x1F4D6;")
    ),
    tags$script(HTML(sprintf(
      "document.getElementById('fab-quran-viewer').addEventListener('click', function() {
         Shiny.setInputValue('%s', Math.random());
       });",
      ns("fab_clicked")
    )))
  )
}

# --- Server -------------------------------------------------------------------
mod_quran_viewer_server <- function(id, all_translations) {

  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ------------------------------------------------------------------
    # Internal reactive: has the user ever clicked Load in this session?
    # Used to decide between placeholder and "loading" states.
    # ------------------------------------------------------------------
    load_clicked <- reactiveVal(FALSE)

    # Reset when modal re-opens so placeholder shows again
    observeEvent(input$fab_clicked, {
      load_clicked(FALSE)
    })

    # ------------------------------------------------------------------
    # Open modal — translation choices populated at open-time (not via
    # updateSelectInput) to avoid the DOM-timing empty-picklist bug.
    # ------------------------------------------------------------------
    observeEvent(input$fab_clicked, {

      current_choices  <- all_translations()
      current_selected <- if ("trans_en_sahih" %in% current_choices) {
        "trans_en_sahih"
      } else {
        current_choices[[1]]
      }

      showModal(
        modalDialog(
          title     = tagList(bsicons::bs_icon("book-half"), " Quran Viewer"),
          size      = "xl",
          easyClose = TRUE,
          fade      = TRUE,
          footer    = modalButton("Close"),

          tags$script(HTML(
            "$(document).ready(function(){
               $('.modal').last().addClass('quran-viewer-modal');
             });"
          )),

          # ---- Filter bar: all controls in one aligned flex row --------
          div(
            class = "quran-filter-bar",

            # Surah selector
            div(
              class = "quran-filter-field",
              tags$label(
                class = "quran-filter-label",
                bsicons::bs_icon("layers"), " Select Surah"
              ),
              selectInput(
                ns("viewer_surah"),
                label    = NULL,
                choices  = SURAH_CHOICES,
                selected = 1,
                width    = "100%"
              )
            ),

            # Translation selector
            div(
              class = "quran-filter-field",
              style = "flex:1.5;",
              tags$label(
                class = "quran-filter-label",
                bsicons::bs_icon("translate"), " Translation"
              ),
              selectInput(
                ns("viewer_translation"),
                label    = NULL,
                choices  = current_choices,
                selected = current_selected,
                width    = "100%"
              )
            ),

            # Load button — vertically centred at the bottom of the label+input stack
            div(
              class = "quran-filter-btn",
              tags$label(class = "quran-filter-label", HTML("&nbsp;")),  # spacer label
              actionButton(
                ns("btn_viewer_load"),
                label = tagList(bsicons::bs_icon("play-fill"), " Load"),
                class = "btn btn-primary w-100"
              )
            )
          ),

          # ---- Ayah display --------------------------------------------
          uiOutput(ns("ayah_display"))
        )
      )
    })

    # ------------------------------------------------------------------
    # Arabic source (loaded once per session)
    # ------------------------------------------------------------------
    arabic_data <- reactive({
      env <- new.env(parent = emptyenv())
      data("quran_arab", package = "XploreQuran", envir = env)
      get("quran_arab", envir = env)
    })

    # ------------------------------------------------------------------
    # Fetch data on Load click.
    # FIX: ignoreInit = TRUE is correct, but renderUI must not use req()
    # on this reactive — see tryCatch pattern in renderUI below.
    # ------------------------------------------------------------------
    viewer_data <- eventReactive(input$btn_viewer_load, {
      req(input$viewer_surah, input$viewer_translation)
      load_clicked(TRUE)

      sid <- as.integer(input$viewer_surah)

      ar <- arabic_data() |>
        dplyr::filter(surah_id == sid) |>
        dplyr::select(surah_id, surah_ayah_id = ayah, arabic = text)

      trans_df <- tryCatch(
        load_translation(input$viewer_translation),
        error = function(e) {
          showNotification(
            paste("Could not load translation:", conditionMessage(e)),
            type = "error", duration = 8
          )
          NULL
        }
      )

      if (is.null(trans_df)) return(NULL)

      text_col <- if ("translation" %in% names(trans_df)) "translation" else "text"

      tr <- trans_df |>
        dplyr::filter(surah_id == sid) |>
        dplyr::select(surah_id, surah_ayah_id,
                      translation = !!rlang::sym(text_col))

      dplyr::left_join(ar, tr, by = c("surah_id", "surah_ayah_id"))
    },
    ignoreNULL = TRUE,
    ignoreInit = TRUE)

    # ------------------------------------------------------------------
    # Render ayah rows.
    #
    # KEY FIX for double-click bug:
    #   Previously used req(viewer_data()) which silently aborted the
    #   render on the first click (eventReactive hadn't returned yet in
    #   that flush cycle), causing an empty output until the second click.
    #
    #   Now uses tryCatch(): if viewer_data() hasn't fired yet it returns
    #   NULL and we show the appropriate state — no abort, no second click.
    # ------------------------------------------------------------------
    output$ayah_display <- renderUI({

      df <- tryCatch(viewer_data(), error = function(e) NULL)

      # Show placeholder if Load hasn't been clicked yet
      if (!isTRUE(load_clicked()) || is.null(df)) {
        if (isTRUE(load_clicked()) && is.null(df)) {
          # Load was clicked but data came back NULL (download error)
          return(div(
            class = "quran-viewer-placeholder",
            bsicons::bs_icon("exclamation-circle"),
            tags$p("Could not load data. Please check the translation selection.")
          ))
        }
        return(div(
          class = "quran-viewer-placeholder",
          bsicons::bs_icon("book"),
          tags$p(
            "Select a Surah and Translation above, then click ",
            strong("Load"), " to display the ayahs."
          )
        ))
      }

      if (nrow(df) == 0) {
        return(div(
          class = "quran-viewer-placeholder",
          bsicons::bs_icon("exclamation-circle"),
          tags$p("No ayahs found for the selected Surah.")
        ))
      }

      # One block per ayah:
      #   [── Surah:Ayah ────────────────]   ← full-width centred number header
      #   [ Arabic (RTL) | Translation   ]   ← two equal columns
      rows <- purrr::pmap(df, function(surah_id, surah_ayah_id,
                                       arabic, translation, ...) {
        div(
          class = "quran-ayah-block",

          # Ayah number header — spans full width, centred
          div(
            class = "quran-ayah-number-row",
            tags$span(
              class = "quran-ayah-badge",
              paste0(surah_id, ":", surah_ayah_id)
            )
          ),

          # Two-column body
          div(
            class = "quran-ayah-row",
            # Arabic column
            div(
              class = "quran-arabic-cell",
              arabic
            ),
            # Translation column
            div(
              class = "quran-trans-cell",
              tags$p(
                style = "margin:0;",
                if (is.na(translation))
                  tags$em("(translation not available)")
                else
                  translation
              )
            )
          )
        )
      })

      do.call(tagList, rows)
    })

  })
}
