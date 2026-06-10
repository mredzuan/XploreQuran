# ==============================================================================
# modules/mod_quran_viewer.R
# Quran Translation Explorer - Floating Quran Viewer Module
#
# Provides a floating action button (FAB) that opens a full-screen modal
# displaying Quran Arabic text alongside a selected translation, filterable
# by Surah. The translation list is kept in sync with global TRANSLATIONS
# plus any user-added custom translations.
# ==============================================================================

# --- UI -----------------------------------------------------------------------
mod_quran_viewer_ui <- function(id) {
  ns <- NS(id)

  tagList(
    # Floating Action Button — fixed position via CSS (#fab-quran-viewer)
    tags$button(
      id      = "fab-quran-viewer",   # CSS targets this ID directly
      title   = "Open Quran Viewer",
      `aria-label` = "Open Quran Viewer",
      # Book + translation icon using Unicode
      HTML("&#x1F4D6;")
    ),

    # JavaScript: wire the FAB click to trigger Shiny input
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
  # all_translations: a reactive() returning a named list of translation choices
  # (built-in TRANSLATIONS merged with any user custom translations)

  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ------------------------------------------------------------------
    # Open modal when FAB is clicked
    # ------------------------------------------------------------------
    observeEvent(input$fab_clicked, {

      showModal(
        modalDialog(
          title = tagList(
            bsicons::bs_icon("book-half"),
            " Quran Viewer"
          ),
          size        = "xl",
          easyClose   = TRUE,
          fade        = TRUE,
          footer      = modalButton("Close"),

          # Apply custom modal class via JS after render
          tags$script(HTML(
            "$(document).ready(function(){
               $('.modal-dialog').closest('.modal').addClass('quran-viewer-modal');
             });"
          )),

          # ---- Filter bar ------------------------------------------------
          div(
            class = "quran-filter-bar",

            # Surah selector
            div(
              style = "min-width:200px; flex:1;",
              tags$label(
                class = "form-label text-muted small fw-semibold",
                bsicons::bs_icon("layers"), " Select Surah"
              ),
              selectInput(
                ns("viewer_surah"),
                label   = NULL,
                choices = SURAH_CHOICES,
                selected = 1,
                width   = "100%"
              )
            ),

            # Translation selector
            div(
              style = "min-width:220px; flex:1.5;",
              tags$label(
                class = "form-label text-muted small fw-semibold",
                bsicons::bs_icon("translate"), " Translation"
              ),
              selectInput(
                ns("viewer_translation"),
                label   = NULL,
                choices = NULL,   # populated in server via updateSelectInput
                width   = "100%"
              )
            ),

            # Load button
            div(
              style = "flex:0 0 auto; padding-top:1px;",
              actionButton(
                ns("btn_viewer_load"),
                label = tagList(bsicons::bs_icon("arrow-clockwise"), " Load"),
                class = "btn btn-primary"
              )
            )
          ),

          # ---- Ayah display area -----------------------------------------
          uiOutput(ns("ayah_display"))
        )
      )
    })

    # ------------------------------------------------------------------
    # Keep translation selector in sync with all_translations reactive
    # ------------------------------------------------------------------
    observe({
      req(all_translations())
      updateSelectInput(
        session  = session,
        inputId  = "viewer_translation",
        choices  = all_translations(),
        selected = "trans_en_sahih"
      )
    })

    # ------------------------------------------------------------------
    # Load and render ayahs
    # ------------------------------------------------------------------

    # Load Arabic data once (static)
    arabic_data <- reactive({
      env <- new.env(parent = emptyenv())
      data("quran_arab", package = "XploreQuran", envir = env)
      get("quran_arab", envir = env)
    })

    viewer_data <- eventReactive(input$btn_viewer_load, {
      req(input$viewer_surah, input$viewer_translation)

      sid <- as.integer(input$viewer_surah)

      # Load Arabic
      ar <- arabic_data() |>
        dplyr::filter(surah_id == sid) |>
        dplyr::select(surah_id, surah_ayah_id = ayah, arabic = text)

      # Load translation
      trans_df <- tryCatch(
        load_translation(input$viewer_translation),
        error = function(e) {
          showNotification(
            paste("Error loading translation:", conditionMessage(e)),
            type = "error", duration = 6
          )
          return(NULL)
        }
      )

      if (is.null(trans_df)) return(NULL)

      text_col <- if ("translation" %in% names(trans_df)) "translation" else "text"

      tr <- trans_df |>
        dplyr::filter(surah_id == sid) |>
        dplyr::select(surah_id, surah_ayah_id, translation = !!rlang::sym(text_col))

      # Join Arabic + translation on ayah position
      dplyr::left_join(ar, tr, by = c("surah_id", "surah_ayah_id"))
    },
    ignoreInit = TRUE)

    # ------------------------------------------------------------------
    # Render the ayah rows
    # ------------------------------------------------------------------
    output$ayah_display <- renderUI({

      # Placeholder before first load
      if (is.null(input$btn_viewer_load) || input$btn_viewer_load == 0) {
        return(
          div(
            class = "quran-viewer-placeholder",
            bsicons::bs_icon("book"),
            tags$p("Select a Surah and Translation, then click", strong("Load"), "to display.")
          )
        )
      }

      req(viewer_data())
      df <- viewer_data()

      if (nrow(df) == 0) {
        return(div(
          class = "quran-viewer-placeholder",
          tags$p("No ayahs found for the selected Surah.")
        ))
      }

      # Build one row per ayah
      rows <- purrr::pmap(df, function(surah_id, surah_ayah_id, arabic, translation, ...) {
        div(
          class = "quran-ayah-row",
          # Arabic cell (right side logically, left in grid)
          div(
            class = "quran-arabic-cell",
            tags$span(
              class = "quran-ayah-badge",
              paste0(surah_id, ":", surah_ayah_id)
            ),
            tags$br(),
            arabic
          ),
          # Translation cell
          div(
            class = "quran-trans-cell",
            tags$span(
              class = "quran-ayah-badge",
              paste0("Ayah ", surah_ayah_id)
            ),
            tags$p(
              style = "margin:0;",
              if (is.na(translation)) tags$em("(translation not available)") else translation
            )
          )
        )
      })

      do.call(tagList, rows)
    })

  })
}
