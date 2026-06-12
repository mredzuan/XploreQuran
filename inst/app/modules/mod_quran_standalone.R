# ==============================================================================
# modules/mod_quran_standalone.R
# Quran Translation Explorer - Standalone Quran Viewer Module
#
# A full-screen standalone module to display Quran Arabic text alongside 
# the selected translation, filterable by Surah. This is used when the app 
# is accessed with ?viewer=1.
# ==============================================================================

# --- UI -----------------------------------------------------------------------
mod_quran_standalone_ui <- function(id, current_choices, current_selected) {
  ns <- NS(id)
  
  tagList(
    # Container for standalone viewer to constrain width slightly and add padding
    div(
      class = "container-fluid py-4",
      style = "max-width: 1200px;",
      
      div(
        class = "mb-4",
        h3(class = "fw-bold text-primary", bsicons::bs_icon("book-half"), " Quran Viewer")
      ),
      
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
}

# --- Server -------------------------------------------------------------------
mod_quran_standalone_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # ------------------------------------------------------------------
    # Internal reactive: has the user ever clicked Load in this session?
    # ------------------------------------------------------------------
    load_clicked <- reactiveVal(FALSE)
    
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
    # ------------------------------------------------------------------
    output$ayah_display <- renderUI({
      
      df <- tryCatch(viewer_data(), error = function(e) NULL)
      
      # Show placeholder if Load hasn't been clicked yet
      if (!isTRUE(load_clicked()) || is.null(df)) {
        if (isTRUE(load_clicked()) && is.null(df)) {
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
      
      # One block per ayah
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
            ),
            # Arabic column
            div(
              class = "quran-arabic-cell",
              arabic
            )
          )
        )
      })
      
      do.call(tagList, rows)
    })
    
  })
}
