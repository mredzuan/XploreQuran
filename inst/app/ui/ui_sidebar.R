# ==============================================================================
# ui/ui_sidebar.R
# Quran Translation Explorer - Sidebar: Translation Analytic Configuration
#
# Contains:
#   - Built-in translation selector
#   - Analyse By / Surah / Juz selectors
#   - Text Processing collapsible accordion
#   - Select Number for Top Term slider
#   - Custom Translation URL inputs (up to 4)
#   - Run Analysis button
# ==============================================================================

ui_sidebar <- function() {
  sidebar(
    id    = "main_sidebar",
    width = 300,

    # --- Brand ----------------------------------------------------------------
    div(
      class = "text-center mb-2",
      tags$img(src = "logo.png", height = "52px", alt = "Quran Translation Explorer logo",
               style = "opacity:0.92;"),
      tags$p(class = "text-muted small mt-1 mb-0 fw-semibold",
             "Quran Translation Explorer")
    ),

    hr(style = "border-color:#30363d; margin:0.6rem 0;"),

    # --- Section Header -------------------------------------------------------
    tags$p(
      class = "text-uppercase fw-bold mb-2 mt-1",
      style = "font-size:0.7rem; letter-spacing:0.1em; color:#58a6ff;",
      bsicons::bs_icon("sliders2"), " Translation Analytic Configuration"
    ),

    # --- Translation Selection ------------------------------------------------
    div(
      class = "mb-2",
      tags$label(
        class = "form-label text-muted small fw-semibold mb-1",
        bsicons::bs_icon("book"), " Translation"
      ),
      selectInput(
        inputId  = "sel_translation",
        label    = NULL,
        choices  = TRANSLATIONS,
        selected = "trans_en_sahih",
        width    = "100%"
      )
    ),

    # --- Grouping Dimension ---------------------------------------------------
    div(
      class = "mb-2",
      tags$label(
        class = "form-label text-muted small fw-semibold mb-1",
        bsicons::bs_icon("stack"), " Analyse By"
      ),
      radioButtons(
        inputId  = "sel_by",
        label    = NULL,
        choices  = c("Surah" = "surah", "Juz" = "juz"),
        selected = "surah",
        inline   = TRUE
      )
    ),

    # --- Surah Selector -------------------------------------------------------
    conditionalPanel(
      condition = "input.sel_by == 'surah'",
      div(
        class = "mb-2",
        tags$label(
          class = "form-label text-muted small fw-semibold mb-1",
          bsicons::bs_icon("list-ol"), " Select Surah(s)"
        ),
        selectInput(
          inputId  = "sel_surah",
          label    = NULL,
          choices  = SURAH_CHOICES,
          multiple = TRUE,
          selected = NULL,
          width    = "100%"
        )
      )
    ),

    # --- Juz Selector ---------------------------------------------------------
    conditionalPanel(
      condition = "input.sel_by == 'juz'",
      div(
        class = "mb-2",
        tags$label(
          class = "form-label text-muted small fw-semibold mb-1",
          bsicons::bs_icon("list-ol"), " Select Juz"
        ),
        selectInput(
          inputId  = "sel_juz",
          label    = NULL,
          choices  = setNames(JUZ_CHOICES, paste("Juz", JUZ_CHOICES)),
          multiple = TRUE,
          selected = NULL,
          width    = "100%"
        )
      )
    ),

    # --- Top N Terms ----------------------------------------------------------
    div(
      class = "mb-2",
      sliderInput(
        inputId = "sl_top_n",
        label   = tags$span(
          class = "text-muted small fw-semibold",
          bsicons::bs_icon("hash"), " Select Number for Top Term"
        ),
        min = 5, max = 50, value = 20, step = 5,
        width = "100%"
      )
    ),

    hr(style = "border-color:#30363d; margin:0.6rem 0;"),

    # --- Text Processing (Collapsible Accordion) ------------------------------
    accordion(
      id    = "acc_text_processing",
      open  = FALSE,   # collapsed by default
      accordion_panel(
        title  = tagList(bsicons::bs_icon("funnel"), " Text Processing"),
        value  = "panel_text_proc",

        div(
          class = "ps-1",
          checkboxInput("chk_stopwords", "Remove Stop Words",  value = TRUE),
          checkboxInput("chk_normalize", "Stem Words",         value = FALSE),
          checkboxInput("chk_special",   "Remove Punctuation", value = TRUE)
        )
      )
    ),

    # --- Custom Translation URLs (Collapsible Accordion) ----------------------
    accordion(
      id   = "acc_custom_trans",
      open = FALSE,
      accordion_panel(
        title = tagList(bsicons::bs_icon("link-45deg"), " Custom Translations"),
        value = "panel_custom_trans",

        tags$p(
          class = "text-muted small mb-2",
          "Add up to 4 translations from",
          tags$a("tanzil.net", href = "https://tanzil.net/trans/",
                 target = "_blank", style = "color:#58a6ff;"),
          ". Enter the full URL and a display name."
        ),

        # 4 custom translation input rows
        purrr::map(1:4, function(i) {
          div(
            class = "mb-3",
            tags$label(
              class = "form-label text-muted small fw-semibold mb-1",
              paste0("Custom #", i)
            ),
            textInput(
              inputId     = paste0("custom_url_", i),
              label       = NULL,
              placeholder = "https://tanzil.net/trans/xx.name",
              width       = "100%"
            ),
            textInput(
              inputId     = paste0("custom_name_", i),
              label       = NULL,
              placeholder = paste0("Display Name #", i),
              width       = "100%"
            )
          )
        }),

        # Load custom translations button
        div(
          class = "d-grid mt-1",
          actionButton(
            inputId = "btn_load_custom",
            label   = tagList(bsicons::bs_icon("cloud-download"), " Load Custom Translations"),
            class   = "btn btn-outline-secondary btn-sm fw-semibold"
          )
        ),

        # Status message output
        uiOutput("custom_trans_status")
      )
    ),

    hr(style = "border-color:#30363d; margin:0.75rem 0;"),

    # --- Run Analysis Button --------------------------------------------------
    div(
      class = "d-grid",
      actionButton(
        inputId = "btn_apply",
        label   = tagList(bsicons::bs_icon("play-fill"), " Run Analysis"),
        class   = "btn btn-primary fw-semibold"
      )
    ),

    tags$p(
      class = "text-muted small text-center mt-2 mb-0",
      "Click \u2018Run Analysis\u2019 to update all panels."
    )
  )
}
