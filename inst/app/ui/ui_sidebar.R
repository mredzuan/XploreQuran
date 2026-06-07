# ==============================================================================
# ui/ui_sidebar.R
# XploreQuran - Shared Sidebar Controls
# These inputs are shared reactives passed into all feature modules.
# ==============================================================================

ui_sidebar <- function() {
  sidebar(
    id    = "main_sidebar",
    width = 280,

    # --- Brand ------------------------------------------------------------------
    div(
      class = "text-center mb-3",
      tags$img(src = "logo.png", height = "48px", alt = "XploreQuran logo",
               style = "opacity:0.9;"),
      tags$p(class = "text-muted small mt-1 mb-0", "Quran Translation Analytics")
    ),

    hr(style = "border-color:#30363d;"),

    # --- Translation Selection -------------------------------------------------
    selectInput(
      inputId  = "sel_translation",
      label    = tags$span(icon("book-open"), " Translation"),
      choices  = TRANSLATIONS,
      selected = "trans_en_sahih",
      width    = "100%"
    ),

    # --- Grouping Dimension ---------------------------------------------------
    radioButtons(
      inputId  = "sel_by",
      label    = tags$span(icon("layer-group"), " Analyse By"),
      choices  = c("Surah" = "surah", "Juz" = "juz"),
      selected = "surah",
      inline   = TRUE
    ),

    # --- Surah Selector (shown when by = surah) --------------------------------
    conditionalPanel(
      condition = "input.sel_by == 'surah'",
      selectInput(
        inputId  = "sel_surah",
        label    = "Select Surah(s)",
        choices  = SURAH_CHOICES,
        multiple = TRUE,
        selected = NULL,
        width    = "100%"
      )
    ),

    # --- Juz Selector (shown when by = juz) -----------------------------------
    conditionalPanel(
      condition = "input.sel_by == 'juz'",
      selectInput(
        inputId  = "sel_juz",
        label    = "Select Juz",
        choices  = setNames(JUZ_CHOICES, paste("Juz", JUZ_CHOICES)),
        multiple = TRUE,
        selected = NULL,
        width    = "100%"
      )
    ),

    hr(style = "border-color:#30363d;"),

    # --- Text Preprocessing ---------------------------------------------------
    tags$p(class = "text-muted small fw-semibold mb-1",
           icon("sliders"), " Preprocessing"),

    checkboxInput("chk_stopwords",  "Remove Stop Words",    value = TRUE),
    checkboxInput("chk_normalize",  "Stem Words",           value = FALSE),
    checkboxInput("chk_special",    "Remove Punctuation",   value = TRUE),

    # --- Top N terms -----------------------------------------------------------
    sliderInput(
      inputId = "sl_top_n",
      label   = "Top N Terms",
      min = 5, max = 50, value = 20, step = 5,
      width = "100%"
    ),

    hr(style = "border-color:#30363d;"),

    # --- Apply Button ----------------------------------------------------------
    div(
      class = "d-grid",
      actionButton(
        inputId = "btn_apply",
        label   = tagList(icon("play"), " Run Analysis"),
        class   = "btn btn-primary fw-semibold"
      )
    ),

    tags$p(
      class = "text-muted small text-center mt-2 mb-0",
      "Click 'Run Analysis' to update all panels."
    )
  )
}
