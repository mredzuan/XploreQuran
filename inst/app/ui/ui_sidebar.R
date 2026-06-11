# ==============================================================================
# ui/ui_sidebar.R
# Quran Translation Explorer - Sidebar: Translation Analytic Configuration
#
# Layout hierarchy:
#   Translation Analytic Configuration  (panel title, no icon)
#   ├─ Select Translation               [dropdown] [⬇ import]
#   ├─ Analyse by:
#   │   ├─ [Surah] [Juz]  radio
#   │   └─ Select Surah(s) / Select Juz  (conditional picklist)
#   ├─ Select Number for Top Term       [numericInput]
#   ├─ Text Pre-Processing              (collapsible accordion)
#   │   ├─ Remove Stop Words
#   │   ├─ Stem Words
#   │   └─ Remove Punctuation
#   └─ [Run Analysis]
# ==============================================================================

ui_sidebar <- function() {
  sidebar(
    id    = "main_sidebar",
    width = 300,

    # ── Panel Title ────────────────────────────────────────────────────────────
    div(
      class = "mb-4 text-center",
      tags$p(
        class = "fw-bold mb-0",
        style = "font-size:0.95rem; letter-spacing:0.02em; color:#58a6ff;",
        "Analytic Configuration"
      ),
      tags$p(
        class = "text-muted small mb-0",
        style = "font-size:0.7rem;",
        "Customize your Quran exploration"
      )
    ),

    # ── Select Translation ─────────────────────────────────────────────────────
    div(
      class = "mb-4",
      div(
        style = "display:flex; justify-content:space-between; align-items:center; margin-bottom:0.3rem;",
        tags$label(
          class = "form-label fw-semibold mb-0",
          style = "font-size:0.8rem; color:#c9d1d9;",
          "Translation"
        ),
        actionButton(
          inputId = "btn_open_custom_import",
          label   = bsicons::bs_icon("cloud-download"),
          title   = "Import a translation from tanzil.net",
          class   = "btn btn-sm btn-outline-secondary p-1",
          style   = "line-height:1; font-size:0.8rem; border-color:transparent;"
        )
      ),
      selectInput(
        inputId  = "sel_translation",
        label    = NULL,
        choices  = TRANSLATIONS,
        selected = "trans_en_sahih",
        width    = "100%"
      )
    ),

    # ── Analyse by: ────────────────────────────────────────────────────────────
    div(
      class = "mb-4",
      tags$label(
        class = "form-label fw-semibold mb-2",
        style = "font-size:0.8rem; color:#c9d1d9;",
        "Grouping Level"
      ),
      
      div(
        class = "mb-2",
        radioButtons(
          inputId  = "sel_by",
          label    = NULL,
          choices  = c("Surah" = "surah", "Juz" = "juz"),
          selected = "surah",
          inline   = TRUE
        )
      ),

      # Conditional: Select Surah(s)
      conditionalPanel(
        condition = "input.sel_by == 'surah'",
        selectInput(
          inputId  = "sel_surah",
          label    = tags$span(style="font-size:0.75rem; color:#8b949e;", "Select Surah(s)"),
          choices  = SURAH_CHOICES,
          multiple = TRUE,
          selected = NULL,
          width    = "100%"
        )
      ),

      # Conditional: Select Juz
      conditionalPanel(
        condition = "input.sel_by == 'juz'",
        selectInput(
          inputId  = "sel_juz",
          label    = tags$span(style="font-size:0.75rem; color:#8b949e;", "Select Juz"),
          choices  = setNames(JUZ_CHOICES, paste("Juz", JUZ_CHOICES)),
          multiple = TRUE,
          selected = NULL,
          width    = "100%"
        )
      )
    ),

    # ── Select Number for Top Term ─────────────────────────────────────────────
    div(
      class = "mb-4",
      tags$label(
        `for`  = "n_top_n",
        class  = "form-label fw-semibold mb-1",
        style  = "font-size:0.8rem; color:#c9d1d9;",
        "Top Terms Limit"
      ),
      numericInput(
        inputId = "n_top_n",
        label   = NULL,
        value   = 20,
        min     = 5,
        max     = 100,
        step    = 5,
        width   = "100%"
      )
    ),

    # ── Text Pre-Processing ────────────────────────────────────────────────────
    div(
      class = "mb-4 p-3",
      style = "background-color: #1c2128; border: 1px solid #30363d; border-radius: 0.5rem;",
      tags$label(
        class = "form-label fw-semibold mb-2",
        style = "font-size:0.8rem; color:#c9d1d9; display:block; border-bottom: 1px solid #30363d; padding-bottom:0.4rem;",
        "Text Pre-Processing"
      ),
      div(
        class = "pt-1 text-processing-checkboxes",
        checkboxInput("chk_stopwords", "Remove Stop Words",  value = TRUE),
        checkboxInput("chk_normalize", "Stem Words",         value = FALSE),
        checkboxInput("chk_special",   "Remove Punctuation", value = TRUE)
      )
    ),

    # ── Run Analysis ───────────────────────────────────────────────────────────
    div(
      class = "d-grid mt-4",
      actionButton(
        inputId = "btn_apply",
        label   = tagList(bsicons::bs_icon("play-fill"), " Run Analysis"),
        class   = "btn btn-primary fw-semibold"
      )
    )
  )
}
