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
    tags$p(
      class = "fw-bold mb-3 mt-1",
      style = "font-size:0.85rem; letter-spacing:0.02em; color:#58a6ff;
               border-bottom:1px solid #30363d; padding-bottom:0.5rem;",
      "Translation Analytic Configuration"
    ),

    # ── Select Translation ─────────────────────────────────────────────────────
    div(
      class = "mb-3",

      # Row: label left, import button right
      div(
        style = "display:flex; justify-content:space-between; align-items:center;
                 margin-bottom:5px;",
        tags$label(
          `for`  = "sel_translation",
          class  = "form-label fw-semibold mb-0",
          style  = "font-size:0.8rem; color:#c9d1d9;",
          "Select Translation"
        ),
        actionButton(
          inputId = "btn_open_custom_import",
          label   = bsicons::bs_icon("cloud-download"),
          title   = "Import a translation from tanzil.net",
          class   = "btn btn-sm btn-outline-secondary p-1",
          style   = "line-height:1; font-size:0.8rem;"
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
      class = "mb-1",
      tags$label(
        class = "form-label fw-semibold mb-1",
        style = "font-size:0.8rem; color:#c9d1d9;",
        "Analyse by:"
      ),

      # Radio buttons indented
      div(
        class = "ps-2",
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
        div(
          class = "ps-2 mb-2",
          tags$label(
            class = "form-label text-muted mb-1",
            style = "font-size:0.75rem;",
            "Select Surah(s)"
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

      # Conditional: Select Juz
      conditionalPanel(
        condition = "input.sel_by == 'juz'",
        div(
          class = "ps-2 mb-2",
          tags$label(
            class = "form-label text-muted mb-1",
            style = "font-size:0.75rem;",
            "Select Juz"
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
      )
    ),

    # ── Select Number for Top Term ─────────────────────────────────────────────
    div(
      class = "mb-3",
      tags$label(
        `for`  = "n_top_n",
        class  = "form-label fw-semibold mb-1",
        style  = "font-size:0.8rem; color:#c9d1d9;",
        "Select Number for Top Term"
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

    hr(style = "border-color:#30363d; margin:0.5rem 0 0.75rem 0;"),

    # ── Text Pre-Processing (collapsible) ──────────────────────────────────────
    accordion(
      id   = "acc_text_processing",
      open = FALSE,
      accordion_panel(
        title = "Text Pre-Processing",
        value = "panel_text_proc",
        div(
          class = "ps-1 pt-1",
          checkboxInput("chk_stopwords", "Remove Stop Words",  value = TRUE),
          checkboxInput("chk_normalize", "Stem Words",         value = FALSE),
          checkboxInput("chk_special",   "Remove Punctuation", value = TRUE)
        )
      )
    ),

    hr(style = "border-color:#30363d; margin:0.75rem 0;"),

    # ── Run Analysis ───────────────────────────────────────────────────────────
    div(
      class = "d-grid",
      actionButton(
        inputId = "btn_apply",
        label   = tagList(bsicons::bs_icon("play-fill"), " Run Analysis"),
        class   = "btn btn-primary fw-semibold"
      )
    ),

    tags$p(
      class = "text-muted mt-2 mb-0 text-center",
      style = "font-size:0.72rem;",
      "Configure above and click \u2018Run Analysis\u2019 to update all panels."
    )
  )
}
