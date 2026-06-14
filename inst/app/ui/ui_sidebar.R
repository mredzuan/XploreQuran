# ==============================================================================
# ui/ui_sidebar.R
# Quran Translation Explorer - Sidebar: Translation Analytic Configuration
#
# Layout hierarchy:
#   Translation Analytic Configuration  (panel title, no icon)
#   ├─ Select Translation               [dropdown] [⬇ import]
#   ├─ Select Surah(s)                  [dropdown multiple]
#   ├─ Text Pre-Processing              [themed group]
#   │   ├─ Remove Stop Words            [checkbox]
#   │   ├─ Stem Words                   [checkbox]
#   │   └─ Remove Punctuation           [checkbox]
#   ├─ Top Terms Limit                  [numericInput] [info-circle tooltip]
#   └─ [Run Analysis]                   [actionButton]
# ==============================================================================

ui_sidebar <- function() {
  sidebar(
    id    = "main_sidebar",
    width = 300,

    # ── Panel Title ────────────────────────────────────────────────────────────
    div(
      class = "mb-2 text-center",
      tags$p(
        class = "fw-bold mb-0",
        style = "font-size:0.95rem; letter-spacing:0.02em; color: var(--cq-primary);",
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
      class = "mb-2",
      div(
        style = "display:flex; justify-content:space-between; align-items:center; margin-bottom:0.3rem;",
        tags$label(
          class = "form-label fw-semibold mb-0",
          style = "font-size:0.8rem; color: var(--cq-fg);",
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

    # ── Select Surah(s) ────────────────────────────────────────────────────────
    div(
      class = "mb-2",
      tags$label(
        `for`  = "sel_surah",
        class  = "form-label fw-semibold mb-1",
        style  = "font-size:0.8rem; color: var(--cq-fg);",
        "Select Surah(s)"
      ),
      selectInput(
        inputId  = "sel_surah",
        label    = NULL,
        choices  = c("All Surahs" = "All", SURAH_CHOICES),
        multiple = TRUE,
        selected = "All",
        width    = "100%"
      )
    ),

    # ── Text Pre-Processing ────────────────────────────────────────────────────
    div(
      class = "mb-2 p-3",
      style = "background-color: var(--cq-card-bg); border: 1px solid var(--cq-card-border); border-radius: 0.5rem;",
      tags$label(
        class = "form-label fw-semibold mb-2",
        style = "font-size:0.8rem; color: var(--cq-fg); display:block; border-bottom: 1px solid var(--cq-card-border); padding-bottom:0.4rem;",
        "Text Pre-Processing"
      ),
      div(
        class = "pt-1 text-processing-checkboxes",
        checkboxInput("chk_stopwords", "Remove Stop Words",  value = TRUE),
        checkboxInput("chk_normalize", "Stem Words",         value = TRUE),
        checkboxInput("chk_special",   "Remove Punctuation", value = TRUE)
      )
    ),

    # ── Remove Word(s) ─────────────────────────────────────────────────────────
    div(
      class = "mb-2",
      tags$label(
        `for`  = "txt_remove_words",
        class  = "form-label fw-semibold mb-1",
        style  = "font-size:0.8rem; color: var(--cq-fg);",
        "Remove Custom Words"
      ),
      textInput(
        inputId     = "txt_remove_words",
        label       = NULL,
        value       = "",
        placeholder = "e.g. word1, word2 (comma separated)",
        width       = "100%"
      )
    ),

    # ── Select Number for Top Term (Top Terms Limit) ───────────────────────────
    div(
      class = "mb-2",
      div(
        style = "display:flex; align-items:center; gap:0.3rem; margin-bottom:0.3rem;",
        tags$label(
          `for`  = "n_top_n",
          class  = "form-label fw-semibold mb-0",
          style  = "font-size:0.8rem; color: var(--cq-fg);",
          "Top Terms Limit"
        ),
        bslib::tooltip(
          bsicons::bs_icon("info-circle", class = "text-muted", style = "font-size:0.8rem; cursor:pointer;"),
          "Controls the maximum number of most frequent words to display in analysis charts and word clouds.",
          placement = "right"
        )
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

    # ── Run Analysis ───────────────────────────────────────────────────────────
    div(
      class = "d-grid mt-3",
      actionButton(
        inputId = "btn_apply",
        label   = tagList(bsicons::bs_icon("play-fill"), " Run Analysis"),
        class   = "btn btn-primary fw-semibold"
      )
    )
  )
}
