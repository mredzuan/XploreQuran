# ==============================================================================
# app.R
# Quran Translation Explorer - Main Shiny Application Entry Point
# Run via: XploreQuran::run_app()  or  shiny::runApp("inst/app")
# ==============================================================================

# --- Source shared files ------------------------------------------------------
source("global.R")
source("ui/ui_theme.R")
source("ui/ui_sidebar.R")

# --- Source modules -----------------------------------------------------------
source("modules/mod_overview.R")
source("modules/mod_word_analysis.R")
source("modules/mod_sentiment.R")
source("modules/mod_wordcloud.R")
source("modules/mod_ngram.R")
source("modules/mod_topic_model.R")
source("modules/mod_network.R")
source("modules/mod_compare.R")
source("modules/mod_quran_viewer.R")
source("modules/mod_custom_import.R")
source("modules/mod_quran_standalone.R")

# ==============================================================================
# UI ROUTING
# ==============================================================================

# ---------------------------------------------------------------------------
# Main App UI
# ---------------------------------------------------------------------------
ui_main <- page_navbar(
  title = tags$span(
    tags$img(src = "logo.png", height = "32px",
             style = "margin-right:10px; vertical-align:middle;"),
    tags$span("Quran Translation Explorer", style = "vertical-align:middle;")
  ),
  theme  = xplore_theme,
  id     = "main_navbar",
  lang   = "en",

  # Page-level head tags
  header = tags$head(
    tags$link(rel = "stylesheet", href = "custom.css"),
    tags$link(rel = "icon", type = "image/png", href = "logo.png"),
    tags$title("Quran Translation Explorer")
  ),

  # Sidebar
  sidebar = ui_sidebar(),

  # --- Navigation Tabs --------------------------------------------------------
  nav_panel(
    title = tagList(bsicons::bs_icon("house"),            " Overview"),
    value = "tab_overview",
    mod_overview_ui("overview")
  ),

  nav_panel(
    title = tagList(bsicons::bs_icon("bar-chart"),        " Words"),
    value = "tab_words",
    mod_word_analysis_ui("word_analysis")
  ),

  nav_panel(
    title = tagList(bsicons::bs_icon("emoji-smile"),      " Sentiment"),
    value = "tab_sentiment",
    mod_sentiment_ui("sentiment")
  ),

  nav_panel(
    title = tagList(bsicons::bs_icon("cloud"),            " Wordcloud"),
    value = "tab_wordcloud",
    mod_wordcloud_ui("wordcloud")
  ),

  nav_panel(
    title = tagList(bsicons::bs_icon("diagram-3"),        " N-grams"),
    value = "tab_ngram",
    mod_ngram_ui("ngram")
  ),

  nav_panel(
    title = tagList(bsicons::bs_icon("lightbulb"),        " Topics"),
    value = "tab_topics",
    mod_topic_model_ui("topic_model")
  ),

  nav_panel(
    title = tagList(bsicons::bs_icon("diagram-2"),        " Network"),
    value = "tab_network",
    mod_network_ui("network")
  ),

  nav_panel(
    title = tagList(bsicons::bs_icon("arrow-left-right"), " Compare"),
    value = "tab_compare",
    mod_compare_ui("compare")
  ),

  # Right-side version label
  nav_spacer(),
  nav_item(
    tags$span(
      class = "text-muted small",
      style = "line-height:2.5rem;",
      paste0("v", packageVersion("XploreQuran"))
    )
  ),

  # Floating Quran Viewer FAB (page footer — always visible)
  footer = mod_quran_viewer_ui("quran_viewer")
)

# ---------------------------------------------------------------------------
# Router: Check query string to determine which UI to render
# ---------------------------------------------------------------------------
ui <- function(req) {
  query <- shiny::parseQueryString(req$QUERY_STRING)
  if (!is.null(query$viewer) && query$viewer == "1") {
    # Provide the standalone UI for the viewer
    # We populate the choices manually since it's initial load. 
    # The server will handle updating if needed, but for standalone it's mostly static.
    # To keep it simple, we use TRANSLATIONS list directly.
    page_fluid(
      theme = xplore_theme,
      tags$head(
        tags$link(rel = "stylesheet", href = "custom.css"),
        tags$link(rel = "icon", type = "image/png", href = "logo.png"),
        tags$title("Quran Viewer")
      ),
      mod_quran_standalone_ui("viewer_standalone", TRANSLATIONS, "trans_en_sahih")
    )
  } else {
    ui_main
  }
}

# ==============================================================================
# SERVER
# ==============================================================================

server <- function(input, output, session) {

  # ---------------------------------------------------------------------------
  # Shared custom translations store
  # reactiveVal: named list of key -> list(data, lang, name, key)
  # Updated by mod_custom_import_server; read by all_translations & viewer
  # ---------------------------------------------------------------------------
  custom_trans_rv <- reactiveVal(list())

  # ---------------------------------------------------------------------------
  # Merged translation choices: built-in TRANSLATIONS + any custom entries
  # Consumed by: sel_translation dropdown, Quran Viewer modal
  # ---------------------------------------------------------------------------
  all_translations <- reactive({
    custom <- custom_trans_rv()
    custom_choices <- if (length(custom) > 0) {
      setNames(
        lapply(custom, function(x) x$key),
        lapply(custom, function(x) x$name)
      )
    } else {
      list()
    }
    c(TRANSLATIONS, custom_choices)
  })

  # Sync the sidebar Translation dropdown whenever custom translations change
  observe({
    updateSelectInput(
      session  = session,
      inputId  = "sel_translation",
      choices  = all_translations(),
      selected = isolate(input$sel_translation) %||% "trans_en_sahih"
    )
  })

  # ---------------------------------------------------------------------------
  # Trigger for custom import modal — passed as reactive to the module
  # so the module can observe it without crossing namespace boundaries
  # ---------------------------------------------------------------------------
  custom_import_trigger <- reactiveVal(0)

  observeEvent(input$btn_open_custom_import, {
    custom_import_trigger(isolate(custom_import_trigger()) + 1)
  })

  # ---------------------------------------------------------------------------
  # Module Server Calls
  # ---------------------------------------------------------------------------

  # Custom import modal (reads/writes custom_trans_rv; observes trigger)
  mod_custom_import_server(
    "custom_import",
    custom_trans_rv = custom_trans_rv,
    open_trigger    = custom_import_trigger
  )

  # Quran Viewer FAB modal (reads all_translations for picklist)
  mod_quran_viewer_server("quran_viewer", all_translations = all_translations)

  # Standalone Quran Viewer (served on ?viewer=1)
  mod_quran_standalone_server("viewer_standalone")

  # ---------------------------------------------------------------------------
  # Helper: resolve a translation key -> data frame
  # Works for both built-in dataset names and custom keys
  # ---------------------------------------------------------------------------
  resolve_translation_df <- function(key) {
    custom <- custom_trans_rv()
    if (key %in% names(custom)) return(custom[[key]]$data)
    load_translation(key)
  }

  # ---------------------------------------------------------------------------
  # Reactive: ISO language code for the currently selected translation
  # ---------------------------------------------------------------------------
  active_lang <- reactive({
    key    <- req(input$sel_translation)
    custom <- custom_trans_rv()

    if (key %in% names(custom)) return(custom[[key]]$lang)

    dplyr::case_when(
      grepl("_ms_", key) ~ "ms",
      grepl("_id_", key) ~ "id",
      TRUE               ~ "en"
    )
  })

  # ---------------------------------------------------------------------------
  # Reactive: Raw (filtered) translation data frame
  # Triggered only when user clicks Run Analysis
  # ---------------------------------------------------------------------------
  quran_df <- eventReactive(input$btn_apply, {
    req(input$sel_translation)

    withProgress(message = "Loading translation data\u2026", value = 0.3, {
      df <- resolve_translation_df(input$sel_translation)

      sub_by <- if (input$sel_by == "surah") {
        as.integer(input$sel_surah)
      } else {
        as.integer(input$sel_juz)
      }

      df <- filter_quran(df, by = input$sel_by, sub_by = sub_by)
      incProgress(1)
      df
    })
  },
  ignoreNULL = FALSE,
  ignoreInit = FALSE
  )

  # ---------------------------------------------------------------------------
  # Reactive: Tokenised words (shared across all analysis modules)
  # ---------------------------------------------------------------------------
  tokens_df <- reactive({
    req(quran_df())
    tokenise_translation(
      df           = quran_df(),
      remove_sw    = isTRUE(input$chk_stopwords),
      sw_lang      = active_lang(),
      remove_words = NULL
    )
  })

  # Top N — captured only when Run Analysis is clicked, not on every keystroke
  top_n <- eventReactive(input$btn_apply, {
    val <- as.integer(input$n_top_n)
    # Clamp to sensible range in case user types outside 5-100
    min(max(val, 5L), 100L)
  }, ignoreNULL = FALSE, ignoreInit = FALSE)

  # ---------------------------------------------------------------------------
  # Analysis Module Server Calls
  # (placed after all reactives are defined)
  # ---------------------------------------------------------------------------
  mod_overview_server("overview",           quran_df  = quran_df, tokens_df = tokens_df)
  mod_word_analysis_server("word_analysis", tokens_df = tokens_df, top_n = top_n)
  mod_sentiment_server("sentiment",         tokens_df = tokens_df)
  mod_wordcloud_server("wordcloud",         tokens_df = tokens_df)
  mod_ngram_server("ngram",                 quran_df  = quran_df)
  mod_topic_model_server("topic_model",     tokens_df = tokens_df)
  mod_network_server("network",             tokens_df = tokens_df)
  mod_compare_server("compare")

}

# ==============================================================================
# Run
# ==============================================================================
shinyApp(ui = ui, server = server)
