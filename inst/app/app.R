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
  theme  = xplore_theme_light,
  id     = "main_navbar",
  lang   = "en",

  # Page-level head tags and Floating theme toggle
  header = tagList(
    tags$head(
      tags$link(rel = "stylesheet", href = "custom.css"),
      tags$link(rel = "icon", type = "image/png", href = "logo.png"),
      tags$title("Quran Translation Explorer"),
      tags$script("
        Shiny.addCustomMessageHandler('toggle_theme_class', function(message) {
          if(message === 'Dark') {
            document.body.classList.add('dark-mode');
          } else {
            document.body.classList.remove('dark-mode');
          }
        });
      ")
    ),
    tags$div(
      class = "theme-toggle-container",
      tags$span(
        class = "theme-toggle-version",
        paste0("v", packageVersion("XploreQuran"))
      ),
      tags$div(
        class = "theme-toggle-wrapper",
        tags$input(
          type = "checkbox",
          id = "theme_toggle",
          class = "theme-toggle-input"
        ),
        tags$label(
          `for` = "theme_toggle",
          class = "theme-toggle-label",
          tags$span(
            class = "theme-toggle-icon light-icon",
            bsicons::bs_icon("sun-fill")
          ),
          tags$span(
            class = "theme-toggle-icon dark-icon",
            bsicons::bs_icon("moon-fill")
          ),
          tags$span(class = "theme-toggle-ball")
        )
      )
    )
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

  nav_panel(
    title = tagList(bsicons::bs_icon("info-circle"), " About"),
    value = "tab_about",
    card(
      card_header(bsicons::bs_icon("info-circle"), " About XploreQuran"),
      card_body(
        p("Welcome to ", strong("XploreQuran"), " — an interactive text mining
          and analytics platform for Quran translations."),
        p("Use the sidebar to select a translation, choose a Surah range,
          configure preprocessing options, and click ", strong("Run Analysis"),
          " to explore the results across all tabs."),
        tags$ul(
          tags$li(strong("Word Analysis:"), " Word frequency, TF-IDF rankings."),
          tags$li(strong("Sentiment:"), " Positive/negative/neutral tone by section."),
          tags$li(strong("Wordcloud:"), " Visual frequency map of key terms."),
          tags$li(strong("N-grams:"), " Bigrams, trigrams and word correlations."),
          tags$li(strong("Topic Model:"), " Latent Dirichlet Allocation (LDA) topics."),
          tags$li(strong("Network:"), " Word co-occurrence graph."),
          tags$li(strong("Compare:"), " Side-by-side translation comparison.")
        )
      )
    )
  ),

  # Right-side version label and theme toggle removed (now floating in header)

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
      theme = xplore_theme_light,
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
  # Theme toggle logic (custom sliding checkbox widget)
  # ---------------------------------------------------------------------------
  observeEvent(input$theme_toggle, {
    if (isTRUE(input$theme_toggle)) {
      session$setCurrentTheme(xplore_theme_dark)
      session$sendCustomMessage("toggle_theme_class", "Dark")
    } else {
      session$setCurrentTheme(xplore_theme_light)
      session$sendCustomMessage("toggle_theme_class", "Light")
    }
  }, ignoreInit = TRUE)

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

      # Handle "All" selection (or empty select) to display all surahs
      sub_by <- if ("All" %in% input$sel_surah || is.null(input$sel_surah) || length(input$sel_surah) == 0) {
        NULL
      } else {
        as.integer(input$sel_surah)
      }

      df <- filter_quran(df, by = "surah", sub_by = sub_by)
      incProgress(1)
      df
    })
  },
  ignoreNULL = FALSE,
  ignoreInit = FALSE
  )

  # Parse custom removed words (comma-separated input)
  remove_words_vec <- reactive({
    if (is.null(input$txt_remove_words) || input$txt_remove_words == "") {
      return(NULL)
    }
    words <- strsplit(input$txt_remove_words, ",")[[1]]
    words <- trimws(words)
    words <- words[words != ""]
    if (length(words) == 0) return(NULL)
    words
  })

  # ---------------------------------------------------------------------------
  # Reactive: Tokenised words (shared across all analysis modules)
  # ---------------------------------------------------------------------------
  tokens_df <- reactive({
    req(quran_df())
    tokenise_translation(
      df           = quran_df(),
      remove_sw    = isTRUE(input$chk_stopwords),
      sw_lang      = active_lang(),
      remove_words = remove_words_vec(),
      normalize    = isTRUE(input$chk_normalize)
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
  mod_overview_server("overview",           quran_df  = quran_df, tokens_df = tokens_df, top_n = top_n, is_dark = reactive({ isTRUE(input$theme_toggle) }))
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
