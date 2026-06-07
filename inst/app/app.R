# ==============================================================================
# app.R
# XploreQuran - Main Shiny Application Entry Point
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

# ==============================================================================
# UI
# ==============================================================================

ui <- page_navbar(
  title  = tags$span(
    tags$img(src = "logo.png", height = "28px", style = "margin-right:8px;"),
    "XploreQuran"
  ),
  theme  = xplore_theme,
  id     = "main_navbar",
  lang   = "en",

  # Link custom CSS
  header = tags$head(
    tags$link(rel = "stylesheet", href = "custom.css"),
    tags$link(rel = "icon", type = "image/png", href = "logo.png"),
    tags$title("XploreQuran — Quran Translation Analytics")
  ),

  # Sidebar (shared across all tabs)
  sidebar = ui_sidebar(),

  # --- Navigation Tabs --------------------------------------------------------
  nav_panel(
    title = tagList(bsicons::bs_icon("house"),      " Overview"),
    value = "tab_overview",
    mod_overview_ui("overview")
  ),

  nav_panel(
    title = tagList(bsicons::bs_icon("bar-chart"),  " Words"),
    value = "tab_words",
    mod_word_analysis_ui("word_analysis")
  ),

  nav_panel(
    title = tagList(bsicons::bs_icon("emoji-smile"), " Sentiment"),
    value = "tab_sentiment",
    mod_sentiment_ui("sentiment")
  ),

  nav_panel(
    title = tagList(bsicons::bs_icon("cloud"),       " Wordcloud"),
    value = "tab_wordcloud",
    mod_wordcloud_ui("wordcloud")
  ),

  nav_panel(
    title = tagList(bsicons::bs_icon("diagram-3"),   " N-grams"),
    value = "tab_ngram",
    mod_ngram_ui("ngram")
  ),

  nav_panel(
    title = tagList(bsicons::bs_icon("lightbulb"),   " Topics"),
    value = "tab_topics",
    mod_topic_model_ui("topic_model")
  ),

  nav_panel(
    title = tagList(bsicons::bs_icon("diagram-2"),   " Network"),
    value = "tab_network",
    mod_network_ui("network")
  ),

  nav_panel(
    title = tagList(bsicons::bs_icon("arrow-left-right"), " Compare"),
    value = "tab_compare",
    mod_compare_ui("compare")
  ),

  # Right-side spacer + version label
  nav_spacer(),
  nav_item(
    tags$span(
      class = "text-muted small",
      style = "line-height:2.5rem;",
      paste0("v", packageVersion("XploreQuran"))
    )
  )
)

# ==============================================================================
# SERVER
# ==============================================================================

server <- function(input, output, session) {

  # ---------------------------------------------------------------------------
  # Reactive: Load & filter raw translation data
  # Triggered only when user clicks "Run Analysis"
  # ---------------------------------------------------------------------------
  quran_df <- eventReactive(input$btn_apply, {
    req(input$sel_translation)

    withProgress(message = "Loading translation data…", value = 0.3, {
      df <- load_translation(input$sel_translation)

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
  ignoreNULL = FALSE,   # Run once on startup with defaults
  ignoreInit = FALSE
  )

  # ---------------------------------------------------------------------------
  # Reactive: Tokenised data (shared across all modules)
  # ---------------------------------------------------------------------------
  tokens_df <- reactive({
    req(quran_df())

    # Detect language from dataset name for stop word selection
    sw_lang <- dplyr::case_when(
      grepl("_ms_",  input$sel_translation) ~ "ms",
      grepl("_id_",  input$sel_translation) ~ "id",
      TRUE                                  ~ "en"
    )

    tokenise_translation(
      df           = quran_df(),
      remove_sw    = isTRUE(input$chk_stopwords),
      sw_lang      = sw_lang,
      remove_words = NULL
    )
  })

  # Convenience reactive for top_n (passed to word module)
  top_n <- reactive({ as.integer(input$sl_top_n) })

  # ---------------------------------------------------------------------------
  # Module Server Calls
  # ---------------------------------------------------------------------------
  mod_overview_server("overview",      quran_df  = quran_df, tokens_df = tokens_df)
  mod_word_analysis_server("word_analysis", tokens_df = tokens_df, top_n = top_n)
  mod_sentiment_server("sentiment",    tokens_df = tokens_df)
  mod_wordcloud_server("wordcloud",    tokens_df = tokens_df)
  mod_ngram_server("ngram",            quran_df  = quran_df)
  mod_topic_model_server("topic_model", tokens_df = tokens_df)
  mod_network_server("network",        tokens_df = tokens_df)
  mod_compare_server("compare")

}

# ==============================================================================
# Run
# ==============================================================================
shinyApp(ui = ui, server = server)
