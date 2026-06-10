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

# ==============================================================================
# UI
# ==============================================================================

ui <- page_navbar(
  title = tags$span(
    tags$img(src = "logo.png", height = "32px",
             style = "margin-right:10px; vertical-align:middle;"),
    tags$span("Quran Translation Explorer", style = "vertical-align:middle;")
  ),
  theme  = xplore_theme,
  id     = "main_navbar",
  lang   = "en",

  # Link custom CSS + page meta
  header = tags$head(
    tags$link(rel = "stylesheet", href = "custom.css"),
    tags$link(rel = "icon", type = "image/png", href = "logo.png"),
    tags$title("Quran Translation Explorer")
  ),

  # Sidebar (shared across all tabs)
  sidebar = ui_sidebar(),

  # --- Navigation Tabs --------------------------------------------------------
  nav_panel(
    title = tagList(bsicons::bs_icon("house"),           " Overview"),
    value = "tab_overview",
    mod_overview_ui("overview")
  ),

  nav_panel(
    title = tagList(bsicons::bs_icon("bar-chart"),       " Words"),
    value = "tab_words",
    mod_word_analysis_ui("word_analysis")
  ),

  nav_panel(
    title = tagList(bsicons::bs_icon("emoji-smile"),     " Sentiment"),
    value = "tab_sentiment",
    mod_sentiment_ui("sentiment")
  ),

  nav_panel(
    title = tagList(bsicons::bs_icon("cloud"),           " Wordcloud"),
    value = "tab_wordcloud",
    mod_wordcloud_ui("wordcloud")
  ),

  nav_panel(
    title = tagList(bsicons::bs_icon("diagram-3"),       " N-grams"),
    value = "tab_ngram",
    mod_ngram_ui("ngram")
  ),

  nav_panel(
    title = tagList(bsicons::bs_icon("lightbulb"),       " Topics"),
    value = "tab_topics",
    mod_topic_model_ui("topic_model")
  ),

  nav_panel(
    title = tagList(bsicons::bs_icon("diagram-2"),       " Network"),
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

  # Floating Quran Viewer button (injected at page level)
  footer = mod_quran_viewer_ui("quran_viewer")
)

# ==============================================================================
# SERVER
# ==============================================================================

server <- function(input, output, session) {

  # ---------------------------------------------------------------------------
  # Custom translations store
  # reactiveVal holds a named list: key -> list(data, lang, name, key)
  # ---------------------------------------------------------------------------
  custom_trans_rv <- reactiveVal(list())

  # ---------------------------------------------------------------------------
  # Merged translation choices: built-in + custom
  # Used by the main selector AND the Quran Viewer modal
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

  # Sync sel_translation dropdown when custom translations change
  observe({
    updateSelectInput(session, "sel_translation",
                      choices  = all_translations(),
                      selected = isolate(input$sel_translation))
  })

  # ---------------------------------------------------------------------------
  # Observer: Load Custom Translations button
  # ---------------------------------------------------------------------------
  output$custom_trans_status <- renderUI({ NULL })   # initialise empty

  observeEvent(input$btn_load_custom, {

    store    <- list()
    messages <- list()

    for (i in seq_len(4)) {
      url  <- input[[paste0("custom_url_",  i)]]
      name <- input[[paste0("custom_name_", i)]]

      # Skip blank rows
      if (is.null(url) || trimws(url) == "") next

      result <- tryCatch(
        load_custom_translation(url, name),
        error = function(e) {
          showNotification(
            tagList(
              tags$strong(paste0("Custom #", i, " — Error:")),
              tags$br(), conditionMessage(e)
            ),
            type     = "error",
            duration = 10
          )
          NULL
        }
      )

      if (!is.null(result)) {
        store[[result$key]] <- result

        # Alert if language was auto-defaulted to English
        if (isTRUE(result$lang_defaulted)) {
          showNotification(
            tagList(
              tags$strong(paste0("\u2139\ufe0f  Custom #", i, " — Language Notice")),
              tags$br(),
              paste0(
                "The language code for \u2018", result$name,
                "\u2019 could not be detected from the URL. ",
                "Stop word removal will default to English (en). ",
                "This may affect text analysis quality for non-English translations."
              )
            ),
            type     = "warning",
            duration = 12
          )
        }

        messages[[length(messages) + 1]] <- paste0(
          "\u2713 Loaded: ", result$name,
          " (language: ", toupper(result$lang), ")"
        )
      }
    }

    # Update the store
    custom_trans_rv(store)

    # Show summary status in sidebar
    output$custom_trans_status <- renderUI({
      if (length(messages) == 0) {
        div(class = "text-muted small mt-2",
            bsicons::bs_icon("exclamation-circle"), " No translations loaded.")
      } else {
        div(
          class = "mt-2",
          purrr::map(messages, function(m) {
            tags$p(class = "text-success small mb-1",
                   bsicons::bs_icon("check-circle"), " ", m)
          })
        )
      }
    })

    if (length(messages) > 0) {
      showNotification(
        paste0(length(messages), " custom translation(s) loaded successfully."),
        type = "message", duration = 5
      )
    }
  })

  # ---------------------------------------------------------------------------
  # Helper: resolve a translation key to a data frame
  # Handles both built-in dataset names and custom keys
  # ---------------------------------------------------------------------------
  resolve_translation_df <- function(key) {
    custom <- custom_trans_rv()
    if (key %in% names(custom)) {
      return(custom[[key]]$data)
    }
    load_translation(key)
  }

  # ---------------------------------------------------------------------------
  # Reactive: language code for the active translation
  # ---------------------------------------------------------------------------
  active_lang <- reactive({
    key    <- input$sel_translation
    custom <- custom_trans_rv()

    if (key %in% names(custom)) {
      return(custom[[key]]$lang)
    }

    dplyr::case_when(
      grepl("_ms_", key) ~ "ms",
      grepl("_id_", key) ~ "id",
      TRUE               ~ "en"
    )
  })

  # ---------------------------------------------------------------------------
  # Reactive: Load & filter raw translation data
  # Triggered only when user clicks "Run Analysis"
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
  # Reactive: Tokenised data (shared across all modules)
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

  # Convenience reactive for top_n (passed to word module)
  top_n <- reactive({ as.integer(input$sl_top_n) })

  # ---------------------------------------------------------------------------
  # Module Server Calls
  # ---------------------------------------------------------------------------
  mod_overview_server("overview",       quran_df  = quran_df, tokens_df = tokens_df)
  mod_word_analysis_server("word_analysis", tokens_df = tokens_df, top_n = top_n)
  mod_sentiment_server("sentiment",     tokens_df = tokens_df)
  mod_wordcloud_server("wordcloud",     tokens_df = tokens_df)
  mod_ngram_server("ngram",             quran_df  = quran_df)
  mod_topic_model_server("topic_model", tokens_df = tokens_df)
  mod_network_server("network",         tokens_df = tokens_df)
  mod_compare_server("compare")
  mod_quran_viewer_server("quran_viewer", all_translations = all_translations)

}

# ==============================================================================
# Run
# ==============================================================================
shinyApp(ui = ui, server = server)
