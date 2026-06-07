# ==============================================================================
# modules/mod_overview.R
# XploreQuran - Overview / Dashboard Module
# Displays summary statistics and a welcome panel.
# ==============================================================================

# --- UI -----------------------------------------------------------------------
mod_overview_ui <- function(id) {
  ns <- NS(id)

  tagList(
    # Top stats row
    layout_columns(
      col_widths = c(3, 3, 3, 3),
      fill = FALSE,
      value_box(
        id       = ns("vb_ayahs"),
        title    = "Total Ayahs",
        value    = textOutput(ns("n_ayahs")),
        showcase = bsicons::bs_icon("book"),
        theme    = "primary"
      ),
      value_box(
        id       = ns("vb_surahs"),
        title    = "Surahs Selected",
        value    = textOutput(ns("n_surahs")),
        showcase = bsicons::bs_icon("layers"),
        theme    = "secondary"
      ),
      value_box(
        id       = ns("vb_words"),
        title    = "Total Words",
        value    = textOutput(ns("n_words")),
        showcase = bsicons::bs_icon("alphabet"),
        theme    = "info"
      ),
      value_box(
        id       = ns("vb_unique"),
        title    = "Unique Words",
        value    = textOutput(ns("n_unique")),
        showcase = bsicons::bs_icon("stars"),
        theme    = "success"
      )
    ),

    # About card
    card(
      card_header(bsicons::bs_icon("info-circle"), " About XploreQuran"),
      card_body(
        p("Welcome to ", strong("XploreQuran"), " — an interactive text mining
          and analytics platform for Quran translations."),
        p("Use the sidebar to select a translation, choose a Surah or Juz range,
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
    ),

    # Ayah words per surah chart
    card(
      card_header(bsicons::bs_icon("bar-chart"), " Word Count by Surah"),
      card_body(
        plotlyOutput(ns("plot_surah_words"), height = "380px")
      )
    )
  )
}

# --- Server -------------------------------------------------------------------
mod_overview_server <- function(id, quran_df, tokens_df) {
  moduleServer(id, function(input, output, session) {

    output$n_ayahs <- renderText({
      req(quran_df())
      format(nrow(quran_df()), big.mark = ",")
    })

    output$n_surahs <- renderText({
      req(quran_df())
      n_distinct(quran_df()$surah_id)
    })

    output$n_words <- renderText({
      req(tokens_df())
      format(nrow(tokens_df()), big.mark = ",")
    })

    output$n_unique <- renderText({
      req(tokens_df())
      format(n_distinct(tokens_df()$word), big.mark = ",")
    })

    output$plot_surah_words <- renderPlotly({
      req(tokens_df())

      wc <- tokens_df() |>
        count(surah_id, surah_title_en, sort = FALSE) |>
        arrange(surah_id)

      plot_ly(
        data   = wc,
        x      = ~surah_id,
        y      = ~n,
        type   = "bar",
        marker = list(
          color = ~n,
          colorscale = "Blues",
          showscale  = FALSE
        ),
        hovertemplate = paste0(
          "<b>%{customdata}</b><br>",
          "Words: %{y:,}<extra></extra>"
        ),
        customdata = ~surah_title_en
      ) |>
        layout(
          paper_bgcolor = "rgba(0,0,0,0)",
          plot_bgcolor  = "rgba(0,0,0,0)",
          font  = list(color = "#e6edf3", family = "Inter"),
          xaxis = list(
            title      = "Surah Number",
            gridcolor  = "#30363d",
            zerolinecolor = "#30363d"
          ),
          yaxis = list(
            title      = "Word Count",
            gridcolor  = "#30363d",
            zerolinecolor = "#30363d"
          ),
          hoverlabel = list(bgcolor = "#1c2128", font = list(color = "#e6edf3"))
        )
    })

  })
}
