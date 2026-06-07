# ==============================================================================
# modules/mod_word_analysis.R
# XploreQuran - Word Frequency & TF-IDF Module
# ==============================================================================

# --- UI -----------------------------------------------------------------------
mod_word_analysis_ui <- function(id) {
  ns <- NS(id)

  tagList(
    navset_card_tab(
      id = ns("tabs_word"),

      # Tab 1: Top N Word Frequency
      nav_panel(
        "Word Frequency",
        card_body(
          layout_columns(
            col_widths = c(9, 3),
            plotlyOutput(ns("plot_freq"), height = "420px"),
            div(
              tags$p(class = "text-muted small fw-semibold", "Options"),
              radioButtons(
                ns("freq_group"),
                "Group by",
                choices  = c("Overall" = "overall", "Surah" = "surah"),
                selected = "overall"
              ),
              hr(),
              downloadButton(ns("dl_freq"), "Download CSV",
                             class = "btn btn-sm btn-outline-secondary w-100")
            )
          )
        )
      ),

      # Tab 2: TF-IDF
      nav_panel(
        "TF-IDF",
        card_body(
          layout_columns(
            col_widths = c(9, 3),
            plotlyOutput(ns("plot_tfidf"), height = "420px"),
            div(
              tags$p(class = "text-muted small fw-semibold", "Top N per Surah"),
              sliderInput(ns("tfidf_top"), NULL, min = 3, max = 15, value = 5, step = 1),
              hr(),
              downloadButton(ns("dl_tfidf"), "Download CSV",
                             class = "btn btn-sm btn-outline-secondary w-100")
            )
          )
        )
      )
    )
  )
}

# --- Server -------------------------------------------------------------------
mod_word_analysis_server <- function(id, tokens_df, top_n) {
  moduleServer(id, function(input, output, session) {

    # --- Word Frequency -------------------------------------------------------
    freq_data <- reactive({
      req(tokens_df())

      if (input$freq_group == "overall") {
        tokens_df() |> count(word, sort = TRUE) |> slice_head(n = top_n())
      } else {
        tokens_df() |>
          count(surah_title_en, word, sort = TRUE) |>
          group_by(surah_title_en) |>
          slice_max(n, n = top_n()) |>
          ungroup()
      }
    })

    output$plot_freq <- renderPlotly({
      req(freq_data())
      df <- freq_data()

      if (input$freq_group == "overall") {
        plot_ly(
          data = df |> arrange(n),
          x    = ~n, y = ~reorder(word, n),
          type = "bar", orientation = "h",
          marker = list(color = "#58a6ff")
        ) |>
          layout(
            paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)",
            font  = list(color = "#e6edf3", family = "Inter"),
            xaxis = list(title = "Count",   gridcolor = "#30363d"),
            yaxis = list(title = "",        gridcolor = "#30363d"),
            hoverlabel = list(bgcolor = "#1c2128")
          )
      } else {
        # Faceted-like: coloured by surah
        plot_ly(
          data = df |> arrange(surah_title_en, n),
          x    = ~n, y = ~reorder(word, n),
          color = ~surah_title_en,
          type  = "bar", orientation = "h"
        ) |>
          layout(
            barmode = "stack",
            paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)",
            font  = list(color = "#e6edf3", family = "Inter"),
            xaxis = list(title = "Count", gridcolor = "#30363d"),
            yaxis = list(title = "",      gridcolor = "#30363d"),
            hoverlabel = list(bgcolor = "#1c2128")
          )
      }
    })

    output$dl_freq <- downloadHandler(
      filename = function() paste0("word_frequency_", Sys.Date(), ".csv"),
      content  = function(file) write.csv(freq_data(), file, row.names = FALSE)
    )

    # --- TF-IDF ---------------------------------------------------------------
    tfidf_data <- reactive({
      req(tokens_df())

      tokens_df() |>
        count(surah_title_en, word, sort = TRUE) |>
        bind_tf_idf(word, surah_title_en, n) |>
        group_by(surah_title_en) |>
        slice_max(tf_idf, n = input$tfidf_top) |>
        ungroup() |>
        arrange(surah_title_en, desc(tf_idf))
    })

    output$plot_tfidf <- renderPlotly({
      req(tfidf_data())

      plot_ly(
        data      = tfidf_data(),
        x         = ~tf_idf, y = ~reorder(word, tf_idf),
        color     = ~surah_title_en,
        type      = "bar", orientation = "h",
        hovertemplate = "<b>%{y}</b><br>TF-IDF: %{x:.4f}<br>Surah: %{customdata}<extra></extra>",
        customdata = ~surah_title_en
      ) |>
        layout(
          barmode = "group",
          paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)",
          font  = list(color = "#e6edf3", family = "Inter"),
          xaxis = list(title = "TF-IDF Score", gridcolor = "#30363d"),
          yaxis = list(title = "",              gridcolor = "#30363d"),
          hoverlabel = list(bgcolor = "#1c2128")
        )
    })

    output$dl_tfidf <- downloadHandler(
      filename = function() paste0("tfidf_", Sys.Date(), ".csv"),
      content  = function(file) write.csv(tfidf_data(), file, row.names = FALSE)
    )

  })
}
