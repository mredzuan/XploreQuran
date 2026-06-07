# ==============================================================================
# modules/mod_ngram.R
# XploreQuran - N-grams & Word Correlations Module
# ==============================================================================

# --- UI -----------------------------------------------------------------------
mod_ngram_ui <- function(id) {
  ns <- NS(id)

  tagList(
    navset_card_tab(
      id = ns("tabs_ngram"),

      # Tab 1: N-gram frequency
      nav_panel(
        "N-gram Frequency",
        card_body(
          layout_columns(
            col_widths = c(9, 3),
            plotlyOutput(ns("plot_ngram"), height = "420px"),
            div(
              tags$p(class = "text-muted small fw-semibold", "Options"),
              radioButtons(
                ns("ngram_n"), "N-gram Type",
                choices  = c("Bigram" = 2, "Trigram" = 3),
                selected = 2
              ),
              sliderInput(
                ns("ngram_top"), "Top N",
                min = 5, max = 30, value = 15, step = 5
              ),
              hr(),
              downloadButton(ns("dl_ngram"), "Download CSV",
                             class = "btn btn-sm btn-outline-secondary w-100")
            )
          )
        )
      ),

      # Tab 2: Word correlation / network
      nav_panel(
        "Word Correlations",
        card_body(
          layout_columns(
            col_widths = c(9, 3),
            plotlyOutput(ns("plot_corr"), height = "420px"),
            div(
              tags$p(class = "text-muted small fw-semibold", "Options"),
              sliderInput(ns("corr_min"), "Min Correlation",
                          min = 0.1, max = 0.9, value = 0.3, step = 0.05),
              sliderInput(ns("corr_top_words"), "Top Words Considered",
                          min = 20, max = 200, value = 100, step = 10),
              hr(),
              downloadButton(ns("dl_corr"), "Download CSV",
                             class = "btn btn-sm btn-outline-secondary w-100")
            )
          )
        )
      )
    )
  )
}

# --- Server -------------------------------------------------------------------
mod_ngram_server <- function(id, quran_df) {
  moduleServer(id, function(input, output, session) {

    # N-gram tokenise directly from raw text (not pre-tokenised)
    ngram_data <- reactive({
      req(quran_df(), input$ngram_n)

      text_col <- if ("translation" %in% names(quran_df())) "translation" else "text"
      n        <- as.integer(input$ngram_n)

      quran_df() |>
        tidytext::unnest_tokens(ngram, !!rlang::sym(text_col),
                                token = "ngrams", n = n) |>
        filter(!is.na(ngram)) |>
        count(ngram, sort = TRUE) |>
        slice_head(n = input$ngram_top)
    })

    output$plot_ngram <- renderPlotly({
      req(ngram_data())

      plot_ly(
        data   = ngram_data() |> arrange(n),
        x      = ~n, y = ~reorder(ngram, n),
        type   = "bar", orientation = "h",
        marker = list(color = "#58a6ff")
      ) |>
        layout(
          paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)",
          font  = list(color = "#e6edf3", family = "Inter"),
          xaxis = list(title = "Count",   gridcolor = "#30363d"),
          yaxis = list(title = "",        gridcolor = "#30363d"),
          hoverlabel = list(bgcolor = "#1c2128")
        )
    })

    output$dl_ngram <- downloadHandler(
      filename = function() paste0("ngrams_", Sys.Date(), ".csv"),
      content  = function(file) write.csv(ngram_data(), file, row.names = FALSE)
    )

    # Word pair correlations using widyr package (if available)
    corr_data <- reactive({
      req(quran_df())

      if (!requireNamespace("widyr", quietly = TRUE)) {
        return(NULL)
      }

      text_col <- if ("translation" %in% names(quran_df())) "translation" else "text"

      top_words <- quran_df() |>
        tidytext::unnest_tokens(word, !!rlang::sym(text_col)) |>
        count(word, sort = TRUE) |>
        slice_head(n = input$corr_top_words) |>
        pull(word)

      quran_df() |>
        tidytext::unnest_tokens(word, !!rlang::sym(text_col)) |>
        filter(word %in% top_words) |>
        widyr::pairwise_cor(word, ayah_id, sort = TRUE) |>
        filter(correlation >= input$corr_min)
    })

    output$plot_corr <- renderPlotly({

      if (!requireNamespace("widyr", quietly = TRUE)) {
        return(plotly_empty() |>
                 layout(
                   title = list(
                     text = "Install 'widyr' package to enable word correlation analysis",
                     font = list(color = "#8b949e")
                   ),
                   paper_bgcolor = "rgba(0,0,0,0)",
                   plot_bgcolor  = "rgba(0,0,0,0)"
                 ))
      }

      req(corr_data())
      df <- corr_data()

      validate(need(nrow(df) > 0,
                    "No word pairs meet the correlation threshold. Try lowering the value."))

      plot_ly(
        data = df |> head(50),
        x    = ~item1, y = ~item2, z = ~correlation,
        type = "heatmap",
        colorscale = "Blues",
        hovertemplate = "<b>%{x}</b> ↔ <b>%{y}</b><br>Correlation: %{z:.3f}<extra></extra>"
      ) |>
        layout(
          paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)",
          font  = list(color = "#e6edf3", family = "Inter"),
          xaxis = list(title = "", tickangle = -45),
          yaxis = list(title = ""),
          hoverlabel = list(bgcolor = "#1c2128")
        )
    })

    output$dl_corr <- downloadHandler(
      filename = function() paste0("word_correlations_", Sys.Date(), ".csv"),
      content  = function(file) {
        if (is.null(corr_data())) {
          write.csv(data.frame(message = "widyr package required"), file, row.names = FALSE)
        } else {
          write.csv(corr_data(), file, row.names = FALSE)
        }
      }
    )

  })
}
