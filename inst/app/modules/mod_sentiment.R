# ==============================================================================
# modules/mod_sentiment.R
# XploreQuran - Sentiment Analysis Module
# ==============================================================================

# --- UI -----------------------------------------------------------------------
mod_sentiment_ui <- function(id) {
  ns <- NS(id)

  tagList(
    layout_columns(
      col_widths = c(3, 3, 3, 3),
      fill = FALSE,
      value_box(
        title    = "Positive Words",
        value    = textOutput(ns("vb_pos")),
        showcase = bsicons::bs_icon("emoji-smile"),
        theme    = "success"
      ),
      value_box(
        title    = "Negative Words",
        value    = textOutput(ns("vb_neg")),
        showcase = bsicons::bs_icon("emoji-frown"),
        theme    = "danger"
      ),
      value_box(
        title    = "Net Sentiment",
        value    = textOutput(ns("vb_net")),
        showcase = bsicons::bs_icon("graph-up-arrow"),
        theme    = "primary"
      ),
      value_box(
        title    = "Lexicon Used",
        value    = textOutput(ns("vb_lexicon")),
        showcase = bsicons::bs_icon("journal-text"),
        theme    = "secondary"
      )
    ),

    navset_card_tab(
      id = ns("tabs_sentiment"),

      nav_panel(
        "Sentiment by Section",
        card_body(
          layout_columns(
            col_widths = c(9, 3),
            plotlyOutput(ns("plot_sent_section"), height = "400px"),
            div(
              tags$p(class = "text-muted small fw-semibold", "Options"),
              selectInput(
                ns("sel_lexicon"), "Sentiment Lexicon",
                choices  = c("Bing" = "bing", "AFINN" = "afinn", "NRC" = "nrc"),
                selected = "bing"
              ),
              conditionalPanel(
                condition = paste0("input['", ns("sel_lexicon"), "'] == 'nrc'"),
                selectInput(
                  ns("nrc_emotion"), "NRC Emotion",
                  choices  = c("positive", "negative", "anger", "fear",
                               "anticipation", "trust", "surprise", "sadness",
                               "joy", "disgust"),
                  selected = "positive"
                )
              ),
              hr(),
              downloadButton(ns("dl_sentiment"), "Download CSV",
                             class = "btn btn-sm btn-outline-secondary w-100")
            )
          )
        )
      ),

      nav_panel(
        "Top Sentiment Words",
        card_body(
          plotlyOutput(ns("plot_sent_words"), height = "400px")
        )
      )
    )
  )
}

# --- Server -------------------------------------------------------------------
mod_sentiment_server <- function(id, tokens_df) {
  moduleServer(id, function(input, output, session) {

    # Sentiment join
    sent_data <- reactive({
      req(tokens_df(), input$sel_lexicon)

      lexicon <- input$sel_lexicon

      if (lexicon == "afinn") {
        sentiment_df <- tidytext::get_sentiments("afinn")
        tokens_df() |>
          inner_join(sentiment_df, by = "word") |>
          group_by(surah_id, surah_title_en) |>
          summarise(sentiment = sum(value, na.rm = TRUE), .groups = "drop")

      } else if (lexicon == "bing") {
        sentiment_df <- tidytext::get_sentiments("bing")
        tokens_df() |>
          inner_join(sentiment_df, by = "word") |>
          count(surah_id, surah_title_en, sentiment) |>
          tidyr::pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) |>
          mutate(sentiment = positive - negative)

      } else {  # nrc
        sentiment_df <- tidytext::get_sentiments("nrc") |>
          filter(sentiment == input$nrc_emotion)
        tokens_df() |>
          inner_join(sentiment_df, by = "word") |>
          count(surah_id, surah_title_en) |>
          rename(sentiment = n)
      }
    })

    # Top sentiment words
    sent_words <- reactive({
      req(tokens_df(), input$sel_lexicon)

      if (input$sel_lexicon == "bing") {
        tidytext::get_sentiments("bing") |>
          inner_join(tokens_df() |> count(word, sort = TRUE), by = "word") |>
          group_by(sentiment) |>
          slice_max(n, n = 15) |>
          ungroup()
      } else if (input$sel_lexicon == "nrc") {
        tidytext::get_sentiments("nrc") |>
          filter(sentiment == input$nrc_emotion) |>
          inner_join(tokens_df() |> count(word, sort = TRUE), by = "word") |>
          slice_max(n, n = 30) |>
          mutate(sentiment = input$nrc_emotion)
      } else {
        tidytext::get_sentiments("afinn") |>
          inner_join(tokens_df() |> count(word, sort = TRUE), by = "word") |>
          mutate(sentiment = ifelse(value > 0, "positive", "negative")) |>
          group_by(sentiment) |>
          slice_max(n, n = 15) |>
          ungroup()
      }
    })

    # Value boxes
    output$vb_pos <- renderText({
      req(sent_words())
      sw <- sent_words()
      if ("sentiment" %in% names(sw)) {
        format(sum(sw$n[sw$sentiment == "positive"], na.rm = TRUE), big.mark = ",")
      } else "—"
    })

    output$vb_neg <- renderText({
      req(sent_words())
      sw <- sent_words()
      if ("sentiment" %in% names(sw)) {
        format(sum(sw$n[sw$sentiment == "negative"], na.rm = TRUE), big.mark = ",")
      } else "—"
    })

    output$vb_net <- renderText({
      req(sent_data())
      sd <- sent_data()
      if ("sentiment" %in% names(sd)) {
        net <- sum(sd$sentiment, na.rm = TRUE)
        paste0(if (net >= 0) "+" else "", format(net, big.mark = ","))
      } else "—"
    })

    output$vb_lexicon <- renderText({ toupper(input$sel_lexicon) })

    # Plot: sentiment by surah
    output$plot_sent_section <- renderPlotly({
      req(sent_data())
      df <- sent_data()

      colors <- ifelse(df$sentiment >= 0, "#3fb950", "#f85149")

      plot_ly(
        data   = df |> arrange(surah_id),
        x      = ~surah_id, y = ~sentiment,
        type   = "bar",
        marker = list(color = colors),
        hovertemplate = "<b>%{customdata}</b><br>Score: %{y}<extra></extra>",
        customdata = ~surah_title_en
      ) |>
        layout(
          paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)",
          font  = list(color = "#e6edf3", family = "Inter"),
          xaxis = list(title = "Surah",          gridcolor = "#30363d"),
          yaxis = list(title = "Sentiment Score", gridcolor = "#30363d",
                       zerolinecolor = "#58a6ff"),
          hoverlabel = list(bgcolor = "#1c2128")
        )
    })

    # Plot: top words
    output$plot_sent_words <- renderPlotly({
      req(sent_words())
      df <- sent_words() |> arrange(desc(n)) |> head(30)

      bar_colors <- ifelse(df$sentiment == "positive", "#3fb950", "#f85149")

      plot_ly(
        data   = df |> arrange(n),
        x      = ~n, y = ~reorder(word, n),
        type   = "bar", orientation = "h",
        marker = list(color = bar_colors)
      ) |>
        layout(
          paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)",
          font  = list(color = "#e6edf3", family = "Inter"),
          xaxis = list(title = "Count", gridcolor = "#30363d"),
          yaxis = list(title = "",      gridcolor = "#30363d"),
          hoverlabel = list(bgcolor = "#1c2128")
        )
    })

    output$dl_sentiment <- downloadHandler(
      filename = function() paste0("sentiment_", Sys.Date(), ".csv"),
      content  = function(file) write.csv(sent_data(), file, row.names = FALSE)
    )

  })
}
