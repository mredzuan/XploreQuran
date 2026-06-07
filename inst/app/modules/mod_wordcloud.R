# ==============================================================================
# modules/mod_wordcloud.R
# XploreQuran - Wordcloud Module
# ==============================================================================

# --- UI -----------------------------------------------------------------------
mod_wordcloud_ui <- function(id) {
  ns <- NS(id)

  tagList(
    card(
      card_header(bsicons::bs_icon("cloud"), " Word Cloud"),
      card_body(
        layout_columns(
          col_widths = c(9, 3),
          plotOutput(ns("plot_wordcloud"), height = "480px"),
          div(
            tags$p(class = "text-muted small fw-semibold", "Options"),
            sliderInput(ns("max_words"), "Max Words",
                        min = 50, max = 300, value = 150, step = 25),
            selectInput(
              ns("palette"), "Colour Palette",
              choices  = c("Dark2", "Set1", "Set2", "Paired", "Spectral",
                           "Blues", "Greens", "Oranges"),
              selected = "Dark2"
            ),
            sliderInput(ns("min_freq"), "Min Frequency",
                        min = 1, max = 20, value = 2, step = 1),
            hr(),
            div(
              class = "d-grid",
              downloadButton(ns("dl_wordcloud"), "Save PNG",
                             class = "btn btn-sm btn-outline-secondary")
            )
          )
        )
      )
    )
  )
}

# --- Server -------------------------------------------------------------------
mod_wordcloud_server <- function(id, tokens_df) {
  moduleServer(id, function(input, output, session) {

    wc_data <- reactive({
      req(tokens_df())
      tokens_df() |>
        count(word, sort = TRUE) |>
        filter(n >= input$min_freq)
    })

    output$plot_wordcloud <- renderPlot({
      req(wc_data())
      df <- wc_data()

      validate(
        need(nrow(df) > 0,
             "No words meet the minimum frequency threshold. Try lowering Min Frequency.")
      )

      par(bg = "#161b22")
      wordcloud::wordcloud(
        words        = df$word,
        freq         = df$n,
        max.words    = input$max_words,
        random.order = FALSE,
        rot.per      = 0.25,
        colors       = RColorBrewer::brewer.pal(8, input$palette),
        scale        = c(4, 0.5)
      )
    }, bg = "#161b22")

    # Download as PNG using recordPlot
    output$dl_wordcloud <- downloadHandler(
      filename = function() paste0("wordcloud_", Sys.Date(), ".png"),
      content  = function(file) {
        req(wc_data())
        df <- wc_data()
        png(file, width = 1200, height = 900, bg = "#161b22")
        par(bg = "#161b22")
        wordcloud::wordcloud(
          words        = df$word,
          freq         = df$n,
          max.words    = input$max_words,
          random.order = FALSE,
          rot.per      = 0.25,
          colors       = RColorBrewer::brewer.pal(8, input$palette),
          scale        = c(5, 0.8)
        )
        dev.off()
      }
    )

  })
}
