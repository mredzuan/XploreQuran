# ==============================================================================
# modules/mod_topic_model.R
# XploreQuran - Topic Modelling (LDA) Module
# Requires: topicmodels package
# ==============================================================================

# --- UI -----------------------------------------------------------------------
mod_topic_model_ui <- function(id) {
  ns <- NS(id)

  tagList(
    navset_card_tab(
      id = ns("tabs_lda"),

      nav_panel(
        "Topic Overview",
        card_body(
          layout_columns(
            col_widths = c(9, 3),
            plotlyOutput(ns("plot_lda_terms"), height = "440px"),
            div(
              tags$p(class = "text-muted small fw-semibold", "LDA Settings"),
              sliderInput(ns("n_topics"), "Number of Topics (K)",
                          min = 2, max = 15, value = 5, step = 1),
              sliderInput(ns("top_terms"), "Top Terms per Topic",
                          min = 3, max = 15, value = 8, step = 1),
              sliderInput(ns("lda_iter"), "Iterations",
                          min = 100, max = 2000, value = 500, step = 100),
              actionButton(ns("btn_run_lda"), "Fit Model",
                           class = "btn btn-primary w-100 mt-2",
                           icon  = icon("gears")),
              hr(),
              downloadButton(ns("dl_lda"), "Download CSV",
                             class = "btn btn-sm btn-outline-secondary w-100")
            )
          )
        )
      ),

      nav_panel(
        "Document-Topic Distribution",
        card_body(
          plotlyOutput(ns("plot_doc_topic"), height = "440px")
        )
      )
    )
  )
}

# --- Server -------------------------------------------------------------------
mod_topic_model_server <- function(id, tokens_df) {
  moduleServer(id, function(input, output, session) {

    # Fit LDA only when user clicks the button
    lda_model <- eventReactive(input$btn_run_lda, {

      if (!requireNamespace("topicmodels", quietly = TRUE)) {
        showNotification(
          "Please install the 'topicmodels' package: install.packages('topicmodels')",
          type = "error", duration = 8
        )
        return(NULL)
      }

      req(tokens_df())
      df <- tokens_df()

      withProgress(message = "Fitting LDA model…", value = 0, {
        incProgress(0.2, detail = "Building document-term matrix")

        dtm <- df |>
          count(ayah_id, word) |>
          tidytext::cast_dtm(ayah_id, word, n)

        # Remove empty docs
        dtm <- dtm[slam::row_sums(dtm) > 0, ]

        incProgress(0.4, detail = "Running LDA")

        model <- topicmodels::LDA(
          dtm,
          k       = input$n_topics,
          control = list(seed = 42, iter = input$lda_iter)
        )

        incProgress(1, detail = "Done")
        model
      })
    })

    # Per-topic per-word probabilities (beta)
    lda_terms <- reactive({
      req(lda_model())
      tidytext::tidy(lda_model(), matrix = "beta") |>
        group_by(topic) |>
        slice_max(beta, n = input$top_terms) |>
        ungroup() |>
        mutate(topic = paste("Topic", topic))
    })

    # Per-document per-topic probabilities (gamma)
    lda_docs <- reactive({
      req(lda_model())
      tidytext::tidy(lda_model(), matrix = "gamma") |>
        mutate(topic = paste("Topic", topic))
    })

    output$plot_lda_terms <- renderPlotly({
      validate(
        need(!is.null(lda_model()),
             "Click 'Fit Model' in the sidebar panel to run LDA.")
      )
      req(lda_terms())

      plot_ly(
        data      = lda_terms() |> arrange(topic, beta),
        x         = ~beta,
        y         = ~reorder_within(term, beta, topic),
        color     = ~topic,
        type      = "bar",
        orientation = "h"
      ) |>
        layout(
          barmode = "group",
          paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)",
          font  = list(color = "#e6edf3", family = "Inter"),
          xaxis = list(title = "β (Word-Topic Probability)", gridcolor = "#30363d"),
          yaxis = list(title = "", gridcolor = "#30363d"),
          hoverlabel = list(bgcolor = "#1c2128")
        )
    })

    output$plot_doc_topic <- renderPlotly({
      validate(
        need(!is.null(lda_model()),
             "Click 'Fit Model' to run LDA first.")
      )
      req(lda_docs())

      # Aggregate gamma per topic
      df <- lda_docs() |>
        group_by(topic) |>
        summarise(mean_gamma = mean(gamma), .groups = "drop")

      plot_ly(
        data   = df,
        labels = ~topic, values = ~mean_gamma,
        type   = "pie",
        hole   = 0.4,
        marker = list(
          colors = RColorBrewer::brewer.pal(max(nrow(df), 3), "Set2"),
          line   = list(color = "#0d1117", width = 2)
        )
      ) |>
        layout(
          paper_bgcolor = "rgba(0,0,0,0)",
          font  = list(color = "#e6edf3", family = "Inter"),
          showlegend = TRUE,
          legend = list(font = list(color = "#e6edf3"))
        )
    })

    output$dl_lda <- downloadHandler(
      filename = function() paste0("lda_topics_", Sys.Date(), ".csv"),
      content  = function(file) {
        if (is.null(lda_model())) {
          write.csv(data.frame(message = "Run the model first."), file, row.names = FALSE)
        } else {
          write.csv(lda_terms(), file, row.names = FALSE)
        }
      }
    )

  })
}
