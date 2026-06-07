# ==============================================================================
# modules/mod_compare.R
# XploreQuran - Cross-Translation / Cross-Surah Comparison Module
# ==============================================================================

# --- UI -----------------------------------------------------------------------
mod_compare_ui <- function(id) {
  ns <- NS(id)

  tagList(
    navset_card_tab(
      id = ns("tabs_compare"),

      # Tab 1: Vocabulary Overlap
      nav_panel(
        "Vocabulary Overlap",
        card_body(
          layout_columns(
            col_widths = c(9, 3),
            plotlyOutput(ns("plot_overlap"), height = "420px"),
            div(
              tags$p(class = "text-muted small fw-semibold", "Compare Translations"),
              selectInput(
                ns("cmp_trans_a"), "Translation A",
                choices  = TRANSLATIONS,
                selected = "trans_en_sahih"
              ),
              selectInput(
                ns("cmp_trans_b"), "Translation B",
                choices  = TRANSLATIONS,
                selected = "trans_en_yusufali"
              ),
              hr(),
              downloadButton(ns("dl_overlap"), "Download CSV",
                             class = "btn btn-sm btn-outline-secondary w-100")
            )
          )
        )
      ),

      # Tab 2: Unique Words % per Surah (multi-translation)
      nav_panel(
        "Unique Words by Surah",
        card_body(
          layout_columns(
            col_widths = c(9, 3),
            plotlyOutput(ns("plot_unique_pct"), height = "420px"),
            div(
              tags$p(class = "text-muted small fw-semibold", "Select Translations"),
              checkboxGroupInput(
                ns("cmp_multi_trans"),
                label    = NULL,
                choices  = TRANSLATIONS,
                selected = c("trans_en_sahih", "trans_en_yusufali",
                             "trans_ms_basmeih")
              ),
              hr(),
              downloadButton(ns("dl_unique_pct"), "Download CSV",
                             class = "btn btn-sm btn-outline-secondary w-100")
            )
          )
        )
      ),

      # Tab 3: Side-by-Side Ayah viewer
      nav_panel(
        "Ayah Viewer",
        card_body(
          layout_columns(
            col_widths = c(3, 9),
            div(
              tags$p(class = "text-muted small fw-semibold", "Select Ayah"),
              selectInput(
                ns("view_surah"), "Surah",
                choices  = SURAH_CHOICES,
                selected = 1
              ),
              uiOutput(ns("view_ayah_ui")),
              selectInput(
                ns("view_trans_list"),
                "Translations to Compare",
                choices  = TRANSLATIONS,
                selected = names(TRANSLATIONS),
                multiple = TRUE
              )
            ),
            uiOutput(ns("ayah_cards"))
          )
        )
      )
    )
  )
}

# --- Server -------------------------------------------------------------------
mod_compare_server <- function(id) {
  moduleServer(id, function(input, output, session) {

    # Helper: load + tokenise a named translation
    load_tokens <- function(dataset_name, sw_lang = "en") {
      df <- load_translation(dataset_name)
      tokenise_translation(df, remove_sw = TRUE, sw_lang = sw_lang)
    }

    # Tab 1: Vocabulary Overlap -------------------------------------------------
    overlap_data <- reactive({
      req(input$cmp_trans_a, input$cmp_trans_b)
      req(input$cmp_trans_a != input$cmp_trans_b)

      tok_a <- load_tokens(input$cmp_trans_a) |> distinct(word) |> pull(word)
      tok_b <- load_tokens(input$cmp_trans_b) |> distinct(word) |> pull(word)

      shared    <- intersect(tok_a, tok_b)
      only_a    <- setdiff(tok_a, tok_b)
      only_b    <- setdiff(tok_b, tok_a)

      list(
        shared = shared,
        only_a = only_a,
        only_b = only_b,
        label_a = names(TRANSLATIONS)[TRANSLATIONS == input$cmp_trans_a],
        label_b = names(TRANSLATIONS)[TRANSLATIONS == input$cmp_trans_b]
      )
    })

    output$plot_overlap <- renderPlotly({
      req(overlap_data())
      ol <- overlap_data()

      plot_ly(
        type   = "bar",
        x      = c("Shared", ol$label_a, ol$label_b),
        y      = c(length(ol$shared), length(ol$only_a), length(ol$only_b)),
        marker = list(color = c("#58a6ff", "#3fb950", "#f0a500"))
      ) |>
        layout(
          title  = list(text = "Vocabulary Overlap", font = list(color = "#e6edf3")),
          paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)",
          font  = list(color = "#e6edf3", family = "Inter"),
          xaxis = list(title = "",              gridcolor = "#30363d"),
          yaxis = list(title = "Unique Words",  gridcolor = "#30363d"),
          hoverlabel = list(bgcolor = "#1c2128")
        )
    })

    output$dl_overlap <- downloadHandler(
      filename = function() paste0("vocab_overlap_", Sys.Date(), ".csv"),
      content  = function(file) {
        ol <- overlap_data()
        df <- data.frame(
          category = c(
            rep("shared",  length(ol$shared)),
            rep(ol$label_a, length(ol$only_a)),
            rep(ol$label_b, length(ol$only_b))
          ),
          word = c(ol$shared, ol$only_a, ol$only_b)
        )
        write.csv(df, file, row.names = FALSE)
      }
    )

    # Tab 2: Unique Words % per Surah ------------------------------------------
    unique_pct_data <- reactive({
      req(input$cmp_multi_trans)

      purrr::map_dfr(input$cmp_multi_trans, function(ds) {
        label <- names(TRANSLATIONS)[TRANSLATIONS == ds]
        tok   <- load_tokens(ds)

        tok |>
          group_by(surah_id, surah_title_en) |>
          summarise(unique_words = n_distinct(word), .groups = "drop") |>
          mutate(
            pct         = unique_words / sum(unique_words) * 100,
            translation = label
          )
      })
    })

    output$plot_unique_pct <- renderPlotly({
      req(unique_pct_data())
      df <- unique_pct_data()

      plot_ly(
        data       = df,
        x          = ~surah_id, y = ~pct,
        color      = ~translation,
        type       = "scatter", mode = "lines+markers",
        hovertemplate = "<b>%{customdata}</b><br>%{y:.2f}%<extra></extra>",
        customdata = ~surah_title_en
      ) |>
        layout(
          paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)",
          font  = list(color = "#e6edf3", family = "Inter"),
          xaxis = list(title = "Surah Number",         gridcolor = "#30363d"),
          yaxis = list(title = "Unique Words (%)",      gridcolor = "#30363d"),
          legend = list(font = list(color = "#e6edf3")),
          hoverlabel = list(bgcolor = "#1c2128")
        )
    })

    output$dl_unique_pct <- downloadHandler(
      filename = function() paste0("unique_words_pct_", Sys.Date(), ".csv"),
      content  = function(file) write.csv(unique_pct_data(), file, row.names = FALSE)
    )

    # Tab 3: Ayah Viewer -------------------------------------------------------
    output$view_ayah_ui <- renderUI({
      req(input$view_surah)
      ns <- session$ns

      # Load ayah range for chosen surah
      n_ayahs <- quran_index |>
        filter(surah_id == as.integer(input$view_surah)) |>
        pull(n_ayah)

      selectInput(
        ns("view_ayah"), "Ayah",
        choices  = 1:n_ayahs,
        selected = 1
      )
    })

    output$ayah_cards <- renderUI({
      req(input$view_surah, input$view_ayah, input$view_trans_list)

      sid  <- as.integer(input$view_surah)
      aid  <- as.integer(input$view_ayah)

      cards <- purrr::map(input$view_trans_list, function(ds) {
        label <- names(TRANSLATIONS)[TRANSLATIONS == ds]
        df    <- load_translation(ds)
        text_col <- if ("translation" %in% names(df)) "translation" else "text"

        ayah_text <- df |>
          filter(surah_id == sid, surah_ayah_id == aid) |>
          pull(!!text_col)

        ayah_text <- if (length(ayah_text) == 0) "(Not found)" else ayah_text[1]

        div(
          class = "ayah-card",
          tags$span(class = "ayah-ref",
                    paste0(label, " — ", sid, ":", aid)),
          tags$p(class = "ayah-translation mt-1 mb-0", ayah_text)
        )
      })

      do.call(tagList, cards)
    })

  })
}
