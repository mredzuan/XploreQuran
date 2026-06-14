# ==============================================================================
# modules/mod_overview.R
# XploreQuran - Overview / Dashboard Module
# Displays summary statistics and a welcome panel.
# ==============================================================================

# --- UI -----------------------------------------------------------------------
mod_overview_ui <- function(id) {
  ns <- NS(id)

  tagList(
    # Top stats row (Reordered: Surahs Selected, Total Ayahs, Total Words, Unique Words)
    layout_columns(
      col_widths = c(3, 3, 3, 3),
      fill = FALSE,
      value_box(
        id       = ns("vb_surahs"),
        title    = "Surahs Selected",
        value    = textOutput(ns("n_surahs")),
        showcase = bsicons::bs_icon("layers"),
        theme    = "secondary"
      ),
      value_box(
        id       = ns("vb_ayahs"),
        title    = "Total Ayahs",
        value    = textOutput(ns("n_ayahs")),
        showcase = bsicons::bs_icon("book"),
        theme    = "primary"
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

    # Tabbing below the cards visualization
    navset_card_tab(
      id = ns("overview_tabs"),
      nav_panel(
        title = tagList(bsicons::bs_icon("bar-chart-line"), " Word Frequency"),
        plotlyOutput(ns("plot_word_freq"), height = "400px")
      ),
      nav_panel(
        title = tagList(bsicons::bs_icon("cloud"), " Word Cloud"),
        div(
          style = "position: relative;",
          # Zoom buttons overlay on top-right of plot area
          div(
            style = "position: absolute; top: 10px; right: 10px; z-index: 10; display: flex; gap: 0.25rem;",
            actionButton(
              inputId = ns("zoom_out"),
              label   = bsicons::bs_icon("dash-lg"),
              title   = "Zoom Out Word Cloud",
              class   = "btn btn-sm btn-outline-secondary p-0",
              style   = "width:30px; height:30px; display:flex; align-items:center; justify-content:center;"
            ),
            actionButton(
              inputId = ns("zoom_in"),
              label   = bsicons::bs_icon("plus-lg"),
              title   = "Zoom In Word Cloud",
              class   = "btn btn-sm btn-outline-secondary p-0",
              style   = "width:30px; height:30px; display:flex; align-items:center; justify-content:center;"
            )
          ),
          plotOutput(ns("plot_wordcloud"), height = "400px")
        )
      ),
      nav_panel(
        title = tagList(
          bsicons::bs_icon("calculator"),
          "Term Freq",
          bslib::tooltip(
            bsicons::bs_icon("info-circle", class = "text-muted", style = "font-size:0.8rem; cursor:pointer; margin-left:4px;"),
            "Term Frequency (TF) = (Word Count in Surah) / (Total Words in Surah). Hover to learn more.",
            placement = "top"
          )
        ),
        value = "tab_term_freq",
        div(
          style = "margin-bottom: 1rem;",
          radioButtons(
            inputId  = ns("tf_view"),
            label    = NULL,
            choices  = c("Table View" = "table", "Histogram View" = "histogram"),
            selected = "table",
            inline   = TRUE
          )
        ),
        # Toggleable output panels
        conditionalPanel(
          condition = "input.tf_view == 'table'",
          ns = ns,
          div(
            style = "border: 1px solid var(--cq-card-border); border-radius: 0.5rem; background-color: var(--cq-card-bg); padding: 1rem;",
            DT::DTOutput(ns("tbl_term_freq"))
          )
        ),
        conditionalPanel(
          condition = "input.tf_view == 'histogram'",
          ns = ns,
          plotOutput(ns("plot_tf_hist"), height = "500px")
        )
      )
    )
  )
}

# --- Server -------------------------------------------------------------------
mod_overview_server <- function(id, quran_df, tokens_df, top_n, is_dark) {
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

    # Tab 1: Word Frequency Bar Chart (Sorted Descending)
    output$plot_word_freq <- renderPlotly({
      req(tokens_df(), top_n())
      
      df_counts <- tokens_df() |>
        count(word, sort = TRUE) |>
        head(top_n())
      
      validate(
        need(nrow(df_counts) > 0, "No word data available for chart. Try matching translation.")
      )
      
      # Factor to lock the descending sort order on the chart x-axis
      df_counts$word <- factor(df_counts$word, levels = df_counts$word)
      
      plot_ly(
        data   = df_counts,
        x      = ~word,
        y      = ~n,
        type   = "bar",
        marker = list(
          color = ~n,
          colorscale = "Viridis",
          showscale  = FALSE
        ),
        hovertemplate = "<b>%{x}</b><br>Count: %{y:,}<extra></extra>"
      ) |>
        layout(
          paper_bgcolor = "rgba(0,0,0,0)",
          plot_bgcolor  = "rgba(0,0,0,0)",
          font  = list(color = "var(--cq-fg)", family = "Inter"),
          xaxis = list(
            title      = "Word",
            gridcolor  = "var(--cq-card-border)",
            zerolinecolor = "var(--cq-card-border)"
          ),
          yaxis = list(
            title      = "Occurrence Count",
            gridcolor  = "var(--cq-card-border)",
            zerolinecolor = "var(--cq-card-border)"
          ),
          hoverlabel = list(bgcolor = "var(--cq-card-bg)", font = list(color = "var(--cq-fg)")),
          margin = list(b = 60)
        )
    })

    # Zoom scale state
    cloud_scale <- reactiveVal(4.0)

    observeEvent(input$zoom_in, {
      new_scale <- min(cloud_scale() + 0.5, 8.0)
      cloud_scale(new_scale)
    })

    observeEvent(input$zoom_out, {
      new_scale <- max(cloud_scale() - 0.5, 2.0)
      cloud_scale(new_scale)
    })

    # Tab 2: Word Cloud Chart (Transparent Background)
    output$plot_wordcloud <- renderPlot({
      req(tokens_df())
      
      df <- tokens_df() |>
        count(word, sort = TRUE)
      
      validate(
        need(nrow(df) > 0, "No word data available for word cloud.")
      )
      
      # Determine text colors based on the current theme mode
      palette_colors <- if (is_dark()) {
        # Dark mode: vibrant colors
        RColorBrewer::brewer.pal(8, "Spectral")
      } else {
        # Light mode: deeper, high-contrast colors
        RColorBrewer::brewer.pal(8, "Dark2")
      }
      
      par(mar = c(0, 0, 0, 0), bg = NA) # Set margins to 0 and background to transparent
      wordcloud::wordcloud(
        words        = df$word,
        freq         = df$n,
        max.words    = 100,
        random.order = FALSE,
        rot.per      = 0.15,
        colors       = palette_colors,
        scale        = c(cloud_scale(), cloud_scale() * 0.125)
      )
    }, bg = "transparent")

    # Compute Term Frequency data frame reactive
    tf_data <- reactive({
      req(tokens_df())
      
      # Calculate total words per surah
      totals <- tokens_df() |>
        count(surah_id, name = "total_words")
      
      # Count occurrences of each word in each surah
      counts <- tokens_df() |>
        count(surah_id, surah_title_en, word, name = "word_count")
      
      # Calculate TF
      counts |>
        left_join(totals, by = "surah_id") |>
        mutate(
          term_freq = word_count / total_words
        ) |>
        arrange(surah_id, desc(term_freq))
    })

    # Tab 3 (Table View): Render Term Frequency summary table
    output$tbl_term_freq <- DT::renderDT({
      req(tf_data())
      
      # Remove top_n limit to show all records
      df <- tf_data()
      
      # Rename and format columns for display
      display_df <- data.frame(
        `Surah`                = paste0(df$surah_id, ". ", df$surah_title_en),
        `Word`                 = df$word,
        `Word Count in Surah`  = df$word_count,
        `Total Words in Surah` = df$total_words,
        `Term Frequency`       = df$term_freq,
        check.names            = FALSE
      )
      
      DT::datatable(
        display_df,
        filter = "top", # Adds search boxes for each column
        rownames = FALSE,
        style = "bootstrap5",
        options = list(
          pageLength = 10,
          autoWidth = TRUE,
          order = list(), # Preserve pre-sorted order from dplyr (Surah ID asc, TF desc)
          scrollX = TRUE
        )
      ) |>
        DT::formatRound(columns = "Term Frequency", digits = 5)
    })

    # Tab 3 (Histogram View): Render ggplot2 Histogram faceted by Surah
    output$plot_tf_hist <- renderPlot({
      req(tf_data())
      df <- tf_data()
      
      # If there are too many surahs, limit the facetting to top 9 for readability
      unique_surahs <- unique(df$surah_id)
      if (length(unique_surahs) > 9) {
        top_surahs_to_plot <- unique_surahs[1:9]
        df_plot <- df |> dplyr::filter(surah_id %in% top_surahs_to_plot)
        subtitle_note <- "Showing top 9 surahs for visual legibility. Filter surahs in sidebar to customize."
      } else {
        df_plot <- df
        subtitle_note <- NULL
      }
      
      # Create label for strip facets
      df_plot <- df_plot |>
        mutate(surah_label = paste0(surah_id, ". ", surah_title_en))
      
      # Preserve numeric surah order for facets
      df_plot$surah_label <- factor(
        df_plot$surah_label,
        levels = unique(df_plot$surah_label[order(df_plot$surah_id)])
      )
      
      library(ggplot2)
      
      # Dynamic styling matching active Light/Dark theme
      if (is_dark()) {
        bg_color <- "#161b22"
        theme_mode <- theme_minimal() +
        theme(
          plot.background  = element_rect(fill = bg_color, color = NA),
          panel.background = element_rect(fill = bg_color, color = NA),
          text             = element_text(color = "#e6edf3", family = "Inter"),
          axis.text        = element_text(color = "#8b949e"),
          panel.grid       = element_line(color = "#30363d"),
          strip.text       = element_text(color = "#e6edf3", face = "bold")
        )
      } else {
        bg_color <- "#ffffff"
        theme_mode <- theme_minimal() +
        theme(
          plot.background  = element_rect(fill = bg_color, color = NA),
          panel.background = element_rect(fill = bg_color, color = NA),
          text             = element_text(color = "#212529", family = "Inter"),
          axis.text        = element_text(color = "#495057"),
          panel.grid       = element_line(color = "#dee2e6"),
          strip.text       = element_text(color = "#212529", face = "bold")
        )
      }
      
      ggplot(df_plot, aes(x = term_freq)) +
        geom_histogram(bins = 20, fill = "#58a6ff", color = bg_color, alpha = 0.85) +
        facet_wrap(~ surah_label, scales = "free_y") +
        labs(
          x = "Term Frequency Ratio",
          y = "Count of Terms",
          title = "Term Frequency Distribution by Surah",
          subtitle = subtitle_note
        ) +
        theme_mode
    }, bg = "transparent")

  })
}
