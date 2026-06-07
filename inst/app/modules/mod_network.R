# ==============================================================================
# modules/mod_network.R
# XploreQuran - Word Co-occurrence Network Module
# Requires: igraph, ggraph / networkD3
# ==============================================================================

# --- UI -----------------------------------------------------------------------
mod_network_ui <- function(id) {
  ns <- NS(id)

  tagList(
    navset_card_tab(
      id = ns("tabs_network"),

      nav_panel(
        "Co-occurrence Network",
        card_body(
          layout_columns(
            col_widths = c(9, 3),
            # networkD3 renders in HTML widget
            networkD3::forceNetworkOutput(ns("plot_network"), height = "500px"),
            div(
              tags$p(class = "text-muted small fw-semibold", "Network Options"),
              sliderInput(ns("net_top_words"), "Top Words",
                          min = 20, max = 200, value = 80, step = 10),
              sliderInput(ns("net_min_cooc"), "Min Co-occurrences",
                          min = 1, max = 20, value = 3, step = 1),
              sliderInput(ns("net_charge"), "Node Repulsion",
                          min = -200, max = -10, value = -80, step = 10),
              hr(),
              tags$p(class = "text-muted small",
                     "Drag nodes to explore connections. Hover for word labels.")
            )
          )
        )
      ),

      nav_panel(
        "Network Statistics",
        card_body(
          layout_columns(
            col_widths = c(6, 6),
            card(
              card_header("Top Nodes by Degree"),
              card_body(
                tableOutput(ns("tbl_degree"))
              )
            ),
            card(
              card_header("Network Summary"),
              card_body(
                verbatimTextOutput(ns("txt_summary"))
              )
            )
          )
        )
      )
    )
  )
}

# --- Server -------------------------------------------------------------------
mod_network_server <- function(id, tokens_df) {
  moduleServer(id, function(input, output, session) {

    # Check for required packages
    has_pkgs <- reactive({
      requireNamespace("igraph",    quietly = TRUE) &&
        requireNamespace("networkD3", quietly = TRUE)
    })

    # Build co-occurrence pairs from bigrams
    cooc_data <- reactive({
      req(tokens_df())

      if (!has_pkgs()) return(NULL)

      top_words <- tokens_df() |>
        count(word, sort = TRUE) |>
        slice_head(n = input$net_top_words) |>
        pull(word)

      tokens_filtered <- tokens_df() |>
        filter(word %in% top_words)

      # Pairwise co-occurrence within each ayah
      pairs <- tokens_filtered |>
        select(ayah_id, word) |>
        inner_join(tokens_filtered |> select(ayah_id, word),
                   by = "ayah_id", relationship = "many-to-many") |>
        filter(word.x < word.y) |>    # avoid duplicates
        count(word.x, word.y, sort = TRUE) |>
        filter(n >= input$net_min_cooc) |>
        rename(from = word.x, to = word.y, weight = n)

      pairs
    })

    # Build igraph graph object
    graph_obj <- reactive({
      req(cooc_data())
      if (nrow(cooc_data()) == 0) return(NULL)
      igraph::graph_from_data_frame(cooc_data(), directed = FALSE)
    })

    output$plot_network <- networkD3::renderForceNetwork({
      if (!has_pkgs()) {
        return(NULL)
      }

      req(graph_obj())
      g <- graph_obj()

      # Convert igraph to networkD3 format
      d3 <- networkD3::igraph_to_networkD3(g, group = rep(1, igraph::vcount(g)))

      # Node sizes proportional to degree
      d3$nodes$size <- igraph::degree(g)[d3$nodes$name] + 1

      networkD3::forceNetwork(
        Links         = d3$links,
        Nodes         = d3$nodes,
        Source        = "source",
        Target        = "target",
        Value         = "value",
        NodeID        = "name",
        Group         = "group",
        Nodesize      = "size",
        opacity       = 0.85,
        zoom          = TRUE,
        legend        = FALSE,
        charge        = input$net_charge,
        fontSize      = 11,
        colourScale   = networkD3::JS(
          "d3.scaleOrdinal().range(['#58a6ff','#3fb950','#f0a500','#f85149'])"
        ),
        linkColour    = "#30363d",
        backgroundColor = "#161b22"
      )
    })

    output$tbl_degree <- renderTable({
      req(graph_obj())
      g <- graph_obj()

      data.frame(
        Word   = igraph::V(g)$name,
        Degree = igraph::degree(g)
      ) |>
        arrange(desc(Degree)) |>
        head(20)
    }, striped = TRUE, hover = TRUE, bordered = FALSE,
    digits = 0)

    output$txt_summary <- renderPrint({
      req(graph_obj())
      g <- graph_obj()
      cat("Nodes      :", igraph::vcount(g), "\n")
      cat("Edges      :", igraph::ecount(g), "\n")
      cat("Density    :", round(igraph::edge_density(g), 4), "\n")
      cat("Diameter   :", igraph::diameter(g), "\n")
      cat("Avg Degree :", round(mean(igraph::degree(g)), 2), "\n")
      cat("Transitivity:", round(igraph::transitivity(g), 4), "\n")
    })

  })
}
