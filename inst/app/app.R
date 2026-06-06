library(shiny)
library(bslib)

ui <- page_fluid(
  theme = bs_theme(version = 5, bootswatch = "minty"),
  titlePanel("XploreQuran - Hello World"),
  
  sidebarLayout(
    sidebarPanel(
      h3("Welcome"),
      p("This is the base Shiny application for XploreQuran.")
    ),
    
    mainPanel(
      h1("Hello World"),
      p("The project structure is successfully set up!"),
      hr(),
      textOutput("status_text")
    )
  )
)

server <- function(input, output, session) {
  output$status_text <- renderText({
    "Shiny application is running smoothly."
  })
}

shinyApp(ui, server)
