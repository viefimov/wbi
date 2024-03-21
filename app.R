
library(rsconnect)
library(shiny)
library(shinyjs)

# Define server logic
ui <- fluidPage(
  includeCSS("ui/index.css"),
  useShinyjs(), 
  htmlTemplate("ui/index.html")
)

server <- function(input, output, session) {
  load("functions/test.RData")
  dataset <- read.csv("data/Wellbeing_and_lifestyle_data_Kaggle.csv")
  
  observe({
    result <-  test(input$input, dataset$DAILY_STRESS)
    runjs(paste0("document.getElementById('final').textContent = '", result, "';"))
  })
}

# Run shiny app
shinyApp(ui = ui, server = server)

