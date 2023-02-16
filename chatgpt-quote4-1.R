library(shiny)
library(plotly)
library(dplyr)
library(leaflet)

# Load the storms dataset
data(storms)

# Create a UI
ui <- fluidPage(
  titlePanel("Storms Dashboard"),
  sidebarLayout(
    sidebarPanel(
      selectInput("year", "Select Year:",
                  choices = unique(storms$year),
                  selected = 2000),
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Table",
                 dataTableOutput("table")),
        tabPanel("Map",
                 leafletOutput("map")),
        tabPanel("Chart",
                 plotlyOutput("chart"))
      )
    )
  )
)

# Create a server
server <- function(input, output) {
  
  # Create the table
  output$table <- renderDataTable({
    storms %>% filter(year == input$year)
  })
  
  # Create the map
  output$map <- renderLeaflet({
    storms %>% filter(year == input$year) %>%
      leaflet() %>% addTiles() %>%
      addMarkers(~longitude, ~latitude, popup = ~as.character(date))
  })
  
  # Create the chart
  output$chart <- renderPlotly({
    storms %>% filter(year == input$year) %>%
      plot_ly(x = ~date, y = ~wind) %>%
      add_lines()
  })
}

# Run the app
shinyApp(ui = ui, server = server)
