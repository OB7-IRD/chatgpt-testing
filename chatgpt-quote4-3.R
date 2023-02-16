library(shiny)
library(plotly)
library(dplyr)
library(leaflet)
library(bs4Dash)

# Load the storms dataset
data(storms)

# Create a UI
ui <- dashboardPage(
  header = dashboardHeader(),
  sidebar = dashboardSidebar(
    selectInput("year", "Select Year:",
                choices = unique(storms$year),
                selected = 2000),
    sidebarMenu(
      menuItem("Table", tabName = "table", icon = icon("table")),
      menuItem("Map", tabName = "map", icon = icon("map")),
      menuItem("Chart", tabName = "chart", icon = icon("chart-line"))
    )
  ),
  body = dashboardBody(
    tabItems(
      tabItem(tabName = "table",
              fluidRow(
                box(
                  title = "Table",
                  dataTableOutput("table")
                )
              )
      ),
      tabItem(tabName = "map",
              fluidRow(
                box(
                  title = "Map",
                  leafletOutput("map")
                )
              )
      ),
      tabItem(tabName = "chart",
              fluidRow(
                box(
                  title = "Chart",
                  plotlyOutput("chart")
                )
              )
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
      addMarkers(lng = ~long, lat = ~lat, popup = ~name)
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
