library(shiny)
library(ggplot2)
library(leaflet)

# Load the iris dataset
data(iris)

# Load the storms dataset
data(storms)

# Define UI
ui <- fluidPage(
  titlePanel("Iris and Storms Data Dashboard"),
  sidebarLayout(
    sidebarPanel(
      # Tab Selector
      tabsetPanel(
        tabPanel("Iris Dataset", 
                 h4("Iris Data"),
                 dataTableOutput("iris_table"),
                 h4("Iris Sepal Length"),
                 plotOutput("iris_plot1"),
                 h4("Iris Sepal Width"),
                 plotOutput("iris_plot2")
                 ),
        tabPanel("Storms Dataset",
                 h4("Storms Data"),
                 dataTableOutput("storms_table"),
                 h4("Storms Map"),
                 leafletOutput("storms_map"),
                 h4("Storms Events in 2015"),
                 plotOutput("storms_plot")
                 )
      )
    ),
    mainPanel()
  )
)

# Define Server
server <- function(input, output) {
  
  # Iris Dataset
  output$iris_table <- renderDataTable({
    iris
  })
  
  output$iris_plot1 <- renderPlot({
    ggplot(iris, aes(x = Sepal.Length)) +
      geom_histogram(fill = "blue", bins = 30) +
      labs(title = "Distribution of Sepal Length")
  })
  
  output$iris_plot2 <- renderPlot({
    ggplot(iris, aes(x = Sepal.Width)) +
      geom_histogram(fill = "red", bins = 30) +
      labs(title = "Distribution of Sepal Width")
  })
  
  # Storms Dataset
  output$storms_table <- renderDataTable({
    storms
  })
  
  output$storms_map <- renderLeaflet({
    leaflet(storms) %>%
      addTiles() %>%
      addMarkers(~longitude, ~latitude)
  })
  
  output$storms_plot <- renderPlot({
    storms_2015 <- storms[year(storms$date) == 2015, ]
    ggplot(storms_2015, aes(x = factor(status))) +
      geom_bar(fill = "green") +
      labs(title = "Storms Events in 2015")
  })
  
}

# Run the app
shinyApp(ui, server)
