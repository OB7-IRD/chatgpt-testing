library(shiny)
library(ggplot2)
library(plotly)

# Load datasets
data(iris)
data(storms)

# Define UI
ui <- fluidPage(
  titlePanel("Multi-tab dashboard"),
  sidebarLayout(
    sidebarPanel(
      h3("Choose a dataset:"),
      radioButtons("dataset", "Dataset:", 
                   choices = c("Iris", "Storms"), selected = "Iris")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Dataset summary",
          tableOutput("summary"),
          plotOutput("histogram")
        ),
        tabPanel("Storms dataset",
          tabsetPanel(
            tabPanel("2015",
              tableOutput("storm_table_2015"),
              plotOutput("storm_map_2015"),
              plotlyOutput("storm_chart_2015")
            )
          )
        )
      )
    )
  )
)

# Define server
server <- function(input, output) {
  
  # Create summary table and histogram for iris dataset
  output$summary <- renderTable({
    if (input$dataset == "Iris") {
      summary(iris)
    } else {
      summary(storms)
    }
  })
  
  output$histogram <- renderPlot({
    if (input$dataset == "Iris") {
      ggplot(iris, aes(x = Sepal.Length)) + geom_histogram()
    } else {
      ggplot(storms, aes(x = DATE)) + geom_histogram() + xlab("Date")
    }
  })
  
  # Create table, map, and chart for storms dataset for 2015
  output$storm_table_2015 <- renderTable({
    subset(storms, YEAR == 2015)
  })
  
  output$storm_map_2015 <- renderPlot({
    # Create map using plotly
    if (!is.null(input$dataset) && input$dataset == "Storms") {
      p <- ggplot(storms, aes(x = -LONGITUDE, y = LATITUDE)) + 
        geom_point(aes(color = CATEGORY)) + 
        scale_color_gradient(low = "green", high = "red") +
        labs(x = "Longitude", y = "Latitude", title = "Storms in 2015")
      ggplotly(p)
    }
  })
  
  output$storm_chart_2015 <- renderPlotly({
    # Create chart using plotly
    if (!is.null(input$dataset) && input$dataset == "Storms") {
      storms_2015 <- subset(storms, YEAR == 2015)
      p <- ggplot(storms_2015, aes(x = as.factor(MONTH_NAME), fill = as.factor(CATEGORY))) + 
        geom_bar(position = "dodge") + 
        labs(x = "Month", y = "Count", title = "Storms in 2015 by category")
      plotly_build(p)
    }
  })
}

# Run app
shinyApp(ui, server)
