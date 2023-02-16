library(shiny)
library(ggplot2)
library(dplyr)
library(plotly)

# Load the iris dataset
data(iris)

# Define UI
ui <- fluidPage(
  titlePanel("Iris Dataset"),
  sidebarLayout(
    sidebarPanel(
      selectInput("xcol", "X-Axis Variable", names(iris)),
      selectInput("ycol", "Y-Axis Variable", names(iris),
                  selected = names(iris)[[2]]),
      radioButtons("plottype", "Plot Type",
                   c("Scatter Plot" = "scatter",
                     "Box Plot" = "box"))
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Table", dataTableOutput("table")),
        tabPanel("Plot", plotlyOutput("plot"))
      )
    )
  )
)

# Define server logic
server <- function(input, output) {

  # Create a reactive subset of the iris dataset based on user inputs
  iris_subset <- reactive({
    iris %>%
      select(input$xcol, input$ycol)
  })

  # Render the table
  output$table <- renderDataTable({
    iris_subset()
  })

  # Render the plot
  output$plot <- renderPlotly({
    if (input$plottype == "scatter") {
      p <- ggplot(iris_subset(), aes_string(x = input$xcol, y = input$ycol)) +
        geom_point()
    } else {
      p <- ggplot(iris_subset(), aes_string(x = input$xcol, y = input$ycol)) +
        geom_boxplot()
    }
    ggplotly(p)
  })

}

# Run the app
shinyApp(ui = ui, server = server)
