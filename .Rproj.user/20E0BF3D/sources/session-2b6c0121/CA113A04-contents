library(shiny)

ui <- fluidPage(
  titlePanel("Kiera's customised example"),
  sidebarLayout(
    sidebarPanel(
      selectInput("dataset", "Choose a dataset or something, idk:",
                  choices = c("rock", "pressure", "cars", "faithful")),
      numericInput("obs", "Number of observations to view:", 10),
      checkboxInput("numeric_only", "Display numeric columns only", FALSE),
      sliderInput("slider_obs", "Number of observations to view:",
                  min = 1, max = 100, value = 10),
      helpText("Note: while the data view will show only the specified",
               "number of observations, the summary will still be based",
               "on the full dataset."),
      actionButton("update", "Update View")
    ),
    mainPanel(
      h4("summary table here"),
      verbatimTextOutput("summary"),
      h4("all the observations you want!"),
      tableOutput("view")
    )
  )
)

server <- function(input, output) {
  
  datasetInput <- eventReactive(input$update, {
    switch(input$dataset,
           "rock" = rock,
           "pressure" = pressure,
           "cars" = cars,
           "faithful" = faithful)
  }, ignoreNULL = FALSE)
  
  # Generate a summary of the dataset ----
  output$summary <- renderPrint({
    dataset <- datasetInput()
    summary(dataset)
  })
  
  output$view <- renderTable({
    dataset <- datasetInput()
    if (input$numeric_only) {
      dataset <- dataset[, sapply(dataset, is.numeric)]
    }
    head(dataset, n = isolate(input$slider_obs))
  })
  
}

shinyApp(ui, server)
