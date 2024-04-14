# UI
library(viridis)
library(ggplot2)
library(dplyr)
library(readr)
library(shiny)

laptop_data <- read_csv("laptop_data.csv")

#reduced CPUs and top 9 brands
filtered_data <- laptop_data %>%
  group_by(Cpu) %>%
  filter(n() > 40) %>%
  mutate(Price = Price * 0.016) %>%
  mutate(Weight_numeric = as.numeric(sub("kg", "", as.character(Weight))))

options(scipen=999)
ui <- fluidPage(
  titlePanel("Laptop Data Analysis"),
  sidebarLayout(
    sidebarPanel(
      selectInput("variable", "Select a variable:",
                  choices = c("Company", "Cpu", "Ram", "Weight_numeric","TypeName")),
      uiOutput("slider")
    ),
    mainPanel(
      plotOutput("plot")
    )
  )
)

# Server
server <- function(input, output, session) {
  
  output$slider <- renderUI({
    if (input$variable %in% c("Company", "Cpu", "Ram", "TypeName")) {
      return(NULL)  # No slider for categorical variables
    } else {
      max_weight <- max(filtered_data$Weight_numeric, na.rm = TRUE)
      sliderInput("weightRange", "Select weight range:",
                  min = 0, max = max_weight, value = c(0, max_weight), step = 0.1)
    }
  })
  
  output$plot <- renderPlot({
    if (input$variable %in% c("Company", "Cpu", "Ram", "TypeName")) {
      ggplot(filtered_data, aes_string(x = input$variable, y = "Price", color = input$variable)) +
        geom_point(size = 3) +
        labs(x = input$variable, y = "Price (SGD)", color = input$variable)
    } else {
      ggplot(filtered_data %>% filter(Weight_numeric >= input$weightRange[1] & Weight_numeric <= input$weightRange[2]), 
             aes_string(x = input$variable, y = "Price", color = input$variable)) +
        geom_point(size = 3) +
        labs(x = input$variable, y = "Price (SGD)", color = input$variable)
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)

