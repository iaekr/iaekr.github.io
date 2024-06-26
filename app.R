library(shiny)
library(tidyverse)

# Read in the dataset

laptop_data <- read_csv("laptop_data.csv")
laptop_data
options(scipen=999)

# Define UI for application
ui <- fluidPage(
  titlePanel("Laptop Prices"),
  sidebarLayout(
    sidebarPanel(
      selectInput("currency", "Select Currency:",
                  choices = c("SGD", "INR", "USD"),
                  selected = "SGD"),
      ),
    mainPanel(
      plotOutput("distPlot")
    )
  )
)

# Define server logic
server <- function(input, output) {
  output$distPlot <- renderPlot({
    # Convert prices to selected currency
    brand_quantity <- laptop_data %>%
      select(Company, Weight, Price) %>%
      mutate(Price = case_when(
        input$currency == "SGD" ~ Price*0.012,
        input$currency == "INR" ~ Price,
        input$currency == "USD" ~ Price *0.016
      ))
    
    # Filter brands with more than 10 laptops
    brand_quantity <- brand_quantity %>%
      group_by(Company) %>%
      mutate(count = n()) %>%
      filter(count > 10) %>%
      ungroup() %>%
      select(-count)
    
    # Plot
    ggplot(brand_quantity, aes(x = Price, y = Company)) +
      geom_boxplot() +
      labs(title = paste("Laptop Prices (", input$currency, ")", sep = ""))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
