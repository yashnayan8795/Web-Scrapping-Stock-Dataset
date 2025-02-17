library(shiny)
library(DT)
library(ggplot2)
library(dplyr)

stock_data <- read.csv("cleaned_stock_data_updated.csv")

ui <- fluidPage(
  titlePanel("Stock Data Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("sector", "Select GICS Sector:", choices = unique(stock_data$GICS_Sector)),
      selectInput("industry", "Select GICS Sub-Industry:", choices = NULL),
      sliderInput("price_range", "Price Range:",
                  min = min(stock_data$Closing_Price, na.rm = TRUE),
                  max = max(stock_data$Closing_Price, na.rm = TRUE),
                  value = c(min(stock_data$Closing_Price, na.rm = TRUE), max(stock_data$Closing_Price, na.rm = TRUE))),
      checkboxGroupInput("market_movement", "Market Movement:",
                         choices = unique(stock_data$market_movement),
                         selected = unique(stock_data$market_movement))
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Data Table", DTOutput("stock_table")),
        tabPanel("Price Distribution", plotOutput("price_plot")),
        tabPanel("Volume Analysis", plotOutput("volume_plot"))
      )
    )
  )
)

server <- function(input, output, session) {
  # Update the industry choices based on the selected sector
  observe({
    updateSelectInput(session, "industry",
                      choices = unique(stock_data$GICS_Sub_Industry[stock_data$GICS_Sector == input$sector]))
  })
  
  # Filter data based on user inputs
  filtered_data <- reactive({
    stock_data %>%
      filter(GICS_Sector == input$sector,
             GICS_Sub_Industry == input$industry,
             Closing_Price >= input$price_range[1] & Closing_Price <= input$price_range[2],
             market_movement %in% input$market_movement)
  })
  
  # Render the data table
  output$stock_table <- renderDT({
    datatable(filtered_data(), options = list(pageLength = 10))
  })
  
  # Render the price distribution plot
  output$price_plot <- renderPlot({
    ggplot(filtered_data(), aes(x = Closing_Price)) +
      geom_histogram(binwidth = 10, fill = "blue", color = "black") +
      labs(title = "Distribution of Closing Prices", x = "Closing Price", y = "Count")
  })
  
  # Render the volume analysis plot
  output$volume_plot <- renderPlot({
    ggplot(filtered_data(), aes(x = Trade_Volume, y = Closing_Price)) +
      geom_point(aes(color = market_movement)) +
      labs(title = "Trade Volume vs Closing Price", x = "Trade Volume", y = "Closing Price")
  })
}

# Run the app
shinyApp(ui = ui, server = server)



library(rsconnect)

rsconnect::setAccountInfo(name='yash-nayan-22bds0274',
                          token='E4850DDD525573A15F45291D8631F341',
                          secret='uuyygABUC9O4CIxKCH8WCqTZHWQyENuCmGe8QAwH')

rsconnect::deployApp("D:\\VIT-Vellore\\22BDS0274-Yash-Nayan\\coding\\R-lab\\final-project\\shiny_dashboard.R")

