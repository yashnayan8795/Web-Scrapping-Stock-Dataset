library(shiny)
library(DT)
library(plotly)
library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)
library(readr)
library(shinythemes)
library(shinyWidgets)
library(shinydashboard)
library(shinyjs)
library(stringr)

# ----------------------------
# Data Loading with Validation
# ----------------------------
load_stock_data <- function() {
  tryCatch({
    if (!file.exists("cleaned_stock_dataset.csv")) {
      stop("Data file 'cleaned_stock_dataset.csv' not found")
    }
    
    data <- read.csv("cleaned_stock_dataset.csv", stringsAsFactors = FALSE)
    
    if (nrow(data) == 0) {
      stop("Data file is empty")
    }
    
    # Required columns with default values
    required_columns <- list(
      ID = seq_len(nrow(data)),
      Symbol = NA_character_,
      Company = NA_character_,
      Year = as.integer(format(Sys.Date(), "%Y")),
      avg_open = NA_real_,
      avg_high = NA_real_,
      avg_low = NA_real_,
      avg_close = NA_real_,
      avg_volume = NA_real_,
      GICS_Sector = "Unknown",
      Index_Weighting = NA_real_,
      Stock_Range = NA_real_,
      Price_Change_Percent = NA_real_,
      Stock_Age = as.integer(format(Sys.Date(), "%Y")) - as.integer(format(Sys.Date(), "%Y"))
    )
    
    # Add missing columns
    for (col in names(required_columns)) {
      if (!col %in% names(data)) {
        data[[col]] <- required_columns[[col]]
      }
    }
    
    # Convert numeric columns safely
    numeric_cols <- c("Year", "avg_open", "avg_high", "avg_low", "avg_close",
                      "avg_volume", "Index_Weighting", "Stock_Range",
                      "Price_Change_Percent", "Stock_Age")
    
    data[numeric_cols] <- lapply(data[numeric_cols], function(x) {
      suppressWarnings(as.numeric(x))
    })
    
    # Validate data
    if (any(is.na(data$Symbol)) || any(is.na(data$Company))) {
      warning("Some records are missing Symbol or Company information")
    }
    
    return(data)
  }, error = function(e) {
    showNotification(paste("Error loading data:", e$message), type = "error")
    return(data.frame()) # Return empty dataframe as fallback
  })
}

initialData <- load_stock_data()

# ----------------------------
# UI Definition (Corrected)
# ----------------------------
ui <- fluidPage(
  theme = shinytheme("flatly"),
  useShinyjs(),
  tags$head(
    tags$style(HTML("
      .required-field::after {
        content: ' *';
        color: red;
        font-weight: bold;
      }
      .box-highlight {
        border-left: 4px solid #3c8dbc;
        padding: 15px;
        background-color: #f8f9fa;
        border-radius: 4px;
      }
      .chart-container {
        background: white;
        padding: 15px;
        border-radius: 5px;
        box-shadow: 0 1px 3px rgba(0,0,0,0.12);
      }
      .form-group {
        margin-bottom: 20px;
      }
      .control-label {
        font-weight: 500;
        color: #2c3e50;
      }
      .form-control {
        border: 1px solid #ddd;
        border-radius: 4px;
        padding: 8px 12px;
      }
      .btn {
        text-transform: none;
        font-weight: 500;
      }
      .btn-group {
        margin-bottom: 15px;
        width: 100%;
      }
      .btn-save {
        background-color: #34495e;
        color: white;
        margin-top: 10px;
        width: 100%;
      }
      hr {
        margin: 20px 0;
        border-top: 1px solid #ddd;
      }
      .help-block {
        color: #7b8a8b;
        font-size: 0.9em;
      }
    "))
  ),
  
  wellPanel(
    fluidRow(
      column(8, h2("Stock Data Dashboard", style = "margin-top: 0;")),
      column(4, actionButton("helpBtn", "Help", icon = icon("question-circle"), 
                             class = "btn-info pull-right"))
    ),
    fluidRow(
      column(4, textInput("search", "Search:", width = "100%", placeholder = "Symbol, Company, Year")),
      column(4, selectInput("sectorFilter", "Filter Sector:", choices = c("All"), selected = "All", width = "100%")),
      column(4, actionButton("resetFilters", "Reset Filters", icon = icon("refresh"), width = "100%"))
    )
  ),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      div(class = "box-highlight",
          h4("Manage Records", style = "margin-top: 0; margin-bottom: 20px; color: #2c3e50;"),
          
          div(class = "form-group",
              textInput("crudSymbol", 
                       label = span("Symbol", class = "required-field"), 
                       width = "100%")),
          
          div(class = "form-group",
              textInput("crudCompany", 
                       label = span("Company", class = "required-field"), 
                       width = "100%")),
          
          div(class = "form-group",
              numericInput("crudYear", 
                          label = span("Year", class = "required-field"), 
                          value = as.integer(format(Sys.Date(), "%Y")), 
                          min = 1900, 
                          max = 2100, 
                          width = "100%")),
          
          div(class = "form-group",
              numericInput("crudAvgOpen", 
                          label = span("Avg Open", class = "required-field"), 
                          value = NA, 
                          width = "100%")),
          
          div(class = "form-group",
              numericInput("crudAvgClose", 
                          label = span("Avg Close", class = "required-field"), 
                          value = NA, 
                          width = "100%")),
          
          div(class = "form-group",
              selectizeInput("crudSector", 
                           label = span("Sector", class = "required-field"), 
                           choices = unique(initialData$GICS_Sector), 
                           width = "100%")),
          
          div(class = "btn-group",
              fluidRow(
                column(4, actionButton("crudAdd", "Add", 
                                     icon = icon("plus"), 
                                     class = "btn-success", 
                                     width = "100%")),
                column(4, actionButton("crudUpdate", "Update", 
                                     icon = icon("edit"), 
                                     class = "btn-warning", 
                                     width = "100%")),
                column(4, actionButton("crudDelete", "Delete", 
                                     icon = icon("trash"), 
                                     class = "btn-danger", 
                                     width = "100%"))
              )
          ),
          
          actionButton("saveChanges", 
                      "Save to CSV", 
                      icon = icon("save"), 
                      class = "btn btn-save", 
                      width = "100%"),
          
          tags$hr(),
          
          helpText("Select a row in the table to edit", 
                  class = "help-block"),
          
          uiOutput("recordStats")
      )
    ),
    
    mainPanel(
      width = 9,
      tabsetPanel(
        id = "mainTabs",
        tabPanel("Data Explorer", DTOutput("stockTable")),
        tabPanel("Price Analysis", 
                 fluidRow(
                   column(4, selectInput("priceMetric", "Select Metric:", 
                                         choices = c("Open Price" = "avg_open", 
                                                     "High Price" = "avg_high",
                                                     "Low Price" = "avg_low",
                                                     "Close Price" = "avg_close"))),
                   column(4, sliderInput("priceYearRange", "Year Range:",
                                         min = min(initialData$Year, na.rm = TRUE),
                                         max = max(initialData$Year, na.rm = TRUE),
                                         value = c(min(initialData$Year, na.rm = TRUE),
                                                   max(initialData$Year, na.rm = TRUE)))),
                   column(4, selectizeInput("priceCompanies", "Select Companies:",
                                            choices = unique(initialData$Company),
                                            multiple = TRUE))
                 ),
                 plotlyOutput("priceTrendPlot")
        ),
        tabPanel("Volume & Performance", 
                 fluidRow(
                   column(4, selectInput("volumeYear", "Select Year:",
                                         choices = sort(unique(initialData$Year)))),
                   column(4, sliderInput("volumeBins", "Number of Bins:",
                                         min = 5, max = 50, value = 20)),
                   column(4, selectInput("sectorMetric", "Select Metric:",
                                         choices = c("Close Price" = "avg_close",
                                                     "Volume" = "avg_volume",
                                                     "Price Change %" = "Price_Change_Percent"))),
                   column(4, checkboxInput("showOutliers", "Show Outliers", value = FALSE))
                 ),
                 fluidRow(
                   column(6, plotlyOutput("volumeHistogram")),
                   column(6, plotlyOutput("sectorBoxplot"))
                 )
        ),
        tabPanel("Sector Trends", 
                 fluidRow(
                   column(4, selectInput("trendMetric", "Select Metric:",
                                         choices = c("Price Change %" = "Price_Change_Percent",
                                                     "Volume" = "avg_volume",
                                                     "Stock Age" = "Stock_Age"))),
                   column(4, sliderInput("trendYearRange", "Year Range:",
                                         min = min(initialData$Year, na.rm = TRUE),
                                         max = max(initialData$Year, na.rm = TRUE),
                                         value = c(min(initialData$Year, na.rm = TRUE),
                                                   max(initialData$Year, na.rm = TRUE))))
                 ),
                 plotlyOutput("sectorTrendPlot")
        ),
        tabPanel("Dashboard", 
                 fluidRow(
                   valueBoxOutput("totalStocksBox"),
                   valueBoxOutput("avgPriceBox"),
                   valueBoxOutput("topSectorBox"),
                   valueBoxOutput("priceRangeBox")
                 ),
                 fluidRow(
                   column(6, plotlyOutput("priceVolumeScatter")),
                   column(6, plotlyOutput("sectorPerformanceBar"))
                 ),
                 fluidRow(
                   column(12, DTOutput("sectorStatsTable"))
                 )
        )
      )
    )
  )
)

# ----------------------------
# Server Logic
# ----------------------------
server <- function(input, output, session) {
  # Reactive data storage
  stock_data <- reactiveVal(initialData)
  selected_row <- reactiveVal(NULL)
  
  # Update sector choices dynamically
  observe({
    sectors <- sort(unique(stock_data()$GICS_Sector))
    updateSelectInput(session, "sectorFilter", choices = c("All", sectors))
    updateSelectizeInput(session, "crudSector", choices = sectors)
  })
  
  # Filtered data with debounce
  display_data <- debounce(reactive({
    data <- stock_data()
    
    # Apply search filter
    if (nzchar(input$search)) {
      search_term <- tolower(input$search)
      data <- data %>%
        filter(
          str_detect(tolower(Symbol), search_term) |
            str_detect(tolower(Company), search_term) |
            str_detect(as.character(Year), search_term)
        )
    }
    
    # Apply sector filter
    if (input$sectorFilter != "All") {
      data <- data %>% filter(GICS_Sector == input$sectorFilter)
    }
    
    data
  }), 500)
  
  # Enhanced Data Table
  output$stockTable <- renderDT({
    req(nrow(display_data()) > 0)
    
    datatable(
      display_data() %>%
        select(Symbol, Company, Year, GICS_Sector, avg_open, avg_close, avg_volume, Price_Change_Percent),
      extensions = c('Buttons', 'Scroller'),
      options = list(
        scrollX = TRUE,
        scrollY = 500,
        scroller = TRUE,
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel', 'colvis'),
        columnDefs = list(
          list(targets = c(4:7), className = 'dt-right'),
          list(targets = 7, render = JS("function(data, type) {
            if (type === 'display') {
              var color = data >= 0 ? 'green' : 'red';
              return '<span style=\"color:' + color + '\">' + data + '%</span>';
            }
            return data;
          }"))
        ),
        selection = list(mode = 'single', selected = NULL),
        rownames = FALSE,
        class = 'cell-border stripe hover'
      )
    ) %>%
      formatCurrency(columns = c('avg_open', 'avg_close'), '$') %>%
      formatRound(columns = 'avg_volume', digits = 0) %>%
      formatPercentage(columns = 'Price_Change_Percent', digits = 2)
  })
    
    # CRUD Operations
    observeEvent(input$stockTable_rows_selected, {
      selected <- input$stockTable_rows_selected
      if (length(selected) == 1) {
        row_data <- display_data()[selected, ]
        selected_row(row_data$ID)
        
        updateTextInput(session, "crudSymbol", value = row_data$Symbol)
        updateTextInput(session, "crudCompany", value = row_data$Company)
        updateNumericInput(session, "crudYear", value = row_data$Year)
        updateNumericInput(session, "crudAvgOpen", value = row_data$avg_open)
        updateNumericInput(session, "crudAvgClose", value = row_data$avg_close)
        updateSelectizeInput(session, "crudSector", selected = row_data$GICS_Sector)
      }
    })
    
    # Add new record
    observeEvent(input$crudAdd, {
      tryCatch({
        req(input$crudSymbol, input$crudCompany, input$crudYear,
            input$crudAvgOpen, input$crudAvgClose, input$crudSector)
        
        new_id <- max(stock_data()$ID, 0) + 1
        new_record <- tibble(
          ID = new_id,
          Symbol = str_to_upper(input$crudSymbol),
          Company = input$crudCompany,
          Year = as.integer(input$crudYear),
          avg_open = as.numeric(input$crudAvgOpen),
          avg_high = NA_real_,
          avg_low = NA_real_,
          avg_close = as.numeric(input$crudAvgClose),
          avg_volume = NA_real_,
          GICS_Sector = input$crudSector,
          Index_Weighting = NA_real_,
          Stock_Range = NA_real_,
          Price_Change_Percent = NA_real_,
          Stock_Age = as.integer(format(Sys.Date(), "%Y")) - as.integer(input$crudYear)
        )
        
        # Validate no duplicate symbol/year
        duplicates <- stock_data() %>%
          filter(Symbol == new_record$Symbol, Year == new_record$Year)
        
        if (nrow(duplicates) > 0) {
          stop("This symbol/year combination already exists!")
        }
        
        stock_data(bind_rows(stock_data(), new_record))
        showNotification("Record added successfully!", type = "message")
        reset_form()
      }, error = function(e) {
        showNotification(paste("Error:", e$message), type = "error")
      })
    })
    
    # Update record
    observeEvent(input$crudUpdate, {
      tryCatch({
        req(selected_row(), input$crudSymbol, input$crudCompany, input$crudYear,
            input$crudAvgOpen, input$crudAvgClose, input$crudSector)
        
        updated_data <- stock_data() %>%
          mutate(
            across(everything(), ~ifelse(
              ID == selected_row(),
              case_when(
                names(cur_column()) == "Symbol" ~ str_to_upper(input$crudSymbol),
                names(cur_column()) == "Company" ~ input$crudCompany,
                names(cur_column()) == "Year" ~ as.integer(input$crudYear),
                names(cur_column()) == "avg_open" ~ as.numeric(input$crudAvgOpen),
                names(cur_column()) == "avg_close" ~ as.numeric(input$crudAvgClose),
                names(cur_column()) == "GICS_Sector" ~ input$crudSector,
                names(cur_column()) == "Stock_Age" ~ as.integer(format(Sys.Date(), "%Y")) - as.integer(input$crudYear),
                TRUE ~ .x
              ),
              .x
            )
            ))
        
        stock_data(updated_data)
        showNotification("Record updated successfully!", type = "message")
      }, error = function(e) {
        showNotification(paste("Error:", e$message), type = "error")
      })
    })
    
    # Delete record
    observeEvent(input$crudDelete, {
      req(selected_row())
      
      showModal(modalDialog(
        title = "Confirm Deletion",
        paste("Are you sure you want to delete", input$crudSymbol, "for year", input$crudYear, "?"),
        footer = tagList(
          modalButton("Cancel"),
          actionButton("confirmDelete", "Delete", class = "btn-danger")
        )
      ))
    })
    
    observeEvent(input$confirmDelete, {
      tryCatch({
        stock_data(stock_data() %>% filter(ID != selected_row()))
        reset_form()
        removeModal()
        showNotification("Record deleted successfully!", type = "message")
      }, error = function(e) {
        showNotification(paste("Error:", e$message), type = "error")
      })
    })
    
    # Save to CSV
    observeEvent(input$saveChanges, {
      tryCatch({
        write.csv(stock_data(), "updated_stock_data.csv", row.names = FALSE)
        showNotification("Data saved successfully to updated_stock_data.csv", type = "message")
      }, error = function(e) {
        showNotification(paste("Error saving file:", e$message), type = "error")
      })
    })
    
    # Reset form function
    reset_form <- function() {
      updateTextInput(session, "crudSymbol", value = "")
      updateTextInput(session, "crudCompany", value = "")
      updateNumericInput(session, "crudYear", value = as.integer(format(Sys.Date(), "%Y")))
      updateNumericInput(session, "crudAvgOpen", value = NA)
      updateNumericInput(session, "crudAvgClose", value = NA)
      updateSelectizeInput(session, "crudSector", selected = "")
      selected_row(NULL)
    }
    
    observeEvent(input$resetFilters, {
      updateTextInput(session, "search", value = "")
      updateSelectInput(session, "sectorFilter", selected = "All")
      reset_form()
    })
    
    # =============================================
    # PRICE ANALYSIS VISUALIZATIONS
    # =============================================
    
    output$priceTrendPlot <- renderPlotly({
      req(input$priceMetric, input$priceYearRange)
      
      plot_data <- stock_data() %>%
        filter(Year >= input$priceYearRange[1] & Year <= input$priceYearRange[2])
      
      if (!is.null(input$priceCompanies) && length(input$priceCompanies) > 0) {
        plot_data <- plot_data %>% filter(Company %in% input$priceCompanies)
      }
      
      if (nrow(plot_data) == 0) {
        return(plotly_empty(type = "scatter") %>% 
                 layout(title = "No data available for selected filters"))
      }
      
      metric_name <- switch(input$priceMetric,
                            "avg_open" = "Open Price",
                            "avg_high" = "High Price",
                            "avg_low" = "Low Price",
                            "avg_close" = "Close Price")
      
      plot_ly(plot_data, x = ~Year, y = ~get(input$priceMetric),
              color = ~Company, type = 'scatter', mode = 'lines+markers',
              hoverinfo = "text",
              text = ~paste0("<b>", Company, "</b>\n",
                             "Year: ", Year, "\n",
                             metric_name, ": $", round(get(input$priceMetric), 2)),
              line = list(width = 2),
              marker = list(size = 8)) %>%
        layout(title = paste("Stock", metric_name, "Trend"),
               xaxis = list(title = "Year", tickmode = "linear"),
               yaxis = list(title = paste(metric_name, "(USD)"), tickprefix = "$"),
               hovermode = "closest",
               legend = list(orientation = "h", y = -0.2))
    })
    
    # =============================================
    # VOLUME & PERFORMANCE VISUALIZATIONS
    # =============================================
    
    output$volumeHistogram <- renderPlotly({
      req(input$volumeYear)
      
      plot_data <- stock_data() %>% 
        filter(Year == input$volumeYear) %>%
        mutate(volume_millions = avg_volume / 1000000)
      
      # Remove top 1% outliers for better visualization
      volume_cutoff <- quantile(plot_data$volume_millions, 0.99, na.rm = TRUE)
      plot_data <- plot_data %>% filter(volume_millions <= volume_cutoff)
      
      plot_ly(plot_data, x = ~volume_millions, type = "histogram",
              nbinsx = input$volumeBins,
              marker = list(color = "#3c8dbc",
                            line = list(color = "white", width = 1))) %>%
        layout(title = paste("Trading Volume Distribution -", input$volumeYear),
               xaxis = list(title = "Average Volume (Millions)"),
               yaxis = list(title = "Count"))
    })
    
    output$sectorBoxplot <- renderPlotly({
      req(input$sectorMetric)
      
      plot_data <- stock_data()
      
      if (!input$showOutliers) {
        # Calculate IQR for outlier detection
        qnt <- quantile(plot_data[[input$sectorMetric]], probs = c(0.25, 0.75), na.rm = TRUE)
        iqr <- 1.5 * IQR(plot_data[[input$sectorMetric]], na.rm = TRUE)
        plot_data <- plot_data %>%
          filter(get(input$sectorMetric) > (qnt[1] - iqr) & 
                   get(input$sectorMetric) < (qnt[2] + iqr))
      }
      
      metric_name <- switch(input$sectorMetric,
                            "avg_close" = "Average Close Price",
                            "avg_volume" = "Average Volume",
                            "Price_Change_Percent" = "Price Change %")
      
      plot_ly(plot_data, y = ~get(input$sectorMetric), color = ~GICS_Sector,
              type = "box", boxpoints = ifelse(input$showOutliers, "all", FALSE),
              hoverinfo = "y") %>%
        layout(title = paste("Sector Comparison -", metric_name),
               yaxis = list(title = metric_name,
                            tickprefix = ifelse(input$sectorMetric == "avg_close", "$", ""),
                            ticksuffix = ifelse(input$sectorMetric == "Price_Change_Percent", "%", "")),
               showlegend = FALSE)
    })
    
    # =============================================
    # SECTOR TRENDS VISUALIZATIONS
    # =============================================
    
    output$sectorTrendPlot <- renderPlotly({
      req(input$trendMetric, input$trendYearRange)
      
      plot_data <- stock_data() %>%
        filter(Year >= input$trendYearRange[1] & Year <= input$trendYearRange[2]) %>%
        group_by(GICS_Sector, Year) %>%
        summarise(
          metric_value = mean(get(input$trendMetric), na.rm = TRUE),
          count = n(),
          .groups = "drop"
        )
      
      metric_name <- switch(input$trendMetric,
                            "Price_Change_Percent" = "Average Price Change %",
                            "avg_volume" = "Average Volume",
                            "Stock_Age" = "Average Stock Age")
      
      plot_ly(plot_data, x = ~Year, y = ~metric_value,
              color = ~GICS_Sector, type = 'scatter', mode = 'lines+markers',
              hoverinfo = "text",
              text = ~paste0("<b>", GICS_Sector, "</b>\n",
                             "Year: ", Year, "\n",
                             metric_name, ": ", round(metric_value, 2),
                             ifelse(input$trendMetric == "Price_Change_Percent", "%", ""),
                             "\nStocks: ", count),
              line = list(width = 3),
              marker = list(size = 8)) %>%
        layout(title = paste("Sector Trend -", metric_name),
               xaxis = list(title = "Year"),
               yaxis = list(title = metric_name,
                            ticksuffix = ifelse(input$trendMetric == "Price_Change_Percent", "%", "")),
               hovermode = "closest")
    })
    
    # =============================================
    # DASHBOARD VISUALIZATIONS
    # =============================================
    
    output$totalStocksBox <- renderValueBox({
      count <- nrow(stock_data())
      valueBox(
        value = format(count, big.mark = ","),
        subtitle = "Total Stocks",
        icon = icon("database"),
        color = "aqua"
      )
    })
    
    output$avgPriceBox <- renderValueBox({
      avg_price <- mean(stock_data()$avg_close, na.rm = TRUE)
      valueBox(
        value = dollar_format()(avg_price),
        subtitle = "Average Close Price",
        icon = icon("dollar-sign"),
        color = "green"
      )
    })
    
    output$topSectorBox <- renderValueBox({
      sector_counts <- stock_data() %>%
        count(GICS_Sector) %>%
        arrange(desc(n))
      
      valueBox(
        value = sector_counts$GICS_Sector[1],
        subtitle = paste("Top Sector (", sector_counts$n[1], "stocks)"),
        icon = icon("star"),
        color = "yellow"
      )
    })
    
    output$priceRangeBox <- renderValueBox({
      range_data <- stock_data() %>%
        summarise(min = min(avg_close, na.rm = TRUE),
                  max = max(avg_close, na.rm = TRUE))
      
      valueBox(
        value = paste(dollar_format()(range_data$min), "-", dollar_format()(range_data$max)),
        subtitle = "Price Range",
        icon = icon("arrows-alt-h"),
        color = "red"
      )
    })
    
    output$priceVolumeScatter <- renderPlotly({
      plot_data <- stock_data() %>%
        filter(Year == max(Year, na.rm = TRUE)) %>%
        mutate(volume_millions = avg_volume / 1000000)
      
      plot_ly(plot_data, x = ~volume_millions, y = ~avg_close,
              color = ~GICS_Sector,
              type = "scatter", mode = "markers",
              hoverinfo = "text",
              text = ~paste0("<b>", Company, "</b>\n",
                             "Price: $", round(avg_close, 2), "\n",
                             "Volume: ", round(volume_millions, 1), "M\n",
                             "Sector: ", GICS_Sector)) %>%
        layout(title = "Current Year: Price vs Volume",
               xaxis = list(title = "Volume (Millions)"),
               yaxis = list(title = "Close Price ($)", tickprefix = "$"))
    })
    
    output$sectorPerformanceBar <- renderPlotly({
      plot_data <- stock_data() %>%
        filter(Year == max(Year, na.rm = TRUE)) %>%
        group_by(GICS_Sector) %>%
        summarise(avg_performance = mean(Price_Change_Percent, na.rm = TRUE)) %>%
        arrange(desc(avg_performance)) %>%
        head(5)
      
      plot_ly(plot_data, x = ~reorder(GICS_Sector, avg_performance), y = ~avg_performance,
              type = "bar", color = ~GICS_Sector,
              hoverinfo = "text",
              text = ~paste0("<b>", GICS_Sector, "</b>\n",
                             "Avg Change: ", round(avg_performance, 1), "%")) %>%
        layout(title = "Top Performing Sectors (Current Year)",
               xaxis = list(title = ""),
               yaxis = list(title = "Average Price Change %", ticksuffix = "%"),
               showlegend = FALSE)
    })
    
    output$sectorStatsTable <- renderDT({
      sector_stats <- stock_data() %>%
        group_by(GICS_Sector) %>%
        summarise(
          `Stock Count` = n(),
          `Avg Price` = mean(avg_close, na.rm = TRUE),
          `Median Volume` = median(avg_volume, na.rm = TRUE),
          `Avg Price Change %` = mean(Price_Change_Percent, na.rm = TRUE),
          `Avg Stock Age` = mean(Stock_Age, na.rm = TRUE)
        ) %>%
        arrange(desc(`Stock Count`))
      
      datatable(
        sector_stats,
        extensions = 'Buttons',
        options = list(
          dom = 'Bfrtip',
          buttons = c('copy', 'csv', 'excel'),
          pageLength = 5
        ),
        rownames = FALSE
      ) %>%
        formatCurrency("Avg Price", "$") %>%
        formatRound("Median Volume", 0) %>%
        formatPercentage("Avg Price Change %", 1) %>%
        formatRound("Avg Stock Age", 1)
    })
    
    # Help system
    observeEvent(input$helpBtn, {
      showModal(modalDialog(
        title = "Dashboard Help",
        HTML("
        <h4>How to Use This Dashboard</h4>
        <p><strong>Data Explorer:</strong> View and filter the complete dataset. Click on rows to populate the edit form.</p>
        <p><strong>Price Analysis:</strong> Compare price trends across companies and years.</p>
        <p><strong>Volume & Performance:</strong> Analyze trading volume distributions and sector comparisons.</p>
        <p><strong>Sector Trends:</strong> Track performance metrics across sectors over time.</p>
        <p><strong>Dashboard:</strong> Key metrics and visual summaries of the data.</p>
        <h4>CRUD Operations</h4>
        <ul>
          <li><strong>Add:</strong> Fill all required fields (marked with *) and click Add</li>
          <li><strong>Update:</strong> Select a row, modify values, and click Update</li>
          <li><strong>Delete:</strong> Select a row and click Delete</li>
          <li><strong>Save:</strong> Export current data to CSV</li>
        </ul>
      "),
        easyClose = TRUE,
        footer = modalButton("Close")
      ))
    })
}

# Run the application
shinyApp(ui = ui, server = server)