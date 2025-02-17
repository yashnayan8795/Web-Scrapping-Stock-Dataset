library(quantmod)
library(rvest)
library(dplyr)
library(purrr) 
library(lubridate)
library(readr)
library(pander)
library(httr)


# Set browser-like user-agent to avoid Yahoo Finance blocks
options(HTTPUserAgent = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/58.0.3029.110 Safari/537.36")
# Read symbols and company names from the CSV file
company_data <- read.csv("company_names_and_symbols.csv", stringsAsFactors = FALSE)
symbols <- company_data$Symbol
enterprise_names <- company_data$Company_Name
# Fetch data for all symbols
stock_data <- list()
for (i in seq_along(symbols)) {
  symbol <- symbols[i]
  enterprise <- enterprise_names[i]
  tryCatch({
    # Fetch data from Yahoo Finance
    data <- getSymbols(
      symbol, 
      src = "yahoo", 
      from = "2019-01-01", 
      to = "2024-01-01", 
      auto.assign = FALSE
    )
    # Convert to dataframe with standardized column names
    data_df <- data.frame(
      date = index(data),
      symbol = symbol,
      enterprise = enterprise,
      open = as.numeric(data[, paste0(symbol, ".Open")]),
      high = as.numeric(data[, paste0(symbol, ".High")]),
      low = as.numeric(data[, paste0(symbol, ".Low")]),
      close = as.numeric(data[, paste0(symbol, ".Close")]),
      volume = as.numeric(data[, paste0(symbol, ".Volume")])
    )
    # Append to list (more efficient)
    stock_data[[symbol]] <- data_df
    # Debug: Print number of rows fetched
    message("Fetched ", nrow(data_df), " rows for ", symbol)
  }, error = function(e) {
    message("Failed to fetch data for ", symbol, ": ", e$message)
  })
}
# Combine list into a single dataframe
stock_data <- bind_rows(stock_data)
# Check total rows and unique symbols
message("Total rows fetched: ", nrow(stock_data))
message("Unique symbols: ", paste(unique(stock_data$symbol), collapse = ", "))
# Calculate year-wise averages
# Calculate year-wise averages
yearly_averages <- stock_data %>%
  mutate(
    date = as.Date(date),  # Fix: Convert to Date type
    year = year(date)
  ) %>%
  group_by(symbol, enterprise, year) %>%
  summarise(
    avg_open = mean(open, na.rm = TRUE),
    avg_high = mean(high, na.rm = TRUE),
    avg_low = mean(low, na.rm = TRUE),
    avg_close = mean(close, na.rm = TRUE),
    avg_volume = mean(volume, na.rm = TRUE)
  ) %>%
  ungroup()
# Save the yearly averages to a CSV file
write.csv(yearly_averages, "yearly_averages.csv", row.names = FALSE)
print("Yearly averages saved to yearly_averages.csv")