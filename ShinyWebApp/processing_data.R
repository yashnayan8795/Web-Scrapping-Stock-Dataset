# Load necessary libraries
library(dplyr)

# Load dataset
file_path <- "final_merged_dataset.csv"  # Change path if needed
stock_data <- read.csv(file_path, stringsAsFactors = FALSE)

# Check for non-numeric values in 'Founded'
stock_data$Founded <- suppressWarnings(as.numeric(stock_data$Founded))  # Convert and ignore warnings
stock_data$Index_Weighting <- suppressWarnings(as.numeric(stock_data$Index_Weighting))  # Convert safely

# Replace NAs in 'Founded' with median value (or any logical value)
stock_data$Founded[is.na(stock_data$Founded)] <- median(stock_data$Founded, na.rm = TRUE)

# Replace NAs in 'Index_Weighting' with mean value (or any logical value)
stock_data$Index_Weighting[is.na(stock_data$Index_Weighting)] <- mean(stock_data$Index_Weighting, na.rm = TRUE)

# Create new derived features
stock_data <- stock_data %>%
  mutate(
    Stock_Range = avg_high - avg_low,
    Price_Change_Percent = ((avg_close - avg_open) / avg_open) * 100,
    Stock_Age = Year - Founded
  )

# Save cleaned data sets
write.csv(stock_data, "cleaned_stock_dataset.csv", row.names = FALSE)

# Display the first few rows
head(stock_data)
