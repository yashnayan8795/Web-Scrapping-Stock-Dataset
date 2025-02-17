# Load required libraries
library(dplyr)
library(readr)

# Load Yahoo Finance yearly averages
yahoo_data <- read.csv("yearly_averages.csv", stringsAsFactors = FALSE)
# Load merged company data from Wikipedia
wiki_data <- read.csv("merged_company_data.csv", stringsAsFactors = FALSE)
# Standardize column names for merging
yahoo_data <- yahoo_data %>%
  rename(Symbol = symbol)  # Match column name with wiki_data
# Merge datasets using Symbol as the key
final_data <- yahoo_data %>%
  left_join(wiki_data, by = "Symbol") %>%
  # Clean and organize columns
  select(
    Symbol,
    Company = Company_Name,
    Year = year,
    avg_open,
    avg_high,
    avg_low,
    avg_close,
    avg_volume,
    GICS_Sector = GICS.Sector,
    GICS_Sub_Industry = GICS.Sub.Industry,
    Headquarters = Headquarters.Location,
    Founded,
    Industry,
    Index_Weighting = Index.weighting
  )
write.csv(final_data, "final_dataset.csv", row.names = FALSE)



# Handle NA values in critical columns
final_data <- final_data %>%
  mutate(
    across(c(avg_open, avg_high, avg_low, avg_close, avg_volume), 
           ~ ifelse(is.na(.), 0, .)),
    across(c(GICS_Sector, GICS_Sub_Industry, Headquarters, Industry), 
           ~ ifelse(is.na(.), "Unknown", .)),
    Index_Weighting = ifelse(is.na(Index_Weighting), 0, Index_Weighting)
  )
# Save final merged dataset
write.csv(final_data, "final_merged_dataset.csv", row.names = FALSE)
print("Final merged dataset saved to final_merged_dataset.csv")