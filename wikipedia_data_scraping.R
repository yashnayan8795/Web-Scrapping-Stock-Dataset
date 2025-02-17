scrape_sp500 <- function(url) {
  tryCatch({
    page <- read_html(GET(url, user_agent("Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/58.0.3029.110 Safari/537.36")))
    table <- html_nodes(page, "table.wikitable")[[1]]
    df <- html_table(table, fill = TRUE)
    sp500_data <- df %>%
      select(
        Symbol = Symbol,
        `GICS Sector` = `GICS Sector`,
        `GICS Sub-Industry` = `GICS Sub-Industry`,
        `Headquarters Location` = `Headquarters Location`,
        `Date added` = `Date added`,
        CIK = CIK,
        Founded = Founded
      )
    return(sp500_data)
  }, error = function(e) {
    message("Error: ", e$message)
    return(NULL)
  })
}
url_sp500 <- "https://en.wikipedia.org/wiki/List_of_S%26P_500_companies"
sp500_data <- scrape_sp500(url_sp500)

# Defin URLs
url_sp500 <- "https://en.wikipedia.org/wiki/List_of_S%26P_500_companies"
url_market_cap <- "https://en.wikipedia.org/wiki/List_of_public_corporations_by_market_capitalization"
# Function to extract company names and symbols from a Wikipedia table
extract_company_data <- function(url, name_column, symbol_column = NULL) {
  tryCatch({
    page <- read_html(url)
    table <- html_nodes(page, "table.wikitable")[[1]]  # Get first table
    df <- html_table(table, fill = TRUE)
    # Extract company names
    companies <- df[[name_column]] %>% unique() %>% na.omit()
    symbols <- if (!is.null(symbol_column)) {
      df[[symbol_column]] %>% unique() %>% na.omit()
    } else {
      rep(NA_character_, length(companies))  # Use NA_character_ for consistency
    }
    data.frame(Company_Name = companies, Symbol = symbols, stringsAsFactors = FALSE)
  }, error = function(e) {
    message("Error accessing URL: ", url)
    return(data.frame(Company_Name = character(0), Symbol = character(0)))
  })
}
# Extract S&P 500 companies (using 'Security' for names and 'Symbol' for tickers)
sp500_data <- extract_company_data(url_sp500, "Security", "Symbol")
# Extract market cap companies (using 'Name' for names, no symbol column)
market_cap_data <- extract_company_data(url_market_cap, "Name")
all_data <- bind_rows(sp500_data, market_cap_data)# Combine the data
all_data <- all_data %>%
  distinct(Company_Name, .keep_all = TRUE)
pander(head(all_data, 5))
# Save as CSV
write.csv(all_data, "company_names_and_symbols.csv", row.names = FALSE)
cat("CSV file saved successfully with ", nrow(all_data), " companies.")
