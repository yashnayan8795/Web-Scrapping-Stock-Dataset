# Defining sentiment colors
sentiment_colors <- c(
  "Positive" = "#2ECC71",             # Green
  "Positive but Volatile" = "#27AE60",  # Darker Green
  "Negative" = "#E74C3C",             # Red
  "Negative and Volatile" = "#C0392B",  # Darker Red
  "Neutral" = "light blue"               # blue
)
# 1. Top 10 Best Performing Companies by Price Change Percentage
top_companies <- stock_data %>%
  arrange(desc(price_change_pct)) %>%
  slice_head(n = 10)
plot_top <- ggplot(top_companies, aes(x = reorder(Company_Name, price_change_pct), y = price_change_pct, fill = price_sentiment)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = sentiment_colors) +
  coord_flip() +
  labs(title = "Top 10 Best Performing Companies", 
       x = "Company", 
       y = "Price Change (%)") +
  theme_minimal()
print(plot_top)
# 2. Top 10 Worst Performing Companies by Price Change Percentage
worst_companies <- stock_data %>%
  arrange(price_change_pct) %>%
  slice_head(n = 10)
plot_worst <- ggplot(worst_companies, aes(x = reorder(Company_Name, -price_change_pct), y = price_change_pct, fill = price_sentiment)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = sentiment_colors) +
  coord_flip() +
  labs(title = "Top 10 Worst Performing Companies", 
       x = "Company", 
       y = "Price Change (%)") +
  theme_minimal()
print(plot_worst)
# 3. Scatter Plot: Price Change vs. Volatility
plot_scatter <- ggplot(stock_data, aes(x = price_change_pct, y = volatility_pct, color = price_sentiment)) +
  geom_point(alpha = 0.7, size = 3) +
  scale_color_manual(values = sentiment_colors) +
  geom_text(aes(label = Company_Name), color = "black", hjust = 1.1, vjust = 0.5, check_overlap = TRUE, size = 3) +
  labs(title = "Price Change vs. Volatility by Company", 
       x = "Price Change (%)", 
       y = "Volatility (%)") +
  theme_minimal()
print(plot_scatter)
# 4. Compute Investibility Score and Identify Top Investible Companies
# Investibility Score is defined as price_change_pct divided by volatility_pct.
stock_data <- stock_data %>%
  mutate(investibility_score = price_change_pct / volatility_pct)
top_investible <- stock_data %>%
  arrange(desc(investibility_score)) %>%
  slice_head(n = 10)
plot_investible <- ggplot(top_investible, aes(x = reorder(Company_Name, investibility_score), y = investibility_score, fill = price_sentiment)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = sentiment_colors) +
  coord_flip() +
  labs(title = "Top 10 Investible Companies", 
       x = "Company", 
       y = "Investibility Score (Price Change / Volatility)") +
  theme_minimal()
print(plot_investible)
# Identify High Risk Companies based on volatility_pct above the 90th percentile
high_risk_threshold <- quantile(stock_data$volatility_pct, 0.9, na.rm = TRUE)
high_risk_companies <- stock_data %>%
  filter(volatility_pct > high_risk_threshold) %>%
  arrange(desc(volatility_pct))
cat("Number of high risk companies:", nrow(high_risk_companies), "\n")
# Visualize High Risk Companies: Bar plot of volatility by company
plot_high_risk <- ggplot(high_risk_companies, aes(x = reorder(Company_Name, volatility_pct), y = volatility_pct, fill = price_sentiment)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = sentiment_colors) +
  coord_flip() +
  labs(title = "High Risk Companies (High Volatility)",
       x = "Company",
       y = "Volatility (%)") +
  theme_minimal()
print(plot_high_risk)