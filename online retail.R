# 📦 Load necessary libraries
library(readxl)
library(dplyr)
library(tidyr)
library(arules)
library(arulesViz)

# 📂 Load and clean data
df <- read_excel("C:/Users/Admin/OneDrive/Desktop/Online Retail.xlsx")
df <- df[!is.na(df$CustomerID), ]
df <- df[!startsWith(as.character(df$InvoiceNo), "C"), ]
df <- df[df$Quantity > 0, ]

# 🌍 Identify top 3 countries by transaction count
top_countries <- df %>%
  count(Country, sort = TRUE) %>%
  slice_head(n = 3) %>%
  pull(Country)

# 🔁 Analyze each of the top 3 countries
for (country in top_countries) {
  cat("\n🛒 Analyzing country:", country, "\n")
  
  # Filter for the selected country
  country_data <- df %>%
    filter(Country == country)
  
  # 🧺 Create basket format: transaction x items
  transactions_data <- country_data %>%
    group_by(InvoiceNo, Description) %>%
    summarise(Quantity = sum(Quantity), .groups = "drop") %>%
    mutate(Quantity = ifelse(Quantity > 0, 1, 0)) %>%
    pivot_wider(names_from = Description, values_from = Quantity, values_fill = 0)
  
  # Drop InvoiceNo column (we'll use rownames as transaction IDs)
  rownames(transactions_data) <- transactions_data$InvoiceNo
  transactions_data <- transactions_data[, -1]
  
  # Convert to 'transactions' class
  trans <- as(transactions_data > 0, "transactions")
  
  # ⚙️ Apply Apriori algorithm
  itemsets <- apriori(trans, parameter = list(support = 0.02, target = "frequent itemsets"))
  cat("Frequent itemsets found:", length(itemsets), "\n")
  
  # 🔗 Generate rules
  rules <- apriori(trans, parameter = list(support = 0.02, confidence = 0.5, minlen = 2))
  rules <- sort(rules, by = "lift", decreasing = TRUE)
  
  # 🖨️ Show top 5 rules
  if (length(rules) > 0) {
    cat("\nTop 5 rules:\n")
    inspect(head(rules, 5))
  } else {
    cat("No strong rules found.\n")
  }
  
  # 📊  plot rules 
  # plot(head(rules, 10), method = "graph", control = list(type = "items"))
}