# Query: https://wrds-www.wharton.upenn.edu/query-manager/query/8308123/
# Merge the loan data with matched sample

library(tidyverse)
library(stargazer)

# Load data
loan <- read_csv("data/dealscan.csv") %>%
  arrange(Tranche_Active_Date) %>%
  distinct(Ticker, Tranche_Active_Date, .keep_all = TRUE) %>%
  select(-Borrower_Id, -Parent_Ticker, -Deal_Active_Date)

matched <- read_csv("data/matched.csv")

# Inner join by ticker
data <- loan %>%
  inner_join(matched, by = c("Ticker" = "tic"), relationship = "many-to-many") %>%
  select(-Borrower_Name) %>%
  filter(Tranche_Active_Date >= as.Date("2003-01-01"), Tranche_Active_Date <= as.Date("2020-12-31"))

# Calculate loan characteristics
data <- data %>%
  mutate(
    loan_year = year(Tranche_Active_Date),
    loan_spread = All_In_Spread_Drawn_bps,
    loan_amount = Tranche_Amount_Converted,
    loan_maturity = as.numeric(difftime(Tranche_Maturity_Date, Tranche_Active_Date, units = "days")) / 30,
    performance_pricing = ifelse(is.na(Performance_Pricing), FALSE, TRUE),
    secured = ifelse(Secured == "Yes", 1, 0),
    financial_covenants = str_count(All_Covenants_Financial, ":"),
    general_covenants = str_count(All_Covenants_General, ":"),
    total_covenants = financial_covenants + general_covenants,
    post = loan_year >= fyear,
    treated = as.numeric(breach) * as.numeric(post)
  ) %>%
  select(-All_In_Spread_Drawn_bps, -Tranche_Amount_Converted, -Tranche_Maturity_Date, -Tranche_Active_Date, -All_Covenants_Financial, -All_Covenants_General, -Performance_Pricing, -Secured, -financial_covenants, -general_covenants) %>%
  na.omit()

summary(data)

# Test correlation between treated and loan characteristics
# Show signifinance
data %>%
  select(treated, loan_spread, loan_amount, loan_maturity, total_covenants, secured) %>%
  cor() %>%
  stargazer(type = "text")

# Save data
write_csv(data, "data/panel.csv")
