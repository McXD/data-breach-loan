# Query: https://wrds-www.wharton.upenn.edu/query-manager/query/8308123/
# Clean loan data

library(tidyverse)
library(stargazer)

# Load data
loan <- read_csv("data/dealscan.csv") %>%
  arrange(Tranche_Active_Date) %>%
  distinct(Ticker, Tranche_Active_Date, .keep_all = TRUE) %>%
  select(-Borrower_Id, -Parent_Ticker, -Deal_Active_Date)

# Calculate loan characteristics
loan <- loan %>%
  mutate(
    loan_year = year(Tranche_Active_Date),
    loan_spread = All_In_Spread_Drawn_bps,
    loan_amount = Tranche_Amount_Converted,
    loan_maturity = as.numeric(difftime(Tranche_Maturity_Date, Tranche_Active_Date, units = "days")) / 30,
    performance_pricing = ifelse(is.na(Performance_Pricing), FALSE, TRUE),
    secured = ifelse(Secured == "Yes", 1, 0),
    financial_covenants = str_count(All_Covenants_Financial, ":"),
    general_covenants = str_count(All_Covenants_General, ":"),
    total_covenants = financial_covenants + general_covenants
  ) %>%
  select(-All_In_Spread_Drawn_bps, -Tranche_Amount_Converted, -Tranche_Maturity_Date, -Tranche_Active_Date, -All_Covenants_Financial, -All_Covenants_General, -Performance_Pricing, -Secured, -financial_covenants, -general_covenants)

summary(loan)

# Save data
write_csv(loan, "data/loan.csv")
