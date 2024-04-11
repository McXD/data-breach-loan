# Data obtained from Wang Qian

library(tidyverse)

# Load data
breach <- read_csv("data/breach_wrds.csv") %>%
  select(
    edgar_ticker = `BEST EDGAR TICKER`,
    breach_disclosure_date = `BREACH DISCLOSURE DATE`,
    number_of_records_lost = `NUMBER OF RECORDS LOST`,
    breach_information_type = `BREACH INFORMATION TYPE`,
    type_of_attack_list = `TYPE OF ATTACK LIST`,
    breach_cost_usd = `BREACH COST USD`
  ) %>%
  mutate(
    breach_disclosure_date = as.Date(breach_disclosure_date, format = "%d/%m/%Y"),
  )

breach

# Check percentage of missing values
breach %>%
  summarise_all(~ sum(is.na(.)) / n()) %>%
  gather() %>%
  arrange(desc(value))

# Check range of dates
breach %>%
  summarise(
    min_date = min(breach_disclosure_date),
    max_date = max(breach_disclosure_date)
  )

# Save cvs
write_csv(breach, "data/breach.csv")