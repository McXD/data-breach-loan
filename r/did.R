library(tidyverse)
library(plm)
library(stargazer)


# Load the data
loan <- read_csv("data/loan.csv") %>% select(-Borrower_Name)
firm <- read_csv("data/firm.csv") %>% select(-datadate, -sic, -industry_abbr) # characteristics are lagged
it_expertise <- read_csv("data/it_expertise.csv") # characteristics are lagged
matched <- read_csv("data/matched.csv") %>% select(breach_year = fyear, tic, breach)

# Merge the data
panel <- loan %>%
  filter(loan_year >= 2008 & loan_year <= 2022) %>%
  inner_join(matched, by = c("Ticker" = "tic")) %>%
  left_join(firm, by = c("Ticker" = "tic", "loan_year" = "fyear")) %>%
  left_join(it_expertise, by = c("Ticker" = "TICKER", "loan_year" = "YEAR"))

panel <- panel %>%
  arrange(loan_year, breach_year) %>%
  mutate(relative_year = loan_year - breach_year) %>%
  mutate(
    year_pre_1 = ifelse(relative_year == -1, 1, 0),
    year_0 = ifelse(relative_year == 0, 1, 0),
    year_post_1 = ifelse(relative_year == 1, 1, 0),
    year_post_2 = ifelse(relative_year >= 2, 1, 0)
  ) %>%
  mutate(breach = as.numeric(breach)) %>%
  filter(relative_year >= -2 & relative_year <= 2) %>%
  relocate(breach_year, .after = loan_year)

# Check panel balance
panel %>%
  group_by(breach) %>%
  summarise(n = n())

panel %>%
  mutate(pre = loan_year <= breach_year) %>%
  group_by(pre) %>%
  summarise(n = n())

rhs_formula <- ~ year_pre_1:breach + year_0:breach + year_post_1:breach + year_post_2:breach +
  log(loan_amount + 1) + log(loan_maturity + 1) + performance_pricing +
  firm_size + leverage + roa + operational_risk + tangibility +
  z_score + mb + it_expertise

model_loan_spread <- plm(update.formula(rhs_formula, log(loan_spread) ~ .), data = panel, index = c("Ticker", "loan_year"), model = "within", effect = "twoways")
model_total_covenants <- plm(update.formula(rhs_formula, total_covenants ~ .), data = panel, index = c("Ticker", "loan_year"), model = "within", effect = "twoways")
model_secured <- plm(update.formula(rhs_formula, secured ~ .), data = panel, index = c("Ticker", "loan_year"), model = "within", effect = "twoways")

stargazer(model_loan_spread, model_total_covenants, model_secured,
  type = "text",
  title = "Panel Regression Results",
  column.labels = c("Loan Spread", "Total Covenants", "Secured"),
  dep.var.labels = c("Loan Spread", "Total Covenants", "Secured"),
  align = TRUE,
  no.space = TRUE
)

panel %>%
  relocate(loan_year, breach_year, breach, loan_spread, total_covenants, secured, .after = Ticker) %>%
  arrange(Ticker, loan_year) %>%
  View()
