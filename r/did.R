library(tidyverse)
library(plm)
library(stargazer)


# Load the data
panel <- read_csv("data/panel.csv") %>%
  # rearrange the columns
  relocate(loan_year, .before = fyear) %>%
  arrange(loan_year, fyear) %>%
  mutate(relative_year = loan_year - fyear) %>%
  mutate(
    year_pre_1 = ifelse(relative_year == -1, 1, 0),
    year_0 = ifelse(relative_year == 0, 1, 0),
    year_post_1 = ifelse(relative_year == 1, 1, 0),
    year_post_2 = ifelse(relative_year >= 2, 1, 0)
  ) %>%
  mutate(breach = as.numeric(breach))

rhs_formula <- ~ year_pre_1:breach + year_0:breach + year_post_1:breach + year_post_2:breach +
  log(loan_amount + 1) + log(loan_maturity + 1) + performance_pricing +
  log(firm_size + 1) + leverage + roa + operational_risk + tangibility +
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
