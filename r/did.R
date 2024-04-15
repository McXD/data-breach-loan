library(tidyverse)
library(plm)
library(stargazer)


# Load the data
loan <- read_csv("data/loan.csv") %>% select(-Borrower_Name)
firm <- read_csv("data/firm.csv") %>% select(-datadate, -sic, -industry_abbr) # characteristics are lagged
it_expertise <- read_csv("data/it_expertise.csv") # characteristics are lagged
matched <- read_csv("data/matched.csv") %>% select(breach_year = fyear, tic, breach)

# Merge the data
did_panel <- loan %>%
  inner_join(matched, by = c("Ticker" = "tic")) %>%
  left_join(firm, by = c("Ticker" = "tic", "loan_year" = "fyear")) %>%
  left_join(it_expertise, by = c("Ticker" = "TICKER", "loan_year" = "YEAR"))

# Filter loans
did_panel <- did_panel %T>%
  {
    print(paste("Total: ", nrow(.)))
  } %>%
  filter(loan_year >= 2008 & loan_year <= 2022) %T>%
  {
    print(paste("2008-2022: ", nrow(.)))
  } %>%
  # borrowers in financial service industry
  filter(!industry %in% c(44, 45, 46, 47)) %T>%
  {
    print(paste("Without financial service: ", nrow(.)))
  } %>%
  # missing values
  filter(!is.na(loan_amount) & !is.na(loan_maturity)) %T>% {
    print(paste("Without missing controls: ", nrow(.)))
  }

# How many event firms?
did_panel %>%
  filter(breach == 1) %>%
  distinct(Ticker)

# Create dependent variables
did_panel <- did_panel %>%
  arrange(loan_year, breach_year) %>%
  mutate(relative_year = loan_year - breach_year) %>%
  mutate(
    post = as.numeric(relative_year > 0),
    year_pre_1 = ifelse(relative_year == -1, 1, 0),
    year_0 = ifelse(relative_year == 0, 1, 0),
    year_post_1 = ifelse(relative_year == 1, 1, 0),
    year_post_2 = ifelse(relative_year >= 2, 1, 0)
  ) %>%
  mutate(breach = as.numeric(breach)) %>%
  relocate(breach_year, .after = loan_year)

# Check did_panel balance
# Summarise numerical columns
did_panel %>%
  mutate(performance_pricing = as.numeric(performance_pricing)) %>%
  summarise(across(where(is.numeric), .fns = list(
    n = ~ sum(!is.na(.)),
    mean = ~ mean(., na.rm = TRUE),
    sd = ~ sd(., na.rm = TRUE),
    p25 = ~ quantile(., probs = 0.25, na.rm = TRUE),
    median = ~ median(., na.rm = TRUE),
    p75 = ~ quantile(., probs = 0.75, na.rm = TRUE)
  ))) %>%
  pivot_longer(
    cols = everything(),
    names_to = c("Variables", ".value"),
    names_pattern = "(.*)_(.*)"
  ) %>%
  mutate(across(where(is.numeric), ~ round(., 3))) %>%
  kable()

did_panel %>%
  group_by(breach) %>%
  summarise(n = n())

did_panel %>%
  mutate(pre = loan_year <= breach_year) %>%
  group_by(pre) %>%
  summarise(n = n())

rhs_formula <- ~
  breach:year_pre_1 + breach:year_0 + breach:year_post_1 + breach:year_post_2 + log(loan_amount + 1) + log(loan_maturity + 1) + performance_pricing + firm_size + leverage + roa + operational_risk + tangibility + z_score + mb + it_expertise

model_loan_spread <- plm(update.formula(rhs_formula, log(loan_spread) ~ .), data = did_panel, index = c("Ticker", "loan_year"), model = "within", effect = "twoways")
model_total_covenants <- plm(update.formula(rhs_formula, total_covenants ~ .), data = did_panel, index = c("Ticker", "loan_year"), model = "within", effect = "twoways")
model_secured <- plm(update.formula(rhs_formula, secured ~ .), data = did_panel, index = c("Ticker", "loan_year"), model = "within", effect = "twoways")

stargazer(model_loan_spread, model_total_covenants, model_secured,
  type = "latex",
  title = "Regression Results",
  column.labels = c("Loan Spread", "Total Covenants", "Secured"),
  dep.var.labels = c("Loan Spread", "Total Covenants", "Secured"),
  align = TRUE,
  no.space = TRUE,
  covariate.labels = c(
      "Log(Loan Amount)", "Log(Loan Maturity)", "Performance Pricing", "Firm Size", "Leverage", "ROA", "Operational Risk", "Tangibility", "Z-Score", "MB", "IT Expertise", "Breach:Year -1", "Breach:Year 0", "Breach:Year 1", "Breach:Year 2+"
    )
)

did_panel %>%
  relocate(loan_year, breach_year, breach, loan_spread, total_covenants, secured, .after = Ticker) %>%
  arrange(Ticker, loan_year) %>%
  View()

rhs_formula_no_conrol <- ~
  breach:year_pre_1 + breach:year_0 + breach:year_post_1 + breach:year_post_2

model_loan_spread_no_control <- plm(update.formula(rhs_formula_no_conrol, log(loan_spread) ~ .), data = did_panel, index = c("Ticker", "loan_year"), model = "within", effect = "twoways")

model_total_covenants_no_control <- plm(update.formula(rhs_formula_no_conrol, total_covenants ~ .), data = did_panel, index = c("Ticker", "loan_year"), model = "within", effect = "twoways")

model_secured_no_control <- plm(update.formula(rhs_formula_no_conrol, secured ~ .), data = did_panel, index = c("Ticker", "loan_year"), model = "within", effect = "twoways")

stargazer(model_loan_spread_no_control, model_total_covenants_no_control,
  type = "text",
  title = "Regression Results",
  column.labels = c("Loan Spread", "Total Covenants", "Secured"),
  dep.var.labels = c("Loan Spread", "Total Covenants", "Secured"),
  align = TRUE,
  no.space = TRUE,
  covariate.labels = c(
      "Breach:Year -1", "Breach:Year 0", "Breach:Year 1", "Breach:Year 2+"
    )
)
