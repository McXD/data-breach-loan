# Propensity score matching
library(tidyverse)
library(MatchIt)
library(knitr)

# Constants
START <- 2010
END <- 2020

firm <- read_csv("data/firm.csv")
breach <- read_csv("data/breach.csv")
it <- read_csv("data/it_expertise.csv")

data <- firm %>%
  left_join(it, by = c("tic" = "TICKER", "fyear" = "YEAR")) %>%
  select(-datadate, -GVKEY, -CONAME, -CUSIP) %>%
  # Remove duplicates
  distinct(fyear, tic, .keep_all = TRUE)

breach <- breach %>%
  mutate(breach_year = year(breach_disclosure_date)) %>%
  mutate(breach = 1) %>%
  select(-breach_disclosure_date)

# How many breaches are happen to the same firm?
breach %>%
  group_by(edgar_ticker) %>%
  summarise(n = n()) %>%
  filter(n > 1)

# If a company experience multiple breaches, we only consider the most severe one
breach <- breach %>%
  group_by(edgar_ticker) %>%
  # turn NA to 0
  mutate(number_of_records_lost = ifelse(is.na(number_of_records_lost), 0, number_of_records_lost)) %>%
  filter(number_of_records_lost == max(number_of_records_lost)) %>%
  slice(1) %>%
  ungroup() %>%
  select(-number_of_records_lost, - breach_cost_usd, -breach_information_type, -type_of_attack_list)

# Merge breach and compustat
data <- data %>%
  left_join(breach, by = c("tic" = "edgar_ticker", "fyear" = "breach_year")) %>%
  mutate(breach = ifelse(is.na(breach), 0, 1)) %>%
  arrange(fyear, tic) %>%
  na.omit()

# Filter time period
data <- data %>%
  filter(fyear >= START, fyear <= END)

# How many breaches?
summary(data$breach)

# PSM

# Turn time and industry to factors
data <- data %>%
  mutate(
    fyear = as.factor(fyear),
    industry = as.factor(industry)
  )

reg_formula <- breach ~ firm_size + leverage + roa + operational_risk + tangibility + z_score + mb + it_expertise + fyear + industry

ps_model <- glm(reg_formula, data = data, family = binomial(link = "probit"))

summary(ps_model)

match_data <- matchit(reg_formula, method = "nearest", data = data, link = "probit")

summary(match_data)

data_matched <- match.data(match_data)

data_matched %>%
  select(-distance, -weights, -subclass) %>%
  write_csv("data/matched.csv")

# Distribution data breach by Fama-French Industry in table
data_matched %>%
  group_by(industry) %>%
  filter(breach == 1) %>%
  summarise(
    industry_abbr = first(industry_abbr),
    breach = sum(breach)
  ) %>%
  arrange(desc(breach)) %>%
  kable()

N <- nrow(data_matched) / 2

# Calculate difference in mean between treated and control with t-value
data_matched %>%
  select(-weights, -distance, -sic, -ebit) %>%
  group_by(breach) %>%
  summarise(across(
    where(is.numeric),
    .fns = list(mean = mean, sd = sd),
    .names = "{.col}_{.fn}"
  )) %>%
  ungroup() %>%
  pivot_longer(cols = -breach, names_to = c("variable", "stat"), names_pattern = "(.*)_(mean|sd)") %>%
  pivot_wider(names_from = c("breach", "stat"), values_from = value) %>%
  mutate(
    diff = `1_mean` - `0_mean`,
    se = sqrt(`1_sd`^2 / N + `0_sd`^2 / N),
    t = diff / se,
    p = 2 * pt(-abs(t), df = 2 * N - 2)
  ) %>%
  select(variable, `1_mean`, `0_mean`, diff, t, p) %>%
  rename("Variable" = variable, "Treated" = `1_mean`, "Control" = `0_mean`, "Diff." = diff) %>%
  kable()
