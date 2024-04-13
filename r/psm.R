# Propensity score matching
library(tidyverse)
library(MatchIt)

# Constants
START <- 2005
END <- 2014

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

# How many breaches are happen to the same firm-year?
breach %>%
  group_by(edgar_ticker, breach_year) %>%
  summarise(n = n()) %>%
  filter(n > 1)

# If a company experience multiple breaches, we only consider the most severe one
breach <- breach %>%
  group_by(edgar_ticker) %>%
  # turn NA to 0
  mutate(number_of_records_lost = ifelse(is.na(number_of_records_lost), 0, number_of_records_lost)) %>%
  filter(number_of_records_lost == max(number_of_records_lost)) %>%
  ungroup() %>%
  select(-number_of_records_lost)

# Merge breach and compustat
data <- data %>%
  left_join(breach, by = c("tic" = "edgar_ticker", "fyear" = "breach_year")) %>%
  mutate(breach = ifelse(is.na(breach), 0, 1)) %>%
  select(-breach_information_type, -type_of_attack_list, -breach_cost_usd) %>%
  arrange(fyear, tic) %>%
  na.omit()

# How many breaches?
summary(data$breach)
summary(data$fyear)

data %>%
  filter(fyear >= START, fyear <= END) %>%
  summarise(n = sum(breach))

# PSM

# Turn time and industry to factors
data <- data %>%
  mutate(
    fyear = as.factor(fyear),
    industry = as.factor(industry)
  )

reg_formula <- as.formula("breach ~ firm_size + leverage + roa + operational_risk + tangibility + z_score + mb + it_expertise")

ps_model <- glm(reg_formula, data = data, family = "binomial")

summary(ps_model)

match_data <- matchit(reg_formula, method = "nearest", data = data)

summary(match_data)

data_matched <- match.data(match_data)

data_matched %>%
  select(-distance, -weights, -subclass) %>%
  write_csv("data/matched.csv")
