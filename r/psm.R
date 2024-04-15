# Propensity score matching
library(tidyverse)
library(magrittr)
library(MatchIt)
library(knitr)
library(stargazer)

firm <- read_csv("data/firm.csv")
breach <- read_csv("data/breach.csv")
it <- read_csv("data/it_expertise.csv")

firm <- firm %>%
  left_join(it, by = c("tic" = "TICKER", "fyear" = "YEAR")) %>%
  select(-datadate) %>%
  # Remove duplicates
  distinct(fyear, tic, .keep_all = TRUE)

breach <- breach %>%
  mutate(breach_year = year(breach_disclosure_date)) %>%
  mutate(breach = 1) %>%
  select(-breach_disclosure_date)

# Merge breach and compustat
treatment <- firm %>%
  left_join(breach, by = c("tic" = "edgar_ticker", "fyear" = "breach_year")) %>%
  filter(!is.na(breach)) %>%
  arrange(fyear, tic)

# Check number of event firms
treatment <- treatment %>%
  filter(breach == 1) %T>%
  {
    print(paste("Total (2004-2023):", nrow(.)))
  } %>%
  filter(fyear >= 2010 & fyear <= 2020) %T>%
  {
    print(paste("2010-2020:", nrow(.)))
  } %>%
  {
    # Filter firms with prior breach in 2008-2009
    tmp <- filter(breach, breach_year >= 2008 & breach_year <= 2009)
    filter(., !tic %in% tmp$edgar_ticker)
  } %T>%
  {
    print(paste("2010-2020 (no prior breach):", nrow(.)))
  } %>%
  {
    # If a company experience multiple breaches, we only consider the most severe one
    mutate(., number_of_records_lost = ifelse(is.na(number_of_records_lost), 0, number_of_records_lost)) %>%
      group_by(tic) %>%
      filter(number_of_records_lost == max(number_of_records_lost)) %>%
      slice(1) %>%
      ungroup()
  } %T>%
  {
    print(paste("2010-2020 (most severe):", nrow(.)))
  } %>%
  na.omit() %T>% {
    print(paste("2010-2020 (complete data):", nrow(.)))
  }

# PSM
psm_panel <- firm %>%
  left_join(treatment %>% select(tic, fyear, breach), by = c("tic", "fyear")) %>%
  mutate(breach = ifelse(is.na(breach), 0, 1)) %>%
  mutate(
    fyear = as.factor(fyear),
    industry = as.factor(industry)
  ) %>%
  na.omit()

reg_formula <- breach ~ firm_size + leverage + roa + operational_risk + tangibility + z_score + mb + it_expertise + fyear + industry

ps_model <- glm(reg_formula, data = psm_panel, family = binomial(link = "probit"))

# Calculate pseudo R-squared
pseduo_r2 <- 1 - ps_model$deviance / ps_model$null.deviance

stargazer(
  ps_model,
  type = "text",
  title = "Probit regression",
  omit = c("fyear", "industry"),
  add.lines = list(c("Pseudo R-squared", round(pseduo_r2, 3)))
)

match_data <- matchit(reg_formula, method = "nearest", data = psm_panel, link = "probit")

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
  select(variable, `1_mean`, `0_mean`, diff, p) %>%
  rename("Variable" = variable, "Treated" = `1_mean`, "Control" = `0_mean`, "Diff." = diff) %>%
  mutate(across(where(is.numeric), ~ round(., 3))) %>%
  kable()
