# Query: https://wrds-www.wharton.upenn.edu/query-manager/query/8301923/
# Clean firm level variables:
# - Firm Size: natural log of total assets.
# - Leverage: total debt scaled by total assets
# - ROA: EBITDA scaled by total assets
# - Operational Risk: standard deviation of yearly cash flows from operations divided by total assets over the past 5 fiscal years
# - Tangibility: PP&E scaled by total assets
# - Z-score: 1.2 * (working capital / total assets) + 1.4 * (retained earnings / total assets) + 3.3 * (EBITDA / total assets) + 0.6 * (market value of equity / total liabilities) + 0.999 * (sales / total assets)
# - MB: market value of equity divided by total assets

library(tidyverse)
library(slider)
library(stargazer)
library(knitr)


# Load data
data <- read_csv("data/crsp_compustat_merged.csv") %>%
  select(-GVKEY, -indfmt, -consol, -popsrc, -datafmt, -cusip, -curcd, -costat)

summary(data)

data <- data %>%
  mutate(
    firm_size = log(1 + at),
    leverage = lt / at,
    roa = ebitda / at,
    tangibility = ppent / at,
    z_score = 1.2 * (wcap / at + 1.4 * (re / at) + 3.3 * (ebitda / at) + 0.6 * (mkvalt / lt) + 0.999 * (sale / at)),
    mb = mkvalt / at
  )

# Calculate operational risk with rolling window
data <- data %>%
  group_by(tic) %>%
  arrange(fyear) %>%
  mutate(tmp = oancf / at) %>%
  mutate(
    operational_risk = slide_dbl(
      tmp,
      .before = 0,
      .after = 4,
      .step = 1,
      .f = sd
    )
  ) %>%
  select(-tmp) %>%
  ungroup( )

# Drop base columns
data <- data %>%
  select(-at, -lt, -ebitda, -ppent, -wcap, -re, -mkvalt, -sale, -oancf)

# Drop rows with Inf
data <- data %>%
  filter_all(all_vars(!is.infinite(.)))

# Summarise numerical columns
data %>%
  summarise(across(where(is.numeric), .fns = list(
      n = ~sum(!is.na(.)),
      mean = ~mean(., na.rm = TRUE),
      sd = ~sd(., na.rm = TRUE),
      p25 = ~quantile(., probs = 0.25, na.rm = TRUE),
      median = ~median(., na.rm = TRUE),
      p75 = ~quantile(., probs = 0.75, na.rm = TRUE)
    ))) %>%
  pivot_longer(
    cols = everything(),
    names_to = c("Variables", ".value"),
    names_pattern = "(.*)_(.*)"
  ) %>%
  kable()
