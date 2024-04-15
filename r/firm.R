# Query: https://wrds-www.wharton.upenn.edu/query-manager/query/8321513/
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

# Load data
firm <- read_csv("data/crsp_compustat_merged.csv") %>%
  select(-GVKEY, -indfmt, -consol, -popsrc, -datafmt, -cusip, -curcd, -costat)

# Calculate firm level variables
firm <- firm %>%
  mutate(
    firm_size = log(1 + at),
    leverage = lt / at,
    roa = ebitda / at,
    tangibility = ppent / at,
    z_score = 1.2 * (wcap / at + 1.4 * (re / at) + 3.3 * (ebitda / at) + 0.6 * (mkvalt / lt) + 0.999 * (sale / at)),
    mb = mkvalt / at
  )

# Calculate operational risk with rolling window
firm <- firm %>%
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
  ungroup()

# Clean
firm <- firm %>%
  select(-at, -lt, -ebitda, -ppent, -wcap, -re, -mkvalt, -sale, -oancf) %>%
  filter_all(all_vars(!is.infinite(.)))

# Map sic code to indsutry
sic_map <- read_csv("data/Siccodes48.csv") %>%
  select(index, name_abbr, sub_range_start, sub_range_end)

firm <- firm %>%
  mutate(sic = as.numeric(sic)) %>%
  cross_join(sic_map) %>%
  filter(sic >= sub_range_start & sic <= sub_range_end) %>%
  rename(industry = index, industry_abbr = name_abbr) %>%
  select(-sub_range_start, -sub_range_end)

# Lag firm level variables
firm <- firm %>%
  group_by(tic) %>%
  arrange(fyear) %>%
  mutate(
    firm_size = dplyr::lag(firm_size, 1),
    leverage = dplyr::lag(leverage, 1),
    roa = dplyr::lag(roa, 1),
    operational_risk = dplyr::lag(operational_risk, 1),
    tangibility = dplyr::lag(tangibility, 1),
    z_score = dplyr::lag(z_score, 1),
    mb = dplyr::lag(mb, 1)
  ) %>%
  ungroup()

# Save data
write_csv(firm, "data/firm.csv")
