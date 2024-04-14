# Query: https://wrds-www.wharton.upenn.edu/query-manager/query/8321508/
# IT Expertise: Indicator variable that equals 1 if the borrower has a chief information officer, a chief security officer, or any high-ranking officer devoted to information or security, and 0 otherwise.

# Load data
it <- read_csv("data/execucomp.csv")

# filter for IT expertise
KEYWORDS <- c("information", "security", "technology")

it <- it %>%
  mutate(TITLE = ifelse(is.na(TITLE), "", TITLE)) %>%
  mutate(
    it_expertise = as.numeric(str_detect(tolower(TITLE), str_c(KEYWORDS, collapse = "|")))
  ) %>%
  select(GVKEY, TICKER, CONAME, CUSIP, YEAR, it_expertise) %>%
  arrange(GVKEY, YEAR)

it <- it %>%
  group_by(TICKER, YEAR) %>%
  summarise(it_expertise = max(it_expertise))

summary(it$it_expertise)

# Lag it_expertise
it <- it %>%
  group_by(TICKER) %>%
  arrange(YEAR) %>%
  mutate(it_expertise = dplyr::lag(it_expertise, 1)) %>%
  ungroup()

# Save data
write_csv(it, "data/it_expertise.csv")
