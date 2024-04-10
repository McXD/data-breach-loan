# Query: https://wrds-www.wharton.upenn.edu/query-manager/query/8301973/
# IT Expertise: Indicator variable that equals 1 if the borrower has a chief information officer, a chief security officer, or any high-ranking officer devoted to information or security, and 0 otherwise.

# Load data
data <- read_csv("data/execucomp.csv")

# filter for IT expertise
KEYWORDS <- c("information", "security", "technology")

data %>%
  filter(
    str_detect(tolower(TITLE), str_c(KEYWORDS, collapse = "|"))
  ) %>%
  View()

data <- data %>%
  mutate(
    it_expertise = as.numeric(str_detect(tolower(TITLE), str_c(KEYWORDS, collapse = "|")))
  )

data <- data %>%
  select(GVKEY, TICKER, CONAME, CUSIP, YEAR, it_expertise)

summary(data$it_expertise)
