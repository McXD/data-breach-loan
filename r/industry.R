# Define the path to the text file
file_path <- "data/Siccodes48.txt"

# Open the file for reading
file_conn <- file(file_path, "r")

# Read the entire file into a vector, line by line
lines <- readLines(file_conn)

# Close the file connection
close(file_conn)

# Initialize an empty data frame to store the extracted data
data <- data.frame(
  index = integer(),
  name_abbr = character(),
  name_full = character(),
  sub_name = character(),
  sub_range_start = integer(),
  sub_range_end = integer(),
  stringsAsFactors = FALSE
)

# Track the current main item
current_index <- NA
current_abbr <- ""
current_full <- ""

# Process each line
for (line in lines) {
  if (grepl("^\\d+.*", line)) {
    # This is a main item line
    parts <- strsplit(gsub("\\s{2,}", " ", trimws(line)), " ")[[1]]
    current_index <- as.integer(parts[1])
    current_abbr <- parts[2]
    current_full <- paste(parts[-c(1, 2)], collapse = " ")
  } else if (grepl("\\s+\\d+-\\d+.*", line)) {
    # This is a sub-item line
    parts <- strsplit(gsub("\\s{2,}", " ", trimws(line)), " ")[[1]]
    range <- strsplit(parts[1], "-")[[1]]
    sub_range_start <- range[1]
    sub_range_end <- range[2]
    sub_name <- paste(parts[-1], collapse = " ")

    # Add to the data frame
    data <- rbind(data, data.frame(
      index = current_index,
      name_abbr = current_abbr,
      name_full = current_full,
      sub_name = sub_name,
      sub_range_start = as.numeric(sub_range_start),
      sub_range_end = as.numeric(sub_range_end),
      stringsAsFactors = FALSE
    ))
  }
  # Ignore empty lines or any lines that don't match the above criteria
}

# Write the data frame to a CSV file
write.csv(data, "data/Siccodes48.csv", row.names = FALSE)
