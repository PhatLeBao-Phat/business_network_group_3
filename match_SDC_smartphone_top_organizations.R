# Load required libraries
library(dplyr)

# Read the CSV files
smartphone_data <- read.csv('data/smartphone_SDC_2010_2016.csv')
patents_data <- read.csv('data/patents_views.csv')

# Function to extract the first string (ignoring case)
extract_first_string <- function(x) {
  tolower(substring(x, 1, regexpr("\\s|,", x)))
}

# Normalize the columns for comparison (ignoring case)
smartphone_data$participants <- tolower(smartphone_data$participants)
patents_data$disambig_assignee_organization <- tolower(patents_data$disambig_assignee_organization)

# Find exact matches of the first string
matched_smartphone <- smartphone_data %>%
  filter(extract_first_string(participants) %in% extract_first_string(patents_data$disambig_assignee_organization))

matched_patents_views <- patents_data %>%
  filter(extract_first_string(disambig_assignee_organization) %in% extract_first_string(smartphone_data$participants))

# Save the matched data to CSV files
write.csv(matched_smartphone, file = 'data/matched_smartphone_SDC_2010_2016.csv', row.names = FALSE)
write.csv(matched_patents_views, file = 'data/matched_patents_views.csv', row.names = FALSE)


