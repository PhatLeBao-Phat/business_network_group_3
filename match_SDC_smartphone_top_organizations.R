# Load required libraries
library(dplyr)
# Read the CSV files
smartphone_data <- read.csv('data/smartphone_SDC_2010_2016.csv')
patents_data <- read.csv('data/patents_views.csv')

# Function to remove dots and commas and convert to lowercase
normalize_string <- function(x) {
  tolower(gsub("[.,]", "", x))
}

# Normalize the columns for comparison
smartphone_data$participants_normalized <- normalize_string(smartphone_data$participants)
patents_data$disambig_assignee_organization_normalized <- normalize_string(patents_data$disambig_assignee_organization)

# Find exact matches
matched_smartphone <- smartphone_data %>%
  filter(participants_normalized %in% patents_data$disambig_assignee_organization_normalized)

matched_patents_views <- patents_data %>%
  filter(disambig_assignee_organization_normalized %in% smartphone_data$participants_normalized)

# Remove empty rows
matched_patents_views <- matched_patents_views %>%
  filter(!is.na(disambig_assignee_organization) & disambig_assignee_organization != "")

# Save the matched data to CSV files
write.csv(matched_smartphone, file = 'data/matched_smartphone_SDC_2010_2016.csv', row.names = FALSE)
write.csv(matched_patents_views, file = 'data/matched_patents_views.csv', row.names = FALSE)
