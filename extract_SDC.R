# Import dataset SDC
# install.packages("stringdist")
library(tidyr)
library(dplyr)
library(igraph)
library(stringdist)


sdc_data <- readRDS('data/SDC_data_2021.rds')

# The term you want to fuzzy match
search_term <- "Apple Inc"

# Define a function to perform the fuzzy filter
fuzzy_filter <- function(data, column, search_term, max_dist = 1) {
  data %>%
    filter(stringdistmatrix(data[[column]], search_term) <= max_dist)
}

# Apply the fuzzy filter
filtered_df <- fuzzy_filter(sdc_data, "participants", search_term)

# Print the filtered dataframe
print(filtered_df)







Regenerate


