# EXTRACT SDC dataset
# install.packages("stringdist")
# install.packages("readxl")
library(tidyr)
library(readxl)
library(dplyr)
library(igraph)
library(stringdist)
library(patentsview)

# Read data 
sdc_data <- readRDS('data/SDC_data_2021.rds')
sdc_data$participants <- toupper(sdc_data$participants)
# company_data <- read_excel('data/companies_info.xlsx')
# colnames(company_data)[1] <- 'idx'

# Initial filter on SDC
sdc_data <- sdc_data %>% filter(status == "Completed/Signed",
                    # date_terminated == "",
                    # type == "Strategic Alliance",
                    date_announced > "2010-01-01",
                    date_announced < "2016-12-31"
                    # participant_nation == "United States"
                    )

# Define a function to perform the fuzzy filter
fuzzy_filter <- function(data, column, search_term, max_dist = 2) {
  data %>%
    filter(stringdistmatrix(data[[column]], search_term) <= max_dist)
}

# SIC to filter 
sic_dict <- list(
  # "3661" = "Communication Equipment Manufacturing",
  # "3674" = "Semiconductor and Related Device Manufacturing",
  "3663" = "Radio and Television Broadcasting and Communications Equipment",
  "5731" = "Radio, TV, & electronic stores",
  "3571" = "Electronic Computers",
  # "5734" = "Computer and Computer Software Stores",
  # "4822" = "Telegraph and Other Message Communications",
  "4812" = "Radiotelephone Communications (Cellular carriers)",
  "5731" = "Radio, Television, and Consumer Electronics Stores"
  # "7376" = "Computer Facilities Management Services",
  # "3571" = "Electronic Computers"
)

# Filter on SDC 
sic <- names(sic_dict)
sp_data <- sdc_data[sdc_data$SIC_primary %in% sic,]
print(length(unique(sp_data$participants)))

# Save the dataset 
saveRDS(sp_data, file="data/smartphone_SDC_2010_2016.rds")
