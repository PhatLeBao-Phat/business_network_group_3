library(tidyr)
library(dplyr)
library(igraph)

sdc_data <- readRDS('data/SDC_data_2021.rds')

count_per_deal <- sdc_data %>% 
  group_by(deal_number) %>%
  summarise(count = n_distinct(participants)) 

count_per_company <- sdc_data %>% 
  group_by(participants) %>% 
  summarise(count = n_distinct(deal_number))

print(unique(sdc_data$type))
print(unique(sdc_data$business_description))
