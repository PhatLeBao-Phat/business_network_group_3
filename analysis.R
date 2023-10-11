library(tidyr)
library(readxl)
library(dplyr)
library(igraph)
library(stringdist)
library(patentsview)

# Import data 
sdc_data <- readRDS('data/smartphone_SDC_2010_2016.rds')
patents_data <- read.csv('patent_data/patents_views.csv')
patents_data %>% 
  subset(select = c("patent_id", "assignee_id", "disambig_assignee_organization"
                    , "patent_application_type"))