# METHOD
devtools::install_github("stasvlasov/nstandr")
install.packages('migraph')
library(devtools)
library(nstandr)
library(igraph)
library(tidyr)
library(dplyr)
library(migraph)
library(igraph)

# Internal import 
source("sample_sdc.R")
source("helpers.R")

# SDC data
sdc_data <- sp_data # Try different samples from sample_sdc.R
# sdc_data <- readRDS("data/smartphone_SDC_2010_2016.rds")
sdc_data$participants <- sdc_data$participants %>% standardize_magerman()
# write.csv(sdc_data, "data/sdc_data.csv")
# saveRDS(sdc_data, "data/final_sdc.rds")
sdc_data <- readRDS("data/final_sdc.rds")

# Patent data
patent_data <- read.csv("patent_data/patents_views.csv", row.names = 1)
patent_data$disambig_assignee_organization <- patent_data$disambig_assignee_organization %>% standardize_magerman()
patent_data <- patent_data %>%
  mutate(filing_date = as.Date(filing_date)) %>%
  mutate(year = lubridate::year(filing_date)) %>%
  filter(year >= 2010 & year <= 2020)

# saveRDS(patent_data, "data/final_patent.rds")
patent_data <- readRDS("data/final_patent.rds")

# Make Pivot Table 
pivot_table <- patent_data %>%
  select(disambig_assignee_organization, year) %>%
  group_by(disambig_assignee_organization, year) %>%
  summarize(count = n()) %>%
  pivot_wider(names_from = year, values_from = count, values_fill = 0) 

pivot_table[which(pivot_table$disambig_assignee_organization == "SAMSUNG DISPLAY "), "disambig_assignee_organization"] = "SAMSUNG ELECTRONIC "

pivot_table <- pivot_table %>%
  filter(disambig_assignee_organization %in% unique(sdc_data$participants)) %>%
  group_by(disambig_assignee_organization) %>%
  summarize(across(starts_with("20"), sum, na.rm = TRUE))

# saveRDS(pivot_table, "data/final_pivot_table.rds")
pivot_table <- readRDS("data/final_pivot_table.rds")

# From graphRDS
sdc_graph.proj1 <- make_plot(sdc_data)$proj1
print(sum(degree(sdc_graph.proj1) >= 1)) 
print(length(V(sdc_graph.proj1)))

# Another way of filtering 
sdc_graph.proj1 <- sdc_graph.proj1 %>%
  delete.vertices(which(degree(sdc_graph.proj1) < 1))
orgs.names <- V(sdc_graph.proj1)$name %>% data.frame(disambig_assignee_organization=.)
final <- orgs.names %>% merge(pivot_table, by = "disambig_assignee_organization", all.x = TRUE)
new_order_indices <- match(V(sdc_graph.proj1)$name, final$disambig_assignee_organization)
final <- final[new_order_indices, ]
row.names(final) <- 1:nrow(final)
print("Number of orgs with available patents count:")
print(sum(!is.na(final["2010"])))
print("Number of orgs connected to the network:")
print(sum(degree(sdc_graph.proj1) >= 1))

# Drop vertices that do not have patents count and have low degree 
# not_patents.index <- which(is.na(final["2010"]))
# low_degree.index <- which(degree(sdc_graph.proj1) == 1)
# del.index <- intersect(not_patents.index, low_degree.index) %>%
#   sample(size = 60)
# sdc_graph.proj1 <- delete.vertices(sdc_graph.proj1, del.index)
# View(V(sdc_graph.proj1))
# print("Number of orgs connected to the network:")
# print(sum(degree(sdc_graph.proj1) >= 1))
# final <- final[-del.index, ]
# print("Number of orgs with available patents count:")
# print(sum(!is.na(final["2010"])))
final <- replace(final, is.na(final), 0)
# row.names(final) <- 1:nrow(final)
# saveRDS(final, "data/final.rds")
# saveRDS(sdc_graph.proj1, "data/proj1.rds")

#### READ FROM HERE
final <- readRDS("data/final.rds")
sdc_graph.proj1 <- readRDS("data/proj1.rds")
final["total_patents"] <- rowSums(final[-1])

#### Feature Engineering  
centrality.degree <- degree(sdc_graph.proj1)
centrality.betweeness <- betweenness(sdc_graph.proj1)
# Clique
# Find all cliques in the graph
all_cliques <- cliques(sdc_graph.proj1)
cliques_count <- vector("list", vcount(sdc_graph.proj1))
for (i in seq_along(all_cliques)) {
  for (vertex in all_cliques[[i]]) {
    cliques_count[[vertex]] <- length(cliques_count[[vertex]]) + 1
  }
}
cliques_count <- unlist(cliques_count)
# Redundancy
vertex.redundancy <- node_redundancy(sdc_graph.proj1)
# Deal diversity 
deal.diversity <- sdc_data %>%
  filter(participants %in% final$disambig_assignee_organization) %>%
  group_by(participants) %>%
  summarise(
    deal_sic.count = n_distinct(alliance_SIC_code),
    nations.count = n_distinct(alliance_nation))
new_order_indices <- match(final$disambig_assignee_organization, deal.diversity$participants)
deal.diversity <- deal.diversity[new_order_indices, ]
row.names(deal.diversity) <- 1:nrow(deal.diversity)

#### OLS
df <- final %>%
  mutate(centrality.degree = centrality.degree) %>%
  mutate(centrality.betweeness = centrality.betweeness) %>%
  mutate(cliques_count = cliques_count) %>%
  mutate(redundancy = vertex.redundancy) %>%
  mutate(deal.diversity.sic = deal.diversity$deal_sic.count) %>%
  mutate(deal.diversity.country = deal.diversity$nations.count)
View(df)
