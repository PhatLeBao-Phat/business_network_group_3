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


sdc_data <- readRDS("data/smartphone_SDC_2010_2016.rds")
patent_data <- read.csv("patent_data/patents_views.csv", row.names = 1)
sdc_data$participants <- sdc_data$participants %>% standardize_magerman()
patent_data$disambig_assignee_organization <- patent_data$disambig_assignee_organization %>% standardize_magerman()
write.csv(sdc_data, "data/sdc_data.csv")
saveRDS(sdc_data, "data/final_sdc.rds")
# Plot
plot_sdc <- function(sdc_data) {
    sdc_net <- sdc_data %>%
        select(participants, deal_number) %>%
        as.matrix()
    par(mfrow = c(1, 2))
    sdc_graph <- graph.data.frame(sdc_net, directed = FALSE)
    V(sdc_graph)$type <- ifelse(V(sdc_graph)$name %in% sdc_net[,2],
                                yes = TRUE, no = FALSE)
    sdc_graph.proj1 <- bipartite.projection(sdc_graph)$proj1 %>%
                              simplify(remove.loops = TRUE,
                                       remove.multiple = TRUE)
    coords = layout_nicely(sdc_graph)
    plot(sdc_graph.proj1, layout=coords, vertex.color = "coral2",
         vertex.label=NA, vertex.size = 2, edge.width = 0.002,
         edge.color = "black")
    sdc_graph <- simplify(sdc_graph, remove.loops = TRUE,
                          remove.multiple = TRUE)
    set.seed(06062003)
    coords <- layout_with_kk(sdc_graph)
    vertex.type1.color <- c("red") %>% rep(length(unique(sdc_net[, 1])))
    vertex.type2.color <- c("blue") %>% rep(length(unique(sdc_net[, 2])))
    vertex.color <- c(vertex.type1.color, vertex.type2.color)
    plot(sdc_graph, layout=coords, vertex.color = vertex.color,
         vertex.label=NA, vertex.size = 2, edge.width = 0.002,
         edge.color = "black")
    legend("topleft", legend=c("organization", "deal"), fill=c("red", "blue"))
}

# Patent data
patent_data <- patent_data %>%
  mutate(filing_date = as.Date(filing_date)) %>%
  mutate(year = lubridate::year(filing_date)) %>%
  filter(year >= 2010 & year <= 2020)

saveRDS(patent_data, "data/final_patent.rds")
patent_data <- readRDS("data/final_patent.rds")

pivot_table <- patent_data %>%
  select(disambig_assignee_organization, year) %>%
  group_by(disambig_assignee_organization, year) %>%
  summarize(count = n()) %>%
  pivot_wider(names_from = year, values_from = count, values_fill = 0) 

pivot_table$disambig_assignee_organization <- standardize_magerman(pivot_table$disambig_assignee_organization) 
pivot_table <- pivot_table %>%
  filter(disambig_assignee_organization %in% unique(sdc_data$participants)) %>%
  group_by(disambig_assignee_organization) %>%
  summarize(across(starts_with("20"), sum, na.rm = TRUE))

saveRDS(pivot_table, "data/final_pivot_table.rds")
pivot_table <- readRDS("data/final_pivot_table.rds")

# From graphRDS
sdc_graph.proj1 <- readRDS("graphRDS/2010_2016_sp_sdc_orgs.rds")
sdc_graph <- readRDS("graphRDS/2010_2016_sp_sdc_bipartite.rds")

