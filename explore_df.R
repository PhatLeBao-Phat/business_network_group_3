# Import dataset SDC
library(tidyr)
library(dplyr)
library(igraph)

sdc_data <- readRDS('data/SDC_data_2021.rds')

us_alliances <- sdc_data %>% filter(status == "Completed/Signed",
                                    date_terminated == "",
                                    type == "Strategic Alliance",
                                    date_announced > "2012-01-01",
                                    date_announced < "2016-12-31",
                                    participant_nation == "United States") %>%
  select(participants, date_announced, type, SIC_primary,
         participant_nation, deal_number)

us_alliances_net <- us_alliances %>%
  select(participants, deal_number) %>%
  as.matrix()
us_alliances_graph <- graph.data.frame(us_alliances_net, directed = FALSE)
V(us_alliances_graph)$type <- ifelse(V(us_alliances_graph)$name %in%
                                       us_alliances_net[,2],
                                     yes = TRUE, no = FALSE)
us_alliances_graph <- bipartite.projection(us_alliances_graph)$proj1
us_alliances_graph <- simplify(us_alliances_graph, remove.loops = TRUE,
                               remove.multiple = TRUE)
set.seed(2001525)
coords = layout_with_lgl(us_alliances_graph)
plot(us_alliances_graph, layout=coords, vertex.color = "coral2",
     vertex.label=NA, vertex.size = 0.05, edge.width = 0.002,
     edge.color = adjustcolor("black", alpha.f = 0.1))
