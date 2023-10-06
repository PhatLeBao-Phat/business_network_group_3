# Import dataset SDC
library(tidyr)
library(dplyr)
library(igraph)

# SDC data
sdc_data <- readRDS('data/smartphone_SDC_2010_2016.rds') %>%
  select(participants, date_announced, type, SIC_primary,
         participant_nation, deal_number)


# Plot
sdc_net <- sdc_data %>%
  select(participants, deal_number) %>%
  as.matrix()

sdc_graph <- graph.data.frame(sdc_net, directed = FALSE)
V(sdc_graph)$type <- ifelse(V(sdc_graph)$name %in%
                                       sdc_net[,2],
                                     yes = TRUE, no = FALSE)

sdc_graph <- bipartite.projection(sdc_graph)$proj1
sdc_graph <- simplify(sdc_graph, remove.loops = TRUE,
                               remove.multiple = TRUE)
set.seed(2001525)
coords = layout_with_lgl(sdc_graph)
plot(sdc_graph, layout=coords, vertex.color = "coral2",
     vertex.label=NA, vertex.size = 2, edge.width = 0.002,
     edge.color = adjustcolor("black", alpha.f = 0.1))


# Plot bipartite 
sdc_net <- sdc_data %>% select(participants, deal_number) %>% as.matrix()
sdc_graph <- graph.data.frame(sdc_net, directed = FALSE)
V(sdc_graph)$type <- ifelse(V(sdc_graph)$name %in%
                              sdc_net[,2],
                            yes = TRUE, no = FALSE)
sdc_graph <- simplify(sdc_graph, remove.loops = TRUE,
                      remove.multiple = TRUE)
set.seed(06062003)
coords <- layout_with_kk(sdc_graph)
vertex.type1.color <- c("red") %>% rep(length(unique(sdc_net[, 1])))
vertex.type2.color <- c("blue") %>% rep(length(unique(sdc_net[, 2])))
vertex.color <- c(vertex.type1.color, vertex.type2.color)
plot(sdc_graph, layout=coords, vertex.color = vertex.color,
     vertex.label=NA, vertex.size = 2, edge.width = 0.002,
     edge.color = adjustcolor("black", alpha.f = 0.1))
legend("topleft", legend=c("organization", "deal"), fill=c("red", "blue"))
