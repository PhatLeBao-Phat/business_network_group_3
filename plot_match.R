# Import dataset SDC
library(tidyr)
library(dplyr)
library(igraph)

#Matched SDC data
matched_SDC_data <- read.csv('data/matched_smartphone_SDC_2010_2016.csv') %>%
  select(participants, date_announced, type, SIC_primary,
         participant_nation, deal_number)


# Plot
matched_SDC_net <- matched_SDC_data %>%
  select(participants, deal_number) %>%
  as.matrix()

matched_SDC_graph <- graph.data.frame(matched_SDC_net, directed = FALSE)
V(matched_SDC_graph)$type <- ifelse(V(matched_SDC_graph)$name %in%
                                       matched_SDC_net[,2],
                                     yes = TRUE, no = FALSE)

# matched_sdc_graph <- bipartite.projection(matched_sdc_graph)$proj1
matched_SDC_graph <- simplify(matched_SDC_graph, remove.loops = TRUE,
                               remove.multiple = TRUE)
set.seed(2001525)
coords = layout_with_lgl(matched_SDC_graph)
plot(matched_SDC_graph, layout=coords, vertex.color = "coral2",
     vertex.label=NA, vertex.size = 2, edge.width = 0.002,
     edge.color = adjustcolor("black", alpha.f = 0.1))


# Plot bipartite
matched_SDC_net <- matched_SDC_data %>% select(participants, deal_number) %>% as.matrix()
matched_SDC_graph <- graph.data.frame(matched_SDC_net, directed = FALSE)
V(matched_SDC_graph)$type <- ifelse(V(matched_SDC_graph)$name %in%
                              matched_SDC_net[,2],
                            yes = TRUE, no = FALSE)
matched_SDC_graph <- simplify(matched_SDC_graph, remove.loops = TRUE,
                      remove.multiple = TRUE)
set.seed(06062003)
coords <- layout_with_kk(matched_SDC_graph)
vertex.type1.color <- c("red") %>% rep(length(unique(matched_SDC_net[, 1])))
vertex.type2.color <- c("blue") %>% rep(length(unique(matched_SDC_net[, 2])))
vertex.color <- c(vertex.type1.color, vertex.type2.color)
plot(matched_SDC_graph, layout=coords, vertex.color = vertex.color,
     vertex.label=NA, vertex.size = 2, edge.width = 0.002,
     edge.color = "black")
legend("topleft", legend=c("organization", "deal"), fill=c("red", "blue"))
png("fig/matched_smartphone_SDC.png", width = 800, height = 800, res = 100)
