# Import dataset SDC
library(tidyr)
library(dplyr)
library(igraph)
#matched_patents_views <-read.csv("data/matched_patents_views.csv")
# with matched smartphone SDC data
matched_smartphone <- read.csv('data/matched_smartphone_SDC_2010_2016.csv') %>%
  select(participants, date_announced, type, SIC_primary,
         participant_nation, deal_number)


# Plot
matched_smartphone_net <- matched_smartphone %>%
  select(participants, deal_number) %>%
  as.matrix()

matched_smartphone_graph <- graph.data.frame(matched_smartphone_net, directed = FALSE)
V(matched_smartphone_graph)$type <- ifelse(V(matched_smartphone_graph)$name %in%
                                       matched_smartphone_net[,2],
                                     yes = TRUE, no = FALSE)

matched_smartphone_graph <- bipartite.projection(matched_smartphone_graph)$proj1
matched_smartphone_graph <- simplify(matched_smartphone_graph, remove.loops = TRUE,
                               remove.multiple = TRUE)
set.seed(2001525)
coords = layout_with_lgl(sdc_graph)
plot(matched_smartphone_graph, layout=coords, vertex.color = "coral2",
     vertex.label=NA, vertex.size = 2, edge.width = 0.002,
     edge.color = adjustcolor("black", alpha.f = 0.1))


# Plot bipartite
matched_smartphone_net <- matched_smartphone %>% select(participants, deal_number) %>% as.matrix()
matched_smartphone_graph <- graph.data.frame(matched_smartphone_net, directed = FALSE)
V(matched_smartphone_graph)$type <- ifelse(V(matched_smartphone_graph)$name %in%
                              matched_smartphone_net[,2],
                            yes = TRUE, no = FALSE)
sdc_graph <- simplify(matched_smartphone_graph, remove.loops = TRUE,
                      remove.multiple = TRUE)
set.seed(06062003)
coords <- layout_with_kk(sdc_graph)
vertex.type1.color <- c("red") %>% rep(length(unique(matched_smartphone_net[, 1])))
vertex.type2.color <- c("blue") %>% rep(length(unique(matched_smartphone_net[, 2])))
vertex.color <- c(vertex.type1.color, vertex.type2.color)
plot(matched_smartphone_graph, layout=coords, vertex.color = vertex.color,
     vertex.label=NA, vertex.size = 2, edge.width = 0.002,
     edge.color = adjustcolor("black", alpha.f = 0.1))
legend("topleft", legend=c("organization", "deal"), fill=c("red", "blue"))

# save the network plot
png("fig/matched_smartphone_SDC.png", width = 800, height = 800, res = 100)  # Adjust width, height, and resolution as needed
plot(matched_smartphone_graph, layout = coords, vertex.color = "coral2",
     vertex.label = NA, vertex.size = 2, edge.width = 0.002,
     edge.color = adjustcolor("black", alpha.f = 0.1))