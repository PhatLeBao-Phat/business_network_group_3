# HELPERS FUNCTION 
library(nstandr)
library(igraph)
library(tidyr)
library(dplyr)
library(migraph)
library(igraph)

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
make_plot <- function(sdc_data) {
  sdc_net <- sdc_data %>%
    select(participants, deal_number) %>%
    as.matrix()
  sdc_graph <- graph.data.frame(sdc_net, directed = FALSE)
  V(sdc_graph)$type <- ifelse(V(sdc_graph)$name %in% sdc_net[,2],
                              yes = TRUE, no = FALSE)
  sdc_graph.proj1 <- bipartite.projection(sdc_graph)$proj1
  return(list(bipartite=sdc_graph, proj1=sdc_graph.proj1))
}