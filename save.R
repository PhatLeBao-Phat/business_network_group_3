# orgs.intersect <- intersect(
#   unique(V(sdc_graph.proj1)$name), 
#   unique(pivot_table$disambig_assignee_organization))
# sdc_graph.proj1 <- sdc_graph.proj1 %>%
#   delete_vertices(V(sdc_graph.proj1)[!V(sdc_graph.proj1)$name %in% orgs.intersect])
# orgs.names <- V(sdc_graph.proj1)$name %>% data.frame(disambig_assignee_organization=.)
# final <- orgs.names %>% merge(pivot_table, by = "disambig_assignee_organization", all.x = TRUE)
# print("Number of orgs with available patents count:")
# print(sum(!is.na(final["2010"])))
# print("Number of orgs connected to the network:")
# print(sum(degree(sdc_graph.proj1) >= 1))