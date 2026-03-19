community_plot <- function(graph, x, clusters, colors, layout, edge_attr = "louvain") {
  focus <- clusters
  
  is_focus_edge <- get.edge.attribute(graph, edge_attr)  == focus[x]
  is_focus_node <- get.vertex.attribute(graph, edge_attr)  == focus[x]
  edge_to_focus <- (get.edge.attribute(graph, paste0(edge_attr, "_a"))  == focus[x] | get.edge.attribute(graph, paste0(edge_attr, "_b")) == focus[x]) & !is_focus_edge
  
  graph %>% 
    ggraph(layout = layout) +
    #Edges
    geom_edge_link0(aes(filter=!is_focus_edge & !edge_to_focus), # plotter alle andre edges
                    color='grey70', width=0.1, alpha=0.1) +
    #Edges
    geom_edge_link0(aes(filter=edge_to_focus), # plotter alle andre edges
                    color='grey10', width=0.2, alpha=0.3) +
    geom_edge_link0(aes(filter= is_focus_edge, color = get(edge_attr)), width=0.3, alpha=0.8)  +
    #Nodes
    geom_node_point(aes(filter=!is_focus_node), color = "grey30", alpha=0.15, size=1) + 
    geom_node_point(aes(filter=is_focus_node, fill = get(edge_attr)), color = "black", shape = 21, alpha = 0.8, size=1.5) + 
    #Labels
    #geom_node_label(aes(filter=louvain == focus[x], label=name, color=louvain), alpha = 0.7, size = 2.5, repel=TRUE, force = 10, show.legend = F) + guides(fill = "none") +
    scale_fill_manual(values = colors, drop = FALSE) +
    scale_edge_color_manual(values = colors, drop = FALSE) +  guides(edge_color = "none", color = "none", fill = "none") + ggtitle(focus[x]) + theme(axis.title = element_text(family = "serif")) +
    theme_graph() 
}
