ego_net_plot <- function(graph, nodes, mode = c("constraint", "transitivity")) {
  mode <- mode
  require(tidygraph)
  plot_constraint <- function(graph = graph, x) {
    ego_net <- make_ego_graph(graph, nodes = x, order = 2)[[1]]
    ego_net1 <- make_ego_graph(graph, nodes = x, order = 1)[[1]]
    V(ego_net)$ego <- V(ego_net)$name == x  
    V(ego_net)$dist_ego <- as.vector(distances(ego_net, v=x, to = V(ego_net)))
    #measures
    tr    <- transitivity(graph, type = "local", x) %>% round(2)
    const <- constraint(graph, nodes = x) %>% round(2)
    brok  <- (1/const) %>% round(2) 
    burt_c <- constraint(ego_net1, nodes = x) %>% round(2)
    burt_brok <- (1/burt_c) %>% round(2)
    bet    <- betweenness(graph, x, directed = F, cutoff = 2, normalized = T) %>% round(3)
    ego_net <- ego_net %>% as_tbl_graph() %>% activate(edges) %>%
      mutate(e1 = (.N()$dist_ego[from]+ (.N()$dist_ego[to])))
    ego_net %>% ggraph('stress') +
      #geom_edge_link0(width = 0.3, alpha = 0.4) + 
      geom_edge_link(aes(filter = e1 <2 ), width = 0.4, alpha = 0.4) +
      geom_edge_link(aes(filter = e1 ==2), width = 0.4, alpha = 0.4) +
      geom_edge_link(aes(filter = e1 ==3), width = 0.2, alpha = 0.2, linetype = 2) +
      geom_node_point(aes(filter = dist_ego < 2 & dist_ego != 0, size = degree), color = "black") +
      geom_node_point(aes(filter = ego, size = degree), color = "salmon") + 
      scale_size_continuous(range = c(0.5,3))+
      # geom_node_label(aes(filter=dist_ego <2, label = name),
      #                  family = "serif",
      #                  size = 2.5,
      #                  label.size = 0.2,
      #                 alpha = 0.8,
      #                  color = "black",
      #                  repel = T, force = 25) +
      labs(caption = paste0("local transitivity =", tr, "\nigraph_constraint =", const, " | brokerage = ", brok,
                            "\nburt_constraint =", burt_c, " | brokerage = ", burt_brok,
                            "\nlocal betweenness =", bet)) +
      theme_graph(base_family = "serif") + guides(color = "none", size = "none") + ggtitle(x) + theme(plot.title = element_text(family = "serif", size = 8, color = "salmon"), plot.caption = element_text(family = "serif", size = 5))
  }
  
  plot_transitivity <- function(graph = graph, x) {
    ego_net <- make_ego_graph(graph, nodes = x, order = 1)[[1]]
    V(ego_net)$ego <- V(ego_net)$name == x   
    ego_net %>% ggraph('fr') +
      geom_edge_link0(width = 0.3, alpha = 0.4) + 
      geom_node_point(size = 2) +
      geom_node_point(aes(color = ego), size = 2) + scale_color_manual(values = c("black", "salmon")) +
      geom_node_label(aes(filter=ego==TRUE, label = name), size = 2, repel = T) + 
      labs(caption = paste0("local transitivity = ", transitivity(graph, type = "local")[(V(graph)$name == x)] %>% round(., 2))) +
      theme_graph(base_family = "serif") + guides(color = "none")
  }
  
  if(mode == "transitivity") { 
    out <- map(nodes, .f = ~plot_transitivity(graph, .x))
  }else if (mode == "constraint"){
    out <- map(nodes, .f = ~plot_constraint(graph, .x))
  }
  return(out)
}