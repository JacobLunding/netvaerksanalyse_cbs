coreness_viz <- function(net, algorithm = 'fr') {
  layout <- create_layout(net, layout = 'igraph', algorithm = algorithm)
  core <- coreness(net)
  pl <- lapply(sort(unique(core)), function(x) {
    s  <- core >= x
    e  <- get.edgelist(net)
    ee <- e[,1] %in% V(net)$name[s] & e[,2] %in% V(net)$name[s]
    #e <- edges(largest_comp_virk)
    ggraph(layout) +
      geom_edge_link0(aes(filter= ee==FALSE), edge_width = 0.3, edge_alpha = 0.3, color = "grey80") +
      geom_edge_link0(aes(filter= ee==TRUE), edge_width = 0.6, edge_alpha = 0.5, color = "grey40") +
      geom_node_point(aes(filter= s==FALSE), color = "grey70", alpha = .4) +
      geom_node_point(aes(filter= s==TRUE), color = "salmon2") + 
      labs(title = paste("K =", x, sep = ""), caption = paste0("n=", sum(s))) +
      theme_graph() + theme(plot.title = element_text(family = "serif", size = 10))
  })

  ggpubr::ggarrange(plotlist = pl) %>%  annotate_figure(., top = text_grob("K-core decomposition"))
  
}