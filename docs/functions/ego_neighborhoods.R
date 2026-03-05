ego_neighborhoods <- function(graph, neighborhoods = 4, ego, labels = TRUE) {
  s <- 1:neighborhoods
  
  ego_nets <- map(s, .f = function(x) {
    egr             <- make_ego_graph(graph, order = x)[[which(V(graph)$name == ego)]]
    V(egr)$ego      <- V(egr)$name == ego
    V(egr)$dist_ego <- as.vector(distances(egr, v=ego, to = V(egr)))
    egr
  })
  lay_last  <- graphlayouts::layout_with_stress(ego_nets[[neighborhoods]])
  ego_nets <- lapply(ego_nets, function(x) { 
    x$lay   <- lay_last[V(ego_nets[[neighborhoods]])$name %in% V(x)$name, ]
    x
  })
  
  pl <- map(ego_nets, .f = function(x){
    p <- x %>% ggraph(x$lay) +
      geom_edge_link0(width = 0.4, alpha = 0.4) +
      geom_node_point(aes(color = factor(dist_ego)), size = 2) + scale_color_manual(values = c("0" = "black", "1" = "salmon", "2" = "steelblue", "3"= "lightgreen", "4" ="orange")) + guides(color = "none") + 
      theme_graph() + coord_fixed()
    if(labels == TRUE) p <- p + geom_node_label(aes(label = name), size = 1.3, repel = T)
    return(p)
  })
  return(pl)  
}