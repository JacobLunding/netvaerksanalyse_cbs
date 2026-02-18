clique_plot <- function(graph, n = 3, mode = "edges") {
  cl1        <- max_cliques(graph)
  names(cl1) <- map_dbl(cl1, length)
  cl        <- lapply(cl1, function(x) {
    tmp <- stack(x)  
    tmp <- combn(tmp$values, 2, simplify = F)
    tmp <- lapply(tmp, function(z) {
      names(z) <- c("a", "b") 
      z
    }) %>% bind_rows()
    tmp}
  ) %>% bind_rows(, .id = "clique")
  
  e_c <- cl %>% transmute(X1 = case_when(b < a~b, .default = a), X2 = case_when(b < a~a, .default = b), clique = clique %>% as.numeric()) %>% arrange(-clique) %>% filter(clique %in% n) %>%  distinct(X1, X2, .keep_all = T) 
  
  E(graph)$clique <- as_edgelist(graph, names = F) %>% data.frame() %>% left_join(., e_c) %>% pull(clique)
  
  v_c <- map(cl1, .f = ~stack(.x)) %>% bind_rows(, .id = "clique")
  v_c <- v_c %>% mutate(clique = clique %>% as.numeric()) %>% arrange(-clique) %>% filter(clique %in% n) %>% distinct(values, ind, .keep_all = T) 
  V(graph)$clique <- as.numeric(V(graph)) %>% enframe() %>% left_join(., v_c %>% select(value = values, clique)) %>% pull(clique)
  
  p_e<- graph %>% ggraph("kk") +
    geom_edge_link0(aes(color = factor(clique), alpha = factor(clique %>% is.na()),edge_width = factor(clique %>% is.na()))) +
    scale_edge_alpha_manual(values =c(0.9,0.3), name = "") +
    scale_edge_width_manual(values =c(0.7,0.3), name = "") +
    geom_node_point(size = 1.5, alpha = 0.6) +
    theme_graph() + guides(edge_alpha = "none", edge_width = "none") + labs(edge_color = "Clique:") 
  #scale_edge_color_manual(values = c("grey50", "salmon3"), name = "4clique") +
  #scale_edge_width_manual(values =c(0.3,.7), name = "4clique") +
  
  
  p_v <- graph %>% ggraph("kk") +
    geom_edge_link0(edge_width = .3, alpha = 0.3, color = "grey50") +
    geom_node_point(aes(filter = is.na(factor(clique))), color = "grey50", size = 1) +
    geom_node_point(aes(filter = !is.na(factor(clique)), color = factor(clique)), size = 2.5) +
    theme_graph() + labs(color = "Clique:") 
  if(mode == "vertices") {
    p_v
  }
  if(mode == "edges") {
    p_e  
  }
  if(mode== "both") {
    p <- list()
    p$vertices <- p_v
    p$edges    <- p_e
    p
  }
}



