tri_plot <- function(graph, mode = "closed") {
  mat_tr <- matrix(triangles(graph), ncol = 3, byrow = T)
  e_triad <- bind_rows(data.frame(a = mat_tr[,1], b = mat_tr[,2]), data.frame(a = mat_tr[,1], b = mat_tr[,3]), data.frame(a = mat_tr[,2], b = mat_tr[,3])) %>% distinct()
  e_triad <- e_triad %>% transmute(X1 = case_when(b < a~b, .default = a), X2 = case_when(b < a~a, .default = b))
  
  E(graph)$is.triad_edge <- as_edgelist(graph, names = F) %>% data.frame() %>% left_join(., e_triad %>% mutate(is.triad_edge = TRUE)) %>% pull(is.triad_edge)
  E(graph)$is.triad_edge[is.na(E(graph)$is.triad_edge)] <- FALSE
  if(mode == "open"){
  p_tr <- graph %>% ggraph("fr") +
    geom_edge_link0(aes(color = !is.triad_edge, width= !is.triad_edge, alpha = !is.triad_edge)) +
    geom_node_point(size = 1.5, alpha = 0.7) +
    theme_graph() + guides(edge_alpha = "none") +
    scale_edge_color_manual(values = c("grey50", "salmon3"), name = "open triads") +
    scale_edge_width_manual(values =c(0.3,.7), name = "open triads") +
    scale_edge_alpha_manual(values =c(0.4,.7), name = "") 
  }
  if(mode == "closed"){
    p_tr <- graph %>% ggraph("fr") +
      geom_edge_link0(aes(color = is.triad_edge, width= is.triad_edge, alpha = is.triad_edge)) +
      geom_node_point(size = 1.5, alpha = 0.7) +
      theme_graph() + guides(edge_alpha = "none") +
      scale_edge_color_manual(values = c("grey50", "salmon3"), name = "closed triads") +
      scale_edge_width_manual(values =c(0.3,.7), name = "closed triads") +
      scale_edge_alpha_manual(values =c(0.4,.7), name = "") 
  }
p_tr
}
