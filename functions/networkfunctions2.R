
get_n_largest_component <- function(net, n = 1) {
  require(tidyverse)
  comps        <- net %>% decompose()
  size         <- comps %>% map(., .f = vcount) %>% unlist()
  index        <- tibble(index = 1:length(size), 
                         size) %>% arrange(-size)
  if(!n <= length(size)) stop(paste0("the graph contains only ", length(size), " components"))
  comps[[index$index[n]]]
}


add_vertex_attr <- function(graph = NULL, data = NULL, match_var = NULL) {
  tmp <- tibble(x = V(graph)$name) 
  colnames(tmp) <- match_var
  tmp <- tmp %>% left_join(., data %>% distinct(pick(matches(match_var)), .keep_all = T), by = match_var)
  old <- vertex_attr(graph)
  tmp <- tmp %>% select(-all_of(match_var))
  tmp <- c(old, as.list(tmp))
  vertex_attr(graph) <- tmp
  graph
}

get_edge_coord <- function(graph = comp1, layout = stable_lay, add_link = NULL) {
  e <- get.edgelist(graph) %>% data.frame 
  e <- e %>% left_join(., tibble(X1 = layout$name, x = layout$x, y = layout$y), by = "X1") %>% left_join(., tibble(X2 = layout$name, xend =layout$x,  yend = layout$y), by = "X2") 
  if(!is_null(add_link)){
  new_links <- lapply(add_link, function(x) {
    s1 <- which(layout$name == x[1])
    s2 <- which(layout$name == x[2])
    data.frame(X1 = layout$name[s1] %>% unique(), x = layout$x[s1] %>% unique(), y = layout$y[s1],
               X2 = layout$name[s2] %>% unique(), xend = layout$x[s2] %>% unique(), yend = layout$y[s2])
  }) %>% bind_rows %>% mutate(new = 2)
  
   e <- bind_rows(e, new_links) %>% mutate(new = if_else(is.na(new), 1, new))
   e 
  }else{
   e
  }
  }

cor_plots <- function(metrics_table, plot.title = "Korrelationer mellem centralitetsmål", title.size = 12, name_var = c("name", "affiliation")) {
  require(ggplot2)
  require(tidyverse)
  require(ggpubr)
  metrics_table <- metrics_table %>% select(-any_of(name_var))
  deg <- lapply(colnames(metrics_table), function(t) { 
    lapply(colnames(metrics_table), function(z) {
      tmp <- cbind(metrics_table %>% select(matches(z)), 
                   metrics_table %>% select(matches(t))) 
      colnames(tmp) <- c("x", "y")
      ggplot(data = tmp, aes(x = x, y = y)) + 
        geom_point(alpha = .5) + #geom_smooth(method = smooth_fun, se = FALSE, color = "salmon") + 
        xlab(z) + ylab(t) + theme_minimal(base_family = "serif") + 
        xlim(min(metrics_table %>% pull(var = matches(z))),max(metrics_table %>% pull(var = matches(z)))) +
        ylim(min(metrics_table %>% pull(var = matches(t))),max(metrics_table %>% pull(var = matches(t)))) #+
        # annotate(geom="text", 
        #                 x=max(metrics_table %>% pull(var = matches(z)))*0.1, 
        #                 y=max(metrics_table %>% pull(var = matches(t)))*0.95, 
        #                 label=paste0("τ= ", cor(metrics_table %>%  select(matches(z)),  metrics_table %>%  select(matches(t)), method = method) %>% round(.,3)), color="salmon", hjust = 0) 
                         })
                         })
        
  names(deg) <- colnames(metrics_table)
  ggarrange(plotlist = do.call(what = base::c, deg), align = "hv") %>% annotate_figure(.,top = text_grob(plot.title, face = "bold", size = title.size, family = "serif"))
}


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
  nc <- ceiling(length(pl) / 3)
  nr <- floor(length(pl) / 3)
  ggpubr::ggarrange(plotlist = pl, nrow = nc, ncol = nr) %>%  annotate_figure(., top = text_grob("K-core decomposition"))
  
}

centrality_viz <- function(net, layout = "fr", metrics = NULL, topselect = 15) {
require(ggraph)
require(graphlayouts)
require(ggpubr)
  lay <- create_layout(net, layout = layout)
  lapply(1:length(colnames(metrics)), function(i) {
    rnk <- metrics %>% mutate(across(matches(colnames(metrics)[i]), ~dense_rank(desc(.x)), .names = "rnk")) %>% pull(rnk)
    raw <- metrics %>% pull(var = matches(colnames(metrics)[i]))
  lay %>% ggraph() +
  geom_edge_link0(color='grey', width=0.6, alpha=0.35) + 
  geom_node_point(aes(filter=rnk>topselect), color = "black", alpha = 0.3, size = 1) +
  geom_node_point(aes(filter=rnk<=topselect, color = "salmon", size = raw)) + scale_size_continuous(range = c(2,4))+
  theme_graph() + guides(size = "none", color = "none") + ggtitle(colnames(metrics)[i])
  }) %>% ggarrange(plotlist = .)

}

edge_attr_from_vertex_attr <- function(net = NULL, vertex_attr = NULL) {
a1 <- as_tibble(as_edgelist(net), .repair_names = "unique")

v_attr <- get.vertex.attribute(net, vertex_attr)

tmp <- map2_chr(a1$V1, a1$V2, function(.x, .y){
  ifelse(
    v_attr[which(V(net)$name==.x)] ==
      v_attr[which(V(net)$name==.y)],
    v_attr[which(V(net)$name==.x)],
    "9999") 
  
  })
old <- get.edge.attribute(net)
if(length(old) >0) {
  tmp <- c(old, list(x = tmp))
  names(tmp)[length(tmp)] <- vertex_attr
  edge_attr(net) <- tmp
}else{
  net <- set.edge.attribute(net, name = vertex_attr, value = tmp)
  }
net
}

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
  
  p_e<- graph %>% ggraph("fr") +
    geom_edge_link0(aes(color = factor(clique))) +
    geom_node_point(size = 2) +
    theme_graph() + guides(edge_alpha = "none") + labs(edge_color = "Clique:") 
  #scale_edge_color_manual(values = c("grey50", "salmon3"), name = "4clique") +
  #scale_edge_width_manual(values =c(0.3,.7), name = "4clique") +
  #scale_edge_alpha_manual(values =c(0.4,.7), name = "") 
  
  p_v <- graph %>% ggraph("fr") +
    geom_edge_link0(edge_width = .3, color = "grey50") +
    geom_node_point(aes(color = factor(clique)), size = 3) +
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



tri_plot <- function(graph) {
  mat_tr <- matrix(triangles(graph), ncol = 3, byrow = T)
  e_triad <- bind_rows(data.frame(a = mat_tr[,1], b = mat_tr[,2]), data.frame(a = mat_tr[,1], b = mat_tr[,3]), data.frame(a = mat_tr[,2], b = mat_tr[,3])) %>% distinct()
  e_triad <- e_triad %>% transmute(X1 = case_when(b < a~b, .default = a), X2 = case_when(b < a~a, .default = b))
  
  E(graph)$is.triad_edge <- as_edgelist(graph, names = F) %>% data.frame() %>% left_join(., e_triad %>% mutate(is.triad_edge = TRUE)) %>% pull(is.triad_edge)
  E(graph)$is.triad_edge[is.na(E(graph)$is.triad_edge)] <- FALSE
  p_tr <- graph %>% ggraph("fr") +
    geom_edge_link0(aes(color = is.triad_edge, width= is.triad_edge, alpha = is.triad_edge)) +
    geom_node_point(size = 2) +
    theme_graph() + guides(edge_alpha = "none") +
    scale_edge_color_manual(values = c("grey50", "salmon3"), name = "closed triads") +
    scale_edge_width_manual(values =c(0.3,.7), name = "closed triads") +
    scale_edge_alpha_manual(values =c(0.4,.7), name = "") 
  p_tr
}

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
      geom_node_point(aes(color = factor(dist_ego)), size = 2) + scale_color_manual(values = c("black", "salmon", "steelblue", "lightgreen", "orange")) + guides(color = "none") + 
      theme_graph() + coord_fixed()
    if(labels == TRUE) p <- p + geom_node_label(aes(label = name), size = 1.3, repel = T)
    return(p)
  })
return(pl)  
}
