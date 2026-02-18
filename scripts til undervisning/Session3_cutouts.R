#####################################################/
# Cut points 
#####################################################/

comp1 <- largest_component(gr)
V(comp1)$name %>% tibble() %>% View()

V(comp1)$cut_point <- map_lgl(1:vcount(comp1), .f = ~delete_vertices(comp1, .x) %>% components(.) %>% .$no > 1)
V(comp1)$compred   <- map_dbl(1:vcount(comp1), .f = ~vcount(comp1) - delete_vertices(comp1, .x) %>% components(.) %>% .$csize %>% max())
index <- as_data_frame(comp1, "vertices") %>% 
  mutate(index = 1:vcount(comp1))
V(comp1)$lab <- index$index

p0 <- comp1 %>% ggraph() +
  geom_edge_link0(edge_width = 0.2, edge_color = "grey70") +
  geom_node_point(aes(color = cut_point, size = cut_point)) +
  geom_node_text(aes(filter = cut_point, label = lab), size = 3) +
  scale_color_manual(values = c("FALSE" = "steelblue4", "TRUE" = "salmon3")) +
  scale_size_manual(values = c("FALSE" = 1, "TRUE" = 4)) +
  theme_graph()
p0
ix <- index %>% arrange(-compred) %>% filter(cut_point) %>% 
  pull(index)
p <- map(ix, .f = function(x) {
  delete_vertices(comp1, x) %>% ggraph() +
    geom_edge_link0(edge_width = 0.2, edge_color = "grey70") +
    geom_node_point() + 
    theme_graph() + ggtitle(paste("node", index$lab[x], "removed"))
})

patchwork::wrap_plots(c(list(p0), p))




#################################################/
# Broer : edge betweenness
# Hvor vi sidst kigge på noder, der indgik i mange
# shortest paths og derfor var centrale, kan vi også
# kigge på edges og spørge om det er en bro for meget
# "trafik".
# weak ties vs non-redundant ties.
#################################################/

# Decomposing by edgebetweenness
eb <- edge_betweenness(comp1)
ggplot(eb %>% enframe()) + geom_density(mapping = aes(x = value))


# Lad os visualisere vores netværk og fremhæve edges med den højeste edgebetweenness
p1 <- comp1 %>% ggraph("fr") +
  geom_edge_link0(aes(color = dense_rank(desc(eb)) <25, width= eb, alpha = eb)) +
  geom_node_point(size = 2) +
  theme_graph() + guides(edge_alpha = "none") +
  scale_edge_color_manual(values = c("grey50", "salmon3"), name = "Top25 edge betweenness") +
  scale_edge_width_binned(range =c(0.02,1), name = "edge betweenness")

p2 <- comp1 %>% delete_edges(which(dense_rank(desc(edge_betweenness(comp1))) <25)) %>% ggraph("fr") +
  geom_edge_link0(edge_width = 0.3, edge_alpha = 0.3) +
  geom_node_point(size = 2) +
  theme_graph()

p1 / p2

data <- tibble(t = 0, diameter = diameter(comp1), 
               mean_dist = mean_distance(comp1), 
               n_edges = ecount(comp1), 
               n_components = components(comp1)$no, 
               size_l_component = max(components(comp1)$csize))
data_rand <- tibble(t = 0, diameter = diameter(comp1), 
                    mean_dist = mean_distance(comp1), 
                    n_edges = ecount(comp1), 
                    n_components = components(comp1)$no, 
                    size_l_component = max(components(comp1)$csize))


# Et stykke kode, der løbende sletter edge'en med den højeste edge betweenness

g_tmp <- comp1
g_tmp_rnd <- comp1
i <- 1
while(ecount(g_tmp) > 1){
  g_tmp <- g_tmp %>% delete_edges(g_tmp %>% edge_betweenness() %>% which.max())
  g_tmp_rnd <- g_tmp_rnd %>% delete_edges(sample(1:ecount(g_tmp_rnd), 1))
  data <- data %>% add_row(
    tibble(t = i, diameter = diameter(g_tmp), 
           mean_dist = mean_distance(g_tmp), 
           n_edges = ecount(g_tmp), 
           n_components = components(g_tmp)$no,
           size_l_component = max(components(g_tmp)$csize)))
  
  data_rand <- data_rand %>% add_row(
    tibble(t = i, diameter = diameter(g_tmp_rnd), 
           mean_dist = mean_distance(g_tmp_rnd), 
           n_edges = ecount(g_tmp_rnd), 
           n_components = components(g_tmp_rnd)$no,
           size_l_component = max(components(g_tmp_rnd)$csize)))
  i <- i + 1
}

data2 <- data  %>% pivot_longer(-t) %>% mutate(name = factor(name, levels = c("n_edges", "n_components", "size_l_component", "diameter", "mean_dist"), labels = c("Edges", "#Components", "Largest component size","Diameter", "Avr. path-length")))
data_rand2 <- data_rand  %>% pivot_longer(-t) %>% mutate(name = factor(name, levels = c("n_edges", "n_components", "size_l_component", "diameter", "mean_dist"), labels = c("Edges", "#Components", "Largest component size","Diameter", "Avr. path-length")))

d <- bind_rows(data2 %>% mutate(type = "Sletter 'bridges'"), data_rand2 %>% mutate(type = "Sletter tilfældigt")) %>% mutate(type = factor(type, levels = c("Sletter 'bridges'", "Sletter tilfældigt")))
d1 <- d %>% filter(name != "Edges") %>% droplevels.data.frame()
ggplot(d1) + geom_line(aes(x = t, y = value)) + 
  geom_point(data = d1 %>% group_by(name) %>% dplyr::summarise(MAX = max(value, na.rm = T)) %>% ungroup(), aes(x = 0, y = MAX), colour = "white", alpha = 0) + geom_point(data = d1 %>% group_by(name) %>%dplyr::summarise(MIN = min(value, na.rm = T)) %>% ungroup(), aes(x = 0, y = MIN), colour = "white", alpha = 0) +
  facet_grid(rows = vars(name), cols = vars(type), scale = "free_y", switch = "y") + theme_bw()
