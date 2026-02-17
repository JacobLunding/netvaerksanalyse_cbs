source("functions/custom_functions.R", echo = FALSE)
den <- read_csv("data/den17-no-nordic-letters.csv")
show.all.tags(den)

den %>% count(sector)

den_tran <- has.tags(den , tags = c("BILH","Cars","City planning","Public transport", "TRAN", "Transport", "Traffic", "Infrastruce", "VL", "Roads"), result = "den", mode = "or")


den_tran %>% count(sector)

biadj  <- den_tran %>% xtabs(formula = ~name + affiliation, sparse = TRUE)

adj_c <- t(biadj) %*% biadj 

net <- graph_from_adjacency_matrix(adj_c, weighted = TRUE, mode = "undirected")

lc <- largest_component(net)

V(lc)$sector <- data.frame(name = V(lc)$name) %>% left_join(.,den_tran %>% distinct(affiliation, sector), by = c("name"= "affiliation")) %>% pull(sector)

V(lc)$betweenness <- betweenness(lc)
V(lc)$closeness <- closeness(lc)


ggraph(lc) +
  geom_edge_link0(edge_width = 0.2, alpha = 0.4) +
  geom_node_point(aes(color = sector, size = betweenness)) +
  theme_graph()

metrics <- as_data_frame(lc, "vertices")
metrics <- metrics %>% mutate(across(.cols = -name,.fns = ~dense_rank(desc(.x)), .names = "{.col}_rnk"))

V(lc)$betweenness_rnk <- metrics$betweenness_rnk
ggraph(lc) +
  geom_edge_link0(edge_width = 0.2, alpha = 0.4) +
  geom_node_point(aes(color = sector, size = betweenness)) +
  geom_node_label(aes(label = name, filter = betweenness_rnk <= 5)) +
  theme_graph()

View(metrics)
