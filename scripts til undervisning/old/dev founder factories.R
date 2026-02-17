library(readxl)
dt <- read_xlsx("data/Founder Factories Data.xlsx")

dt <- dt %>% filter(!is.na(`Founder Factory`))
dt <- dt %>% mutate(across(everything(), .fns = ~gsub("\\(optræder.*", "", .x) %>% trimws()))

dt$`Affiliated founder 1`[dt$`New Company` == "issuu"] <- dt$`Investor 4`[dt$`New Company` == "issuu"]
dt$`Investor 4`[dt$`New Company` == "issuu"] <- NA

# Factory -> Founder
founder_fact_founder <- dt %>% select(`Founder Factory`, contains("Affiliated")) %>% pivot_longer(-`Founder Factory`) %>% select(`Founder Factory`, Founder = value) %>% filter(!is.na(Founder)) %>% mutate(role = "affiliated")

# Founder -> New Company
founder_nyvirk <- dt %>% select(`New Company`, contains("Affiliated")) %>% pivot_longer(-`New Company`) %>% select(Founder = value, `New Company`) %>% filter(!is.na(Founder)) %>% mutate(role = "founder")

# Investor -> New Company
investor_nyvirk <- dt %>% select(`New Company`, contains("Investor")) %>% pivot_longer(-`New Company`) %>% select(Investor = value, `New Company`) %>% filter(!is.na(Investor)) %>% mutate(role = "investor")
investor_nyvirk <- investor_nyvirk %>% filter(Investor != "No investors")

data <- bind_rows(founder_fact_founder %>% rename(a = `Founder Factory`, b = Founder), 
          founder_nyvirk %>% rename(a =Founder, b = `New Company`), 
          investor_nyvirk %>% rename(a = Investor, b = `New Company`))


write_xlsx(data, "data/Founder Factories Data.xlsx")

data <- read_xlsx("data/Founder Factories Data.xlsx")
net <- graph_from_data_frame(data)
V(net)$type <- case_when(V(net)$name %in% founder_fact_founder$`Founder Factory`~"Founder factory",
                         V(net)$name %in% founder_nyvirk$Founder~"Affiliated founder",
                         V(net)$name %in% dt$`New Company`~"New company",
                         V(net)$name %in% investor_nyvirk$Investor~"Investor")


net %>% ggraph("fr") +
  geom_edge_link2(aes(edge_linetype = role, color = role), 
                  arrow = arrow(angle = 30, length = unit(0.15, "cm"), 
                                ends = "last", type = "open"), 
                  end_cap = circle(.25, 'cm'), 
                  width = 0.2) +
  geom_node_point(aes(color = type), size = 3) +
  geom_node_label(aes(filter = {type == "Founder factory"}, label = name)) +
  labs(edge_linetype = "edge type", edge_color = "edge type", color = "node type") +
  theme_graph()


fact_affil    <- net %>% delete_edges(which(E(net)$role != "affiliated"))
founder_new_comp <- net %>% delete_edges(which(E(net)$role != "founder"))
invester_new_comp <- net %>% delete_edges(which(E(net)$role != "investor"))

d <- distances(net, mode = "all")
d[is.infinite(d)] <- 0
d[!d %in% c(2,3)] <- 0

net2 <- graph_from_adjacency_matrix(d, mode = "undirected", weighted = TRUE)

V(net2)$type <- "other"
V(net2)$type[V(net2)$name %in% data$a[data$role == "affiliated"]] <- "founder factory"
V(net2)$type[V(net2)$name %in% data$b[data$role == "founder"]] <- "new company"
ggraph(net2, "fr", weight = 1/E(net2)$weight) +
  geom_edge_link0(width = 0.2, alpha = 0.3) +
  geom_node_point(aes(color = type)) +
  theme_graph()

net2
graphjs(net, vertex.label = sprintf("<h2 style='text-align:top;'>%s</h2>", V(net)$name), vertex.size = .5, edge.color = "grey25", brush=TRUE)


table(E(net)$role)
degree(net, mode = "out") %>% enframe() %>% View()


biadj <- xtabs(~`Founder Factory`+Founder, data = founder_fact_founder, sparse = T)
fact_founder <- graph_from_biadjacency_matrix(biadj, directed = FALSE)
ggraph(fact_founder, "fr") +
  geom_edge_link0() +
  geom_node_point(aes(color = type)) +
  scale_color_manual(values = c("salmon3", "steelblue3"), 
                     labels = c("Founder", "Company")) +
  theme_graph()




biadj <- xtabs(~Founder+`New Company`, data = founder_nyvirk, sparse = T)
founder <- graph_from_biadjacency_matrix(biadj, directed = FALSE)
ggraph(founder, "fr") +
  geom_edge_link0(arrow = arrow(angle = 30, length = unit(0.25, "cm"),
                                 ends = "last", type = "open")) +
  geom_node_point(aes(color = type)) +
  scale_color_manual(values = c("salmon3", "steelblue3"), 
                     labels = c("Founder", "Company")) +
  theme_graph()



biadj <- xtabs(~Investor+`New Company`, data = investor_nyvirk, sparse = T)
investor <- graph_from_biadjacency_matrix(biadj, directed = TRUE)

investor  %>% ggraph("fr") +
  geom_edge_link0(arrow = arrow(angle = 30, length = unit(0.25, "cm"),
                                ends = "last", type = "open")) +
  geom_node_point(size = 3) +
  theme_graph()


# Founder factory -> New company
factory_nyvirk <- dt %>% select(`Founder Factory`, `New Company`)

biadj <- xtabs(~`Founder Factory`+`New Company`, data = factory_nyvirk, sparse = T)
#founder_fact <- graph_from_biadjacency_matrix(biadj, directed = TRUE)
founder_fact <- graph_from_data_frame(factory_nyvirk %>% select(`Founder Factory`, `New Company`), directed = TRUE)
V(founder_fact)$Founder_factory <- V(founder_fact)$name %in% factory_nyvirk$`Founder Factory`
ggraph(founder_fact, "kk") +
  geom_edge_link0(arrow = arrow(angle = 30, length = unit(0.25, "cm"),
                                ends = "last", type = "open")) +
  geom_node_point(aes(color = Founder_factory, size = Founder_factory)) +
  geom_node_label(aes(label = name), repel = T) +
  #scale_color_manual(values = c("salmon3", "steelblue3"), 
  #                  labels = c("Founder Factory", "New Company")) +
  theme_graph()




writexl::write_xlsx(list("Founder factory -> New Company" = factory_nyvirk, 
                         "Founder -> New Company" = founder_nyvirk, 
                         "Investor -> New Company" = investor_nyvirk), path = "data/founder factories.xlsx")



library(readxl)
Founderfactory_New_Company <- read_xlsx("data/founder factories.xlsx", sheet = "Founder factory -> New Company")
Founder_New_Company <- read_xlsx("data/founder factories.xlsx", sheet = "Founder -> New Company")
Investor_New_Company <- read_xlsx("data/founder factories.xlsx", sheet = "Investor -> New Company")






