library(writexl)             
library(readxl)
library(tidyverse)
library(tidygraph)
library(Matrix)
library(igraph)
library(ggpubr)
library(ggraph)
library(ggplot2)
library(graphlayouts)
library(patchwork)
source("functions/custom_functions.R")
source("functions/networkfunctions.R")

d1 <- read_orbisxlsx("~/Downloads/Export 26_03_2025 21_22.xlsx", resultsheet = 2)
d2 <- read_orbisxlsx("~/Downloads/Export 26_03_2025 21_23.xlsx", resultsheet = 2)
d  <- bind_rows(d1, d2)
d  <- d %>% mutate(person_geo = case_when(is.na(person_country)~str_extract(person_countries, "^\\.*?(?=;|$)"), .default = person_country))  %>%  select(-person_countries)
write_xlsx(d, "data/ejendomsselskaber.xlsx")

d <- read_xlsx(path = "data/eksamen/ejendomsselskaber.xlsx", guess_max = 10000)
# Subset til persons og current
d  <- d %>% filter(person) %>% filter(role_status == "Current")
# Subset til kun bestyrelse og direktion
d  <- d %>% filter(grepl("ExeC|ExeB|SenMan|BoD", role_type))
# Sletter medarbejder reps og likvidatorer
d  <- d %>% filter(!grepl("Employee|Liquidator", role))
# samler samme personers roller i den samme virksomhed, så de kun har en rolle i en virksomhed
d  <- d %>% group_by(name) %>% mutate(across(.cols = all_of(c("role", "board_type", "role_type", "role_level", "appointment")), .fns = ~paste0(unique(.x), collapse = "|"))) %>% distinct()

# Hvor mange har mere end en bestyrelsespost?
d <- d %>% group_by(name) %>% mutate(n_affil = n_distinct(affiliation)) %>% ungroup()

d %>% distinct(name, .keep_all = T) %>% count(n_affil)

# Dropper folk der kun har en post
d <- d %>% filter(n_affil > 1)

# hvor mange medlemmer er der i hver bestyrelse:
d <- d %>% group_by(affiliation) %>% mutate(n_members = n_distinct(name)) %>% ungroup()

d %>% distinct(affiliation, .keep_all = T) %>% count(n_members)

# Fjern bestyrelser med kun ét medlem
d <- d %>% filter(n_members > 1)

# Antal personer i data:
d %>% distinct(name) %>% pull(name) %>% n_distinct()
# Antal virksomheder i data:
d %>% distinct(affiliation) %>% pull(affiliation) %>% n_distinct()

# laver incidence matrice individ x virksomhed
bi_adj <- d %>% xtabs(formula = ~name + affiliation, sparse = T)

# individ x individ adjacency matrice
adj_ind  <- bi_adj %*% t(bi_adj)
# virksomhed x virksomhed adjacency matrice
adj_virk <- t(bi_adj) %*% bi_adj

# individ x individ netværk
net_ind  <- graph_from_adjacency_matrix(adj_ind, mode = "undirected", diag = F) %>% simplify()

# virksomhed x virksomhed netværk
net_virk  <- graph_from_adjacency_matrix(adj_virk, mode = "undirected", diag = F) %>% simplify()

# Largest component i individ x individ
net_ind_l  <- largest_component(net_ind)
# Largest component i individ x individ
net_virk_l <- largest_component(net_virk)

p1 <- net_virk %>% 
  ggraph(layout='fr') + 
  geom_edge_link0(color='grey', width=0.6, alpha=0.45) + 
  geom_node_point(color='black', alpha=0.6)  + 
  labs(title = paste0("Netværket af virksomheder i \n'Ejendomsselskaber' (n=", vcount(net_virk), ")")) +
  theme_graph()  + theme(plot.title = element_text(family = "serif", size = 12))
p2 <- net_virk_l %>% 
  ggraph(layout='fr') + 
  geom_edge_link0(color='grey', width=0.6, alpha=0.45) + 
  geom_node_point(color='black', alpha=0.6)  + 
  labs(title = paste0("Den største komponent (n=", vcount(net_virk_l),")")) +
  theme_graph() + theme(plot.title = element_text(family = "serif", size = 12))

p1
p2

ggsave("output/Eksamen/Netværks_plot.pdf", plot = p1, width = 15, height = 10)

ggsave("output/Eksamen/Larg_comp.pdf", plot = p2, width = 15, height = 10)


# Vil vi gerne se hvordan virksomheder der forvalter ejendomme er forbundet gennem overlappende bestyrelser. Derfor vil gerne 'udregne' $affiliation \times affiliation$ matricen ved at gange en transponeret udgave af vores biadjacency matrice med sig selv ($B^T \times B$)
adj_c     <- t(bi_adj) %*% bi_adj

#Som altid skal vi lige lave vores adjacency matrice om til et grafobjekt med `igraph`-funktionen `graph_from_adjacency_matrix()`. Her har vi et "undirected", "weighted" netværk, hvor vi ser bort fra diagonalen:
gr <- graph_from_adjacency_matrix(adj_c, mode = "undirected", diag = FALSE, weighted = TRUE)

# Identificer forbundne komponenter
comp <- components(gr)

# Behold kun den største komponent
gr_connected <- induced_subgraph(gr, which(comp$membership == which.max(comp$csize)))

# Plot kun den største komponent
gr_connected %>% ggraph() +
  geom_edge_link() +
  geom_node_point() +
  theme_graph()

# lav netværks objektet for virksomheder
Prop_net_c <- graph_from_adjacency_matrix(adjmatrix = adj_virk, mode = "undirected") %>% simplify()

# lav netværks objektet for individer
Prop_net_ind <- graph_from_adjacency_matrix(adjmatrix = adj_ind, mode = "undirected") %>% simplify()

# Hvad udgør netværkets komponenter 

# Virksomhedsnetværket
count_components(Prop_net_c)
table(components(Prop_net_c)$csize)

comp1_c <- largest_component(Prop_net_c)
comp2_c <- get_n_largest_component(Prop_net_c, n = 2)


# Individnetværket
count_components(Prop_net_ind)
table(components(Prop_net_ind)$csize)

comp1_ind <- largest_component(Prop_net_ind)
comp2_ind <- get_n_largest_component(Prop_net_ind, n = 2)

# Tilføjelse af netværkseksterne node attributes til netværket 
d$birth_place <- d$`DMBirth place` %>% str_extract(., "(?<=, )\\S+$")
table(d$birth_place, d$person_countries, useNA = "ifany") %>% View()

# `DMBirth place`# Viksomheder
comp1_c <- add_vertex_attr(graph = comp1_c, 
                         data = d %>% 
                           select(affiliation, affiliation_country, sector, revenue, 
                                  n_employees),
                         match_var = "affiliation")

comp2_c <- add_vertex_attr(graph = comp2_c, 
                         data = d %>% 
                           select(affiliation, affiliation_country, sector, revenue,
                                  n_employees),
                         match_var = "affiliation")

# Individer
comp1_ind <- add_vertex_attr(graph = comp1_ind, 
                             data = d %>% 
                               select(name, person_gender, person_countries, role_type,
                                      role_level),
                             match_var = "name")

comp2_ind <- add_vertex_attr(graph = comp2_ind, 
                             data = d %>% 
                               select(name, person_gender, person_countries, role_type,
                                      role_level),
                             match_var = "name")


# 6. Komponent visualisering 

# Virksomheder
p <- (Prop_net_c - which(degree(Prop_net_c) == 0)) %>% ggraph("fr") +
  geom_edge_link0(width = 0.3, alpha = 0.3) +
  geom_node_point(size = .5) + ggtitle(paste0("Netværket i københavns ejendomssektor\n'propsektor' (n=", vcount(Prop_net_c), ")")) +
  theme_graph(base_family = "serif")
p1 <- comp1_c %>% ggraph("fr") +
  geom_edge_link0(width = 0.3, alpha = 0.3) +
  geom_node_point(aes(color = sector), size = 1.2) + ggtitle(paste0("Største komponent (n=", vcount(comp1_c), ")")) + guides(color = guide_legend("Sector")) +
  theme_graph(base_family = "serif")
p2 <- comp2_c %>% ggraph("fr") +
  geom_edge_link0(width = 0.3, alpha = 0.3) +
  geom_node_point(aes(color = sector), size = 1.2) + ggtitle(paste0("Næststørste komponent (n=", vcount(comp2_c), ")")) + guides(color = guide_legend("Sector")) +
  theme_graph(base_family = "serif")

pc <- p + p1 + p2

# Individer
p <- (Prop_net_ind - which(degree(Prop_net_ind) == 0)) %>% ggraph("fr") +
  geom_edge_link0(width = 0.3, alpha = 0.3) +
  geom_node_point(size = .5) + ggtitle(paste0("Netværket i københavns ejendomssektor\npropsektor (n=", vcount(Prop_net_ind), ")")) +
  theme_graph(base_family = "serif")
p1 <- comp1_ind %>% ggraph("fr") +
  geom_edge_link0(width = 0.3, alpha = 0.3) +
  geom_node_point(aes(color = person_countries), size = 1.2) + ggtitle(paste0("Største komponent (n=", vcount(comp1_ind), ")")) + guides(color = guide_legend("countries")) +
  theme_graph(base_family = "serif")
p2 <- comp2_ind %>% ggraph("fr") +
  geom_edge_link0(width = 0.3, alpha = 0.3) +
  geom_node_point(aes(color = person_countries), size = 1.2) + ggtitle(paste0("Næststørste komponent (n=", vcount(comp2_ind), ")")) + guides(color = guide_legend("countries")) +
  theme_graph(base_family = "serif")

pi <- p + p1 + p2

pc / pi

# Netværks mål 
dens     <- edge_density(comp1)
trans    <- transitivity(comp1)
radius   <- radius(comp1)
diameter <- diameter(comp1)

net_description <- c("nb. of nodes" = vcount(Prop_net_c),
                     "nb. of edges" = ecount(Prop_net_c),
                     "nb. of components" = count_components(Prop_net_c),
                     "largest component: nb. of nodes" = vcount(comp1), 
                     "largest component: share of nodes" = vcount(comp1) / vcount(Prop_net_c), 
                     "largest component: nb. of edges" = ecount(comp1), 
                     "largest component: share of edges" = ecount(comp1) / ecount(Prop_net_c),
                     "largest component: diameter" = diameter,  
                     "largest component: radius" = radius,
                     "largest component: density" = dens, 
                     "largest component: transitivity" = trans) %>% enframe(name = "Measures", value = "value")

write_xlsx(net_description, "output/Eksamen/Prop_CPH_net.xlsx")

# Burt's constraint 
# Strukturelle huller, brokerage, mål på egonetværks niveau
# Netværkslukning, lokal transitivitet

random_nodes <- sample(V(comp1)$name, 12)
random_nodes <- transitivity(comp1, type = "local", vids = random_nodes)
random_nodes[is.nan(random_nodes)] <- NA

random_nodes <- sort(random_nodes, decreasing = T, na.last = T)
random_nodes <- names(random_nodes)

pl <- ego_net_plot(graph = comp1, nodes = random_nodes, mode = "transitivity")
p <- ggpubr::ggarrange(plotlist = pl)
p
ggsave(plot = p, filename = "output/Eksamen/network closure.pdf", height = 6, width = 10)

# Ego-netværk og lokal transitivitet ----
# åbne vs. lukkede trekanter på ego niveau

# Definer noden
ego <- "1887 A/S"

# Opret et enkelt ego-netværk for orden 1 (valgfrit, hvis du stadig vil bruge det)
ego_net <- make_ego_graph(comp1, order = 1, nodes = "1887 A/S")[[1]]

# Plot det enkelte ego-netværk (valgfrit)
ggraph(ego_net) +
  geom_edge_link0() +
  geom_node_point(size = 2) +
  geom_node_label(aes(filter = {name == "1887 A/S"}, label = name)) +
  theme_graph()

# Opret ego-netværk for ordener 1 til 4
ego_nets <- lapply(1:4, function(order) {
  make_ego_graph(comp1, order = order, nodes = "1887 A/S")[[1]]
})

# Plot hvert ego-netværk
pl <- lapply(ego_nets, function(net) {
  ggraph(net) +
    geom_edge_link0() +
    geom_node_point(size = 2) +
    geom_node_label(aes(filter = {name == "1887 A/S"}, label = name)) +
    theme_graph()
})

# Arranger plottene
ggpubr::ggarrange(plotlist = pl, labels = c("1st neighbourhood", "2nd neighbourhood", "3rd neighbourhood", "4th neighbourhood"))

# constraint funktioen i Igraph beregner Burt's constraint, som er højere jo mere 'constrained' en node er, dvs. lav constraint = høj brokerage. 
ego_constr <- make_ego_graph(comp1)
names(ego_constr) <- V(comp1)$name

# Burts måde
constr_burt <- imap(ego_constr, function(x, y) constraint(x, nodes = which(vertex_attr(x, "name") == y)))

constr    <- constraint(comp1)
# det vil vi ofte gerne 'vende om' så det bliver et mål for brokerage evne.
brokerage <- 1 / constr

# 9.1 Centralitetsmål mv.

# Dem vi kender fra tidligere....:
deg     <-  degree(comp1)
betw    <-  betweenness(comp1, normalized = TRUE)
# local betweenness beregner betweenness i et lokalt område omkring ego, cutoff 2 betyder at vi kun kigger på 2nd neighbourhood..
local_betw    <-  betweenness(comp1, cutoff = 2, normalized = TRUE)

close   <-  closeness(comp1, normalized = TRUE)
eig     <-  eigen_centrality(comp1, directed = FALSE)$vector
core    <-  coreness(comp1)
local_transitivity <- transitivity(comp1, type = "local")

# Lad os samle de nodespecifikke centralitetsmål for den største komponent
cent_metrics <- tibble(
  name        = V(comp1)$name,
  degree      = deg, 
  betweenness = betw,
  local_betweenness = local_betw,
  closeness   = close,
  eigen       = eig,
  constraint  = constr,
  brokerage   = 1/ constr,
  coreness    = core,
  local_trans = local_transitivity)

cent_metrics <- cent_metrics %>% mutate(degree_rank      = dense_rank(desc(degree)),
                                        betw_rank        = dense_rank(desc(betweenness)),
                                        local_betw_rank  = dense_rank(desc(local_betweenness)),
                                        closeness_rank   = dense_rank(desc(closeness)),
                                        brokerage_rank   = dense_rank(desc(brokerage)),
                                        eigen_rank       = dense_rank(desc(eigen)),
                                        coreness_rank    = dense_rank(desc(core)),
                                        local_trans_rank = dense_rank(local_trans))

write_xlsx(cent_metrics, "output/Eksamen/Prop_CPH_centralitymetrics.xlsx")

# Tilføj centralitetsmål som attributes 

comp1 <- set_vertex_attr(comp1, "degree_rank", index = match(cent_metrics$name, V(comp1)$name), cent_metrics$degree_rank)

comp1 <- set_vertex_attr(comp1, "betw_rank", index = match(cent_metrics$name, V(comp1)$name), cent_metrics$betw_rank)

comp1 <- set_vertex_attr(comp1, "local_betw_rank", index = match(cent_metrics$name, V(comp1)$name), cent_metrics$local_betw_rank)

comp1 <- set_vertex_attr(comp1, "closeness_rank", index = match(cent_metrics$name, V(comp1)$name), cent_metrics$closeness_rank)

comp1 <- set_vertex_attr(comp1, "brokerage_rank", index = match(cent_metrics$name, V(comp1)$name), cent_metrics$brokerage_rank)

comp1 <- set_vertex_attr(comp1, "eigen_rank", index = match(cent_metrics$name, V(comp1)$name), cent_metrics$eigen_rank)

comp1 <- set_vertex_attr(comp1, "coreness_rank", index = match(cent_metrics$name, V(comp1)$name), cent_metrics$coreness_rank)

comp1 <- set_vertex_attr(comp1, "local_trans_rank", index = match(cent_metrics$name, V(comp1)$name), cent_metrics$local_trans_rank)

# Example: Constraint vs. closure/transitivity vs. local betweenness

cent_metrics
brokers <- cent_metrics %>% 
  arrange(brokerage_rank) %>% 
  filter(brokerage_rank %in% c(1,15,20,40,50,80, 100)) %>% 
  distinct(brokerage_rank, .keep_all = T) %>% 
  pull(name)

# Eksempel: Genopbyg pl med ego-netværk
ego_nets <- lapply(1:4, function(order) {
  make_ego_graph(comp1, order = order, nodes = "1887 A/S")[[1]]
})

pl <- lapply(ego_nets, function(net) {
  ggraph(net) +
    geom_edge_link0() +
    geom_node_point(size = 2) +
    geom_node_label(aes(filter = {name == "1887 A/S"}, label = name)) +
    theme_graph()
})

# Arranger plottene
p <- ggpubr::ggarrange(plotlist = pl, common.legend = TRUE, legend = "none")

ggsave(plot = p, filename = "output/Eksamen/constraint_brokerage.pdf", height = 6, width = 8)

ego_net <- make_ego_graph(comp1, order = 1, nodes = "1887 A/S")[[1]]

ggraph(ego_net) +
  geom_edge_link0() +
  geom_node_point(size = 2) +
  geom_node_label(aes(filter = {name == "1887 A/S"}, label = name)) +
  theme_graph()

# 11. Visualiseringer af netværk 

# Degree
p0 <- comp1 %>% as_tbl_graph() %>% arrange(degree_rank) %>% ggraph("stress") +
  geom_edge_link0(width =.3, alpha = 0.3) +
  geom_node_point(aes(color = degree_rank, size = degree)) + 
  scale_size_continuous(range = c(2,5)) + 
  geom_node_label(aes(filter=degree_rank<=5, label = paste0(degree_rank, ": ", name)), size = 3, repel = T, force = 25) +
  guides(label = "none", size = "none") +
  theme_graph(base_family = "serif")

p0.1 <- comp1 %>% as_tbl_graph() %>% arrange(coreness_rank) %>% ggraph("stress") +
  geom_edge_link0(width =.3, alpha = 0.3) +
  geom_node_point(aes(color = coreness_rank, size = degree)) + 
  scale_size_continuous(range = c(2,5)) + 
  geom_node_label(aes(filter=coreness_rank==1, label = paste0(coreness_rank, ": ", name)), size = 3, repel = T, force = 25) +
  guides(label = "none", size = "none") +
  theme_graph(base_family = "serif")

# Betweenness
p1 <- comp1 %>% as_tbl_graph() %>% arrange(betw_rank) %>% ggraph("stress") +
  geom_edge_link0(width =.3, alpha = 0.3) +
  geom_node_point(aes(color = betw_rank, size = betw_rank)) + 
  scale_size_continuous(range = c(2,5)) + 
  geom_node_label(aes(filter=betw_rank<=5, label = paste0(betw_rank, ": ", name)), size = 3, repel = T, force = 25) +
  guides(label = "none", size = "none") +
  theme_graph(base_family = "serif")

# local betweenness
p1.1 <- comp1 %>% as_tbl_graph() %>% arrange(local_betw_rank) %>% ggraph("stress") +
  geom_edge_link0(width =.3, alpha = 0.3) +
  geom_node_point(aes(color = local_betw_rank, size = local_betw_rank)) + 
  scale_size_continuous(range = c(2,5)) + 
  geom_node_label(aes(filter=local_betw_rank<=5, label = paste0(local_betw_rank, ": ", name)), size = 3, repel = T, force = 25) +
  guides(label = "none", size = "none") +
  theme_graph(base_family = "serif")


# closeness
p2<- comp1 %>% as_tbl_graph() %>% arrange(closeness_rank) %>% ggraph("stress") +
  geom_edge_link0(width =.3, alpha = 0.3) +
  geom_node_point(aes(color = closeness_rank, size = closeness_rank)) + 
  scale_size_continuous(range = c(2,5)) + 
  geom_node_label(aes(filter=closeness_rank<=5, label = paste0(closeness_rank, ": ", name)), size = 3, repel = T, force = 25) +
  guides(label = "none", size = "none") +
  theme_graph(base_family = "serif")

# brokerage
p3<- comp1 %>% as_tbl_graph() %>% arrange(brokerage_rank) %>% ggraph("stress") +
  geom_edge_link0(width =.3, alpha = 0.3) +
  geom_node_point(aes(color = brokerage_rank, size = degree)) + 
  scale_size_continuous(range = c(2,5)) + 
  geom_node_label(aes(filter=brokerage_rank<=5, label = paste0(brokerage_rank, ": ", name)), size = 3, repel = T, force = 25) +
  guides(label = "none", size = "none") +
  theme_graph(base_family = "serif")

p <- ggarrange(plotlist = list(p0, p1.1, p2, p3)) %>%  annotate_figure(., top = paste0("Største komponent (n=", vcount(comp1), ") top-5:"))

ggsave("output/Eksamen/Prop_CPH_net_plots.pdf", plot = p, width = 15, height = 10)