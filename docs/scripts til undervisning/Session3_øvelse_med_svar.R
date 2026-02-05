##############  #Lektion03 ################
# 
# Virksomhedsstrategi i et netværksperspektiv 
# Centralitetsmål - øvelse
#
###########################################

# 1. Indlæs (library) nødvendige pakker: ----
 # vi skal bruge tidyverse, som altid
 # igraph til grafobjekter og centralitetsmål
 # ggraph og ggplot2 og ggpubr til grafplots
 # og "functions/networkfunctions.R" som skal sources
library(tidyverse)
library(igraph)
library(ggraph)
library(ggplot2)
library(ggpubr)
library(Matrix)
source("functions/networkfunctions.R")




# 2. Indlæs data ----
  # brug igen pharma datasættet, som ligger i data
pharma <- read_csv("data/pharma.csv")


# 3. subset datasættet ----
 # så vi kun kigger på virksomheder med mere end 1 i boardet og hvor data om omsætningen (revenue) ikke er na, og kun personer, der har poster i mere end en koncern:
    # til det skal vi først:
        # først lige sørge for at der ikke er række duplicates samme person i samme virksomhed.
        # lave en ny variabel n_ind som tæller [n_distinct] unikke navne for hver affiliation
        # lave en ny variabel n_afil som tæler [n_distinct] unikke affilialtions for hver person
        # 
pharma <- pharma %>% distinct(name, affiliation, .keep_all = TRUE) %>% 
  mutate(n_ind  = n_distinct(name), .by = affiliation) %>% 
  mutate(n_afil = n_distinct(affiliation), .by = name) 

pharma <- pharma %>% filter(n_afil > 1 & !is.na(revenue) & n_ind > 1)


# 4. Lav et virksomhed x virksomhed netværk ----
# dvs. lav først biadjacency m. xtabs og dernæst adjacency m. t(B) %*% B og tilsidst graph_from_adjacency_matrix

biadj   <- xtabs(data = pharma, ~name +affiliation , sparse = T)

adj_virk <- t(biadj) %*% biadj

net     <- graph_from_adjacency_matrix(adj_virk, mode = "undirected", weighted = TRUE, diag = FALSE)

# 5. Se på grafens komponentstruktur og lav et nyt grafobjekt med den størte komponent ----
comp.list <- components(net)
comp.list$no
comp.list$csize %>% table()

l_comp <- largest_component(net)

l_comp %>% 
  ggraph(layout = 'fr') +
  geom_edge_link0(edge_width = 0.3, edge_alpha = 0.4) +
  geom_node_point(size = 3) +
  theme_graph()




# 6. Udregn forskellige centralitets mål ----
  # og gem dem som vertex attributes i grafobjektet
  # og lave en tibble med vertices og vertex attributes fra grafobjektet:

V(l_comp)$degree          <- degree(l_comp) 
V(l_comp)$betweenness     <- betweenness(l_comp) 
V(l_comp)$closeness       <- closeness(l_comp) 
V(l_comp)$eigen           <- eigen_centrality(l_comp)$vector


net_metrics <- as_data_frame(l_comp, what = "vertices") %>% tibble()

# 7. Tilføj en rankvariable for hver af centralitetsmålene
# enten sådan 1)
net_metrics <- net_metrics %>% mutate(degree_rnk     = dense_rank(desc(degree)),
                                      betw_rnk       = dense_rank(desc(betweenness)),
                                      closeness_rnk  = dense_rank(desc(closeness)),
                                      eigen_rnk      = dense_rank(desc(eigen)))
# eller
# sådan 2)
net_metrics <- net_metrics %>% mutate(across(.cols = -name, .fns = ~dense_rank(desc(.x)), .names = "{.col}_rnk"))



# 8. Åben jeres tibble med centralitetsmål i Vieweren ----
  # ... og kig lidt på hvilke virksomheder der ligger i toppen på de forskellige centralitetsmål
View(net_metrics)  




# 9. Prøv at lave forskellige visualiseringer. ---- 
  # sæt color og size på noderne efter målene..

# denne kode sørger for at man kan sortere noderne så de mest centrale ligger øverst....
lay <- create_layout(l_comp, layout = "fr")
e <- as_data_frame(l_comp, "edges")
e <- e %>% left_join(., lay %>% select(x, y, name), by = c("to"="name"))
e <- e %>% left_join(., lay %>% select(x, y, name) %>% rename(xend = x, yend = y), by = c("from"="name"))

lay %>% arrange(betweenness) %>% ggraph() +
  geom_edge_link0(data = e, color='grey', width=0.3, alpha=0.35) + 
  geom_node_point(aes(filter = betweenness ==0), color = "black", size = .4) +
  geom_node_point(aes(filter = betweenness >0, color=betweenness, size = betweenness)) + scale_size_continuous(range = c(0.5,7)) + scale_alpha(range = c(0.5,1)) + 
  theme_graph() + scale_color_viridis(direction = 1) + guides(size = "none") + labs(color="Betweenness") +
  geom_node_label(aes(
    filter=net_metrics$betweenness_rnk <= 10, label=name), alpha=0.8, size = 3, repel=T, force = 20) 

ggsave('output/pharma-graph-betweenness.png', width=30, height=17.5, unit='cm')

# another example, with closeness
lay %>% arrange(closeness) %>% ggraph() +
  geom_edge_link0(data = e, color='grey', width=0.3, alpha=0.35) + 
  geom_node_point(aes(color=closeness, size = closeness)) + scale_size_continuous(range = c(0.5,5)) + scale_alpha(range = c(0.5,1)) + 
  theme_graph() + scale_color_viridis(direction = 1) + guides(size = "none") + labs(color="closeness") +
  geom_node_label(aes(
    filter=net_metrics$betweenness_rnk <= 10, label=name), alpha=0.8, size = 3, repel=T, force = 20) 

ggsave('output/pharma-graph-closeness.png', width=30, height=17.5, unit='cm')


# 10. EKSTRAOPGAVE: Prøv at lave en simpel regression mellem betweenness og omsætning:

rev <- pharma %>% group_by(affiliation) %>% summarise(revenue = sum(unique(revenue)))
net_metrics <- net_metrics %>% left_join(., rev, by = c("name"="affiliation"))

cor(net_metrics %>% select(revenue), net_metrics %>% select(betweenness, degree, closeness, eigen))

fit <- lm(data = net_metrics, formula = revenue~betweenness+degree+closeness+eigen)
summary(fit)
