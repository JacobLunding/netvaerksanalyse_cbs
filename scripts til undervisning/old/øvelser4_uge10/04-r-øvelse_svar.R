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
source("functions/networkfunctions.R")




# 2. Indlæs data ----
  # brug igen pharma datasættet, som ligger i input
pharma <- read_csv("input/pharma.csv")


# 3. subset datasættet ----
 # så vi kun kigger på virksomheder med mere end 1 i boardet og hvor data om omsætningen (affil_rev) ikke er na, og kun personer, der har poster i mere end en koncern:
    # til det skal vi først:
        # lave en ny variabel n_ind som tæller [n_distinct] unikke navne for hver affiliation
        # lave en ny variabel n_guo som tæler [n_distinct] unikke koncern navne [guo] for hver person
        # 
pharma <- pharma %>% 
  mutate(n_ind = n_distinct(name), .by = affiliation) %>% 
  mutate(n_guo = n_distinct(guo), .by = name)

pharma <- pharma %>% filter(n_guo > 1 & n_ind > 1 & !is.na(affil_rev))


# 4. Lav et individ x individ netværk ----

net_2m        <- pharma %>% select(name, affiliation) %>% graph_from_data_frame(, directed = F)
vertex.names  <- V(net_2m)$name
V(net_2m)$type  <- vertex.names %in% pharma$name
net       <- net_2m %>% bipartite.projection(which= "false", multiplicity = F)


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
  # og gem dem i et data objekt, en 'tibble()'

deg <- degree(l_comp) 
bet <- betweenness(l_comp) 
clo  <- closeness(l_comp) 
eig <- eigen_centrality(l_comp)$vector

net_metrics <- tibble(
  name =          V(l_comp)$name,
  degree =        deg,
  betweenness =   bet,
  closeness =     clo,
  eigen =         eig
)

# 7. Tilføj en rankvariable for hver af centralitetsmålene og lav en samlet centralitets-rank

net_metrics <- net_metrics %>% mutate(degree_rank     = dense_rank(desc(degree)),
                                      betw_rank       = dense_rank(desc(betweenness)),
                                      closeness_rank  = dense_rank(desc(closeness)),
                                      eigen_rank      = dense_rank(desc(eigen)))
net_metrics <- net_metrics %>% mutate(cent_rank = dense_rank(degree_rank + betw_rank + closeness_rank + eigen_rank))

# 8. Åben jeres tibble med centralitetsmål i Vieweren ----
  # ... og kig lidt på hvilke virksomheder der ligger i toppen på de forskellige centralitetsmål
  




# 9. Tilføj centralitetsmålene til netværksobjektet for den størte komponent og lav forskellige visualiseringer. ---- 
  # sæt color og size på noderne efter målene..


V(l_comp)$betw_rank       <- net_metrics$betw_rank
V(l_comp)$betweenness     <- net_metrics$betweenness
V(l_comp)$closeness_rank  <- net_metrics$closeness_rank
V(l_comp)$closeness       <- net_metrics$closeness
V(l_comp)$degree_rank     <- net_metrics$degree_rank
V(l_comp)$degree          <- net_metrics$degree
V(l_comp)$coreness        <- net_metrics$coreness


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
    filter=betw_rank <= 10, label=name), alpha=0.8, size = 3, repel=T, force = 20) 

ggsave('output/pharma-graph-betweenness.png', width=30, height=17.5, unit='cm')

# another example, with closeness
lay %>% arrange(closeness) %>% ggraph() +
  geom_edge_link0(data = e, color='grey', width=0.3, alpha=0.35) + 
  geom_node_point(aes(color=closeness, size = closeness)) + scale_size_continuous(range = c(0.5,5)) + scale_alpha(range = c(0.5,1)) + 
  theme_graph() + scale_color_viridis(direction = 1) + guides(size = "none") + labs(color="closeness") +
  geom_node_label(aes(
    filter=closeness_rank <= 10, label=name), alpha=0.8, size = 3, repel=T, force = 20) 

ggsave('output/pharma-graph-closeness.png', width=30, height=17.5, unit='cm')


# 10. EKSTRAOPGAVE: Prøv at lave en simpel regression mellem betweenness og omsætning:

rev <- pharma %>% group_by(affiliation) %>% summarise(revenue = sum(affil_rev))
net_metrics <- net_metrics %>% left_join(., rev %>% select(affiliation, revenue), by = c("name"="affiliation"))

cor(net_metrics %>% select(betweenness, revenue))
fit <- lm(data = net_metrics, formula = revenue~betweenness+degree)
summary(fit)