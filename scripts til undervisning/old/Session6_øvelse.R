library(tidyverse)
library(readxl)
library(writexl)
library(igraph)
library(ggraph)
library(ggpubr)
library(Matrix)
# Ny pakke at installere
# install.packages("RColorBrewer")
library(RColorBrewer)
source("functions/networkfunctions.R")
source("functions/custom_functions.R")
###################################################################################################/
# 1. Læs datafil ----
###################################################################################################/


###################################################################################################/
# 2. Subset og omkod data m.m. ----
###################################################################################################/

fin  <- c("Finance", "FINA", "Banks", "Venture- og kapitalfonde", "Pensions", "Insurance", "Investment")

den_sub  <- has.tags(den, tags = fin, result = "den")


###################################################################################################/
# 3. Definerer et netværksobjekt  ----
###################################################################################################/

# lav en sparse incidence matrice name x affiliation: 
# (..., formula = ~ name + affiliation) giver en incidence matrice med name (individer) i rækker og affiliation (virksomheder) i kolonner  
bi_adj <- xtabs(data = den_sub, formula = ~name + affiliation, sparse = T)


# hvis vi vil være sikre på at folk ikke har mere end 1 post i den samme bestyrelse kan vi tvinge vores incidence matrice til at være binær (0/1) ved at sætte alle værdier over 1 til 1
bi_adj@x %>% table()
bi_adj[bi_adj > 1] <- 1

# lav virksomhed x virksomhed adjacency matricen: 
# brug matrix multiplikation:  incidence %*% Matrix::t(incidence)
adj_corp  <- t(bi_adj) %*% bi_adj

#ELLER

# lav individ x individ adjacency matricen: 
# brug matrix multiplikation:  incidence %*% Matrix::t(incidence)
adj_corp  <- bi_adj %*% t(bi_adj)

# lav netværks objektet
net <- graph_from_adjacency_matrix(adjmatrix = , mode = "undirected") %>% simplify()

# evt også twomode
net_2m <- graph_from_biadjacency_matrix(bi_adj, directed = FALSE) %>% simplify()

###################################################################################################/
# 4. Netværkets komponenter? ----
###################################################################################################/

# gem fx den største komponent


###################################################################################################/
# 5. Tilføj netværkseksterne node attributes til netværket ----
###################################################################################################/

comp1 <- add_vertex_attr(graph = comp1, 
                         data = den_sub %>% ungroup() %>% 
                           select(), # Hvilke variable
                         match_var = "???") # hvad er matchet

###################################################################################################/
# 6. Komponent visualisering ----
###################################################################################################/

# Two mode plot
# jeg tæller først lige hvor mange der er af hver type (for at bruge det i titlen på plottet)
type <- table(V(net_2m)$type)


p2m <- net_2m %>% ggraph("fr") +
  geom_edge_link0(color = "grey35", width = 0.3, alpha = 0.3) +
  geom_node_point(aes(size = factor(type), color= type, shape = type)) + 
  ggtitle("Mit plot") + 
  scale_color_discrete(labels = c("individuals", "corporations")) + 
  scale_size_manual(values = c(2, 4), labels = c("individuals", "corporations")) + 
  scale_shape_manual(values = c(4, 19), labels = c("individuals", "corporations")) +   labs(size = "node type", color = "node type", shape = "node type") +
  theme_graph(base_family = "serif")

# Alle virksomheder
p <- net %>% ggraph("kk") +
  geom_edge_link0(color = "grey35", width = 0.3, alpha = 0.3) +
  geom_node_point(size = .5) + ggtitle(paste0("Mit plot (n=", vcount(net), ")")) +
  theme_graph(base_family = "serif")

# Største komponent af virksomheder
p1 <- comp1 %>% ggraph("kk") +
  geom_edge_link0(color = "grey35", width = 0.2, alpha = 0.2) +
  geom_node_point(size = 1.5, aes(color = sector)) + ggtitle(paste0("Største komponent (n=", vcount(comp1), ")")) +
  theme_graph(base_family = "serif")


p2m
ggarrange(plotlist = list(p, p1))

###################################################################################################/
# 7. Netværks mål ----
###################################################################################################/

dens     <- edge_density(comp1)
trans    <- transitivity(comp1)
radius   <- radius(comp1)
diameter <- diameter(comp1)

net_description <- c("nb. of nodes" = vcount(comp1),
                     "nb. of edges" = ecount(comp1),
                     "nb. of components" = count_components(comp1),
                     "largest component: nb. of nodes" = vcount(comp1), 
                     "largest component: share of nodes" = vcount(comp1) / vcount(comp1), 
                     "largest component: nb. of edges" = ecount(comp1), 
                     "largest component: share of edges" = ecount(comp1) / ecount(comp1),  
                     "largest component: diameter" = diameter,  
                     "largest component: radius" = radius,
                     "largest component: density" = dens, 
                     "largest component: transitivity" = trans) %>% 
  enframe(name = "Measures", value = "value")

write_xlsx(net_description, "output/store_danske_virksomheder_individer_net_description.xlsx")


###################################################################################################/
# 8.1 Centralitetsmål mv. ----
###################################################################################################/

deg                <-  degree(comp1)
betw               <-  betweenness(comp1, normalized = TRUE)
local_betw         <-  betweenness(comp1, cutoff = 2, normalized = TRUE)
close              <-  closeness(comp1, normalized = TRUE)
eig                <-  eigen_centrality(comp1, directed = FALSE)$vector
core               <-  coreness(comp1)
local_transitivity <- transitivity(comp1, type = "local")
constr             <- constraint(comp1)
brokerage          <- 1 / constr



# Lad os samle de nodespecifikke centralitetsmål for den største komponent
# Her kan I jo tilføje hvad I har brug for:
# 
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

##########/
# Ranking
##########/

cent_metrics <- cent_metrics %>% 
  mutate(degree_rank      = dense_rank(desc(degree)),                                        betw_rank        = dense_rank(desc(betweenness)),
         local_betw_rank  = dense_rank(desc(local_betweenness)),
         closeness_rank   = dense_rank(desc(closeness)),
         brokerage_rank   = dense_rank(desc(brokerage)),
         eigen_rank       = dense_rank(desc(eigen)),
         local_trans_rank = dense_rank(local_trans))

##################/
# Et sammensat rank
##################/
cent_metrics <- cent_metrics %>% mutate(composit_rank = dense_rank(degree_rank + betw_rank + local_betw_rank + closeness_rank + brokerage_rank + eigen_rank))

write_xlsx(cent_metrics, "output/store_danske_virksomheder_individer_centralitymetrics.xlsx")


###################################################################################################/
# 8.2 Tilføj centralitetsmål som attributes ----
###################################################################################################/
comp1 <- add_vertex_attr(graph= comp1, 
                         data = cent_metrics, 
                         match_var = "name")




###############/
# Kliker
###############/
# Kliker er subsets af noder, hvis interne densitet er 1. Dvs et subset af noder hvor alle er forbundne til hinanden. En node kan godt indgå i flere kliker


# Hvor mange kliker er der?

# Hvor mange maximale kliker er der


###################################################################################################/
# 10 Community strukturer i netværket ----
# Kan vi finde andre underindelinger af netværket baseret på netværksstrukturen?
###################################################################################################/
# Findes der et mål for om én inddeling af et netværk i grupper er bedre end en anden inddeling?

# Et bud som anvendes i mange community detection algoritmer er at kigge på forholdet mellem edges (ties) internt i de definerede grupper og edges mellem/på tværs af disse grupper. Det kaldes modularitet (eller modularity). 
# 
# Det udregnes ved at sammenholde det faktisk forhold mellem edges *internt* (within) i klynger (modules) og edges *mellem* (between) klynger (modules) med det samme forhold i et random netværk med samme samme antal noder og edges. Modulariteten er således den faktisk andel af within_group_edges minus andelen af within_group_edges i et ækvivalent men tilfældigt netværk.  
# 
# 
# Hvis vi gerne vil finde en klyngestruktur i et netværk kan vi forsøge at inddele netværket i grupper på en måde der *optimerer modulariteten*.
#
# Louvain clustering er en blandt flere algoritmer, der arbejder ud fra den logik. 
# 
# Forsimplet starter alle noder med at være deres egen gruppe og lægges derefter sammen så modulariteten hele tiden bliver stærkere.
# 
# Bemærk at algoritmen IKKE er deterministisk. Dvs. der kan være situationer, hvor den finder (marginalt) forskellige løsninger.  



# Prøv evt. forskellige alogoritmer:


# tjek antallle af clusters den/de laver


# Prøv at lave en data-frame med navne og clusters

