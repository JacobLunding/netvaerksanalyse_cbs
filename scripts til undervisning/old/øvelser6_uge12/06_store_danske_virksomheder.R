library(tidyverse)
library(readxl)
library(writexl)
library(igraph)
library(ggraph)
library(ggpubr)
# Ny pakke at installere
install.packages("RColorBrewer")
library(RColorBrewer)
source("functions/networkfunctions.R")

###################################################################################################/
# 1. Læs datafil ----
# Husk at en xlsx fil fra orbis har et sheet1 med info. Dvs data ligger i sheet2
# Sæt evt. guess_max til en høj værdi, så R ikke gætter på hvilken type data der skal være i kolonnerne
###################################################################################################/
den <- read_csv("input/den17-no-nordic-letters.csv")
# kig på data, ser det rigtigt ud
den %>% head()


###################################################################################################/
# 2. Subset og omkod data m.m. ----
###################################################################################################/

den %>% count(gender, sort = T)
# Vi omkoder lige gender, så NA og Binominal bliver til Unknown
den <- den %>% 
  mutate(gender = case_when(
    is.na(gender) ~"Unkown",
    gender == "Binominal" ~"Unknown", 
    .default = gender))

# Vi vil gerne subsette til kun at se på virksomheder og erhvervsdrivende eller virksomhedsejede fonde
den1 <- den %>% filter(!type %in% c("Organisation","Netvaerk (VL-gruppe)") &
                         (sector == "Corporations" | (sector == "Foundations" & grepl("Corporation", tags)))
                       )

# Vi vil gerne lave en variabel, der tæller antal poster per individ
den1 <- den1 %>% mutate(n_affil = n_distinct(affiliation), .by = name)

# Den variabel kan vi bruge til kun at kigge på individer, bygger bro mellem to forskellige bestyrelser (altså har mere end en bestyrelsespost)
den1 <- den1 %>% filter(n_affil > 1)


# Vi vil også gerne lave en variable der beskriver individernes roller i de forskellige virksomheder:
den1 <- den1 %>% 
  mutate(role_affil = paste0(role, " i ", affiliation)) %>% 
  mutate(role_affil = paste0(unique(role_affil), collapse = " ### "), .by = name)


###################################################################################################/
# 3. Definerer et netværksobjekt for virksomheder ----
###################################################################################################/

# lav en sparse incidence matrice name x affiliation: 
# (..., formula = ~ name + affiliation) giver en incidence matrice med name (individer) i rækker og affiliation (virksomheder) i kolonner  
incidence <- xtabs(data = den1, formula = ~name + affiliation, sparse = T)

# hvis vi vil være sikre på at folke ikke har mere end 1 post i den samme bestyrelse kan vi tvinge vores incidence matrice til at være binær (0/1) ved at sætte alle værdier over 1 til 1
incidence[incidence > 1] <- 1

# lav individ x individ adjacency matricen: 
# brug matrix multiplikation:  incidence %*% Matrix::t(incidence) eller
# eller brug funktionen: incidence_to_adjacency( ) fra networkfunctions.R, 
      #hvor (..., mode = "row") betyder at den laver adjacency matricen for de enheder der er i incidencematricens rækker (her names) 
adj_ind  <- incidence_to_adjacency(incidence = incidence, mode = c("row"), weigthed = FALSE)

# lav netværks objektet
net_ind <- graph_from_adjacency_matrix(adjmatrix = adj_ind, mode = "undirected") %>% simplify()


###################################################################################################/
# 4. Netværkets komponenter? ----
###################################################################################################/
comp.list <- components(net_ind)

comp.list$no
table(comp.list$csize)

comp1 <- largest_component(net_ind)

###################################################################################################/
# 5. Tilføj netværkseksterne node attributes til netværket ----
###################################################################################################/

comp1 <- add_vertrex_attr(graph = comp1, 
                          data = den1 %>% ungroup() %>% 
                            select(name, role_affil, gender),
                          match_var = "name")

###################################################################################################/
# 6. Komponent visualisering ----
###################################################################################################/

p <- net_ind %>% ggraph("fr") +
  geom_edge_link0(color = "grey35", width = 0.3, alpha = 0.3) +
  geom_node_point(size = .5) + ggtitle(paste0("Store danske virksomheder og fonde (n=", vcount(net_ind), ")")) +
  theme_graph(base_family = "serif")
p1 <- comp1 %>% ggraph("fr") +
  geom_edge_link0(color = "grey35", width = 0.3, alpha = 0.3) +
  geom_node_point(size = 1.2) + ggtitle(paste0("Største komponent (n=", vcount(comp1), ")")) +
  theme_graph(base_family = "serif")

ggarrange(plotlist = list(p, p1))

###################################################################################################/
# 7. Netværks mål ----
###################################################################################################/

dens     <- edge_density(comp1)
trans    <- transitivity(comp1)
radius   <- radius(comp1)
diameter <- diameter(comp1)

net_description <- tibble(Measure = c("nb. of nodes",
                                      "nb. of edges",
                                      "nb. of components",
                                      "largest component: nb. of nodes", 
                                      "largest component: nb. of edges", 
                                      "largest component: diameter",  
                                      "largest component: radius",
                                      "largest component: density", 
                                      "largest component: transitivity"),
                          Value = c(vcount(net_ind), 
                                    ecount(net_ind), 
                                    comp.list$no, 
                                    vcount(comp1),
                                    ecount(comp1),
                                    diameter, 
                                    radius,
                                    dens, 
                                    trans)
)

write_xlsx(net_description, "output/store_danske_virksomheder_individer_net_description.xlsx")


###################################################################################################/
# 8.1 Centralitetsmål mv. ----
###################################################################################################/

deg     <-  degree(comp1)
betw    <-  betweenness(comp1, normalized = TRUE)
local_betw    <-  betweenness(comp1, cutoff = 2, normalized = TRUE)
close   <-  closeness(comp1, normalized = TRUE)
eig     <-  eigen_centrality(comp1, directed = FALSE)$vector
core    <-  coreness(comp1)
local_transitivity <- transitivity(comp1, type = "local")
constr    <- constraint(comp1)
brokerage <- 1 / constr



# Lad os samle de nodespecifikke centralitetsmål for den største komponent
cent_metrics <- tibble(
  name        = V(comp1)$name,
  desciption  = V(comp1)$role_affil,
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
                                        local_trans_rank = dense_rank(local_trans))

cent_metrics <- cent_metrics %>% mutate(composit_rank = dense_rank(degree_rank + betw_rank + local_betw_rank + closeness_rank + brokerage_rank + eigen_rank))

write_xlsx(cent_metrics, "output/store_danske_virksomheder_individer_centralitymetrics.xlsx")


###################################################################################################/
# 8.2 Tilføj centralitetsmål som attributes ----
###################################################################################################/
comp1 <- add_vertrex_attr(graph= comp1, 
                          data = cent_metrics, 
                          match_var = "name")


###################################################################################################/
# 9. Visualiseringer af netværk ----
###################################################################################################/

# lad os lege lidt med visualisering af betweenness centraliteten
comp1 %>% ggraph('stress') +
  geom_edge_link0() + 
  # color betweenness, size degree
  geom_node_point() 





# Det kan være et æstetisk problem!! at noderne bliver plottet i alfabetisk rækkefølge. Hvad nu, hvis vi gerne vil have at de noder vi gerne vil fremhæve, skal ligge øverst.
# Det er der desværre ikke nogen helt nem metode til, men en lille funktion kan hjælpe.

## Først laver vi et fast layout, som vi kan genbruge:
stable_lay <- create_layout(comp1, layout = 'stress')


# Betweenness
     
p1 <- stable_lay %>% arrange(-betw_rank) %>% # minus foran i arrange, betyder at den plotter i omvendt rækkefølge, dvs. højeste værdier først
  ggraph() +
  geom_edge_link0(data = get_edge_coord(comp1, stable_lay), aes(x= x, y=y, xend=xend, yend = yend), width =.3, alpha = 0.3) +
  geom_node_point(aes(fill = betweenness, size = degree), color = "black",shape = 21) + 
  scale_size_continuous(range = c(2,10)) + 
  geom_node_label(aes(filter=betw_rank<=10, 
                      label = paste0(betw_rank, ": ", name)), 
                  size = 3, repel = T, force = 25) +
  guides(label = "none", size = "none", color = "none") +
  theme_graph()
p1


###################################################################################################/
# 10 Community strukturer i netværket ----
# Kan vi finde underindelinger af netværket baseret på netværksstrukturen?
###################################################################################################/
# Findes der et mål for om én inddeling af et netværk i grupper er bedre end en anden inddeling?
# Et bud som anvendes i mange community detection algoritmer er at kigge på forholdet mellem edges (ties) internt i de definerede grupper og edges mellem/på tværs af disse grupper. Det kaldes modularitet (eller modularity). Det udregnes ved at sammenholde det faktisk forhold mellem edges *internt* (within) i klynger (modules) og edges *mellem* (between) klynger (modules) med det samme forhold i et random netværk med samme samme antal noder og edges. Modulariteten er således den faktisk andel af within_group_edges minus andelen af within_group_edges i et ækvivalent men tilfældigt netværk.  
# Hvis vi gerne vil finde en klyngestruktur i et netværk kan vi forsøge at inddele netværket i grupper på en måde der *optimerer modulariteten*.

# Louvain clustering er en blandt flere algoritmer, der arbejder ud fra den logik. Forsimplet starter alle noder med at være deres egen gruppe og lægges derefter sammen så modulariteten hele tiden bliver stærkere.

louvain <- cluster_louvain(comp1)
names(louvain)

n_distinct(louvain$membership) # antallet af klynger
louvain$membership # klyngenummer for hver virksomhed i netværket
table(louvain$membership) # antallet af virksomheder i hver klynge
  
louvain$modularity # Algoritmens er iterativ og her kan vi se modulariteten i de forskellige steps


# Har vi et kluster af kvinder?
cl_gender <- tibble(clusters = louvain$membership, gender = V(comp1)$gender)

cl_gender %>% group_by(clusters) %>% count(gender) %>% mutate(pct = n/ sum(n)) %>% filter(gender == "Women") %>% arrange(-pct)

assortativity_nominal(comp1, types = factor(V(comp1)$gender), directed = F)


###################################################################################################/
# 11. Visualisering af community strukturen ----
# 
###################################################################################################/
# Vi tilføjer klyngemedlemskabet som en node attribute, vi vil gerne have pæne labels så vi tilføjer et nul foran et cifrede tal (det gør vi fordi tal, når de læses som text sorteres anderledes)
tal1 <- c(1:20) %>% as.character()
tal2 <- c(1:20) %>% sprintf("%02d", .)
sort(tal1)
sort(tal2)

V(comp1)$louvain <- sprintf("%02d", louvain$membership)

# Og laver en edge attribute, så vi kan farve klynge-interne edges også
comp1 <- edge_attr_from_vertex_attr(comp1, vertex_attr = "louvain")

# Nu kan vi lave et flot plot
comp1 %>% 
  ggraph(layout = "stress") +
  geom_edge_link0(aes(filter=louvain!= "9999", color = louvain), width = 0.65, alpha = 0.6) +
  geom_edge_link0(aes(filter=louvain == "9999"), color = "black", width = 0.65, alpha = 0.4) + 
  geom_node_point(aes(color=louvain), alpha=0.95, size=3) + 
  theme_graph() 

# Lad os kigge lidt på nogle enkelte klynger
focus <- "06"

comp1 %>% 
  ggraph(layout = "stress") +
  #Edges
  geom_edge_link0(aes(filter=louvain=='9999'), color='black', width=0.3, alpha=0.4) + 
  geom_edge_link0(aes(filter=louvain!=focus & louvain != "9999", color=louvain), width=0.3, alpha=0.4) + 
  geom_edge_link0(aes(filter=louvain==focus, color=louvain), width=0.6, alpha=0.95) + 
  #Nodes
  geom_node_point(aes(filter=louvain!= focus, color=louvain), alpha=0.15, size=3) + 
  geom_node_point(aes(filter=louvain == focus, color = louvain), shape = 21, fill = "black", stroke = 2, alpha = 0.8, size=1.5) + 
  #Labels
  geom_node_label(aes(filter=louvain == focus, label=name, color=louvain), alpha = 0.7, size = 2.5, repel=TRUE, force = 10, show.legend = F) + guides(fill = "none") +
  theme_graph() 


# Farvelægning: 
# Option 1: Hvis det er kontinuert variabel man vil farvelægge efter kan viridis functionen være god: scale_color_viridis() eller scale_edge_color_viridis() 
# Option 2: Hvis det er en variabel med kategoier, kan man vælge farveskalaer fra RColorBrewer palettes

# RcolorBrewer 

# Se farverne
display.brewer.all()

# Vælg en farveskala og tilpas den til det antal kategorier, der skal repræsenteres
amount   <- V(comp1)$louvain %>% n_distinct()
mycolors <- colorRampPalette(brewer.pal(10, "Spectral"))(amount)

rows <- sample(length(mycolors))
mycolors <- mycolors[rows]

comp1 %>% 
  ggraph(layout = "stress") +
  geom_edge_link0(aes(filter=louvain!= "9999", color = louvain), width = 0.8, alpha = 0.6) +
  geom_edge_link0(aes(filter=louvain == "9999"), color = "black", width = 0.3, alpha = 0.2) + 
  geom_node_point(aes(color=louvain), alpha=0.95, size=3) + 
  theme_graph(base_family = 'Serif') + 
  scale_edge_color_manual(values = mycolors) +
  scale_color_manual(values = mycolors) 


# Ekstra: Andre community detection algoritmer ----
# Der findes andre community detection algoritmer end Louvain som er baseret på samme princip - optimering af modularity. 
# De indeholder de samme elementer: membership og modularity, så de kan indsættes i koden ovenfor: 

# cluster_edge_betweenness(comp1)
# cluster_fast_greedy(comp1)
# cluster_label_prop(comp1)
# cluster_leading_eigen(comp1)
# cluster_walktrap(comp1)
# cluster_spinglass(comp1)
# cluster_infomap(comp1)
# multilevel.community(comp1)

#  I kan læse lidt her om forholdet mellem forskellige algoritmer
# https://stackoverflow.com/questions/9471906/what-are-the-differences-between-community-detection-algorithms-in-igraph


################################/
# Walktrap 
###############################/

walktrap <- cluster_walktrap(comp1)
names(walktrap)
table(walktrap$membership)
V(comp1)$walktrap <- sprintf("%02d", walktrap$membership)

# Og laver en edge attribute, så vi kan farve klynge-interne edges også
comp1 <- edge_attr_from_vertex_attr(comp1, vertex_attr = "walktrap")


amount   <- V(comp1)$walktrap %>% n_distinct()
mycolors <- colorRampPalette(brewer.pal(10, "Spectral"))(amount)

rows <- sample(length(mycolors))
mycolors <- mycolors[rows]

# Nu kan vi lave et flot plot
comp1 %>% 
  ggraph(layout = "stress") +
  geom_edge_link0(aes(filter=walktrap!= "9999", color = walktrap), width = 0.65, alpha = 0.6) +
  geom_edge_link0(aes(filter=walktrap == "9999"), color = "black", width = 0.65, alpha = 0.4) + 
  geom_node_point(aes(color=walktrap), alpha=0.95, size=3) + 
  scale_edge_color_manual(values = mycolors) +
  scale_color_manual(values = mycolors) +
  theme_graph() 

V(comp1)$walktrap %>% table() %>% sort(decreasing = T)
# Lad os kigge lidt på nogle enkelte klynger
focus <- c("05", "08", "11")

comp1 %>% 
  ggraph(layout = "stress") +
  #Edges
  geom_edge_link0(aes(filter=!walktrap %in% focus), width=0.3, alpha=0.4, color = "black") + 
  geom_edge_link0(aes(filter=walktrap %in% focus, color=walktrap), width=0.3, alpha=0.95) + 
  #Nodes
  geom_node_point(aes(filter=!walktrap %in% focus), color = "black", alpha=0.15, size=1.5) + 
  geom_node_point(aes(filter=walktrap %in% focus, color = walktrap), alpha = 0.8, size=3) + 
  #Labels
  #geom_node_label(aes(filter=louvain == focus, label=name, color=louvain), alpha = 0.7, size = 2.5, repel=TRUE, force = 10, show.legend = F) + guides(fill = "none") +
  theme_graph() 




################################/
# Spinglass 
###############################/

spinglass <- cluster_spinglass(comp1)
names(spinglass)
table(spinglass$membership)
V(comp1)$spinglass <- sprintf("%02d", spinglass$membership)

# laver en edge attribute, så vi kan farve klynge-interne edges også
comp1 <- edge_attr_from_vertex_attr(comp1, vertex_attr = "spinglass")


amount   <- V(comp1)$spinglass %>% n_distinct()
mycolors <- colorRampPalette(brewer.pal(10, "Spectral"))(amount)

rows <- sample(length(mycolors))
mycolors <- mycolors[rows]

# Nu kan vi lave et flot plot
comp1 %>% 
  ggraph(layout = "stress") +
  geom_edge_link0(aes(filter=spinglass!= "9999", color = spinglass), width = 0.65, alpha = 0.6) +
  geom_edge_link0(aes(filter=spinglass == "9999"), color = "black", width = 0.65, alpha = 0.4) + 
  geom_node_point(aes(color=spinglass), alpha=0.95, size=3) + 
  scale_edge_color_manual(values = mycolors) +
  scale_color_manual(values = mycolors) +
  theme_graph() 

V(comp1)$spinglass %>% table() %>% sort(decreasing = T)

# Lad os kigge lidt på nogle enkelte klynger
focus <- c("22", "13", "08", "07")

comp1 %>% 
  ggraph(layout = "stress") +
  #Edges
  geom_edge_link0(aes(filter=!spinglass %in% focus), width=0.3, alpha=0.4, color = "black") + 
  geom_edge_link0(aes(filter=spinglass %in% focus, color=spinglass), width=0.3, alpha=0.95) + 
  #Nodes
  geom_node_point(aes(filter=!spinglass %in% focus), color = "black", alpha=0.15, size=1.5) + 
  geom_node_point(aes(filter=spinglass %in% focus, color = spinglass), alpha = 0.8, size=3) + 
  #Labels
  #geom_node_label(aes(filter=louvain == focus, label=name, color=louvain), alpha = 0.7, size = 2.5, repel=TRUE, force = 10, show.legend = F) + guides(fill = "none") +
  theme_graph() 




################################/
# fast_greedy 
###############################/

fast_greedy <- cluster_fast_greedy(comp1)

names(fast_greedy)
table(fast_greedy$membership)
V(comp1)$fast_greedy <- sprintf("%02d", fast_greedy$membership)

# laver en edge attribute, så vi kan farve klynge-interne edges også
comp1 <- edge_attr_from_vertex_attr(comp1, vertex_attr = "fast_greedy")


amount   <- V(comp1)$fast_greedy %>% n_distinct()
mycolors <- colorRampPalette(brewer.pal(10, "Spectral"))(amount)

rows <- sample(length(mycolors))
mycolors <- mycolors[rows]

# Nu kan vi lave et flot plot
comp1 %>% 
  ggraph(layout = "stress") +
  geom_edge_link0(aes(filter=fast_greedy!= "9999", color = fast_greedy), width = 0.65, alpha = 0.6) +
  geom_edge_link0(aes(filter=fast_greedy == "9999"), color = "black", width = 0.65, alpha = 0.4) + 
  geom_node_point(aes(color=fast_greedy), alpha=0.95, size=3) + 
  scale_edge_color_manual(values = mycolors) +
  scale_color_manual(values = mycolors) +
  theme_graph() 

V(comp1)$fast_greedy %>% table() %>% sort(decreasing = T)

# Lad os kigge lidt på nogle enkelte klynger
focus <- c("12", "03", "04", "01")

comp1 %>% 
  ggraph(layout = "stress") +
  #Edges
  geom_edge_link0(aes(filter=!fast_greedy %in% focus), width=0.3, alpha=0.4, color = "black") + 
  geom_edge_link0(aes(filter=fast_greedy %in% focus, color=fast_greedy), width=0.3, alpha=0.95) + 
  #Nodes
  geom_node_point(aes(filter=!fast_greedy %in% focus), color = "black", alpha=0.15, size=1.5) + 
  geom_node_point(aes(filter=fast_greedy %in% focus, color = fast_greedy), alpha = 0.8, size=3) + 
  #Labels
  #geom_node_label(aes(filter=louvain == focus, label=name, color=louvain), alpha = 0.7, size = 2.5, repel=TRUE, force = 10, show.legend = F) + guides(fill = "none") +
  theme_graph() 






all_layouts <- c(
  # fra igraph
  'nicely', # vælger automatisk et passende layout
  'stress', # default i ggrapj
  'fr', # fruchterman reingold
  'dh', 
  'drl',
  'gem',
  'graphopt',
  'kk',
  'lgl',
  'mds',
  'sugiyama',
  'bipartite',
  'star',
  'tree')

###################################################################################################



