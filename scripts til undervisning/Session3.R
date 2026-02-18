###############################################################/
#
#  Øvelse 3: Sammenhængskraft; densitet, kliker og strukturelle huller
#
###############################################################/

# indlæs pakker
library(tidyverse)
library(igraph)
library(ggraph)
library(graphlayouts)
library(Matrix)
#install.packages("patchwork")
library(patchwork)
#install.packages("treesj")

source("functions/clique_plot.R")
source("functions/triangle_plot.R")
###############################################################/
# 1. Indlæs og udvalg/behandling af data ----
###############################################################/

den <- read_csv("data/danish_elitenetworks2024.csv")

# Lad os først vælge et subset af datasættet. 
# Til det har vi forskellige mulgiheder med hhv affiliation_branche_niveau1 til den$affiliation_branche_niveau5 og affiliation_tags
den %>% distinct(affiliation, affiliation_branche_niveau1) %>% count(affiliation_branche_niveau1) %>% View()

# lad os kigge "Pengeinstitut- og finansvirksomhed, forsikring"

#Kigger vi på `affiliation_tags` som vi er nød til først at splitte `str_split()` og unliste `unlist()` fordi der kan være flere tags per virksomhed. De er separeret med "; ", så det splitter vi på: 
  
den %>% pull(affiliation_tags) %>% str_split("; ") %>% unlist() %>% table()


den <- den %>% mutate(finans = affiliation_branche_niveau1 == "Pengeinstitut- og finansvirksomhed, forsikring" | 
                        grepl("Erhvervsliv_Finans|Erhvervsliv_Finans_Banker|Erhvervsliv_Finans_Forsikring|Erhvervsliv_Finans_Investering|Erhvervsliv_Finans_Pension", affiliation_tags))
den %>% count(finans)

den_finans <- den %>% filter(finans == TRUE)


den_finans <- den_finans %>% 
  # grupperer data efter 'name'
  group_by(person_name) %>% 
  # laver en ny variabel 'n_memberships' som for hvert individ (vi har jo grupperet data på individer) tæller antallet af unikke boards med n_distinct()
  mutate(n_memberships = n_distinct(affiliation)) 
# til sidst kan vi nu filtrere data på et logisk statement, så vi kun får rækker med individer der har mere end 1 medlemskab.


den_finans <- den_finans %>% filter(n_memberships > 1)



###############################################################/
# 2. Konstruktion af grafobjekt / netværksdata ----
###############################################################/


bi_adj <- xtabs(formula = ~ person_name + affiliation, data = den_finans, sparse = TRUE) #Sparse = TRUE betyder at vi beder funktionen xtabs om at gemme den nye matrice i et hukommelsesbesparende format, hvor den ikke gemmer alle 0'erne (dvs. de ikke-optrædende forbindelser)
adj_c <- bi_adj %*%  t(bi_adj)

# Her betragter vi ikke netværket som vægtet!!
gr    <- adj_c %>% graph_from_adjacency_matrix(mode = "undirected", weighted = NULL, diag = FALSE) %>% simplify()
gr    <- gr %>% as_tbl_graph()

################################################################################################/
# 3. Sammenhængskraft ----
################################################################################################/

##################################################################/
# Densitet ----
# Densiteten i et netværk udtrykker sandsynligheden for at to tilfældige noder i netværker er forbundne. 
# Den udregnes ved at dividere det faktiske antal forbindelser (edges) med den maksimale mulige antal forbindelser (edges).
##################################################################/

# eksempler: forskellige netværk med 40 noder  ### maksimale antal edges = (N_noder * N_noder - 1) / 2
(40 * (40-1)) / 2

# Lad os lige se hvad antallet af noder betyder for det teoretiske max !!
teo_max <- tibble(nodes = c(2:10000)) %>% mutate(teo_max = (nodes * (nodes -1))/2)

ggplot() + geom_line(data = teo_max, aes(x = nodes, y = teo_max))

e1 <- make_full_graph(40, directed = FALSE)
e1 <- e1 %>% as_tbl_graph()

e1 %>% ggraph() +
  geom_edge_link0(edge_width = 0.1, alpha = 0.4) +
  geom_node_point(color = "steelblue1", size = 4) +
  theme_graph()
edge_density(e1, loops=FALSE)

e2 <- make_star(40, mode = "undirected")
e2 <- e2 %>% as_tbl_graph()

e2 %>% ggraph() +
  geom_edge_link0(edge_width = 0.1, alpha = 0.4) +
  geom_node_point(color = "steelblue1", size = 4) +
  theme_graph()
edge_density(e2, loops=FALSE)

ecount(e2) / ((40 * (40-1))/2)


# Lad os nu bergne densiteten af vores virksomhedsnetværk
edges     <- ecount(gr) 
nodes     <- vcount(gr)
edges_max <- (nodes * (nodes-1)) / 2
edges / edges_max

edge_density(gr, loops = FALSE)

##################################################################/ 
# Komponenter ---------------------------------------------------
# en komponent er en sammenhængende undergraf
##################################################################/

# Hvis vi plotter virksomhedsnetværket gr kan vi se komponentstrukturen
gr %>% 
  ggraph() +
  geom_edge_link0(edge_width = .2, edge_alpha = .3) +
  geom_node_point(size=1.5) +
  theme_graph()

# Vi kan lave en variabel på node-delen af vores graf-data:
gr <- gr %>% 
  activate(nodes) %>% 
  mutate(comp = group_components())

# Hvis vi vil vide noget om komponentstrukturen kan vi lave et 'exportere' node-dataen og kigge på det:
gr %>% activate(nodes) %>% as_tibble() %>% count(comp)
# der er 15 komponenter. Den største har 40 'medlemmer'. den næst-største har 8 osv.

# vi kan nu regne densiteten i komponenterne: 
gr %>% filter(comp == 1) %>% edge_density()

# Visualisering af den største komponenter
gr %>% filter(comp==1) %>% 
  ggraph(layout = "fr") +
  geom_edge_link0(color = "gray70", width = 0.1) +
  geom_node_point(size=1.5) +
  theme_graph()

####################################################################################################################/
# Connectedness
# Hvor mange 'par af noder' kan nå hinanden (dvs. er i samme komponent) i forhold til det teoretisk mulige antal par
####################################################################################################################/
sp <- distances(gr)
sp
sp[is.infinite(sp)] <- 0
sp[sp > 0] <- 1
(sum(rowSums(sp, na.rm = T)) / ((vcount(gr)*(vcount(gr)-1))))


##################################################################/ 
# Transitivitet ----
# et mål for antallet af faktiske triader ud af det mulige antal triader
# måler graden af lokal forbundethed:
# Når A kender B og B kender C, hvor hyppigt er det så at A også kender C
# Husk strong ties og triadic closure
##################################################################/ 

# eksempler
# En ring graf, med ingen triader
g1 <- make_ring(10)
autograph(g1) + theme_graph()
transitivity(g1) # 0 - no triads

# I et tilfældigt netværk med x noder og en given densitet
g2 <- sample_gnp(30, p = 2/30)  # p er sandsynligheden for at der er en forbindelse mellem to noder : altså densiteten
autograph(g2) + 
  geom_node_point(aes(filter = {count_triangles(g2) > 0}, color = {count_triangles(g2) >0})) +
  theme_graph() + guides(color = "none")
transitivity(g2) 

# Transitiviteten i virksomhedsnetværket
tr_g <- transitivity(gr, type = "global") 
tr_l <- transitivity(gr, type = "local") 

tr_l %>% enframe %>% tibble() %>% View()


gr %>% filter(comp==1) %>% 
  ggraph(layout = "fr") +
  geom_edge_link0(color = "gray70", width = 0.1) +
  geom_node_point(size=1.5) +
  geom_node_point(aes(filter = centrality_degree() != 1 & local_transitivity() < .33), color = "red")+
  geom_node_label(aes(filter = centrality_degree() != 1 & local_transitivity() < .33, label = local_transitivity() %>% round(2)), color = "red", repel = TRUE)+
  theme_graph()


# Visualisering af open og closed triads
tri_plot(gr, mode = "closed")
tri_plot(gr, mode = "open")


# Kliker
max_cliques(gr) 
max_cliques(gr) %>% sapply(., length) %>% table()
p_cli <- clique_plot(gr, n =4, mode = "both")
p_cli$vertices + p_cli$edges


##################################################################/ 
# Diameter ----
# En netværksgrafs diameter er den længste 'korteste sti' mellem to noder i netværket. Altså den korteste vej mellem netværkets yderpunkter, kan man sige. Giver kun mening for sammenhængende grafer, da den korteste vej mellem to ikke-forbunde noder er uendelig stor.
##################################################################/ 
comp1 <- gr %>% filter(comp == 1)


# diameter på den største komponent i vores virksomhedsnetværk
diameter(comp1, directed = FALSE)

# hvilke to virksomheder ligger længst fra hinanden
farthest_vertices(comp1, directed = FALSE)

# hvad er vejen mellem dem
diam <- get_diameter(comp1, directed = FALSE)
diam <- names(diam)

# Vi kan visualisere den længste sti:

# 1) vi gemmer en attribut til noderne, der fortæller (TRUE/FALSE) om de ligger på stien.
comp1 <- comp1 %>% activate(nodes) %>% 
  mutate(diameter = name %in% diam) 


# 2) vi gemmer en attribut til egdes, der fortæller (TRUE/FALSE) om de indgår i stien
comp1 <- comp1 %>% activate(edges) %>% 
  mutate(diameter = FALSE) %>% 
  morph(to_shortest_path, from = .N()$name == first(diam), to = .N()$name ==last(diam)) %>%
  mutate(diameter = TRUE) %>% unmorph()

# 3) plot 
comp1 %>% 
  ggraph(layout = "fr") +
  geom_edge_link0(aes(filter=diameter==FALSE), color = "gray60") + 
  geom_node_point(aes(filter=diameter==FALSE), color = "black") +
  geom_edge_link0(aes(filter=diameter==TRUE), color = "red", width = 1.5) +
  geom_node_point(aes(filter=diameter==TRUE), color = "red", size =2) +
  geom_node_label(aes(filter=diameter==TRUE, label = name), nudge_y = -0.3, size =2.5, repel = TRUE) + 
  labs(title = 'Diameter in EliteDBs finance component') +
  theme_graph() 


############################################################/
# Ekstra ----
# Den korteste vej mellem to specifikke noder
############################################################/

node1 <- "Louise Caroline Mogensen 52924" # direktør i finanstilsynet
node2 <- "Carsten Egeriis 66665"          # adm. direktør i danske bank

# Vi laver en vertex attribute der er TRUE for alle noder på stien og for alle edges

comp1 <- comp1 %>% activate(nodes) %>% 
  mutate(sh_path = FALSE) %>% 
  morph(to_shortest_path, from = .N()$name == vertex1, to = .N()$name ==vertex2) %>%
  mutate(sh_path = TRUE) %>% unmorph()

comp1 <- comp1 %>% activate(edges) %>% 
  mutate(sh_path = FALSE) %>% 
  morph(to_shortest_path, from = .N()$name == vertex1, to = .N()$name ==vertex2) %>%
  mutate(sh_path = TRUE) %>% unmorph()


comp1 %>% 
  ggraph(layout='fr') + 
  geom_edge_link0(aes(filter=sh_path==FALSE), color='grey50', alpha=0.5) + 
  geom_node_point(aes(filter=sh_path==FALSE), color='black', size=3, alpha=0.25) + 
  geom_edge_link0(aes(filter=sh_path==TRUE), color='red', width=1.2) + 
  geom_node_point(aes(filter=sh_path==TRUE), color='darkred', size=5, alpha=0.5) + 
  geom_node_label(aes(filter=sh_path==TRUE, label=name), color='red', size=2, alpha = 0.8, repel = T) + 
  theme_graph()

