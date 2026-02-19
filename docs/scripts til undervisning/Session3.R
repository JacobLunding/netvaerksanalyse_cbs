###############################################################/
#
#  Øvelse 3: Sammenhængskraft; densitet, kliker og strukturelle huller
#
###############################################################/

# indlæs pakker
library(tidyverse)
library(igraph)
library(tidygraph)
library(ggraph)
library(Matrix)

# Her downloades de opdaterede udgaver af data og funktioner:
download.file("https://jacoblunding.github.io/netvaerksanalyse_cbs/functions/clique_plot.R", "functions/clique_plot.R")
download.file("https://jacoblunding.github.io/netvaerksanalyse_cbs/functions/triangle_plot.R", "functions/triangle_plot.R")
download.file("https://jacoblunding.github.io/netvaerksanalyse_cbs/data/danish_elitenetworks2024.csv", "data/danish_elitenetworks2024.csv")


source("functions/clique_plot.R")
source("functions/triangle_plot.R")
###############################################################/
# 1. Indlæs og udvalg/behandling af data ----
###############################################################/

den <- read_csv("data/danish_elitenetworks2024.csv")

###################################################/
# Lad os først vælge et subset af datasættet. 
# Til det har vi forskellige mulgiheder med hhv affiliation_branche_niveau1 til den$affiliation_branche_niveau5 og affiliation_tags
###################################################/
den %>% distinct(affiliation, .keep_all = T) %>% count(affiliation_branche_niveau1, 
                                                       affiliation_branche_niveau2, 
                                                       affiliation_branche_niveau3, 
                                                       affiliation_branche_niveau4,
                                                       affiliation_branche_niveau5) %>% View()

# lad os kigge det, der på niveau5 hedder: "Banker, sparekasser og andelskasser"

#Kigger vi på `affiliation_tags` som vi er nød til først at splitte `str_split()` og unliste `unlist()` fordi der kan være flere tags per virksomhed. De er separeret med "; ", så det splitter vi på: 
  
den %>% pull(affiliation_tags) %>% str_split("; ") %>% unlist() %>% table()


den <- den %>% mutate(er_bank = affiliation_branche_niveau5 == "Banker, sparekasser og andelskasser" | 
                        grepl("Erhvervsliv_Finans_Banker", affiliation_tags, ignore.case = T))
den %>% count(er_bank)

# Her subsettes den, så vi kun har de poster der er i en bank virksomhed/organisation....
# subset på bankniveau:
den_bank <- den %>% filter(er_bank == TRUE)

# en anden mulighed ville være at tage alle organisationer med hvor en person fra en bank sidder. 
bank_persons <- den_bank %>% pull(person_name) %>% unique()
# subset på personniveau: ved at vende tilbage til det fulde data og i stedet trække alle de personerner, der var i bankdatasættet. Så får vi alle deres andre poster med også
den_bank <- den %>% filter(person_name %in% bank_persons)


# Et næste skridt kan være at lave en variabel der tæller medlemskaber for hver person:

den_bank <- den_bank %>% 
  # grupperer data efter 'name'
  group_by(person_name) %>% 
  # laver en ny variabel 'n_memberships' som for hvert individ (vi har jo grupperet data på individer) tæller antallet af unikke boards med n_distinct()
  mutate(n_memberships = n_distinct(affiliation))  %>% ungroup()
# til sidst kan vi nu filtrere data på et logisk statement, så vi kun får rækker med individer der har mere end 1 medlemskab.


den_bank <- den_bank %>% filter(n_memberships > 1)

den_bank %>% count(affiliation, er_bank) %>% View()

###############################################################/
# 2. Konstruktion af grafobjekt / netværksdata ----
###############################################################/


bi_adj <- xtabs(formula = ~ person_name + affiliation, data = den_bank, sparse = TRUE) #Sparse = TRUE betyder at vi beder funktionen xtabs om at gemme den nye matrice i et hukommelsesbesparende format, hvor den ikke gemmer alle 0'erne (dvs. de ikke-optrædende forbindelser)

# Her kigger vi på individ netværket...:
adj_i <- bi_adj %*%  t(bi_adj)

# Her betragter vi ikke netværket som vægtet!! Vi ønsker ikke self-ties, derfor er diag = FALSE.
# og fordi vi ikke er interesseret i vægte bruger vi simplify til at slette dobbelt
gr    <- adj_i %>% graph_from_adjacency_matrix(mode = "undirected", weighted =  "No", diag = FALSE)
gr    <- gr %>% as_tbl_graph()

gr %>% ggraph() +
  geom_edge_link0(width = 0.3, alpha = 0.3) +
  geom_node_point(alpha = 0.7) +
  theme_graph()
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

############################################################################/
# Lad os lige se hvad antallet af noder betyder for det teoretiske max !!
# ############################################################################/

# Først laver vi et data-objekt (en 'tibble' i tidy-sprog) en variabel 'nodes', der indeholder alle værdier fra 2 til 10000. Derefter beregner vi det teoretiske max for hver situation.
teo_max <- tibble(nodes = c(2:10000)) %>% mutate(teo_max = (nodes * (nodes -1))/2)

# Nedenstående plot viser hvordan det teoretiske max vokser exponentielt når antallet af noder stiger
ggplot() + 
  geom_line(data = teo_max, aes(x = nodes, y = teo_max)) + 
  theme_bw() + 
  scale_y_continuous(labels = scales::comma, name = "theoretical max") +
  scale_x_continuous(labels = scales::comma, name = "number of nodes")


# Lad os se hvordan et netværk med fuld densitet ser ud ved at bruge en funktion fra Igraph, der laver et 'fuldt' netværk med 40 noder, hvor alle noder er forbundne.
e1 <- make_full_graph(n = 40, directed = FALSE)
e1 <- e1 %>% as_tbl_graph()

e1 %>% ggraph() +
  geom_edge_link0(edge_width = 0.1, alpha = 0.2) +
  geom_node_point(color = "steelblue1", size = 4) +
  theme_graph()



# Når man har et graf- eller netværksobjekt kan densiteten beregnes med 'edge_density()' funktionen fra Igraph.
# det fulde netværk, e1, skulle værdien meget gerne være 1. (altså 100% fordi alle ud af de teoretisk mulige forbindelser findes)
edge_density(e1, loops=FALSE)

# Et andet 'ekstremt' eksempel er en stjerne-graf, hvor én node er forbundet til alle, mens de andre kun er forbundet til den (og altså ikke til hinanden)
e2 <- make_star(40, mode = "undirected")
e2 <- e2 %>% as_tbl_graph()

e2 %>% ggraph() +
  geom_edge_link0(edge_width = 0.1, alpha = 0.4) +
  geom_node_point(color = "steelblue1", size = 4) +
  theme_graph()

# Her er det kun 39 forbindelser (altså den ene nodes forbindelse til de andre) ud af de (40*(40-1))/2 mulige
edge_density(e2, loops=FALSE)


# Lad os nu bergne densiteten af vores banknetværk
edges     <- ecount(gr) 
nodes     <- vcount(gr)
edges_max <- (nodes * (nodes-1)) / 2
edges / edges_max

# som jo meget gerne skulle være det samme som med funktionen!!
edge_density(gr, loops = FALSE)

##################################################################/ 
# Komponenter ---------------------------------------------------
# en komponent er en sammenhængende undergraf
##################################################################/

# Hvis vi plotter banknetværket gr kan vi se komponentstrukturen
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
# der er 15 komponenter. Den største har 417 'medlemmer'. den næst-største har 8 osv.

# vi kan nu regne densiteten i komponenterne: 
edge_density(gr %>% filter(comp == 1))
edge_density(gr %>% filter(comp == 2))
edge_density(gr %>% filter(comp == 3))

# Visualisering af den største komponenter
gr %>% filter(comp==1) %>% 
  ggraph(layout = "fr") +
  geom_edge_link0(color = "gray70", width = 0.1) +
  geom_node_point(size=1.5) +
  theme_graph()

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
g2 <- sample_gnp(30, p = 10/100)  # p er sandsynligheden for at der er en forbindelse mellem to noder : altså densiteten
autograph(g2) + 
  geom_node_point(aes(filter = {count_triangles(g2) > 0}, color = {count_triangles(g2) >0})) +
  theme_graph() + guides(color = "none")
transitivity(g2) 

# Transitiviteten i banknetværket
# den globale transitivitet for hele netværket er antallet af faktiske trekanter over de mulige
#     - siger noget om hvor lukket netværket er. Høj global transitivitet fortæller der er mange stærke ties som skaber closure (og dermed tæthed i netværket). En lav transitivitet peger på at der er mange 'svage' forbindelser så trekanter ikke 'lukkes'. Skaber tilgengæld mulighed for at 'bridge' strukturelle huller.
     
# den lokale transitivitet måler hvor mange trekanter den enkelte node indgår i i forhold til hvor mange der kunne være.
tr_g <- transitivity(gr, type = "global") 
tr_g
tr_l <- transitivity(gr, type = "local") 

tr_l %>% enframe %>% tibble() %>% View()

k <- degree(gr)
possible_triangles <- choose(k, 2)
actual_triangles   <- count_triangles(gr)

data.frame(degree = k, possible = possible_triangles,actual = actual_triangles, tr_l) %>% View()


# Vi kan lige se en visualisering, hvor vi tager den største komponent og fremhæver de noder der har en lav lokal transitivitet. Dvs indgår i åbne trekanter.
gr %>% filter(comp==1) %>% 
  ggraph(layout = "fr") +
  geom_edge_link0(color = "gray70", width = 0.1) +
  geom_node_point(size=1.5) +
  geom_node_point(aes(filter = centrality_degree() != 1 & local_transitivity() < .3), color = "red")+
  geom_node_label(aes(filter = centrality_degree() != 1 & local_transitivity() < .3, label = local_transitivity() %>% round(2)), color = "red", repel = TRUE)+
  theme_graph()


# Visualisering af open og closed triads
tri_plot(gr, mode = "closed")
tri_plot(gr, mode = "open")

#############################################/
# Kliker: Triadic closure på en større skala 
# 'et system af overlappende lukkede triader'
# 4-kliker
# 5-kliker
# n-kliker
############################################/
max_cliques(gr) 
max_cliques(gr) %>% sapply(., length) %>% table()
p_cli <- clique_plot(gr, n =4, mode = "both")

p_cli$vertices
p_cli$edges


#########################################################/
# Stilængder - genemsnitlige afstande og diameter
#########################################################/


##################################################################/ 
# Diameter ----
# En netværksgrafs diameter er den længste 'korteste sti' mellem to noder i netværket. Altså den korteste vej mellem netværkets yderpunkter, kan man sige. Giver kun mening for sammenhængende grafer, da den korteste vej mellem to ikke-forbunde noder er uendelig stor.
##################################################################/ 
# Vi laver et nyt graf- eller netværksobjekt kun med den største komponent. Fordi gr er et tidygraph objekt kan vi bruge tidyverse-funktioner som fx. filter()
comp1 <- gr %>% filter(comp == 1)


# diameter på den største komponent i vores banknetværk
diameter(comp1, directed = FALSE)

# hvilke to noder ligger længst fra hinanden
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
  morph(to_shortest_path, from = .N()$name == first(diam), to = .N()$name == last(diam)) %>% 
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

node1 <- "Ulrik Rammeskow Bang-Pedersen 51259" # som sidder i finansiel stabilitet
node2 <- "Carsten Egeriis 66665"          # adm. direktør i danske bank

# Vi laver en vertex attribute der er TRUE for alle noder på stien og for alle edges

comp1 <- comp1 %>% activate(nodes) %>% 
  mutate(sh_path = FALSE) %>% 
  morph(to_shortest_path, from = .N()$name == node1, to = .N()$name == node2) %>%
  mutate(sh_path = TRUE) %>% unmorph()

comp1 <- comp1 %>% activate(edges) %>% 
  mutate(sh_path = FALSE) %>% 
  morph(to_shortest_path, from = .N()$name == node1, to = .N()$name == node2) %>%
  mutate(sh_path = TRUE) %>% unmorph()


comp1 %>% 
  ggraph(layout='fr') + 
  geom_edge_link0(aes(filter=sh_path==FALSE), color='grey50', alpha=0.5) + 
  geom_node_point(aes(filter=sh_path==FALSE), color='black', size=3, alpha=0.25) + 
  geom_edge_link0(aes(filter=sh_path==TRUE), color='red', width=1.2) + 
  geom_node_point(aes(filter=sh_path==TRUE), color='darkred', size=5, alpha=0.5) + 
  geom_node_label(aes(filter=sh_path==TRUE, label=name), color='red', size=2, alpha = 0.8, repel = T) + 
  theme_graph()








############/
# Øvelse
############/

download.file("https://jacoblunding.github.io/netvaerksanalyse_cbs/scripts til undervisning/Session3_øvelse med svar.R", "Session3_øvelse med svar.R")
download.file("https://jacoblunding.github.io/netvaerksanalyse_cbs/data/pharma.csv", "data/pharma.csv")
