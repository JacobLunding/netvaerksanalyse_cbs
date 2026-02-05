###############################################################/
#
#  Øvelse 3: Netværkskomponenter, densitet og stier (paths)
#
###############################################################/


# Opsætning ----


# nye pakker


# indlæs pakker
library(tidyverse)
library(igraph)
library(ggraph)
library(graphlayouts)
library(Matrix)
library(patchwork)
source("functions/networkfunctions.R")
source("functions/custom_functions.R")
###############################################################/
# 1. Indlæs og udvalg/behandling af data ----
###############################################################/

den <- read_csv("data/den17-no-nordic-letters.csv", guess_max = 100000)

# Hvad har vi i data:?
show.all.tags(den) %>% enframe() %>% View()

den_sub <- has.tags(den, tags = c("Finance", "FINA", "Banks", "Investment", "Insurance", "Venture- og kapitalfonde"), result = "den", mode = "or")

den_sub %>% distinct(name, affiliation, .keep_all = TRUE) %>% count(sector, sort = T)

den_sub <- den_sub %>% filter(!is.na(sector) & sector %in% c("Corporations", "Foundations", "NGO", "State","Commissions"))

den_sub <- den_sub %>% 
  # grupperer data efter 'name'
  group_by(name) %>% 
  # laver en ny variabel 'n_memberships' som for hvert individ (vi har jo grupperet data på individer) tæller antallet af unikke boards med n_distinct()
  mutate(n_memberships = n_distinct(affiliation)) 
# til sidst kan vi nu filtrere data på et logisk statement, så vi kun får rækker med individer der har mere end 1 medlemskab.
  
den_sub <- den_sub %>% filter(n_memberships > 1)

###############################################################/
# 2. Konstruktion af grafobjekt / netværksdata ----
###############################################################/

###############################################################/
# Option1:                                                    #/
# data -> incidence matrice -> adjacency -> netværksobjekt    #/
###############################################################/

# 1) lav en incidens matrice over alle eksisterende relationer mellem individer (i rækker) og affiliations (i kolonner)
# 2) fra incidens matricen kan vi lave to forskellige adjanceny matricer (indivd x individ eller affiliation x affiliation). Til et kan I bruge funktionen incidence_to_adjacency( ) fra "networkfunctions.R" den har en option, mode, som skal være enten: mode = "row" hvis det skal være rækkerne eller "col" hvis det skal være kolonnerne. Her vil vi gerne have adjacency matricen for kolonnerne, altså affiliations
# 3) Nu kan vi lave et grafobjekt som vi kalder "gr". Og simplificere det, simplify(), så vi slipper af med loops (A's forbindelse til A, dvs. diagonalen i adjacency matricen) og gør relationer binære 1/0, dvs. vi ser bort fra vægten af en forbindelse (dvs. vi skelner ikke mellem at virksomhed A og B deler 2 medlemmer, mens virksomhed A og C kun deler en)
bi_adj <- xtabs(formula = ~ name + affiliation, data = den_sub, sparse = TRUE) #Sparse = TRUE betyder at vi beder funktionen xtabs om at gemme den nye matrice i et hukommelsesbesparende format, hvor den ikke gemmer alle 0'erne (dvs. de ikke-optrædende forbindelser)
adj_c <-  t(bi_adj) %*% bi_adj
gr    <- adj_c %>% graph_from_adjacency_matrix(mode = "undirected", diag = FALSE) %>% simplify()

################################################################################################/
# 3. Udregningen af netværksmål ----
################################################################################################/


##################################################################/ 
# Transitivitet ----
# et mål antallet af faktiske triader ud af det mulige antal triader
# måler graden af lokal forbundethed:
# Når A kender B og B kender C, hvor hyppigt er det så at A også kender C
##################################################################/ 

# eksempler
# En ring graf, med ingen triader
g1 <- make_ring(10)
autograph(g1) + theme_graph()
transitivity(g1) # 0 - no triads

# I et tilfældigt netværk med 10 noder og en densitet på 40%
g2 <- sample_gnp(20, p = 2/10)  # p er sandsynligheden for at der er en forbindelse mellem to noder : altså densiteten
autograph(g2) + geom_node_point(aes(filter = {count_triangles(g2) > 0}, color = {count_triangles(g2) >0}, size = {count_triangles(g2) >0})) +theme_graph()
transitivity(g2) 


# Transitiviteten i virksomhedsnetværket
transitivity(gr, type = "global") 
transitivity(gr, type = "local") 


# Visualisering af closed triads
mat_tr <- matrix(triangles(gr), ncol = 3, byrow = T)
e_triad <- bind_rows(data.frame(a = mat_tr[,1], b = mat_tr[,2]), data.frame(a = mat_tr[,1], b = mat_tr[,3]), data.frame(a = mat_tr[,2], b = mat_tr[,3])) %>% distinct()
e_triad <- e_triad %>% transmute(X1 = case_when(b < a~b, .default = a), X2 = case_when(b < a~a, .default = b))

E(gr)$is.triad_edge <- as_edgelist(gr, names = F) %>% data.frame() %>% left_join(., e_triad %>% mutate(is.triad_edge = TRUE)) %>% pull(is.triad_edge)
E(gr)$is.triad_edge[is.na(E(gr)$is.triad_edge)] <- FALSE
p_tr <- gr %>% ggraph("fr") +
  geom_edge_link0(aes(color = is.triad_edge, width= is.triad_edge, alpha = is.triad_edge)) +
  geom_node_point(size = 2) +
  theme_graph() + guides(edge_alpha = "none") +
  scale_edge_color_manual(values = c("grey50", "salmon3"), name = "closed triads") +
  scale_edge_width_manual(values =c(0.3,.7), name = "closed triads") +
  scale_edge_alpha_manual(values =c(0.4,.7), name = "") 
p_tr

cl4 <- cliques(gr, min = 4, max = 4) 


clique_plot(gr, n = 4:10, mode = "both")

# Calculate the size of each maximal clique.
table(unlist(lapply(clq, length)))

lc <- largest_cliques(gr)

# Create two new undirected subgraphs, each containing only the vertices of each largest clique.
gs1 <- as.undirected(induced_subgraph(gr, lc[[1]]))



# Plot the two largest cliques side-by-side

par(mfrow=c(1,2)) # To plot two plots side-by-side

plot(gs1,
     vertex.label.color = "black", 
     vertex.label.cex = 0.9,
     vertex.size = 0,
     edge.color = 'gray28',
     main = "Largest Clique 1",
     layout = layout.circle(gs1)
)

plot(gs2,
     vertex.label.color = "black", 
     vertex.label.cex = 0.9,
     vertex.size = 0,
     edge.color = 'gray28',
     main = "Largest Clique 2",
     layout = layout.circle(gs2)
)

















#Decomposing by edgebetweenness

gr_comp <- largest_component(gr)
eb <- edge_betweenness(gr_comp)

E(gr_comp)$egdebet <- eb
ggplot(eb %>% enframe()) + geom_density(mapping = aes(x = value))

p1 <- gr_comp %>% ggraph("fr") +
  geom_node_point(size = 2) +
  geom_edge_link0(aes(color = eb >200, width= eb, alpha = eb)) +
  theme_graph() + guides(edge_alpha = "none") +
  scale_edge_color_manual(values = c("grey50", "salmon3"), name = "eb > 200") +
  scale_edge_width_binned(range =c(0.02,1), name = "edge betweenness") 

p2 <- gr_comp_red %>% ggraph("fr") +
  geom_edge_link0(edge_width = 0.3, edge_alpha = 0.3) +
  geom_node_point(size = 2) +
  theme_graph()

p1 / p2

data <- tibble(t = 0, diameter = diameter(gr_comp), 
       mean_dist = mean_distance(gr_comp), 
       n_edges = ecount(gr_comp), 
       n_components = components(gr_comp)$no, 
       size_l_component = max(components(gr_comp)$csize))
data_rand <- tibble(t = 0, diameter = diameter(gr_comp), 
               mean_dist = mean_distance(gr_comp), 
               n_edges = ecount(gr_comp), 
               n_components = components(gr_comp)$no, 
               size_l_component = max(components(gr_comp)$csize))


g_tmp <- gr_comp
g_tmp_rnd <- gr_comp
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


##################################################################/
# Densitet ----
# Densiteten i et netværk udtrykker sandsynligheden for at to tilfældige noder i netværker er forbundne. 
# Den udregnes ved at dividere det faktiske antal forbindelser (edges) med den maksimale mulige antal forbindelser (edges).
##################################################################/

# eksempler: forskellige netværk med 40 noder  ### maksimale antal edges = (N_noder * N_noder - 1) / 2
(40 * (40-1)) / 2

e1 <- make_full_graph(40, directed = FALSE)
e1 %>% ggraph() +
  geom_edge_link0(edge_width = 0.1, alpha = 0.4) +
  geom_node_point(color = "steelblue1", size = 4) +
  theme_graph()
edge_density(e1, loops=FALSE)

e2 <- make_star(40, mode = "undirected")
e2 %>% ggraph() +
  geom_edge_link0(edge_width = 0.1, alpha = 0.4) +
  geom_node_point(color = "steelblue1", size = 4) +
  theme_graph()
edge_density(e2, loops=FALSE)

e3 <- make_tree(40, children = 3, mode = "undirected")
e3 %>% ggraph() +
  geom_edge_link0(edge_width = 0.1, alpha = 0.4) +
  geom_node_point(color = "steelblue1", size = 4) +
  theme_graph()
edge_density(e3, loops=FALSE)

ecount(e3) / ((40 * (40-1))/2)


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
  ggraph(layout = "fr") +
  geom_edge_link0(edge_width = .2, edge_alpha = .3) +
  geom_node_point(size=1.5) +
  theme_graph()

# inspicer komponentstrukturen i grafen
complist <- components(gr)
str(complist)
complist$no           # antallet af komponenter
complist$csize        # størrelsen på de forskellige komponenter
complist$membership   # hvilke noder ligger i hvilke komponenter

# find Største komponent,(i networkfunctions.Rsom vi 'sourcede'  i starten ligger en funktion til at finde den n'te størte komponent i en graf) 
comp1 <- gr %>%  get_n_largest_component(n = 1)
comp2 <- get_n_largest_component(gr, n = 2)
comp3 <- get_n_largest_component(gr, n = 9)

# vi kan nu regne densiteten i komponenterne: 
edge_density(comp1)
edge_density(comp2)

# Visualisering af de to største komponenter
comp1 %>% 
  ggraph(layout = "fr") +
  geom_edge_link0(color = "gray70") +
  geom_node_point(size=1.5) +
  labs(caption = paste0("density= ", round(edge_density(comp1)*100,1), " %")) +
  theme_graph()

comp2 %>% 
  ggraph(layout = "fr") +
  geom_edge_link0(color = "gray70") +
  geom_node_point(size=2) +
  geom_node_label(aes(label = name)) +
  labs(caption = paste0("density= ", round(edge_density(comp2)*100,1), " %")) +
  theme_graph()


##################################################################/ 
# Stilængde (shortest paths) ----
# hvor lang er den korteste afstand fra A til C i netværket....
##################################################################/ 

# funktionen distances fra igraph giver os en matrice over stilængden mellem samtlige noder (parvist) i netværket

# prøv på hele netværket med alle komponenter: Hvorfor står der inf! nogle steder??
gr %>% distances() %>% view()
# afstande i største komponent
comp1 %>% distances() %>% view


# Den gennemsnitlige sti-længde : giver kun mening for en sammenhængende graf!
# gennemsnitlige korteste sti-længde for den størte komponent
mean_distance(comp1)

# Vi kan også beregne den korteste afstand fra én node til alle andre noder
# her sætter vi parameteret v (den node vi vil fokusere på) til index-tallet for noden
# findes ved at bruge which()
# 
# Vi vælger en target node
V(comp1)$name

target_node  <- "Energi Danmark"

V(comp1)$name ==target_node
which(V(comp1)$name ==target_node)

distances_target <- 
  # distances funktionen
  distances(comp1, 
            # startnode
            v = which(V(comp1)$name ==target_node),
            # til
            to = V(comp1))

distances_target %>% View

# Lad os visualiere afstanden til Lego i netværket....
# Til det skal vi bruge en vertex attribute som indeholder alle virksomheders afstand til lego..
# Men, men men... vi skal lige sikre os at vores virksomhederne i distances_hofor er sorteret ligesom i vores netværksopbjekt

all(colnames(distances_target) == names(V(comp1)))

# Nu kan vi tilføje en en vertex attribute 
comp1 <- comp1 %>% set_vertex_attr("distances_target", value = as.vector(distances_target))
# og en vertex attribute som kun er true for den node der er LEGO
comp1 <- comp1 %>% set_vertex_attr("target", value = V(comp1)$name == target_node)

# second, we plot

comp1 %>% 
  ggraph(layout = "fr") +
  # Vi plotter først alle forbindelser
  geom_edge_link0(edge_width = .3, edge_alpha = .3) +
  # Vi plotter alle noder, der ikke er LEGO, ved at bruge et filter [filter=lego==FALSE] og farver dem efter afstanden til lego
  geom_node_point(aes(filter= (target==FALSE), color=distances_target), 
                  size =4.5) +
  # Vi plotter den node, der er LEGO, [filter=lego==TRUE] og farver den rød
  geom_node_point(aes(filter= (target==TRUE)), 
                  color="darkred", size=4.5) +
  # For alle noder der ikke er lego [filter=lego==FALSE], plotter vi en text, der viser afstanden til lego
  geom_node_text(aes(filter=(target==FALSE), label = distances_target), 
                 color= "gray75", size=3) +
  # Vi kan ændre navnet på legend, så den ikke hedder color, men distance to lego
  labs(color = paste("Distance to ", target_node)) +
  scale_color_viridis(direction = 1) +
  theme_graph() 

# save it - we save it in our output folder. 
ggsave("output/03-distance_to_lego.png", width = 30, height = 17.5, units = "cm")


##################################################################/ 
# Diameter ----
# En netværksgrafs diameter er den længste 'korteste sti' mellem to noder i netværket. Altså den korteste vej mellem netværkets yderpunkter, kan man sige. Giver kun mening for sammenhængende grafer, da den korteste vej mellem to ikke-forbunde noder er uendelig stor.
##################################################################/ 


# diameter på den største komponent i vores virksomhedsnetværk
diameter(comp1, directed = FALSE)

# hvilke to virksomheder ligger længst fra hinanden
farthest.nodes(comp1, directed = FALSE)

# hvad er vejen mellem dem
diam <- get.diameter(comp1, directed = FALSE)
diam 

# Vi kan visualisere den længste sti:

# 1) vi gemmer en attribut til noderne, der fortæller (TRUE/FALSE) om de ligger på stien.
comp1 <- comp1 %>% set_vertex_attr(name = "diameter", 
                                        value = V(comp1)$name %in% names(diam)) 


# 2) vi gemmer en attribut til egdes, der fortæller (TRUE/FALSE) om de indgår i stien
comp1 <- comp1 %>% set_edge_attr(name = "diameter", 
                                      value = E(comp1) %in% E(comp1, path = diam))

# 3) plot 
comp1 %>% 
  ggraph(layout = "fr") +
  geom_edge_link0(aes(filter=diameter==FALSE), color = "gray60") + 
  geom_edge_link0(aes(filter=diameter==TRUE), color = "red", width = 1.5) +
  geom_node_point(aes(filter=diameter==FALSE), color = "black") +
  geom_node_point(aes(filter=diameter==TRUE), color = "red", size =2) +
  geom_node_label(aes(filter=diameter==TRUE, label = name), nudge_y = -0.3, size =2.5, repel = TRUE) + 
  labs(title = 'Diameter in EliteDBs Energy/environment component') +
  theme_graph() 


############################################################/
# Ekstra ----
# Den korteste vej mellem to specifikke noder
############################################################/

path_of_interest <- shortest_paths(comp1, 
                                   from = V(comp1)$name =="Vestas (Direktion)", 
                                   to  = V(comp1)$name == "DONG Energy",
                                   output = "both") # both path nodes and edges
path_of_interest
# Vi laver en vertex attribute der er TRUE for alle noder på stien
comp1 <- comp1 %>% set_vertex_attr("path", value = V(comp1) %in% path_of_interest$vpath[[1]])
comp1 <- comp1 %>% set_edge_attr("path", value = E(comp1) %in% E(comp1, path = path_of_interest$vpath[[1]]))

comp1 %>% 
  ggraph(layout='fr') + 
  geom_edge_link0(aes(filter=path==FALSE), color='grey50', alpha=0.5) + 
  geom_node_point(aes(filter=path==FALSE), color='black', size=3, alpha=0.25) + 
  geom_edge_link0(aes(filter=path==TRUE), color='red', width=1.2) + 
  geom_node_point(aes(filter=path==TRUE), color='darkred', size=5, alpha=0.5) + 
  geom_node_label(aes(filter=path==TRUE, label=name), color='red', size=2, alpha = 0.8, repel = T) + 
  theme_graph()




comp1 <- comp1 %>% set_vertex_attr("betweenness", value = betweenness(comp1))
comp1 <- comp1 %>% set_vertex_attr("betweenness10", value = betweenness(comp1) %in% (betweenness(comp1) %>% enframe() %>% top_n(10, value) %>% pull(value)))

#############################################/
# To måder at tilføje ikke ordnede vertex attr:
# 1) en mapping af en funktion 
# 2) en leftjoin på 
#############################################/
comp1 <- comp1 %>% set_vertex_attr("sector", value = map_chr(V(comp1)$name, 
                                                         .f = ~den_sub %>% ungroup() %>% 
                                                           distinct(affiliation, sector) %>% 
                                                           filter(affiliation == .x) %>% 
                                                           pull(sector)))

V(comp1)$sector <- V(comp1)$name %>% tibble(affiliation = .) %>% left_join(den_sub %>% ungroup() %>% 
                                                                      distinct(affiliation, sector), 
                                                                    by = c("affiliation")) %>% pull(sector)

comp1 %>% ggraph(layout='kk') + 
  geom_edge_link0(color='grey', width=0.6, alpha=0.35) + 
  geom_node_point(aes(color=sector, size = betweenness, alpha = betweenness), show.legend = T) + 
  theme_graph() + labs(color="sector") + 
  geom_node_label(aes( filter=betweenness10 == TRUE, label=name), alpha=0.65, size = 3, repel=T, force = 20)













library(threejs)
graphjs(gr, vertex.label = sprintf("<h2 style='text-align:top;'>%s</h2>", V(gr)$name), vertex.size = 1,  brush=TRUE)
