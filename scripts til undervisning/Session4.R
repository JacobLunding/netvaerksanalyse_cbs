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

download.file("https://jacoblunding.quarto.pub/virkstrat2025/functions/networkfunctions.R", "functions/networkfunctions2.R")

source("functions/networkfunctions.R")
source("functions/custom_functions.R")

###############################################################/
# 1. Indlæs og udvalg/behandling af data ----
###############################################################/

den <- read_csv("data/den17-no-nordic-letters.csv", guess_max = 100000)

# Hvad har vi i data:?
show.all.tags(den) %>% enframe() %>% View()

tags <- c("Finance", "FINA", "Banks", "Investment", "Insurance", "Venture- og kapitalfonde")
tags <- c("BILH","Cars", "Cyclism","Public transport", "TRAN", "Trains","Transport", "Traffic", "Infrastruce", "VL", "Roads")

den_sub <- has.tags(den, tags = tags, result = "den", mode = "or")

den_sub %>% distinct(name, affiliation, .keep_all = TRUE) %>% count(sector, sort = T)

den_sub <- den_sub %>% filter(!is.na(sector) & sector %in% c("Corporations", "Foundations", "NGO", "State","Commissions"))

den_sub <- den_sub %>% 
  # grupperer data efter 'name'
  group_by(name) %>% 
  # laver en ny variabel 'n_memberships' som for hvert individ (vi har jo grupperet data på individer) tæller antallet af unikke boards med n_distinct()
  mutate(n_memberships = n_distinct(affiliation)) 
# til sidst kan vi nu filtrere data på et logisk statement, så vi kun får rækker med individer der har mere end 1 medlemskab.

#omkod state commissions

den_sub <- den_sub %>% mutate(sector2 = case_when(sector == "Commissions"~"State", sector == "Organisation"~"NGO",.default = sector))
table(den_sub$sector2)

den_sub %>% mutate(tal = 1) %>% mutate(tal2 = tal +1 )

den_sub <- den_sub %>% filter(n_memberships > 1)

###############################################################/
# 2. Konstruktion af grafobjekt / netværksdata ----
###############################################################/

###############################################################/
# Option1:                                                    #/
# data -> incidence matrice -> adjacency -> netværksobjekt    #/
###############################################################/

bi_adj <- xtabs(formula = ~ name + affiliation, data = den_sub, sparse = TRUE) #Sparse = TRUE betyder at vi beder funktionen xtabs om at gemme den nye matrice i et hukommelsesbesparende format, hvor den ikke gemmer alle 0'erne (dvs. de ikke-optrædende forbindelser)
adj_c <-  t(bi_adj) %*% bi_adj

# Her betragter vi ikke netværket som vægtet!!
gr    <- adj_c %>% graph_from_adjacency_matrix(mode = "undirected", weighted = NULL, diag = FALSE) %>% simplify()
gr

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

# inspicer komponentstrukturen i grafen
complist <- components(gr)
str(complist)
complist$no           # antallet af komponenter
complist$csize        # størrelsen på de forskellige komponenter
complist$membership   # hvilke noder ligger i hvilke komponenter

# find Største komponent,(i networkfunctions.R, som vi 'sourcede'  i starten ligger en funktion til at finde den n'te størte komponent i en graf) 
comp1 <- largest_component(gr)
comp2 <- get_n_largest_component(gr, n = 2)
comp3 <- get_n_largest_component(gr, n = 3)

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

####################################################################################################################/
# Connectedness
# Hvor mange par af noder kan nå hinanden (dvs. er i samme komponent) i forhold til det teoretisk mulige antal par
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
g2 <- sample_gnp(20, p = 3/20)  # p er sandsynligheden for at der er en forbindelse mellem to noder : altså densiteten
autograph(g2) + geom_node_point(aes(filter = {count_triangles(g2) > 0}, color = {count_triangles(g2) >0}, size = {count_triangles(g2) >0})) +theme_graph()
transitivity(g2) 

# Transitiviteten i virksomhedsnetværket
tr_g <- transitivity(gr, type = "global") 
tr_l <- transitivity(gr, type = "local") 
l
tr_l %>% enframe %>% tibble() %>% View()

library(threejs)

graphjs(gr, vertex.label = sprintf("<h2 style='text-align:top;'>%s</h2>", V(gr)$name), vertex.size = .5, edge.color = "grey25", brush=TRUE)

# Visualisering af closed triads
p_tr  <- tri_plot(gr)
p_tr

max_cliques(gr) 
max_cliques(gr) %>% sapply(., length) %>% max()
p_cli <- clique_plot(gr, n = 5,mode = "both")
p_cli$edges

#####################################################/
# Cut points 
#####################################################/
V(comp1)$name %>% tibble() %>% View()

V(comp1)$cut_point <- map_lgl(1:vcount(comp1), .f = ~delete_vertices(comp1, .x) %>% components(.) %>% .$no > 1)
V(comp1)$compred   <- map_dbl(1:vcount(comp1), .f = ~vcount(comp1) - delete_vertices(comp1, .x) %>% components(.) %>% .$csize %>% max())
index <- as_data_frame(comp1, "vertices") %>% 
  mutate(index = 1:vcount(comp1))
V(comp1)$lab <- index$index

p0 <- comp1 %>% ggraph() +
  geom_edge_link0(edge_width = 0.2, edge_color = "grey70") +
  geom_node_point(aes(color = cut_point, size = cut_point)) +
  geom_node_text(aes(filter = cut_point, label = lab), size = 3) +
  scale_color_manual(values = c("FALSE" = "steelblue4", "TRUE" = "salmon3")) +
  scale_size_manual(values = c("FALSE" = 1, "TRUE" = 4)) +
  theme_graph()
p0
ix <- index %>% arrange(-compred) %>% filter(cut_point) %>% 
  pull(index)
p <- map(ix, .f = function(x) {
  delete_vertices(comp1, x) %>% ggraph() +
    geom_edge_link0(edge_width = 0.2, edge_color = "grey70") +
    geom_node_point() + 
    theme_graph() + ggtitle(paste("node", index$lab[x], "removed"))
  })

patchwork::wrap_plots(c(list(p0), p))




#################################################/
# Broer : edge betweenness
# Hvor vi sidst kigge på noder, der indgik i mange
# shortest paths og derfor var centrale, kan vi også
# kigge på edges og spørge om det er en bro for meget
# "trafik".
# weak ties vs non-redundant ties.
#################################################/

# Decomposing by edgebetweenness
eb <- edge_betweenness(comp1)
ggplot(eb %>% enframe()) + geom_density(mapping = aes(x = value))


# Lad os visualisere vores netværk og fremhæve edges med den højeste edgebetweenness
p1 <- comp1 %>% ggraph("fr") +
  geom_edge_link0(aes(color = dense_rank(desc(eb)) <25, width= eb, alpha = eb)) +
  geom_node_point(size = 2) +
  theme_graph() + guides(edge_alpha = "none") +
  scale_edge_color_manual(values = c("grey50", "salmon3"), name = "Top25 edge betweenness") +
  scale_edge_width_binned(range =c(0.02,1), name = "edge betweenness")
  
p2 <- comp1 %>% delete_edges(which(dense_rank(desc(edge_betweenness(comp1))) <25)) %>% ggraph("fr") +
  geom_edge_link0(edge_width = 0.3, edge_alpha = 0.3) +
  geom_node_point(size = 2) +
  theme_graph()

p1 / p2

data <- tibble(t = 0, diameter = diameter(comp1), 
       mean_dist = mean_distance(comp1), 
       n_edges = ecount(comp1), 
       n_components = components(comp1)$no, 
       size_l_component = max(components(comp1)$csize))
data_rand <- tibble(t = 0, diameter = diameter(comp1), 
               mean_dist = mean_distance(comp1), 
               n_edges = ecount(comp1), 
               n_components = components(comp1)$no, 
               size_l_component = max(components(comp1)$csize))


# Et stykke kode, der løbende sletter edge'en med den højeste edge betweenness

g_tmp <- comp1
g_tmp_rnd <- comp1
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
# Diameter ----
# En netværksgrafs diameter er den længste 'korteste sti' mellem to noder i netværket. Altså den korteste vej mellem netværkets yderpunkter, kan man sige. Giver kun mening for sammenhængende grafer, da den korteste vej mellem to ikke-forbunde noder er uendelig stor.
##################################################################/ 


# diameter på den største komponent i vores virksomhedsnetværk
diameter(comp1, directed = FALSE)

# hvilke to virksomheder ligger længst fra hinanden
farthest.nodes(comp1, directed = FALSE)

# hvad er vejen mellem dem
diam <- get_diameter(comp1, directed = FALSE)
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
  labs(title = 'Diameter in EliteDBs transport component') +
  theme_graph() 




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

V(comp1)$sector <- tibble(affiliation = V(comp1)$name) %>% left_join(den_sub %>% ungroup() %>% 
                                                                       distinct(affiliation, sector), 
                                                                     by = c("affiliation")) %>% pull(sector)

comp1 <- comp1 %>% set_vertex_attr("betweenness", value = betweenness(comp1))
comp1 <- comp1 %>% set_vertex_attr("betweenness10", value = betweenness(comp1) %in% (betweenness(comp1) %>% enframe() %>% top_n(10, value) %>% pull(value)))


comp1 %>% ggraph(layout='kk') + 
  geom_edge_link0(color='grey', width=0.6, alpha=0.35) + 
  geom_node_point(aes(color=sector, size = betweenness, alpha = betweenness), show.legend = T) + 
  theme_graph() + labs(color="sector") + 
  geom_node_label(aes( filter=betweenness10 == TRUE, label=name), alpha=0.65, size = 3, repel=T, force = 20)














############################################################/
# Ekstra ----
# Den korteste vej mellem to specifikke noder
############################################################/

path_of_interest <- shortest_paths(comp1, 
                                   from = V(comp1)$name ==xxxx, 
                                   to  = V(comp1)$name ==xxxx,
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

