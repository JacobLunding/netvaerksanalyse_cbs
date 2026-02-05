library(tidyverse)
library(readxl)
library(writexl)
library(igraph)
library(ggraph)
library(patchwork)
library(ggpubr)
library(Matrix)
source("functions/networkfunctions.R")
source("functions/custom_functions.R")


###################################################################################################/
# 0. Download data fra Orbis ----
# Filters:
# 1. Status:			                            Active companies, Unknown situation
# 2. Activity text search:                    "pharma" + branche kode
# 3. World region/Country/Region in country:  Norden
# 
###################################################################################################/

###################################################################################################/
# 1. Læs downloadet datafil ----
# Husk at en xlsx fil fra orbis har et sheet1 med info. Dvs data ligger i sheet2
# Sæt evt. guess_max til en høj værdi, så R ikke gætter på hvilken type data der skal være i kolonnerne
###################################################################################################/
pharma_nordic <- read_orbisxlsx("data/nordic_pharma2025.xlsx")
colnames(pharma_nordic)
head(pharma_nordic)
###################################################################################################/
# 2. Omkod data m.m. ----
###################################################################################################/

# Vi laver en ny variabel, executive, som er TRUE hvise role_type indeholder **enten** BoD *eller* ExeB *eller* SenMan *eller* ExeC ELLER role er forskellige ting der indikerer executive level:
pharma_nordic <- pharma_nordic %>% mutate(executive = grepl("BoD|ExeB|SenMan|ExeC", role_type) | grepl("Director|directeur|Head of|general|Chief|executive|president|Chairman", role, ignore.case =T))

# Vi har også to forskellige variable for personers geografiske oprindelse
pharma_nordic %>% count(person_country, sort = TRUE)
pharma_nordic %>% filter(is.na(person_country)) %>% count(person_countries, sort = TRUE)

pharma_nordic <- pharma_nordic %>% 
  mutate(person_geo = case_when(is.na(person_country)~
                                  str_extract(person_countries, 
                                              "(^.*?(?=;))|(^.*?$)"),
                                .default = person_country))

pharma_nordic %>% count(person_geo, sort = TRUE)

# Endelig kan vi filtrere på de variable vi ønsker
pharma_nordic <- pharma_nordic %>% filter(person == TRUE)
#pharma_nordic <- pharma_nordic %>% filter(role_status == "Current")
pharma_nordic <- pharma_nordic %>% filter(executive == TRUE)
pharma_nordic <- pharma_nordic %>% distinct(name, affiliation, .keep_all = T)

###################################################################################################/
# 3. Definerer et netværksobjekt for virksomheder ----
###################################################################################################/

# lav en sparse bi_adjacency matrice name x affiliation: 
    # (..., formula = ~ name + affiliation) giver en biadjacency matrice med name (individer) i rækker og affiliation (virksomheder) i kolonner  
bi_adj <- xtabs(data = pharma_nordic, formula = ~name + affiliation, sparse = T)

# Vi kan slette folk der ikke linker mellem bestyrelser: dvs folk der kun har 1 post
sel    <- rowSums(bi_adj) > 1
bi_adj <- bi_adj[sel,]

# lav affiliation x affiliation adjacency matricen: 
    # brug matrix multiplikation Matrix::t(bi_adj) %*% bi_adj eller
adj_virk  <-  t(bi_adj) %*% bi_adj
adj_ind   <-  bi_adj %*% t(bi_adj) 


# lav netværks objektet for virksomheder
pharma_nordic_net_c <- graph_from_adjacency_matrix(adjmatrix = adj_virk, mode = "undirected") %>% simplify()

# lav netværks objektet for individer
pharma_nordic_net_ind <- graph_from_adjacency_matrix(adjmatrix = adj_ind, mode = "undirected") %>% simplify()

###################################################################################################/
# 4. Netværkets komponenter? ----
###################################################################################################/
# Virksomhedsnetværket
count_components(pharma_nordic_net_c)
table(components(pharma_nordic_net_c)$csize)

comp1 <- largest_component(pharma_nordic_net_c)
comp2 <- get_n_largest_component(pharma_nordic_net_c, n = 2)
# Individnetværket
count_components(pharma_nordic_net_ind)
table(components(pharma_nordic_net_ind)$csize)

comp1_ind <- largest_component(pharma_nordic_net_ind)
comp2_ind <- get_n_largest_component(pharma_nordic_net_ind, n = 2)

###################################################################################################/
# 5. Tilføj netværkseksterne node attributes til netværket ----
###################################################################################################/
# Viksomheder
comp1 <- add_vertex_attr(graph = comp1, 
                     data = pharma_nordic %>% 
                            select(affiliation, affiliation_country, sector, revenue,
                                   total_assets, n_employees, guo_name, guo_country, duo_name,
                                   duo_country),
                     match_var = "affiliation")

comp2 <- add_vertex_attr(graph = comp2, 
                         data = pharma_nordic %>% 
                           select(affiliation, affiliation_country, sector, revenue,
                                  total_assets, n_employees, guo_name, guo_country, duo_name,
                                  duo_country),
                         match_var = "affiliation")

# Individer
comp1_ind <- add_vertex_attr(graph = comp1_ind, 
                         data = pharma_nordic %>% 
                           select(name, person_gender, person_geo),
                         match_var = "name")

comp2_ind <- add_vertex_attr(graph = comp2_ind, 
                             data = pharma_nordic %>% 
                               select(name, person_gender, person_geo),
                             match_var = "name")



###################################################################################################/
# 6. Komponent visualisering ----
###################################################################################################/
# Virksomheder
p <- (pharma_nordic_net_c - which(degree(pharma_nordic_net_c) == 0)) %>% ggraph("fr") +
  geom_edge_link0(width = 0.3, alpha = 0.3) +
  geom_node_point(size = .5) + ggtitle(paste0("Netværket i den skandinaviske \npharmasektor (n=", vcount(pharma_nordic_net_c), ")")) +
  theme_graph(base_family = "serif")
p1 <- comp1 %>% ggraph("fr") +
  geom_edge_link0(width = 0.3, alpha = 0.3) +
  geom_node_point(aes(color = affiliation_country), size = 1.2) + ggtitle(paste0("Største komponent (n=", vcount(comp1), ")")) + guides(color = guide_legend("Country")) +
  theme_graph(base_family = "serif")
p2 <- comp2 %>% ggraph("fr") +
  geom_edge_link0(width = 0.3, alpha = 0.3) +
  geom_node_point(aes(color = affiliation_country), size = 1.2) + ggtitle(paste0("Næststørste komponent (n=", vcount(comp2), ")")) + guides(color = guide_legend("Country")) +
  theme_graph(base_family = "serif")

pc <- p + p1 + p2
# Individer
p <- (pharma_nordic_net_ind - which(degree(pharma_nordic_net_ind) == 0)) %>% ggraph("fr") +
  geom_edge_link0(width = 0.3, alpha = 0.3) +
  geom_node_point(size = .5) + ggtitle(paste0("Netværket i den skandinaviske \npharmasektor (n=", vcount(pharma_nordic_net_ind), ")")) +
  theme_graph(base_family = "serif")
p1 <- comp1_ind %>% ggraph("fr") +
  geom_edge_link0(width = 0.3, alpha = 0.3) +
  geom_node_point(aes(color = person_geo), size = 1.2) + ggtitle(paste0("Største komponent (n=", vcount(comp1_ind), ")")) + guides(color = guide_legend("Country")) +
  theme_graph(base_family = "serif")
p2 <- comp2_ind %>% ggraph("fr") +
  geom_edge_link0(width = 0.3, alpha = 0.3) +
  geom_node_point(aes(color = person_geo), size = 1.2) + ggtitle(paste0("Næststørste komponent (n=", vcount(comp2_ind), ")")) + guides(color = guide_legend("Country")) +
  theme_graph(base_family = "serif")

pi <- p + p1 + p2

pc / pi
 
###################################################################################################/
# 7. Netværks mål ----
###################################################################################################/

dens     <- edge_density(comp1)
trans    <- transitivity(comp1)
radius   <- radius(comp1)
diameter <- diameter(comp1)

net_description <- c("nb. of nodes" = vcount(pharma_nordic_net_c),
                     "nb. of edges" = ecount(pharma_nordic_net_c),
                     "nb. of components" = count_components(pharma_nordic_net_c),
                     "largest component: nb. of nodes" = vcount(comp1), 
                     "largest component: share of nodes" = vcount(comp1) / vcount(pharma_nordic_net_c), 
                     "largest component: nb. of edges" = ecount(comp1), 
                     "largest component: share of edges" = ecount(comp1) / ecount(pharma_nordic_net_c),  
                     "largest component: diameter" = diameter,  
                     "largest component: radius" = radius,
                     "largest component: density" = dens, 
                     "largest component: transitivity" = trans) %>% enframe(name = "Measures", value = "value")

write_xlsx(net_description, "output/pharma_nordic_example_net_description.xlsx")


############################################################/
# 8. Ego-netværk og lokal transitivitet ----
# åbne vs. lukkede trekanter på ego niveau
############################################################/

# Hvad er et ego netværk
ego_net <- make_ego_graph(comp1, order = 1, nodes = "H. LUNDBECK A/S")[[1]]

ggraph(ego_net) +
  geom_edge_link0() +
  geom_node_point(size = 2) +
  geom_node_label(aes(filter = {name == "H. LUNDBECK A/S"}, label = name)) +
  theme_graph()

ego <- "H. LUNDBECK A/S"
# Ego neighborhoods er en funktion jeg har skrevet som plotter egonetværk i flere nabolag
pl <- ego_neighborhoods(comp1, 4, ego, labels = FALSE)

ggpubr::ggarrange(plotlist = pl, labels = c("1st neighbourhood", "2nd neighbourhood", "3rd neighbourhood", "4th neighbourhood"))


############################################################/
# 8. NYT MÅL: Burt's constraint ----
# Strukturelle huller, brokerage, mål på egonetværks niveau
############################################################/

############################################/
# Netværkslukning, lokal transitivitet
###########################################/

random_nodes <- sample(V(comp1)$name, 12)
random_nodes <- transitivity(comp1, type = "local", vids = random_nodes)
random_nodes[is.nan(random_nodes)] <- NA

random_nodes <- sort(random_nodes, decreasing = T, na.last = T)
random_nodes <- names(random_nodes)

pl <- ego_net_plot(graph = comp1, nodes = random_nodes, mode = "transitivity")
p <- ggpubr::ggarrange(plotlist = pl)
p
ggsave(plot = p, filename = "output/network closure.pdf", height = 6, width = 10)

## Constraint:
# the extent to which a person‘s contacts are redundant
# Måler 'brokerage', Burt's Constraint måler i hvor høj grad en nodes 'venner' (direkte forbindelser) også er forbundet til hinanden - danner en lukket gruppe. I en lukket gruppe er man begrænset (constrained) ift adgangen til 'ny viden' (tænk ekkokammer) fordi alle ens kontakter også er forbundne. En node der har venner 'uden for' sin klike har derfor en lavere constraint og vil oftere være den, der bringer nye ideer/tanker/viden ind i sin klike. Med andre ord, jo mindre "constraint", desto bedre kan en node fungere som en broker over strukturelle huller i netværket.
# En kombination af tre ting påvirker en nodes (ego) constraint:
# 1) c-size: antallet af egos direkte forbindelser (degree)... 
      # lav degree --> højere constraint
# 2) c-density: antallet af forbindelser mellem egos direkte forbindelser... 
      # mange 'alter'forbindelser --> høj constraint
# 3) c-hierarchy: antallet af andre forbindelser som egos forbindelser har... 
      # færre 'ego-eksterne' forbindelser --> høj constraint


# constraint funktioen i Igraph beregner Burt's constraint, som er højere jo mere 'constrained' en node er, dvs. lav constraint = høj brokerage. 
constr    <- constraint(comp1)
# det vil vi ofte gerne 'vende om' så det bliver et mål for brokerage evne.
brokerage <- 1 / constr


###################################################################################################/
# 9.1 Centralitetsmål mv. ----
###################################################################################################/

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

write_xlsx(cent_metrics, "output/pharma_nordic_example_centralitymetrics.xlsx")

###################################################################################################/
# 9.2 Tilføj centralitetsmål som attributes ----
###################################################################################################/
comp1 <- add_vertex_attr(graph= comp1, data = cent_metrics, match_var = "name")

###################################################################################################/
# 10 Example: Constraint vs. closure/transitivity vs. local betweenness----
###################################################################################################/

### lad os udvælge nogle forskellige virksomheder fra brokerage fordelingen: nr 1, 5, 10, 15, 20, 30 fx
### 
### 


brokers <- cent_metrics %>% 
  arrange(brokerage_rank) %>% 
  filter(brokerage_rank %in% c(1,20, 50, 100)) %>% 
  distinct(brokerage_rank, .keep_all = T) %>% 
  pull(name)

pl <- ego_net_plot(graph = comp1, nodes = brokers, mode = "constraint")

p <- ggpubr::ggarrange(plotlist = pl, common.legend = T, legend = "none")

ggsave(plot = p, filename = "output/constraint_brokerage.pdf", height = 6, width = 8)


ego_net <- make_ego_graph(comp1, order = 1, nodes = "MEDIVIR AB")[[1]]

ggraph(ego_net) +
  geom_edge_link0() +
  geom_node_point(size = 2) +
  geom_node_label(aes(filter = {name == "MEDIVIR AB"}, label = name)) +
  theme_graph()


###################################################################################################/
# 11. Visualiseringer af netværk ----
###################################################################################################/

# Tidygraph!!! tidygraph pakken laver igraph objektet om, så man kan arbejde med netværket ligesom vi gør med andre data objekter (mutate, arrange, filter osv.).. Her vil jeg fx gerne sortere (arrange) netværket, så den plotter noderne i en bestemt rækkefølge..


# Degree
p0 <- comp1 %>% as_tbl_graph() %>% arrange(degree) %>% ggraph("stress") +
  geom_edge_link0(width =.3, alpha = 0.3) +
  geom_node_point(aes(color = degree, size = degree)) + 
  scale_size_continuous(range = c(2,5)) + 
  geom_node_label(aes(filter=degree_rank<=5, label = paste0(degree_rank, ": ", name)), size = 3, repel = T, force = 25) +
  guides(label = "none", size = "none") +
  theme_graph(base_family = "serif")

p0.1 <- comp1 %>% as_tbl_graph() %>% arrange(coreness) %>% ggraph("stress") +
  geom_edge_link0(width =.3, alpha = 0.3) +
  geom_node_point(aes(color = coreness, size = degree)) + 
  scale_size_continuous(range = c(2,5)) + 
  geom_node_label(aes(filter=coreness_rank==1, label = paste0(coreness_rank, ": ", name)), size = 3, repel = T, force = 25) +
  guides(label = "none", size = "none") +
  theme_graph(base_family = "serif")

# Betweenness
p1 <- comp1 %>% as_tbl_graph() %>% arrange(betweenness) %>% ggraph("stress") +
  geom_edge_link0(width =.3, alpha = 0.3) +
  geom_node_point(aes(color = betweenness, size = betweenness)) + 
  scale_size_continuous(range = c(2,5)) + 
  geom_node_label(aes(filter=betw_rank<=5, label = paste0(betw_rank, ": ", name)), size = 3, repel = T, force = 25) +
  guides(label = "none", size = "none") +
  theme_graph(base_family = "serif")

# local betweenness
p1.1 <- comp1 %>% as_tbl_graph() %>% arrange(local_betweenness) %>% ggraph("stress") +
  geom_edge_link0(width =.3, alpha = 0.3) +
  geom_node_point(aes(color = local_betweenness, size = local_betweenness)) + 
  scale_size_continuous(range = c(2,5)) + 
  geom_node_label(aes(filter=local_betw_rank<=5, label = paste0(local_betw_rank, ": ", name)), size = 3, repel = T, force = 25) +
  guides(label = "none", size = "none") +
  theme_graph(base_family = "serif")


# closeness
p2<- comp1 %>% as_tbl_graph() %>% arrange(closeness) %>% ggraph("stress") +
  geom_edge_link0(width =.3, alpha = 0.3) +
  geom_node_point(aes(color = closeness, size = closeness)) + 
  scale_size_continuous(range = c(2,5)) + 
  geom_node_label(aes(filter=closeness_rank<=5, label = paste0(closeness_rank, ": ", name)), size = 3, repel = T, force = 25) +
   guides(label = "none", size = "none") +
  theme_graph(base_family = "serif")


# brokerage
p3<- comp1 %>% as_tbl_graph() %>% arrange(brokerage) %>% ggraph("stress") +
  geom_edge_link0(width =.3, alpha = 0.3) +
  geom_node_point(aes(color = brokerage, size = degree)) + 
  scale_size_continuous(range = c(2,5)) + 
  geom_node_label(aes(filter=brokerage_rank<=5, label = paste0(brokerage_rank, ": ", name)), size = 3, repel = T, force = 25) +
  guides(label = "none", size = "none") +
  theme_graph(base_family = "serif")


p <- ggarrange(plotlist = list(p0, p1.1, p2, p3)) %>%  annotate_figure(., top = paste0("Største komponent (n=", vcount(comp1), ") top-5:"))

ggsave("output/pharma_nordic_example_net_plots.pdf", plot = p, width = 15, height = 10)


###################################################################################################/
# 12. Assortativity (assortative mixing) ----
# homophili i netværk: 
###################################################################################################/

# Det kan være interessant at vide om noder der er ens på forskellige egenskaber er forbundet med hinanden. For at svare på det spørgsmål kan man udregne det der kaldes et netværks Assortativity koefficent, der som en korrelations koefficient kan antage værdier mellem -1 og 1. En høj assortativity koefficient betyder at ensartede noder (på den udvalgte egenskab) i højere grad er forbundne, mens en negativ koefficient (gående mod -1) betyder at der omvendt er tale at forskelligartede noder tendere mod at have forbindelser. En assortatity koefficient på 0 beskriver et netværk, hvor der ingen korrelation (hverken positiv eller negativ) er mellem en bestemt egenskab og tendensen til at danne forbindelser.

# Assortativity for kontinuerte variable----

# Funktion assortativity() bruges til at beregne assortativity for kontinuerte variable
# Funktionen assortativity_nominal() bruges til at beregne netværksassortativity for kategorielle variable

# Vi kan beregne assortativity for degree. Altså, tenderer velforbundne noder mod at være forbundne. "Populær tiltrækker populær".

# Assortativity degree
assortativity(comp1, values = degree(comp1))
assortativity_degree(comp1)

# I mange tilfælde vil det være interessant at beregne assortativity på netværks-eksterne egenskaber. Vi kunne fx. tage antal ansatte, omsætning og assets: er store virksomheder tilbøjelige til at skabe forbindelser til andre store virksomheder:

      # først sætter vi lige missing (NA) til 0 på antal ansatte, omsætning og assets
V(comp1)$n_employees[is.na(V(comp1)$n_employees)] <- 0
V(comp1)$revenue[is.na(V(comp1)$revenue)] <- 0
V(comp1)$total_assets[is.na(V(comp1)$total_assets)] <- 0

assortativity(comp1, values = V(comp1)$n_employees)
assortativity(comp1, values = V(comp1)$revenue)
assortativity(comp1, values = V(comp1)$total_assets)

# Assortativity for kategorielle variable----

# Vi kan også have en kategoriel egenskab, som fx geografi
      # her skal man lige huske at variablen skal behandles som en factor
assortativity_nominal(comp1, types = factor(V(comp1)$affiliation_country))

# En anden kategoriel variable kunne være Global Ultimate Owner (guo):
      # Selskaber der ikke er en del af en koncern har NA på guo, der sætter vi navnet ind i stedet
V(comp1)$guo_name[is.na(V(comp1)$guo_name)] <- V(comp1)$name[is.na(V(comp1)$guo_name)]

assortativity_nominal(comp1, types = factor(V(comp1)$guo_name))
