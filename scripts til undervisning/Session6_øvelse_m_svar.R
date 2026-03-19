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
###################################################################################################/
# 1. Læs datafil ----
###################################################################################################/

den <- read_csv("data/danish_elitenetworks2024.csv")
###################################################################################################/
# 2. Subset og omkod data m.m. ----
###################################################################################################/

# Først laver vi en 'er_landbrug' variabel ud fra branchekode og tags
den <- den %>%
  mutate(landbrug =
           case_when(
             affiliation_branche_niveau1 == "Landbrug, jagt, skovbrug og fiskeri" | grepl("landbrug", affiliation_tags, ignore.case = T)~TRUE,
             .default = FALSE))

# Dernæst laver vi en 'er_landmand' variabel, hvor vi for hver person *group_by()* 'spørger' om vedkommende har nogen (any) positioner i min én *any()* landbrugsting, hvor de også er leder.
den <- den %>% group_by(person_name) %>% mutate(er_landmand = any(landbrug & position_leader))

# Det filter anvender vi og reducerer data til kun at indeholde 'landbrugsledere'
den_land <- den %>% filter(er_landmand)

# Dernæst laver vi to variable; en der tæller hver persons antal medlemskaber og en der tæller hver bestyrelses antal medlemmer
den_land <- den_land %>% group_by(person_name) %>% mutate(memberships = n_distinct(affiliation_orig_name))
den_land <- den_land %>% group_by(affiliation_orig_name) %>% mutate(members = n_distinct(person_name))

# De to variable bruger vi som det næste filter, så vi kun har bestyrelser med mindre end 30 medlemmer og personer med mere end 1 post
den_land <- den_land %>% filter(members < 30 & memberships > 1 & !grepl("Events eller begivenheder", affiliation_tags))

###################################################################################################/
# 3. Definerer et netværksobjekt  ----
###################################################################################################/

# lav en sparse incidence matrice name x affiliation: 
# (..., formula = ~ name + affiliation) giver en incidence matrice med name (individer) i rækker og affiliation (virksomheder) i kolonner  
bi_adj <- xtabs(data = den_land, formula = ~person_name + affiliation, sparse = T)


# lav individ x individ adjacency matricen: 
# brug matrix multiplikation:  incidence %*% Matrix::t(incidence)
adj_ind  <- bi_adj %*% t(bi_adj)
adj_comp  <- t(bi_adj) %*% bi_adj

# lav netværks objektet
net <- graph_from_adjacency_matrix(adjmatrix =adj_ind , mode = "undirected") %>% simplify() %>% as_tbl_graph()
net_c <- graph_from_adjacency_matrix(adjmatrix =adj_comp , mode = "undirected") %>% simplify() %>% as_tbl_graph()

###################################################################################################/
# 4. Netværkets komponenter? ----
###################################################################################################/

net <- net %>% mutate(comp = group_components())
net_c <- net_c %>% mutate(comp = group_components())

net %>% as_tibble() %>% count(comp)
net_c %>% as_tibble() %>% count(comp)

net_1 <- net %>% filter(comp == 1)
net_c1 <- net_c %>% filter(comp == 1)
###################################################################################################/
# 5. Tilføj netværkseksterne node attributes til netværket ----
###################################################################################################/

net_1 <- net_1 %>% left_join(den_land %>% ungroup %>% select(person_køn, person_postdistrikt, person_name) %>% distinct(), by = c("name" = "person_name"))

###################################################################################################/
# 6. Komponent visualisering ----
###################################################################################################/

p1 <- net_1 %>% ggraph("kk") +
  geom_edge_link0(color = "grey35", width = 0.2, alpha = 0.2) +
  geom_node_point(size = 1.5, aes(color = person_køn)) + ggtitle(paste0("Største komponent (n=", vcount(net_1), ")")) +
  theme_graph(base_family = "serif")


p1

###################################################################################################/
# 7. Netværks mål ----
###################################################################################################/


###################################################################################################/
# 8.1 Centralitetsmål mv. ----
###################################################################################################/

net_1 <- net_1 %>% mutate(   )

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

net_1 <- net_1 %>% mutate(cl_greedy = group_fast_greedy(),
                                  cl_louvain = group_louvain(),
                                  cl_leiden = group_leiden(),
                          cl_walk = group_walktrap())

net_c1 <- net_c1 %>% mutate(cl_greedy = group_fast_greedy(),
                          cl_louvain = group_louvain(),
                          cl_leiden = group_leiden())


d <- net_1 %>% as_tibble() 
table(d$cl_louvain, d$cl_greedy)
net_c1 %>% with_graph(graph_modularity(cl_louvain))

p1 <- net_1 %>% ggraph("stress") +
  geom_edge_link0(color = "grey35", width = 0.2, alpha = 0.2) +
  geom_node_point(size = 1.5, aes(color = factor(cl_greedy))) + ggtitle(paste0("Største komponent (n=", vcount(net_1), ")")) +
  theme_graph(base_family = "serif")
p1


net_1 %>% as_tibble() %>% View()
