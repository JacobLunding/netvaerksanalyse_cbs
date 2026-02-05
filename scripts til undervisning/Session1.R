##########################/
#
#  Øvelse 1: Introduktion til netværksanalyse 
#
###########################/

######################/
# 1. Setting up -----
######################/


# Først skal vi have installeret de pakker vi kommer til at bruge (skal kun gøres første gang)
install.packages("tidyverse")
install.packages("ggraph")
install.packages("igraph")
install.packages("Matrix")

# Dernæst indlæser vi pakkerne
library(tidyverse)
library(tidygraph)
library(ggraph)
library(igraph)
library(Matrix)

################################/
# 2. Indlæs og udfors data -----
################################/

# Vi indlæser dernæst data med read_csv() | fordi vi er i et R-project mappe, opfatter R denne mappe som working directory, 
# så vi behøver ikke give hele stien. Vi kan nøjes med at fortælle at den ligger i data-mappen:

den <- read_csv("data/danish_elitenetworks2024.csv")

# Lad os ligge lidt på data
den %>% glimpse()

# Count
den %>% count(org_sector, sort = TRUE)
den %>% distinct(AFFILIATION, org_tags, org_tags_full, first.tag) %>% count(org_tags, org_tags_full, first.tag, sort = TRUE) %>% View()

#######################/
# 3. Subset data -----
#######################/

# Lad os lave to forskellige variable, som vi kan bruge til at finde landbrugsnetværket
den <- den %>% mutate(landbrug_org  = str_detect(org_tags_full, regex("landbrug", ignore_case = TRUE)))
den <- den %>% group_by(NAME) %>% mutate(landbrug_pers = any(landbrug_org)) %>% ungroup()


# Filter
den_corp <- den %>% 
  filter(landbrug_pers == TRUE) 




den_corp %>% slice_sample(n = 10)

# Når nu vi kun har landbrugsforaer, kan vi kigge på hvilke brancher der optræder i data
den_corp %>% distinct(AFFILIATION, org_cvr_branche) %>% count(org_cvr_branche, sort = TRUE)

# select() 
den_corp <- den_corp %>% select(NAME, AFFILIATION, Gender, person_leader)

# Lad os lave en variabel der indikerer at en person har en ledende post (direktør eller bestyrelsesformand) i et eller andet
den_corp <- den_corp %>% 
  group_by(NAME) %>% 
  mutate(person_leader = any(person_leader == TRUE)) %>% 
  ungroup()
# Og lad os filtrere på den, så vi kun har folk, der har en ledende post
den_corp <- den_corp %>% filter(person_leader == TRUE)

den_corp %>% count(Gender)

den_corp %>% filter(is.na(Gender)|Gender == "Binominal") %>% distinct(NAME, Gender) %>% head()

# Omkodning!!
den_corp <- den_corp %>% mutate(Gender = case_when(NAME == "Marc-Dominique Prikazsky 57958"~"Men", 
                                                   NAME == "H. C. Gæmelke 59543"~"Men",
                                                   NAME == "Laury Kristoffersen 22938"~"Men",
                                                   NAME == "Rune-Christoffer Dragsdahl 59891"~"Men",
                                                   NAME == "Tina-Henriette Kristiansen 66185"~"Women",
                                                   NAME == "Valérie, Claire, Aline Mazeaud 57956"~"Women",
                                                   .default = Gender))

den_corp %>% count(Gender)








#########################/
# 4. Adjacency matricer ----
#########################/


# biadjacency matrice individer ('name') i rækker og organisationer ('affiliation') i kolonner. 
# sparte = TRUE fordi det er et megt stort data
den_corp_bi <- den_corp %>% xtabs(formula = ~NAME + AFFILIATION, sparse = TRUE)
den_corp_bi

# adjacency matrix for rækkerne (indvid x individ)
den_corp_ind <- den_corp_bi %*% t(den_corp_bi)

# adjacency matrix for kolonnerne (organisation x organisation)
den_corp_org <- t(den_corp_bi) %*% den_corp_bi


##################################################/
# 5. Netværksobjekter ----
##################################################/


##############################################/
# individ x individ                          #/
##############################################/

#Igraph
g_ind <- den_corp_ind %>% graph_from_adjacency_matrix(mode = "undirected", weighted = TRUE, diag = FALSE) 
g_ind
#Tidygraph
g_ind <- g_ind %>% as_tbl_graph()
g_ind

##############################################/
# organisation x organisation                #/
##############################################/

#Igraph
g_org <- den_corp_org %>% graph_from_adjacency_matrix(mode = "undirected", weighted = TRUE, diag = FALSE) 
g_org
#Tidygraph
g_org <- g_org %>% as_tbl_graph()
g_org

##############################################/
# (Bipartite) netværk individ x organisation #/
##############################################/

#Igraph
g_bi <- den_corp_bi %>% graph_from_biadjacency_matrix(weighted = NULL)
g_bi
#Tidygraph
g_bi <- g_bi %>% as_tbl_graph()
g_bi



##################################################/
# 6. Netværksvisualisering med ggraph ----
##################################################/
# individ x organisations netværket
g_bi_l <- largest_component(g_bi)


ggraph(g_bi_l, layout = "fr") +
  geom_edge_link0(edge_alpha = 0.8, edge_width = 0.1, color = "black") +
  geom_node_point(aes(color = type), size = 1, alpha = 0.6) +
  scale_color_manual(values=c("sienna1", "steelblue2"), labels=c("individuals", "companies")) +
  theme_graph() 

##############################/
# individ projektionen
##############################/
g_ind <- g_ind %>% activate(nodes) %>% 
  left_join(den_corp %>% select(NAME, Gender, person_leader) %>% distinct(), by = c("name" = "NAME"))

g_ind <- g_ind %>% 
  activate(nodes) %>% 
  mutate(comp = group_components())
  
g_ind %>% activate(nodes) %>% as_tibble() %>% count(comp)

g_ind_l <- g_ind %>% filter(comp == 1)  
  
g_ind_l <- g_ind_l %>% activate(nodes) %>% 
  mutate(betweenness = centrality_betweenness(), betweenness_rank = dense_rank(desc(betweenness)))

ggraph(g_ind_l, layout = "stress") +
  geom_edge_link0(edge_alpha = 0.2, edge_width = 0.1) +
  geom_node_point(mapping = aes(filter= betweenness_rank <=10, size = betweenness), color = "sienna1", show.legend = FALSE) + 
  geom_node_label(mapping = aes(filter= betweenness_rank <=10, label = name), repel = TRUE) +
  theme_graph() 

##############################/
# organisations projektionen
##############################/
g_org <- g_org %>% 
  activate(nodes) %>% 
  mutate(comp = group_components())
  
g_org %>% activate(nodes) %>% as_tibble() %>% count(comp)
  
g_org_l <- g_org %>% filter(comp == 1)


g_org_l <- g_org_l %>% activate(nodes) %>% 
  mutate(betweenness = centrality_betweenness(), betweenness_rank = dense_rank(desc(betweenness)))

ggraph(g_org_l) +
  geom_edge_link0(edge_alpha = 0.3, edge_width = 0.1) +
  geom_node_point(size = 0.8) + 
  geom_node_point(mapping = aes(filter= betweenness_rank <=10, size = betweenness), color = "sienna1", show.legend = FALSE) + 
  geom_node_label(mapping = aes(filter= betweenness_rank <=10, label = name), repel = TRUE) +
  theme_graph() 

