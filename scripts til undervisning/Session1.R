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
den %>% count(affiliation_sektor, sort = TRUE)
den %>% distinct(affiliation, affiliation_tags) %>% count(affiliation_tags, sort = TRUE) %>% View()

#######################/
# 3. Subset data -----
#######################/

# Lad os lave to forskellige variable, som vi kan bruge til at finde landbrugsnetværket
den <- den %>% mutate(landbrug_org  = str_detect(affiliation_tags, regex("landbrug", ignore_case = TRUE)))
den <- den %>% group_by(person_name) %>% mutate(landbrug_pers = any(landbrug_org)) %>% ungroup()


# Filter
den_corp <- den %>% 
  filter(landbrug_pers == TRUE) 




den_corp %>% slice_sample(n = 10)

# Når nu vi kun har landbrugsforaer, kan vi kigge på hvilke brancher der optræder i data
den_corp %>% distinct(affiliation, affiliation_branche_niveau5) %>% count(affiliation_branche_niveau5, sort = TRUE)

# select() 
den_corp <- den_corp %>% select(person_name, affiliation, person_køn, position_leader)

# Lad os lave en variabel der indikerer at en person har en ledende post (direktør eller bestyrelsesformand) i et eller andet
den_corp <- den_corp %>% 
  group_by(person_name) %>% 
  mutate(person_leader = any(position_leader == TRUE)) %>% 
  ungroup()
# Og lad os filtrere på den, så vi kun har folk, der har en ledende post
den_corp <- den_corp %>% filter(person_leader == TRUE)

den_corp %>% count(person_køn)

den_corp %>% filter(is.na(person_køn)|person_køn == "Binominal") %>% distinct(person_name, person_køn) %>% head()

# Omkodning!!
den_corp <- den_corp %>% mutate(person_køn = case_when(person_name == "Marc-Dominique Prikazsky 57958"~"Men", 
                                                       person_name == "H. C. Gæmelke 59543"~"Men",
                                                       person_name == "Laury Kristoffersen 22938"~"Men",
                                                       person_name == "Rune-Christoffer Dragsdahl 59891"~"Men",
                                                       person_name == "Tina-Henriette Kristiansen 66185"~"Women",
                                                       person_name == "Valérie, Claire, Aline Mazeaud 57956"~"Women",
                                                  .default = person_køn))

den_corp %>% count(person_køn)








#########################/
# 4. Adjacency matricer ----
#########################/


# biadjacency matrice individer ('name') i rækker og organisationer ('affiliation') i kolonner. 
# sparte = TRUE fordi det er et megt stort data
den_corp_bi <- den_corp %>% xtabs(formula = ~person_name + affiliation, sparse = TRUE)
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
  left_join(den_corp %>% select(person_name, person_køn, person_leader) %>% distinct(), by = c("name" = "person_name"))

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



## Hvis nu vi gerne vil kunne se alle de poster en person har kan vi lave en ny variabel i den_corp, der indeholde alle poster: dvs. vi grupperer på person_name og opsummerer - 'summarise()' - data, så hver bliver til en række og en ny variabel 'memberships' indeholder en sammenlægning - 'paste0(..., collapse = "|")' - af det der før stod i flere rækker i affiliation. Adskilt af separatoren "|").
memberships <- den_corp %>% 
  group_by(person_name) %>% 
  summarise(memberships = paste0(affiliation, collapse = " | "))

# den variabel kan vi joine på vores netværksdata objekt. med leftjoin
g_ind_l <- g_ind_l %>% 
  activate(nodes) %>% 
  left_join(memberships, by = c("name" = "person_name"))

# Hvis vi derefter udtrækker node dataen 'activte(nodes)' som et datasæt (med 'as_tibble') kan vi sortere det efter betweenness rank og inspisere det ('View') for at se hvilke poster de mest centrale personer har.
g_ind_l %>% 
  activate(nodes) %>% 
  as_tibble() %>% 
  arrange(betweenness_rank) %>% View()
