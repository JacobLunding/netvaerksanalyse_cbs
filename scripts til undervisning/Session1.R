##########################/
#
#  Øvelse 1: Introduktion til netværksanalyse 
#
###########################/

######################/
# 1. Settuing up -----
######################/


# Først skal vi have installeret de pakker vi kommer til at bruge (skal kun gøres første gang)
install.packages("tidyverse")
install.packages("ggraph")
install.packages("igraph")
install.packages("Matrix")

# Dernæst indlæser vi pakkerne
library(tidyverse)
library(ggraph)
library(igraph)
library(Matrix)

################################/
# 2. Indlæs og udfors data -----
################################/

# Vi indlæser dernæst data med read_csv() | fordi vi er i et R-project mappe, opfatter R denne mappe som working directory, 
# så vi behøver ikke give hele stien. Vi kan nøjes med at fortælle at den ligger i data-mappen:

den <- read_csv("data/den17-no-nordic-letters.csv")


# Lad os ligge lidt på data
den %>% glimpse()

# Count
den %>% count(sector, sort = TRUE)
den %>% distinct(affiliation, sector) %>% count(sector, sort = TRUE)

#######################/
# 3. Subset data -----
#######################/


# Filter
den_corp <- den %>% 
  filter(sector == "Corporations") 


den_corp %>% slice_sample(n = 10)

# select() 
den_corp <- den_corp %>% select(name, affiliation) 

#########################/
# 4. Adjacency matricer ----
#########################/


# biadjacency matrice individer ('name') i rækker og organisationer ('affiliation') i kolonner. 
# sparte = TRUE fordi det er et megt stort data
den_corp_bi <- den_corp %>% xtabs(formula = ~name + affiliation, sparse = TRUE)
den_corp_bi

# adjacency matrix for rækkerne (indvid x individ)
den_corp_ind <- den_corp_bi %*% t(den_corp_bi)

# adjacency matrix for kolonnerne (organisation x organisation)
den_corp_org <- t(den_corp_bi) %*% den_corp_bi


##################################################/
# 5. Netværksobjekter ----
##################################################/


#####################/
# individ x individ #/
#####################/

g_ind <- den_corp_ind %>% graph_from_adjacency_matrix(mode = "undirected", weighted = TRUE, diag = FALSE)
g_ind

V(g_ind)$name %>% head(20)
E(g_ind)$weight %>% head(20)
E(g_ind)$weight %>% table()

################################/
# organisation x organisation  #/
################################/

g_org <- den_corp_org %>% graph_from_adjacency_matrix(mode = "undirected", weighted = TRUE, diag = FALSE)
g_org

V(g_org)$name %>% head(20)
E(g_org)$weight %>% head(20)
E(g_org)$weight %>% table()

##############################################/
# (Bipartite) netværk individ x organisation #/
##############################################/

g_bi <- den_corp_bi %>% graph_from_biadjacency_matrix()


##################################################/
# 6. Netværksvisualisering med ggraph ----
##################################################/
# individ x organisations netværket
g_bi_l <- largest_component(g_bi)
ggraph(g_bi_l, layout = "fr") +
  geom_edge_link0(edge_alpha = 0.8, edge_width = 0.1, color = "black") +
  geom_node_point(aes(color = type), size = 0.2, alpha = 0.6) +
  scale_color_manual(values=c("sienna1", "steelblue2"), labels=c("individuals", "companies")) +
  theme_graph() 

# individ projektionen
g_ind_l <- largest_component(g_ind) 
ggraph(g_ind_l, layout = "fr") +
  geom_edge_link0(edge_alpha = 0.2, edge_width = 0.1) +
  geom_node_point(size = 0.2) +
  theme_graph() 

# organisations projektionen
g_org_l <- largest_component(g_org) 
ggraph(g_org_l, layout = "fr") +
  geom_edge_link0(edge_alpha = 0.6, edge_width = 0.1) +
  geom_node_point(size = 0.1) + 
  theme_graph() 

