##############  #Lektion03 ################
# 
# Virksomhedsstrategi i et netværksperspektiv 
# Centralitetsmål - øvelse
#
###########################################

# 1. Indlæs (library) nødvendige pakker: ----
# vi skal bruge tidyverse, som altid
# igraph til grafobjekter og centralitetsmål
# ggraph og ggplot2 og ggpubr til grafplots
# og "functions/networkfunctions.R" som skal sources
library(tidyverse)
library(tidygraph)
library(igraph)
library(ggraph)
library(ggplot2)
library(ggpubr)
library(Matrix)
library(readxl)
source("functions/read_orbis.R")

##################################/
# Vælg et af følgende datasæt 
# ELLER
# indlæs noget andet
##################################/

#Nordisk pharma
download.file("https://jacoblunding.github.io/netvaerksanalyse_cbs/data/nordic_pharma2025.xlsx", "data/nordic_pharma2025.xlsx")
dt <- read_orbisxlsx("data/nordic_pharma2025.xlsx")
#Europæiske biler
download.file("https://jacoblunding.github.io/netvaerksanalyse_cbs/data/Cardata.xlsx", "data/Cardata.xlsx")
dt <- read_orbisxlsx("data/Cardata.xlsx")
#Nordisk Elektricitet
download.file("https://jacoblunding.github.io/netvaerksanalyse_cbs/data/nordic_electricity.xlsx", "data/nordic_electricity.xlsx")
dt <- read_orbisxlsx("data/nordic_electricity.xlsx")
#Shipping world-wide
download.file("https://jacoblunding.github.io/netvaerksanalyse_cbs/data/shipping.xlsx", "data/shipping.xlsx")
dt <- read_xlsx("data/shipping.xlsx")

# Antal poster per individ
dt <- dt %>% 
  group_by(person_id) %>% 
  mutate(n_memberships = n_distinct(affiliation)) %>% 
  ungroup()
# Antal individer per bestyrelse
dt <- dt %>% 
  group_by(affiliation) %>%
  mutate(n_members = n_distinct(person_id)) %>% 
  ungroup()

## Byg filter overvej hvilke kriterier I vil bruge
dt <- dt %>% 
  filter(role_status == "Current") %>% 
  filter(person) %>% 
  filter(n_members > 1) %>% 
  filter(n_memberships >1)


##################################/
# NETVÆRK
##################################/

bi_adj <- xtabs(data = dt, formula = ~name + affiliation, sparse = T)

# Hvilket netværk vil I lave, individer eller virksomheder?
# 
# Jeg vil gerne kigge på virksomhederne... så jeg vælger t(x) %*% x
# 
adj_virk <- t(bi_adj) %*% bi_adj

# Ud fra det laver jeg mit igraph objekt, med graph_from_adjacency_matrix() hvor jeg simplifier det
# 

gr_virk <- graph_from_adjacency_matrix(adj_virk) %>% simplify()
# og laver det til et tidygraph objekt
gr_virk <- gr_virk %>% as_tbl_graph()

# Jeg vil gerne finde den største komponent, så jeg laver en variabel i gr_virk, der fortæller hvilken komponent, hver node tilhører
# 
gr_virk <- gr_virk %>% 
  mutate(comp = group_components())

# Med den kan jeg filtrere netværket, så jeg kun ar den størrste komponent
#
gr_virk <- gr_virk %>% filter(comp == 1)

##################################/
# ANALYSE
# beregn forskellige centralitetsmål
##################################/

gr_virk <- gr_virk %>% mutate(degree = centrality_degree(),
                   betweenness = centrality_betweenness(),
                   betweenness_norm = centrality_betweenness(normalized = T),
                   closeness = centrality_closeness(),
                   eigencentrality = centrality_eigen(),
                   degree_rnk = degree %>% desc() %>% dense_rank(),
                   betweenness_rnk = betweenness %>% desc() %>% dense_rank(),
                   closeness_rnk = closeness %>% desc() %>% dense_rank(),
                   eigencentrality_rnk = eigencentrality %>% desc() %>% dense_rank())


gr_virk %>% as_tibble(active = "nodes") %>% View()



gr_virk %>% ggraph() +
  geom_edge_link0(width = 0.3, alpha = 0.3) +
  geom_node_point(aes(color = betweenness_norm, size = betweenness_norm)) +
  geom_node_label(aes(filter = betweenness_rnk < 10, label = name), repel = T, force = 10) +
  guides(size = "none") +
  theme_graph()
