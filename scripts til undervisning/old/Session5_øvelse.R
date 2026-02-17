##########################
#
#  Øvelse 5: Node centralitet og similaritet
#
###########################


# SETTING UP --------------------------------------------------------------

# Indlæs de nødvendige pakker
library(tidyverse)
library(ggraph)
library(igraph)
library(graphlayouts)
library(ggpubr)
source("functions/custom_functions.R")
source("functions/networkfunctions.R")

###################################################################################################/
# 1. Indlæs data ----
###################################################################################################/

den <- read_csv("input/den17-no-nordic-letters.csv")

###################################################################################################/
# 2. Subset og behandle ----
    # has.tags() til at udvælge specifikke brancher/områder mv.  
    # [med filter()], så vi kun kigger på virksomheder
###################################################################################################/

# udvælg nogle tags, fx. ENER og Energy
tags <- c("", "")


# lav et ny datasæt, kald det data, ved at bruge has.tags()  funktionen
data <- has.tags(   .....    ) 

###################################################################################################/
# 3. Definerer et netværksobjekt for affiliations ----
###################################################################################################/

# lav en sparse incidence matrice name x affiliation: 
# (..., formula = ~ name + affiliation) giver en incidence matrice med name (individer) i rækker og affiliation (virksomheder) i kolonner  
bi_adj <- xtabs(  ..... )

# lav affiliation x affiliation adjacency matricen: 
# brug matrix multiplikation t(x) %*% x 
adj_virk  <- #??? 


# lav netværks objektet
gr_virk <- graph_from_adjacency_matrix( .... ) %>% simplify

###################################################################################################/
# 4. Netværkets komponenter ----
###################################################################################################/

# gem den største komponent
comp <- gr_virk %>% largest_component()

###################################################################################################/
# 5. Tilføj netværkseksterne node attributes til netværket ----
###################################################################################################/

comp <- add_vertex_attr(graph = comp, 
                          data =  den_energi %>% 
                            select(affiliation, sector),
                          match_var = "affiliation")


###################################################################################################/
# 6. Komponent visualisering ----
###################################################################################################/

p <- (gr_virk - which(degree(gr_virk) == 0)) %>% ggraph("fr") +
  geom_edge_link0(width = 0.3, alpha = 0.3) +
  geom_node_point(size = .5) + ggtitle(paste0("xxxxxxxxxxxxxxxxxxxxx (n=", vcount(gr_virk), ")")) +
  theme_graph(base_family = "serif")
p1 <- comp %>% ggraph("fr") +
  geom_edge_link0(width = 0.3, alpha = 0.3) +
  geom_node_point(aes(color = sector), size =2) + ggtitle(paste0("Største komponent (n=", vcount(comp), ")")) + guides(color = guide_legend("Sector")) +
  theme_graph(base_family = "serif")

ggarrange(plotlist = list(p, p1))



##################################################################################/
# Øvelser ----
# 1) udregn og gem forskellige centralitetsmål for den største komponent
# degree, betweenness, closeness og constraint/brokerage og local transitivity
# 2) diskuter de forskellige mål ...
##################################################################################/


