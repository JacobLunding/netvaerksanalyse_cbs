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
library(igraph)
library(ggraph)
library(ggplot2)
library(ggpubr)
library(Matrix)
source("functions/networkfunctions.R")




# 2. Indlæs data ----
# brug igen pharma datasættet, som ligger i data



# 3. subset datasættet ----
# så vi kun kigger på virksomheder med mere end 1 i boardet og hvor data om omsætningen (revenue) ikke er na, og kun personer, der har poster i mere end en koncern:
# til det skal vi først:
# først lige sørge for at der ikke er række duplicates samme person i samme virksomhed.
# lave en ny variabel n_ind som tæller [n_distinct] unikke navne for hver affiliation
# lave en ny variabel n_afil som tæler [n_distinct] unikke affilialtions for hver person
# 



# 4. Lav et virksomhed x virksomhed netværk ----
# dvs. lav først biadjacency m. xtabs og dernæst adjacency m. t(B) %*% B og tilsidst graph_from_adjacency_matrix


# 5. Se på grafens komponentstruktur og lav et nyt grafobjekt med den størte komponent ----



# 6. Udregn forskellige centralitets mål ----
# og gem dem som vertex attributes i grafobjektet
# og lave en tibble med vertices og vertex attributes fra grafobjektet:


# 7. Tilføj en rankvariable for hver af centralitetsmålene

# 8. Åben jeres tibble med centralitetsmål i Vieweren ----
# ... og kig lidt på hvilke virksomheder der ligger i toppen på de forskellige centralitetsmål



# 9. Prøv at lave forskellige visualiseringer. ---- 
# sæt color og size på noderne efter målene..


# 10. EKSTRAOPGAVE: Prøv at lave en simpel regression mellem betweenness og omsætning: (lm)
