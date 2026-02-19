###############################
#                              
#  Øvelse 3         
#                              
###############################


# Libraries 
library(tidyverse)
library(igraph)
library(ggraph)
library(tidygraph)
library(Matrix)

#===========================================================================

######################################################################/
# 0. Load data # brug evt. datasættet "pharma.csv", der ligger i data/
######################################################################/



###############################################################################/
# 1. Subset data, så vi kun kigger på personer med mere end 1 bestyrelsespost  /
###############################################################################/
# METODE:
# lav en ny variabel i datasættet der tæller antallet medlemskaber for hvert individ (name):
# funktionen group_by('variabelnavn') grupperer data efter en variabel;
# funktionen mutate('ny variable' = ET ELLER ANDET) tilføjer eller ændrer noget i data: vi vil gerne lave en variabel vi kalder n_memberships
# når vi skal vide hvor mange unikke affiliations hvert individ har kan vi bruge n_distinct()
# med count() kan vi se resultatet


###############################################/
# 2. Lav først matricer og dernæst grafobjekt
# et grafobjekt for virksomhed (affiliation) x virksomhed (affiliation) netværket
###############################################/


# først data <- xtabs() 


# og så t(data) %*% data


# og så et igraph objekt 'graph_from_adjacency_matrix()'

# som vi laver til tidygraph 'as_tbl_graph()'


###################################/
# 3. Visualiser netværk
###################################/
# lav en simpel visualisering af netværket
  # tilpas koden fra vores tidligere visualiseringer af netværk

net %>% ggraph("fr")  +
  geom_edge_link0() +   # tegner edges...
  geom_node_point() +   # tegner nodes
  theme_graph()

###################################/
# 4. netværksmål:
# edge_density(net) 
# transitivity(net) 
# diameter(net)
# mean_distance(net)
# 
# 4.a: Hvor mange komponenter består netværket af? Hvor stor er den største komponent.
net <- net %>% 
  activate(nodes) %>% 
  mutate(comp = group_components())

net %>% as_tibble() %>% count(comp)

# 4.b lav et nyt netværksobjekt, med kun den største komponent: (brug filter og vælg comp == 1)


# 4.c: Udregn: 1) density, 2) transitivity, og 3) average distances for den største komponent. Snak om hvad det betyder... Får I fx en høj transitivitet?? Hvad betyder det?

# 4.d Hvad er diameteren for den største kompoment? Hvad betyder det?
