
###############################
#                              
#  Øvelse 2            
#                              
###############################


# Libraries 
library(tidyverse)
library(igraph)
library(ggraph)
library(graphlayouts)
source("functions/networkfunctions.R")

#===========================================================================

######################################################################/
# 0. Load data # brug evt. datasættet "pharma.csv", der ligger i input/
######################################################################/



###############################################################################/
# 1. Subset data, så vi kun kigger på personer med mere end 1 bestyrelsespost  /
###############################################################################/

# lav en ny variabel i datasættet der tæller antallet medlemskaber for hvert individ (name):
# funktionen group_by('variabelnavn') grupperer data efter en variabel; her name
# funktionen mutate('ny variable' = ET ELLER ANDET) tilføjer eller ændrer noget i data: vi vil gerne lave en variabel vi kalder n_memberships
# når vi skal vide hvor mange unikke affiliations hvert individ har kan vi bruge n_distinct()



###################################/
# 2. Lav grafobjekt
###################################/

# lav et grafobjekt for virksomhed (affiliation) x virksomhed (affiliation) netværket
  # Husk de to muligheder vi så på i dag "scripts/03-r-script.R"
  

###################################/
# 3. Visualiser netværk
###################################/
  
# lav en simpel visualisering af netværket
  # tilpas koden fra vores tidligere visualiseringer af netværk
  
comp1 %>% ggraph() +
  # add edges...
  geom_edge_link0() +
  # add nodes
  geom_node_point() +
  theme_graph()


###################################/
# 4. netværksmål:
# components()
# edge_density() 
# transitivity() 
# mean_distance() 
# diameter()
###################################/  
  
# 4.a: Hvor mange komponenter består netværket af? Hvor stor er den største komponent.


# 4.b lav et nyt netværksobjekt, med kun den største komponent:
      #brug get_n_largest_component fra "netværksfunktioner.R" scriptet


# 4.c: Udregn: 1) density, 2) transitivity, og 3) average distances for den største komponent. Snak om hvad det betyder... Får I fx en høj transitivitet??

# 4.d Hvad er diameteren for den største kompoment?

# 4.e gem oplysninger om den længste sti som attributes i grafobjektet: Hint! brug get.diameter() funktionen fra igraph og hhv set.vertex.attribute() og set.edge.attribute() 

# 4.f visualiser stien i netværket
 # udfyld plotfunktionen med de forskellige lag

comp1 %>% 
  ggraph(layout = "fr") +
  # først alle edges der ikke indgår i diameteren brug den diameter attribute i har lavet, som filter: dvs 
  # filter=diameter == FALSE
  geom_edge_link0(aes(filter= xxxxxxxxxxxxxxxx), color = "gray60") + 
  # dernæst alle noder der ikke ligger på stien
  geom_node_point(aes(filter= xxxxxxxxxxxxxxxx), color = "black") +
    # så alle edges i diameteren 
  geom_edge_link0(aes(filter= xxxxxxxxxxxxxxxx), color = "red", width = 1.5) +
  # så de noder der ligger på stien
  geom_node_point(aes(filter= xxxxxxxxxxxxxxxx), color = "darkred", size =2) +
  # til sidst lables på de noder der ligger på stien, brug igen diameter attributten til filter og name atributten til labels
  geom_node_label(aes(filter= xxxxxxxxxxxxxxxx, label = yyyy), nudge_y = -0.3, size =2.5, repel = TRUE) + 
  labs(title = 'xxxxxxxxxxxxxxx') +
  theme_graph() 


# 4.g prøv at finde den korste vej mellem to udvalgte firmaer: fx. "Novo Nordisk A/S" og en anden stor pharma virksomhed: "JOHNSON & JOHNSON" eller "ELI LILLY AND COMPANY" eller "MERCK & CO., INC." eller "SANOFI"
 
# Hvor langt er der og hvilke firmaer skal man igennem

 # Visualiser denne sti på samme måde som diameteren 
