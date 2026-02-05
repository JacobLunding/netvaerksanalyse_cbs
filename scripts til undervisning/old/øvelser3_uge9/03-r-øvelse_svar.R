
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
library(readxl)
source("functions/networkfunctions.R")

#===========================================================================

######################################################################/
# 0. Load data # brug evt. datasættet "pharma.csv", der ligger i input/
######################################################################/
# pharma <- readxl::read_xlsx("input/world_pharma.xlsx", sheet = 2, guess_max = 1000000)
# 
# pharma <- pharma %>% rename(affiliation = `Company name Latin alphabet`,
#                   affil_country = `Country ISO code`,
#                   affil_id1 = `BvD ID number`,
#                   affil_id2 = `Orbis ID number`,
#                   affil_dir_n = `Number of directors & managers`,
#                   affil_c_dir_n = `Number of current directors & managers`,
#                   affil_rev = `Operating revenue (Turnover)\nth USD Last avail. yr`,
#                   affil_emp = `Number of employees\nLast avail. yr`,
#                   name = `DMFull name`,
#                   name_id = `DMUCI (Unique Contact Identifier)`,
#                   role = `DMJob title (in English)`,
#                   appointment = `DMAppointment date`,
#                   resignation = `DMResignation date`,
#                   role_type = `DMType of role`,
#                   role_type2 = `DMBoard, committee or department`,
#                   role_level = `DMLevel of responsibility`,
#                   guo = `GUO - Name`,
#                   guo_id = `GUO - Orbis ID number`)
# 
# 
# pharma <- pharma %>% group_by(affil_id2) %>% mutate(guo2 = first(guo), guo2_id = first(guo_id))
# 
# pharma <- pharma %>% group_by(affiliation) %>% mutate(affil_guo = case_when(affiliation != guo2 ~ paste0(affiliation, " (GUO: ", guo2, ")"), .default = affiliation))
# pharma <- pharma %>%  mutate(affiliation_old = affiliation, affiliation = affil_guo)
# 
# 
# pharma <- pharma %>% filter(grepl("BoD|ExeC", role_type) & role_level != "Representative")
# pharma <- pharma %>% select(name, role, affiliation, guo2) %>% rename(guo = guo2)
# pharma <- pharma %>% mutate(guo = case_when(is.na(guo) ~affiliation, .default = guo))
# pharma <- pharma %>% mutate(guo = case_when(guo == "GOVERNMENT OF CHINA" ~affiliation, .default = guo))
# pharma %>% select(name, affiliation, guo) %>% distinct() %>% write.csv(., "input/pharma.csv")

pharma <- read_csv("input/pharma.csv")

###############################################################################/
# 1. Subset data, så vi kun kigger på personer med mere end 1 bestyrelsespost  /
###############################################################################/

# lav en ny variabel i datasættet der tæller antallet medlemskaber for hvert individ (name):
# funktionen group_by('variabelnavn') grupperer data efter en variabel; her name
# funktionen mutate('ny variable' = ET ELLER ANDET) tilføjer eller ændrer noget i data: vi vil gerne lave en variabel vi kalder n_memberships
# når vi skal vide hvor mange unikke affiliations hvert individ har kan vi bruge n_distinct()

pharma <- pharma %>% group_by(name) %>% 
  mutate(n_memberships = n_distinct(affiliation))
  
pharma <- pharma %>% filter(n_memberships > 1)

###################################/
# 2. Lav grafobjekt
###################################/

# lav et grafobjekt for virksomhed (affiliation) x virksomhed (affiliation) netværket
# Husk de to muligheder vi så på i dag "scripts/03-r-script.R"

#Option1
incidence <- xtabs(formula = ~ name + affiliation, data = pharma, sparse = T)
adj_c     <- incidence %>% incidence_to_adjacency(mode = "col")
gr_c      <- adj_c %>% graph_from_adjacency_matrix(mode = "undirected") %>% simplify()
#Option2
gr   <- pharma %>% select(name, affiliation) %>% graph_from_data_frame(directed = FALSE)
gr   <- gr %>% set_vertex_attr(name = "type", value = V(gr)$name %in% pharma$name) 
gr_c <- gr %>% bipartite_projection(which = "false", multiplicity = FALSE)


###################################/
# 3. Visualiser netværk
###################################/

# lav en simpel visualisering af netværket
# tilpas koden fra vores tidligere visualiseringer af netværk

gr_c %>% ggraph(layout = "fr") +
  # add edges...
  geom_edge_link0(edge_width = 0.2, edge_alpha = 0.3) +
  # add nodes
  geom_node_point(size = 1.2) +
  #geom_node_point(aes(filter = V(gr_c)$name == "PFIZER INC", color = "red"), size = 1.2) +
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
complist <- components(gr_c)
str(complist)
complist$no           # antallet af komponenter
complist$csize        # størrelsen på de forskellige komponenter
complist$membership   # hvilke noder ligger i hvilke komponenter



# 4.b lav et nyt netværksobjekt, med kun den største komponent:
#brug get_n_largest_component fra "netværksfunktioner.R" scriptet

comp1 <- get_n_largest_component(gr_c, n = 1)
# 4.c: Udregn: 1) density, 2) transitivity, og 3) average distances for den største komponent. Snak om hvad det betyder...

edge_density(comp1)
transitivity(comp1) 
mean_distance(comp1)

# 4.d Hvad er diameteren for den største kompoment? Og hvilke to virksomheder ligger længst fra hinanden
diameter(comp1)
farthest.nodes(comp1, directed = FALSE)

# 4.e gem oplysninger om den længste sti som attributes i grafobjektet: Hint! brug get.diameter() funktionen fra igraph og hhv set.vertex.attribute() og set.edge.attribute() 
diam <- get.diameter(comp1, directed = FALSE)
comp1 <- comp1 %>% set.vertex.attribute(name = "diameter", 
                                        value = V(comp1)$name %in% names(diam)) 
comp1 <- comp1 %>% set.edge.attribute(name = "diameter", 
                                      value = E(comp1) %in% E(comp1, path = diam))


# 4.f visualiser stien i netværket
# udfyld plotfunktionen med de forskellige lag
comp1 %>% 
  ggraph(layout = "fr") +
  geom_edge_link0(aes(filter=diameter==FALSE), color = "gray60") + 
  geom_node_point(aes(filter=diameter==FALSE), color = "black") +
  geom_edge_link0(aes(filter=diameter==TRUE), color = "red", width = 1.5) +
  geom_node_point(aes(filter=diameter==TRUE), color = "darkred", size =2) +
  geom_node_label(aes(filter=diameter==TRUE, label = name), nudge_y = -0.3, size =2.5, repel = TRUE) + 
  labs(title = 'Diameter in world pharma') +
  theme_graph() 


# 4.g
# prøv at finde den korste vej mellem to udvalgte firmaer: fx. Novo Nordisk A/S og PFIZER


path_of_interest <- shortest_paths(comp1, 
                                   from = V(comp1)$name =="NOVO NORDISK A/S (GUO: NOVO NORDISK FONDEN)", 
                                   to  = V(comp1)$name =="SANOFI",
                                   output = "both") # both path nodes and edges
path_of_interest
# Vi laver en vertex attribute der er TRUE for alle noder på stien
comp1 <- comp1 %>% set.vertex.attribute("path", value = V(comp1) %in% path_of_interest$vpath[[1]])
comp1 <- comp1 %>% set.edge.attribute("path", value = E(comp1) %in% E(comp1, path = path_of_interest$vpath[[1]]))

comp1 %>% 
  ggraph(layout='fr') + 
  geom_edge_link0(aes(filter=path==FALSE), color='grey60', alpha=0.5) + 
  geom_node_point(aes(filter=path==FALSE), color='black', size=3, alpha=0.25) + 
  geom_edge_link0(aes(filter=path==TRUE), color='red', width=1.2) + 
  geom_node_point(aes(filter=path==TRUE), color='darkred', size=5, alpha=0.5) + 
  geom_node_label(aes(filter=path==TRUE, label=name), color='red', size=2, alpha = 0.8, repel = T) + 
  theme_graph()