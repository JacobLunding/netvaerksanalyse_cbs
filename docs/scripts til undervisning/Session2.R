#############################################/
# Orbis eksempel ----
#############################################/

library(tidyverse)
library(tidygraph)
library(ggraph)
library(igraph)
library(Matrix)

source("functions/read_orbis.R", echo = FALSE)

#read_orbisxlsx funktionen oversætter bl.a. variabelnavne fra orbis til noget mere meningsfuld og læseligt. Vigtigt: `Current or previous` variablen hedder nu `role_status`. Der er også en ny variabel, `person`, som ud fra identifikationsnummeret 'gætter' om personen faktisk er en person.

df <- read_orbisxlsx(path = "data/tobaco_and_alcohol.xlsx")

df %>% count(sector, sort = T)

## Omkodning af branche variablen
# Jeg ved at alle tobaks-underbrancherne starter med 12 og alle alkohol ditto starter med 11, så jeg først en ny variabel hvor jeg kun tager de første to ciffre af branchekoden
df <- df %>% 
  mutate(sector_2digits = substr(sector, start = 1, stop = 2))

df <- df %>% 
  mutate(sector = case_when(
    sector_2digits == "12"~"Tobak",
    sector_2digits == "11"~"Alkohol",
    .default = NA))

df %>% count(sector, sort = T)

# Vi kan lige se på hvor mange af vores 'individer (name)' der faktisk er personer
df %>% summarise(n = n_distinct(name), .by = person)

# og vi kan lige se hvor mange af vores positioner, der faktisk er aktive
df %>% summarise(n = n_distinct(name, affiliation), .by = role_status)

#Lad os til at begynde med reducere vores data til kun aktive/current poster og poster der faktisk er personer:
df_current <- df %>% 
  filter(person == TRUE & role_status == "Current")

#Hvor mange medlemmer har hver affiliation i data
df_current %>% 
  distinct(name, affiliation) %>% 
  group_by(affiliation) %>% 
  summarise(n = n_distinct(name)) %>% 
  summary(n)


# Kan vi evt. slette nogle medlemmer, så vi kun har bestyrelsen og ledelsen, der ligger en role_level og en role_level_rec variabel i orbisdata (hvis man har eksporteret de rigtige variable fra ORbis)
df_current %>% count(role_level, sort = T)
df_current %>% count(role_level_rec, sort = T)

# Lad os subsette så vi kun har de 'niveauer' vi skal bruge. Dvs. alt undtagen 'other'
df_current <- df_current %>% 
  filter(role_level_rec %in% c("member","chairman", "vice chairman","executive"))



#Vi kan starte med lige at se på hvor mange 'board members' hver virksomhed har: Som I kan se er der nogle meget store 'boards'. Det er et af problemerne med Orbis, nemlig at det er uklart hvad der er registreret per virksomhed.
df_current %>% distinct(name, affiliation) %>% count(affiliation, sort = TRUE)


#Vi kan lave en ny variabel i vores datasæt, der for hvert individ tæller hvor mange virksomeder, de er knyttet til:
df_current <- df_current %>% group_by(name) %>% mutate(n_memberships = n_distinct(affiliation))
df_current %>% ungroup() %>%  count(n_memberships)

# Lad os slette personer, `n = 24333`, der 'kun' sidder i en enkelt virksomhed, da de alligevel ikke laver nogen forbindelser på tværs af virksomheder i vores netværk. Vi sletter så at sige folk i vores affiliation data, der ikke er 'linkere' (`filter()`) og samtidig sørger vi for at hvert individ kun optræder én gang per virksomhed (`distinct()`) - det kan jo være at nogen har mere end en rolle i samme bestyrelse (datasnavs?).

df_current <- df_current %>% 
  filter(n_memberships > 1) %>% 
  distinct(name, affiliation, .keep_all = TRUE)

df_current %>% 
  distinct(name, affiliation) %>% 
  group_by(affiliation) %>% 
  summarise(n = n_distinct(name)) %>% 
  summary(n)

##### Fra data til netværk.

bi_adj <- xtabs(data = df_current, formula = ~name + affiliation, sparse = TRUE)


#I dette eksempel vil vi gerne se hvordan virksomheder der fremstiller tobak og alkohol er forbundet gennem overlappende bestyrelser. Derfor vil gerne 'udregne' $affiliation \times affiliation$ matricen ved at gange en transponeret udgave af vores biadjacency matrice med sig selv ($B^T \times B$)

adj_c     <- t(bi_adj) %*% bi_adj


#Som altid skal vi lige lave vores adjacency matrice om til et grafobjekt med `igraph`-funktionen `graph_from_adjacency_matrix()`. Her har vi et "undirected", "weighted" netværk, hvor vi ser bort fra diagonalen:
  
gr <- graph_from_adjacency_matrix(adj_c, mode = "undirected", diag = FALSE, weighted = TRUE)

# Derefter laver vi graf objektet 'gr' om til et tidygraph objekt:
# 
gr <- gr %>% as_tbl_graph()
#Lad os som det første kigge på et helt minimalistisk plot af vores netværk:
gr %>% ggraph() +
  geom_edge_link0() +
  geom_node_point() +
  theme_graph()


#Som I kan se består netværket af flere 'klynger'. Det kaldes i netværksanalysesprog for komponenter. Det kommer vi tilbage til i løbet af kurset, men en netværkskomponent er et sammenhængende sæt af vertices. Lad os i første omgang prøve at bruge vores branchekode-variabel til at se om vi kan finde noget logik i hvordan komponenterne ser ud. Er der komponenter, hvor der er bestyrelsesoverlap (dvs. edges) mellem virksomheder der fremstiller hhv. alkohol og tobak. Det kræver at vi får lagt vores branchekode ind i grafobjektet som en vertex attribute.

##### Tilføj vertice attributes
#Fordi vores netværksdata objekt er et tidygraph objekt kan vi relativt 'let' tilføje variable til data. For at tilgå de forskellige dele af netværksobjektet bruges funktionen `activate()` fra `tidygraph`. 

#I vores oprindelige data har vi jo variablen `sector` den vil vi gerne tilføje til node-delen af netværksobjektet. Derfor skal vi aktivere node-delen: `activate(nodes)`. Dernæst skal vi bruge `left_join()` som 'joiner' et datasæt på et andet. 
#Vi skal 'kun' bruge `sector` og `affiliation` (for at kunne matche navnene) så vi bruger `select()`. Desuden skal vi lige sørge for at der kun er en række per affiliation / sector kombination, så den ved hvad der skal joines. Sidst men ikke mindst skal vi fortælle left_join hvilke variable der skal matches. Her `name` og `affiliation`

gr <- gr %>% activate(nodes) %>% 
  left_join(df_current %>% ungroup() %>% select(affiliation, sector) %>% distinct(), by = c("name" = "affiliation"))

gr

### 
# Lad os nu lave vores visualisering igen, hvor vi farvelægger noderne efter branche. Vi kommer senere til detaljer i plot funktionerne. Kort fortalt om visualisering:
  
#1)  funktionen `ggraph()` opretter et tilsyneladende tomt plot (tilsyneladende fordi den faktisk udregner et layout for vertices i vores data)
#2)  funktionen `geom_edge_link0()` plotter vores edges som en 'streg'/et link. Der er også andre muligheder fx. `geom_edge_arc()` der plotter dem som en bue.
#3)  funktionen `geom_node_point()` der plotter vores noder som en 'cirkel'/point.

# -   inden for hver af disse `geom'er` kan vi sætte en masse options. Blandt andet kan vi definere nogle `aesthetics`. lad os bruge sector til at sætte farven på vores punkter (`mapping = aes(color = sector)`). Det betyder at vi lader farven på punkter følge værdierne på en bestemt variabel.


gr %>% ggraph() +
  geom_edge_link0() +
  geom_node_point(mapping = aes(color = sector)) +
  theme_graph()


#Med undtagelse af primært den største komponent ser komponenterne ud til at være ret branche homogene. Der sker noget andet i den største komponent, så lad os fokusere vores visualisering på den største komponent i netværket.

# `tidygraph` pakken har en funktion, der giver os netværkets komponenter `group_components()`: den bruger vi ved at aktivere node-delen af grafdataen 'activate(nodes)' og lave en variabel med 'mutate', som vi kalder 'comp' og som vi fylder med resultatet af funktionen 'group_components()'.
gr <- gr %>% 
  activate(nodes) %>% 
  mutate(comp = group_components())

# ved at aktivere node-delen af grafdataen 'activate(nodes)' og lave et datasæt 'as.tibble()', kan vi få en oversigt over hvor mange komponenter netværket består af og hvor mange node de hver især indeholder 'count(comp)'
gr %>% activate(nodes) %>% as_tibble() %>% count(comp)

# Nu er vi klar til at lave et plot:


# 1) først trækker vi netværket ud for den største komponent
# 2) laver et plot  
# 3) tilføjer edges, som vi giver en fast farve og størrelse:
# 4) tilføjer vertices, farvelagt efter sector og med en fast størrelse:
# 5) Tilføjer labels for udvalgte vertices:
#     jeg har 'snydt' og kigget i data, og udvalgt nogle virksomeder
# 6) Ændrer farver og labels.
# 7) Tilføjer overskrifter og navn på labels
# 8) Tilføjer et tema der er flot til netværk...

gr %>% filter(comp == 1) %>% 
  ggraph() + 
  geom_edge_link0(color = "gray40", edge_width = .6) +
  geom_node_point(aes(color = sector), size = 3) +
  geom_node_label(aes(filter = grepl("davide camp|souza cruz s|british american tobacco plc|carlsberg a/S|PHILIP MORRIS INTERNATIONAL INC|HEINEKEN N\\.|diageo p", name, ignore.case = T), label = name, color = sector), size = 3, repel = TRUE, show.legend = FALSE) +
  scale_color_manual(values = c("salmon2", "steelblue", "grey"), 
                     labels = c("Alkohol", "Tobak", "NA")) +
  labs(title = "'Corporate interlocks' i alkohol- og tobaksbrancherne", 
       subtitle = "den største komponent",
       color = "Branche") +
  theme_graph()
