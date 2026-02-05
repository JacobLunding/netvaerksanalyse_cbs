#############################################/
# Orbis eksempel ----
#############################################/

library(tidyverse)
library(ggraph)
library(igraph)
library(Matrix)

source("functions/custom_functions.R", echo = FALSE)

#read_orbisxlsx funktionen oversætter bl.a. variabelnavne fra orbis til noget mere meningsfuld og læseligt. Vigtigt: `Current or previous` variablen hedder nu `role_status`. Der er også en ny variabel, `person`, som ud fra identifikationsnummeret 'gætter' om personen faktisk er en person.

df <- read_orbisxlsx(path = "data/tobaco_and_alcohol.xlsx")


# Vi kan lige se på hvor mange af vores 'individer (name)' der faktisk er personer
df %>% summarise(n = n_distinct(name), .by = person)

# og vi kan lige se hvor mange af vores positioner, der faktisk er aktive
df %>% summarise(n_distinct(name, affiliation), .by = role_status)

#Lad os til at begynde med reducere vores data til kun aktive/current poster og poster der faktisk er personer:
df_current <- df %>% 
  filter(person == TRUE & role_status == "Current")

#Hvor mange medlemmer har hver affiliation i data
df_current %>% 
  distinct(name, affiliation) %>% 
  summarise(n = n_distinct(name), .by = affiliation) %>% 
  summary(n)




#Vi laver en variabel så vi kan vælge et bestemt ledelses/bestyrelses niveau
df_current <- df_current %>% 
  mutate(role_level_rec = case_when(
    grepl("member", role_level, ignore.case = T) ~ "member",
    grepl("executive", role_level, ignore.case = T) ~ "executive",
    grepl("vice (pres|chair)", role_level, ignore.case = T) ~ "vice chairman", 
    grepl("president|chairman", role_level, ignore.case = T) ~ "chairman", 
    .default = "other"))


df_current <- df_current %>% 
  mutate(role_level_rec = case_when(
    role_level_rec == "other" & 
      (grepl("SenMan", role_type, ignore.case = T) | 
         grepl("manager", role_level, ignore.case = T)) ~ "executive",
    role_level_rec == "other" & 
      grepl("chief", role_level, ignore.case = T) ~ "executive",
    .default = role_level_rec))



#Nu kan vi nøjes med at kigge på bestyrelsesmedlemmer og direktion
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


##### Fra data til netværk.

bi_adj <- xtabs(data = df_current, formula = ~name + affiliation, sparse = TRUE)


#I dette eksempel vil vi gerne se hvordan virksomheder der fremstiller tobak og alkohol er forbundet gennem overlappende bestyrelser. Derfor vil gerne 'udregne' $affiliation \times affiliation$ matricen ved at gange en transponeret udgave af vores biadjacency matrice med sig selv ($B^T \times B$)

adj_c     <- t(bi_adj) %*% bi_adj


#Som altid skal vi lige lave vores adjacency matrice om til et grafobjekt med `igraph`-funktionen `graph_from_adjacency_matrix()`. Her har vi et "undirected", "weighted" netværk, hvor vi ser bort fra diagonalen:
  
gr <- graph_from_adjacency_matrix(adj_c, mode = "undirected", diag = FALSE, weighted = TRUE)

gr
#Lad os som det første kigge på et helt minimalistisk plot af vores netværk:
gr %>% ggraph() +
  geom_edge_link0() +
  geom_node_point() +
  theme_graph()


#Som I kan se består netværket af flere 'klynger'. Det kaldes i netværksanalysesprog for komponenter. Det kommer vi tilbage til i løbet af kurset, men en netværkskomponent er et sammenhængende sæt af vertices. Lad os i første omgang prøve at bruge vores branchekode-variabel til at se om vi kan finde noget logik i hvordan komponenterne ser ud. Er der komponenter, hvor der er bestyrelsesoverlap (dvs. edges) mellem virksomheder der fremstiller hhv. alkohol og tobak. Det kræver at vi får lagt vores branchekode ind i grafobjektet som en vertex attribute.

##### Tilføj vertice attributes

#Det gør vi ved først at lave et nyt datasæt, som vi kalder sector, der indeholder én række for hver virksomhed + branchekode (`distinct(affiliation, sector)`), dernæst overskriver vi sector, så vi kun har de to første cifre i branchekoden (da vi er ligeglade med underkategorier), altså en 'substring', `substr(sector, start = 1, stop = 2)`, endelig omkoder vi branchekodens talværdier til et label (`case_when(sector == "12"~"Tobak", sector == "11"~"Alkohol", .default = NA)`). Logikken i case_when-verbet er at man har et logisk udtryk efterfult af `~` hvad der så skal stå, et nyt logisk udtryk efterfulgt af `~` hvad der så skal stå. Altså formen, hvis\~så, hvis\~så osv., afsluttende med `.default =`, hvor vi definerer hvad værdien skal være, hvis ingen af de logisk udtryk passer.

# add sector as attribute...
sector     <- df_current %>% ungroup() %>% 
  distinct(affiliation, sector) %>% 
  mutate(sector = substr(sector, start = 1, stop = 2)) %>% 
  mutate(sector = case_when(
    sector == "12"~"Tobak",
    sector == "11"~"Alkohol",
    .default = NA))


#Nu skal vi have 'merget' vores sector variabel på netværksobjektet. Til det formål skal vi først lige lave en lille data.frame, der indeholder navnene på vores vertices (altså virksomhedsnavnene), sorteret på samme måde som i grafobjektet. Husk fra sidst hvordan vi tilgår vertice attributes i vores grafobjekt vha. `V()`
# Vi laver her en data.frame med variablen `affiliation`, der indeholder virksohedsnavne fra vores grafobjekt.

add_sector   <- data.frame(affiliation = V(gr)$name)

#Nu mangler vi bare at flette sectorvariablen på så de rigtige værdier kommer til at stå ud for de rigtige virksomhedsnavne. Her har tidyverse en genial funktion, `left_join()`, som gør netop det. Hvis vi har datasæt x, vores data.frame med virksomhedsnavne, og datasæt y, vores sector datasæt med virksomhedsnavne og branchelabel, så gør left_join det at den ved at matche på virksomhedsnavnet, `by = "affiliation`, fletter den anden variabel i y på x, altså left_join'er y på x.

add_sector   <- left_join(add_sector, sector, by = "affiliation")

#Nu har vi et datasæt med branchekoder i, der er ordnet ligesom vores grafobjekt, og vi kan derfor assign'e `<-` variablen med branche koder som en ny vertex attribute i vores grafobjekt.

V(gr)$sector <- add_sector$sector

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

# `igraph` pakken har en funktion, der giver os netværkets største komponent `largest_component()`
gr_lc <- gr %>% largest_component()
gr_lc

# 1) først trækker vi netværket ud for den største komponent
# 2) laver et plot  
# 3) tilføjer edges, som vi giver en fast farve og størrelse:
# 4) tilføjer vertices, farvelagt efter sector og med en fast størrelse:
# 5) Tilføjer labels for udvalgte vertices:
#     jeg har 'snydt' og kigget i data, og udvalgt nogle virksomeder
# 6) Ændrer farver og labels.
# 7) Tilføjer overskrifter og navn på labels
# 8) Tilføjer et tema der er flot til netværk...

gr_lc %>% 
  ggraph("kk") + 
  geom_edge_link0(color = "gray40", edge_width = .6) +
  geom_node_point(aes(color = sector), size = 3) +
  geom_node_label(aes(filter = grepl("davide camp|souza cruz s|british american tobacco plc|carlsberg a/S|PHILIP MORRIS INTERNATIONAL INC|HEINEKEN N\\.|diageo p", name, ignore.case = T), label = name, color = sector), size = 3, repel = TRUE, show.legend = FALSE) +
  scale_color_manual(values = c("salmon2", "steelblue", "grey"), 
                     labels = c("Alkohol", "Tobak", "NA")) +
  labs(title = "'Corporate interlocks' i alkohol- og tobaksbrancherne", 
       subtitle = "den største komponent",
       color = "Branche") +
  theme_graph()




# Lad os skrive noget kode sammen!!!

#############################/
# Subset den17----
#############################/


#   1) Indlæs data-filen "data/den17-no-nordic-letters.csv"
#   2) Funktionen show.all.tags() kan bruges til at se de tags, der er i data

#   3) brug has.tags til at subsette på et eller flere tags
        #forslag: c("Banks", "FINA", "Finance", "Investment", "Insurance")

#   4) undersøg sector og overvej om der skal subsettes her også med filter()

