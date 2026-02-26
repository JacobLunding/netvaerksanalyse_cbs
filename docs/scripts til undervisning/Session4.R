##############################/
#
#  Øvelse 4: Centralitets mål
#
##############################/


# 0. SETTING UP --------------------------------------------------------------

# installer nye pakker
# install.packages("ggpubr")

# Indlæs relevante pakker og funktioner

library(ggplot2)
library(ggpubr)
library(tidyverse)
library(tidygraph)
library(igraph)
library(ggraph)
library(Matrix)
library(readxl)
source("functions/correlationplots.R")



#############################################################/
# 1. Indlæs data  ----
# Vi skal læse et datasæt ind. I det her tilfælde bruger vi det danske elitenetværk (2024)
#############################################################/

den <- read_csv("data/danish_elitenetworks2024.csv")

# Til denne øvelse har jeg hentet og gemt navne på alle c25 virksomheder
c25 <- read_csv("data/c25_2026.csv")
head(c25)
# jeg gemmer lige cvrnumrene som en vektor, jeg kan bruge senere.
c25_cvr <- c25$cvr


# For at lave netværket af c25 virksomheder ud fra DEN skal vi først identificere c25 virksomhederne data. 
#   Det gør vi ved at lave en ny variabel, som jeg kalder c25_virk, hvor jeg "spørger" med 'case_when' om en affiliation er i listen af c25 virksomheder. Hvis JA, får den nye variabel værdien "yes", hvis IKKE (.default =), får den værdien "no"
den <- den %>% 
  mutate(c25_virk = case_when(affiliation_cvrnummer %in% c25_cvr ~ "yes", .default = "no"))


# Næste skridt er at lave en variabel den for hver person i data fortæller om de har poster i en c25 virksomhed. 
#   Det gør vi ved at gruppere data på 'person_name', så vi midlertidigt har et lille datasæt for hver enkelt person (hvis personen sidder i otte bestyrelser har det lille data otte rækker). 
#   dernæst kan vi, igen med 'case_when', spørge om de har nogen "yes" udfald på vores nye 'c25_virk' variabel. Det gør vi med funktionen 'any()'. Hvis svaret er JA (altså at de sidder i en eller flere c25 virksomheder), får de værdien "yes" og ellers (.default =) får de "no"
den <- den %>% 
  group_by(person_name) %>% 
  mutate(c25_person = case_when(any(c25_virk == "yes") ~ "yes", .default = "no"))

# 
affil_m_c25_person <- den %>% filter(c25_person == "yes") %>% pull(affiliation) %>% unique()

# nu kan vi subsette data til alle c25 personer
den <- den %>% filter(affiliation %in% affil_m_c25_person)

# I det her tilfælde vil jeg gerne slette alle begivenheder og events, der ikke er kontinuerlige mødesteder
den <- den %>% filter(!grepl("_Begivenhed|Event", affiliation_tags))
den <- den %>% filter(!grepl("Deltagere", affiliation))

# Her laver vi en variabel, som vi kalder n_memberships, som tæller hvor mange bestyrelsesposter hver person har.
den <- den %>% 
  group_by(person_name) %>% 
  mutate(n_memberships = n_distinct(affiliation)) %>% 
  ungroup()

den %>% distinct(person_name, n_memberships) %>% count(n_memberships)

den <- den %>% filter(n_memberships > 1)

# Her laver vi en variabel, som vi kalder n_members, som tæller hvor mange medlemmer hver bestyrelse har.
den <- den %>% 
  group_by(affiliation) %>% 
  mutate(n_members = n_distinct(person_name)) %>% 
  ungroup()

den %>% distinct(affiliation, n_members) %>% count(n_members) %>% arrange(-n_members)

den <- den %>%  filter(n_members > 1)

#############################################################/
# 2. Lav netværksobjektet / grafobjektet ----
#############################################################/
 
biadj    <- xtabs(den, formula = ~person_name + affiliation, sparse = T)
adj_ind  <- biadj %*% t(biadj)
adj_virk <- t(biadj) %*% biadj

# Two-mode netværk individ x virksomhed
gr_bi    <- biadj %>% graph_from_biadjacency_matrix(directed = FALSE) %>% simplify() 
gr_bi    <- as_tbl_graph(gr_bi)

# individ netværket 
gr_ind    <- adj_ind %>% graph_from_adjacency_matrix(mode = "undirected", weighted = NULL, diag = FALSE) %>% simplify()
gr_ind    <- as_tbl_graph(gr_ind)
# virksomheds netværket 
gr_virk   <- adj_virk %>% graph_from_adjacency_matrix(mode = "undirected", weighted = NULL, diag = FALSE) %>% simplify()
gr_virk   <- as_tbl_graph(gr_virk)

#############################################################/
# Extended vs. 'kun c25' netværk på two mode niveau:
#############################################################/

gr_bi <- gr_bi %>% 
  left_join(den %>% distinct(affiliation, c25_virk) %>% select(name = affiliation, c25_virk))
gr_bi <- gr_bi %>% 
  left_join(den %>% distinct(person_name, c25_person) %>% select(name = person_name, c25_person))



p1 <- gr_bi %>% filter(!(c25_virk == "no" & type == TRUE)) %>% 
  ggraph("kk") + 
  geom_edge_link0(color='grey', width=0.6, alpha=0.45) + 
  geom_node_point(aes(filter = centrality_degree() > 0, color=type, size = type), alpha=0.9)  + 
  geom_node_label(aes(filter = centrality_degree() > 1, label = name, color = type), size = 3, repel = T, show.legend = F)  + 
  scale_color_manual(values = c("salmon2", "steelblue3")) +
  theme_graph()  + theme(plot.title = element_text(family = "serif", size = 12)) 
p1

p2 <- gr_bi %>% 
  ggraph("fr") + 
  geom_edge_link0(color='grey', width=0.6, alpha=0.45) + 
  geom_node_point(aes(color=type, size = type, shape = type), alpha=0.6)  +
  geom_node_point(aes(filter = c25_virk == "yes" & type), color = "black", size = 6, shape = 17, alpha=0.6)  +
  geom_node_point(aes(filter = c25_person == "yes" & !type), color = "black", size = 2, shape = 19, alpha=0.6)  +
  scale_color_manual(values = c("salmon2", "steelblue3")) +
  labs(caption = "Sorte prikker: C25 virksomheder") +
  theme_graph()  + theme(plot.title = element_text(family = "serif", size = 12))
p2


######################################################/
# LAD OS GÅ VIDERE MED ET ANDET NETVÆRK
# TØJ I EU
######################################################/
#
# som vi jo har gemt i tidygraph objektet gr_virk
#
gr_virk <- net %>% as_tbl_graph()
##################################################################/
# 3. Kig på komponenter & plot ----
##################################################################/
gr_virk <- gr_virk %>% 
  activate(nodes) %>% 
  mutate(comp = group_components())

# Hvordan ser komponent strukturen ud
gr_virk %>% activate(nodes) %>% as_tibble() %>% count(comp)


# Der er kun én komponent i det udvidede netværk.
# og densiteten i denne komponent er:
edge_density(gr_virk)
# og transitiviteten er:
transitivity(gr_virk)
# relativt høj!

# TILFØJE ekstra variable til et tidygraph objekt!!
# For at tilføje variable skal vi bruge en funktion, der hedder 'left_join' og et datasæt der indeholder den variabel vi gerne vil tilføje samt en variabel, der matcher op mod node-'name' i tidygraph objektet.
# 
add_c25 <- den %>% 
  select(affiliation, c25_virk) %>% 
  distinct() %>% 
  rename(name = affiliation) # her omdøber jeg affiliation til name, så den hedder det samme som i tidygraph-objektet

head(add_c25, 10)

gr_virk <- gr_virk %>% 
    left_join(add_c25)


# De to følgende kodeblokke plotter henholdsvis det lille netværk af 'kun' c25 virksomheder og det udvidede netværk af alle virksomheder, der har en c25 person som medlem:

#Kun c25
p1 <- gr_virk %>% filter(c25_virk == "yes") %>% 
  ggraph(layout='fr') + 
  geom_edge_link0(color='grey', width=0.6, alpha=0.45) + 
  geom_node_point(size = 5)  + 
  geom_node_label(aes(label = name), size = 4, repel = T) +
  labs(title = paste0("Netværket af C25 virksomheder")) +
  theme_graph()  + theme(plot.title = element_text(family = "serif", size = 12))
p1

#Hele netværket (farvet efter om det er c25 virksomheder eller ej og med labels på alle c25 virksomheder)
p2 <- gr_virk %>% 
  ggraph(layout = "fr") + 
  geom_edge_link0(color='grey', width=0.3, alpha=0.2) + 
  geom_node_point(aes(color=c25_virk, size = c25_virk), alpha=0.9)  + 
  scale_color_manual(values = c("steelblue1", "salmon2")) +
  geom_node_label(aes(filter = c25_virk == "yes", label = name), repel = T, show.legend = F) +
  labs(title = paste0("C25 virksomheders udvidede netværk (n=", vcount(gr_bi), ")"), color = "C25", size = "C25") +
  theme_graph()  + theme(plot.title = element_text(family = "serif", size = 12))
p2



##################################################/
# 4. Centralitetsmål  ----
#################################################/
# I det følgende skal vi lære om følgende centraltietsmål
# degree 
# eigenvector centralitet
# betweenness
# closeness


star <- create_star(10, mode = "undirected")

star <- star %>% mutate(name = LETTERS[1:10])

star %>% ggraph("kk") +
  geom_edge_link0() +
  geom_node_label(aes(label = name, size = degree(star)), show.legend = F) +
  scale_size_continuous(range = c(3,6)) +
  theme_graph(base_family = "serif") + labs(title = "Stjernegraf med 10 noder", caption = "Freeman, Linton C. 1979. “Centrality in Social Networks  Conceptual  Clarification.” Social  Networks 1979(1):215–39.")

##################################################/
# 4.a Kontaktbaserede centralitetsmål  ----
#################################################/

#################################################/
# Degree centralitet:
##################################################/
# Ide: en central aktør er en aktør med mange forbindelser, høj aktivitet. 
# I praksis: Tæller hvor mange direkte forbindelser hver node har, dvs hvor mange andre noder den er forbundet til
    # I et ikke-retningsbestemte netværk (undirected) der kun et degree mål. 
    # I retningsbestemte netværk har hver node en:
       # 'out degree' (udadgående forbindelser) 
       # 'in degree' (indkommende forbindelser) 
       # Total degree (summen af de to).

# Vi tilføjer degreemålet til vores grafobjekt som node variable 
gr_virk <- gr_virk %>% 
  activate(nodes) %>% 
  mutate(degree = centrality_degree())

# Det her er plotkode
deg <- gr_virk %>% as_tibble() %>% pull(degree)

hist_deg <- deg %>% tibble() %>% 
  ggplot() +
  geom_histogram(aes(x=.), fill = "grey20") + #binwidth definerer, hvor mange kategorier på x-aksen en søje i histogrammet skal 'opsummere'
  scale_y_continuous(breaks = seq(0,100, 10), name = "Antal") + 
  scale_x_continuous(breaks = c(1, seq(10,max(deg), 10)), minor_breaks = seq(0,max(deg), 1), name ="Degree") + 
  theme_minimal(base_family = "serif")

p_deg <- gr_virk %>% 
  ggraph(layout='stress') + 
  geom_edge_link0(color='grey', width=0.6, alpha=0.45) + 
  geom_node_point(aes(color=degree, size=degree), alpha=0.8)  +
  #geom_node_label(aes(label = name), size = 2) +
  labs(title = paste0("Den største komponent (n=", vcount(gr_virk),")"), subtitle = "degree", color = "") + guides(size = "none") +
  theme_graph() + theme(plot.title = element_text(family = "serif", size = 12), plot.subtitle = element_text(family = "serif", size = 12), legend.position = "bottom")

ggarrange(plotlist = list(hist_deg, p_deg), widths = c(1.4,2))


##################################################/
# Eigenvector centralitet: 
##################################################/
# Ide: som med degree, det er godt at have mange 'venner', men endnu bedre, hvis disse venner også er populære!
# I praksis: Udregnes hurtigt med 'kompliceret' matematik, eigenvector decomposition, deraf navnet. 
# Intuitionen er: Alle noder starter med en 'vægt' på 1, for hver node tælles summen af deres forbindelsers 'vægt', svarer i første omgang til degree, gentages i flere runder, hvorved noder der er forbundne til velforbundne node, stiger hurtigere (= er mere centrale), skaleres til at være mellem 0 og 1, hvor 0 er isolates og en højeste centralitet.

gr_virk <- gr_virk %>% 
  activate(nodes) %>% 
  mutate(eigencentrality = centrality_eigen())

eig <- gr_virk %>% as_tibble(active = "nodes") %>% pull(eigencentrality)

hist_eig <- eig %>% tibble() %>% 
  ggplot() +
  geom_histogram(aes(x=.), fill = "grey20") + 
  scale_y_continuous(name = "Antal") + scale_x_continuous(breaks = seq(0,max(eig), .1), name ="Eigencentralitet") + theme_minimal(base_family = "serif")

p_eig <- gr_virk %>% 
  ggraph(layout='stress') + 
  geom_edge_link0(color='grey', width=0.6, alpha=0.45) + 
  geom_node_point(aes(color=eigencentrality, size = eigencentrality), alpha=0.8)  + 
  theme_graph() + theme(plot.title = element_text(family = "serif", size = 12), plot.subtitle = element_text(family = "serif", size = 12), legend.position = "bottom") +
  labs(title = paste0("Den største komponent (n=", vcount(gr_virk),")"), subtitle = "Eigencentrality", color = "") + guides(size = "none")

ggarrange(plotlist = list(hist_eig, p_eig), widths = c(1.4,2))


##################################################/
# 4.b Stibaserede centralitetsmål  ----
#################################################/


##################################################/
# Closeness centralitet: 
##################################################/
# Ide: en central node er en der (i gennemsnit) er tæt på de andre noder. En der hurtigt kan række ud i netværket og derfor har en grad af uafhængighed.
# I praksis: udregnes (for node A) som : antallet af noder udover A selv / summen af A's netværksafstande til alle andre noder. Svarer til den inverse gennemsnitlige afstand til de andre noder. 

gr_virk <- gr_virk %>% 
  activate(nodes) %>% 
  mutate(closeness = centrality_closeness())

clo <- gr_virk %>% as_tibble(active = "nodes") %>% pull(closeness)

hist_clo <- clo %>% tibble() %>% 
  ggplot() +
  geom_histogram(aes(x=.), fill = "grey20") + 
  scale_y_continuous(name = "Antal") + scale_x_continuous(name ="Closeness") + theme_minimal(base_family = "serif")

p_clo <- gr_virk %>% 
  ggraph(layout='stress') + 
  geom_edge_link0(color='grey', width=0.6, alpha=0.45) + 
  geom_node_point(aes(color=closeness, size = closeness), alpha=0.8)  + 
  theme_graph() + theme(plot.title = element_text(family = "serif", size = 12), plot.subtitle = element_text(family = "serif", size = 12), legend.position = "bottom") +
  labs(title = paste0("Den største komponent (n=", vcount(gr_virk),")"), subtitle = "Closeness", color = "") + guides(size = "none")

ggarrange(plotlist = list(hist_clo, p_clo), widths = c(1.4,2))


##################################################/
# Betweenness centralitet: 
##################################################/
# Ide: En central node er en der er uundværlig for at andre noder, der ikke er direkte forbundne, kan 'nå' hinanden, dvs. én der bygger bro, én gatekeeper osv.
# I praksis: Tæller antallet af shortest paths parvist mellem alle andre noder, der går igennem en given node.

gr_virk <- gr_virk %>% 
  activate(nodes) %>% 
  mutate(betweenness = centrality_betweenness())

bet <- gr_virk %>% as_tibble(active = "nodes") %>% pull(betweenness)

hist_bet <- bet %>% tibble() %>% 
  ggplot() +
  geom_histogram(aes(x=.), fill = "grey20") + 
  scale_y_continuous(name = "Antal") + scale_x_continuous(name ="Betweenness") + theme_minimal(base_family = "serif")

p_bet <- gr_virk %>% 
  ggraph(layout='stress') + 
  geom_edge_link0(color='grey', width=0.6, alpha=0.45) + 
  geom_node_point(aes(color=betweenness, size= betweenness), alpha=0.8)  + 
  theme_graph() + theme(plot.title = element_text(family = "serif", size = 12), plot.subtitle = element_text(family = "serif", size = 12), legend.position = "bottom") +
  labs(title = paste0("Den største komponent (n=", vcount(gr_virk),")"), subtitle = "Betweenness", color = "") + guides(size = "none")

ggarrange(plotlist = list(hist_bet, p_bet), widths = c(1.4,2))








##################################################/
# SAMMEN I R:
# Hvordan vi beregner og gemmer centralitetsmål
##################################################/

# igraph:
degree(gr_virk)
eigen_centrality(gr_virk)
betweenness(gr_virk)
closeness(gr_virk)
#
# vs.
#
# tidygraph:
gr_virk <- gr_virk %>% 
  mutate(degree = centrality_degree(),
         eigencentrality = centrality_eigen(), 
         closeness = centrality_closeness(),
         betweenness = centrality_betweenness(),
         # Under tiden giver det mening at normalisere målene, så skalaen ikke er afhængig af netværkets størrelse (særligt for closeness og betweenness)      
         degree_norm = centrality_degree(normalized = T),
         closeness_norm = centrality_closeness(normalized = T),
         betweenness_norm = centrality_betweenness(normalized = T))

################################/
# Centralitetsrank ----
################################/
# Det kan være en god ide at lave en rankvariabel for de forskellige mål, som ranker alle noder efter deres centralitet på de forskellige mål. Funktionen dense_rank( ) kombineret med desc( ) [descending] giver os et rank hvor noden med den højeste centralitet bliver nr 1 og de andre noder, 2,3,4,5 osv. jo lavere deres centralitet er.

# Funktionen dense_rank() laver en ranking af en fordeling, når vi bruger dense_rank istedet for bare rank, er det fordi vi gerne vil have at node kan dele fx 2nd pladsen. Fordi vi gerne vil have den node med den HØJESTE centralitetsværdi til at være nr. 1, skal vi lige vende fordelingen om, når vi ranker. Det gør vi med desc() som betyder 'descending'
gr_virk <- gr_virk %>% 
  mutate(degree_rnk = degree %>% desc() %>% dense_rank(),
         eigencentrality_rnk = eigencentrality %>% desc() %>% dense_rank(),
         closeness_rnk = closeness %>% desc() %>% dense_rank(),
         betweenness_rnk = betweenness %>% desc() %>% dense_rank())
         
# Hvis vi vil have et dataobjekt (en 'tibble') med de udvalgte centralitetsmål
gr_virk %>% as_tibble(active = "nodes") %>% write_csv("output/centralitetsmål.xlsx")


# korrrelation mellem forskellige former for centralitet 
  # med en 'hjemmelavet' plotfunktion fra 'networkfunctions.R' kan vi lave et hurtigt plot, der viser korrelationen mellem de forskellige centralitetsmål.
gr_virk %>% 
  as_tibble() %>% 
  select(name, contains("_norm"), eigencentrality) %>% 
  cor_plots(., name_var = "name")

# og vi kan se de parvise korrelationer.
gr_virk %>% as_tibble() %>% select(contains("_norm"), eigencentrality) %>% cor(, method = "kendall")

#Konklussion: De er allesammen korrelerede til hinanden i forskellige grad, men ikke perfekt, da de udtrykker forskellige aspekter af hvad det vil sige at være central i et netværk: 
# Degree: 'simpel' popularitet, dvs. hvem har flest forbindelser.
# Closeness: effektiv spreder af information, da høj closeness betyder at resten af netværket er relativt tilgængeligt for denne node. 
# Betweennes: kontrol med information, høj betweenness betyder at en stor del af det, der 'flyder' mellem noder i netværket går gennem denne node.












##############################################/
# Coreness eller K-core decomposition ----
##############################################/
  # En anden måde at tænke centralitet på, med udgangspunkt i netværkets kerne/periferi struktur
  # ide: Første lag K=0: Alle noder; Andet lag K=1 alle noder med < 1 forbindelse slettes; næste lag K=2 alle noder der nu har <2 forbindelser slettes; K=3 alle noder der nu har <3 noder slettes osv. indtil man ikke kan slette noder uden at antallet de reterende noders forbindelser falder....
gr_virk <- gr_virk %>% 
  mutate(coreness = node_coreness())


gr_virk %>% as_tibble(active = "nodes") %>% View()

# et visualierngs eksempel | bruger en funktion fra 
# download.file("https://jacoblunding.quarto.pub/virkstrat2025/scripts%20til%20undervisning/Session4_øvelse.R", "scripts/Session3_øvelse.R")
source("functions/coreness_viz.R")
coreness_viz(gr_virk, algorithm = 'fr')

#######################################################/
# 6. Eksempler på visualiernger af netværk ----
#######################################################/

# Visualisering af betweenness 
p_bet <- gr_virk %>% ggraph(layout='stress') + 
  geom_edge_link0(color='grey', width=0.3, alpha=0.25) + 
  geom_node_point(aes(color=betweenness_norm, size = betweenness_norm, shape = c25_virk), alpha=0.75) + 
  geom_node_label(aes( filter=betweenness_rnk <= 10, label=name), alpha=0.65, size = 3, repel=T) +
  theme_graph() + scale_color_viridis(direction = -1) + 
  labs(color="Betweenness") + guides(size = "none")

p_bet

# Visualisering af closeness
p_clo <- gr_virk %>% ggraph(layout='stress') + 
  geom_edge_link0(color='grey', width=0.6, alpha=0.35) +
  geom_node_point(aes(color=closeness_norm, size = closeness_norm, shape = c25_virk), alpha=0.75) + 
  geom_node_label(aes( filter= closeness_rnk <= 10, label=name), alpha=0.65, repel=T,size=3) +
  theme_graph() + scale_color_viridis(direction = -1) + 
  labs(color="Closeness") + guides(size = "none")

p_clo
ggsave(plot = p_bet, 'output/elitedb-graph-betweenness.png', width=30, height=17.5, unit='cm')
ggsave(plot = p_clo, 'output/elitedb-graph-closeness.png', width=30, height=17.5, unit='cm')


#############################################################################################/
# 7 Centralisering: ----
# Er centraliteten i netværket spredt ud eller koncentreret på få noder
#############################################################################################/
  # udregnes som "summen af differencen mellem centraliteten for den mest centrale node og de andre" divideret med "den teoretiske situation, hvor én node er central og alle andre perifære" For de fleste centralitetsmål er denne teoretiske situation stjernegrafen...

deg_cent   <- centr_degree(gr_virk)$centralization
deg_cent
clo_cent   <- centr_clo(gr_virk)$centralization
clo_cent
betw_cent  <- centr_betw(gr_virk)$centralization
betw_cent
eigen_cent <- centr_eigen(gr_virk)$centralization
eigen_cent


stargr <- create_star(gorder(gr_virk), mode = "undirected") 

lay1 <- create_layout(gr_virk, layout = "fr") 

p0 <- stargr %>% ggraph(layout = "star") + 
  geom_edge_link0(edge_width = .1, edge_alpha = 0.4) + 
  geom_node_point(aes(size = degree(stargr), color = degree(stargr))) + 
  scale_size_continuous(range = c(.5,8)) + 
  guides(size = "none", color = "none", alpha = "none") + 
  labs(caption = paste0("Centralization = 1")) + 
  theme_graph()

p1 <- lay1 %>% ggraph() + 
  geom_edge_link0(edge_width = .1, edge_alpha = 0.4) + 
  geom_node_point(aes(size = degree_norm, color = degree_norm, alpha = degree_norm)) + 
  scale_size_continuous(range = c(0.5, 6)) + 
  guides(size = "none", color = "none", alpha = "none") + 
  labs(caption = paste0("Degree centralization = ", round(deg_cent,2))) + 
  theme_graph()

p2 <- lay1 %>% ggraph() + 
  geom_edge_link0(edge_width = .1, edge_alpha = 0.4) + 
  geom_node_point(aes(size = betweenness_norm, color = betweenness_norm, alpha = betweenness_norm)) +
  scale_size_continuous(range = c(0.5, 6)) + 
  guides(size = "none", color = "none", alpha = "none") +
  labs(caption = paste0("Betwenness centralization = ", round(betw_cent,2))) + 
  theme_graph()

p3 <- lay1 %>% ggraph() + 
  geom_edge_link0(edge_width = .1, edge_alpha = 0.4) + 
  geom_node_point(aes(size = eigencentrality, color = eigencentrality, alpha = eigencentrality)) +
  scale_size_continuous(range = c(0.5, 6)) + 
  guides(size = "none", color = "none", alpha = "none") + 
  labs(caption = paste0("Eigencentrality centralization = ", round(eigen_cent,2))) + 
  theme_graph()

ggarrange(plotlist = list(p0, p1, p2, p3)) %>% annotate_figure(., top = text_grob("Graph level centralization", family = "serif", size = 12, face = "bold"))


#I kan downloade øvelsen her:
download.file("https://jacoblunding.github.io/netvaerksanalyse_cbs/scripts%20til%20undervisning/Session4_øvelse.R", "Session4_øvelse.R")


