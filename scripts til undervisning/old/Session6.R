library(tidyverse)
library(readxl)
library(writexl)
library(igraph)
library(ggraph)
library(ggpubr)
library(Matrix)
# Ny pakke at installere
# install.packages("RColorBrewer")
library(RColorBrewer)
source("functions/networkfunctions.R")
source("functions/custom_functions.R")
###################################################################################################/
# 1. Læs datafil ----
###################################################################################################/
den <- read_csv("data/den17-no-nordic-letters.csv")
# kig på data, ser det rigtigt ud
den %>% head()


###################################################################################################/
# 2. Subset og omkod data m.m. ----
###################################################################################################/

fin  <- c("Finance", "FINA", "Banks", "Venture- og kapitalfonde", "Pensions", "Insurance", "Investment")

den_fin  <- has.tags(den, tags = fin, result = "den")

den_fin  <- den_fin %>% filter(sector != "Events")

# Jeg har i løbet af analysen opdaget at der er mange organisationer, der er delvist de samme. Fx Tryg (Bestyrelse) og Tryg (repræsentantskab) osv.
# Derfor laver jeg en variabel her, hvor jeg 'grov' tjekker om det er den samme organisation:

# En vector med alle unikke affiliation navne
affiliations <- den_fin$affiliation %>% unique()

# expand.grid() er en smart måde at lave alle kombinationer af navne:
namecheck    <- df <- expand.grid(parent = affiliations, child = affiliations)
# vi er kun interesseret i at undersøge de tilfælde hvor det ikke er den samme
namecheck    <- namecheck %>% filter(parent != child)
# det vi nu gør er at lave en rowwise() altså rækkevis undersøgelse af om det der organisationsnavn der står i 'parent' kolonnen, indgår i det der står i 'child' kolonnen. Hvis det gør det bliver is_parent TRUE
namecheck    <- namecheck %>% rowwise %>% mutate(is_parent = grepl(parent, child, fixed = TRUE))
# Nu kan vi subsette vores namecheck, så vi kun har de rækker, hvor der er en 'parent' og en 'child' organisation og kun gemmer parent og child kolonnerne (select())
namecheck    <- namecheck %>% filter(is_parent) %>% select(parent, child)

#Hvis vi left_joiner namecheck på den_fin og fortæller left_join() funktionen at den skal matche 'affiliation' fra den_fin med child fra namecheck, så får vi en ny kolonne med 'parent' ud for de organisationer, der hører under en anden:
den_fin      <- left_join(den_fin, namecheck, by = c("affiliation" = "child"))

# Den nye variabel har værdierne NA, der hvor der ikke er en parent-child relation. De NA'er skal vi lige erstatte med det rigtige affiliation navn. Så: case_when - når parent er NA, skriv affiliation, ellers .default parent
den_fin      <- den_fin %>% mutate(parent = case_when(is.na(parent)~affiliation, .default = parent))

# Der er også nogle tilfælde, hvor vi ikke fanger navne overlappet. Fx hvis der ikke findes en kort version af navne. Det prøver jeg at læse ved at erstatte alt fra en parentes og frem eller alt fra " - Reprasentantskab" og frem med ingenting "".
# Vi bruger tolower for at sikre os at det ikke bare er forskelle på store og små bogstaver og trimws (trim white spaces) til at sikre os at dt ikke bare er fordi der er et mellem til sidst
den_fin      <- den_fin %>% mutate(parent = gsub(" \\(.*| - Repraesentantskab.*| - Advisory Board.*|A/S", "", parent) %>% tolower() %>% trimws())

# Nu kan vi lave en variabel, der tæller om en person er i flere parent organisationer. 
den_fin      <- den_fin %>% group_by(name) %>%  mutate(n_afil_parent = n_distinct(parent))

# Vi vil gerne slette personer, der kun linker indenfor den samme organisationer. Fx sidder i Trygs bestyrelse og i Trygs repræsentantskab osv.
den_fin      <- den_fin %>% filter(n_afil_parent > 1)

###################################################################################################/
# 3. Definerer et netværksobjekt for virksomheder ----
###################################################################################################/

# lav en sparse incidence matrice name x affiliation: 
# (..., formula = ~ name + affiliation) giver en incidence matrice med name (individer) i rækker og affiliation (virksomheder) i kolonner  
bi_adj <- xtabs(data = den_fin, formula = ~name + affiliation, sparse = T)


# hvis vi vil være sikre på at folke ikke har mere end 1 post i den samme bestyrelse kan vi tvinge vores incidence matrice til at være binær (0/1) ved at sætte alle værdier over 1 til 1
bi_adj@x %>% table()
bi_adj[bi_adj > 1] <- 1

# lav virksomhed x virksomhed adjacency matricen: 
# brug matrix multiplikation:  incidence %*% Matrix::t(incidence)
adj_corp  <- t(bi_adj) %*% bi_adj

# lav netværks objektet
net_fin <- graph_from_adjacency_matrix(adjmatrix = adj_corp, mode = "undirected") %>% simplify()

net_fin_2m <- graph_from_biadjacency_matrix(bi_adj, directed = FALSE) %>% simplify()

###################################################################################################/
# 4. Netværkets komponenter? ----
###################################################################################################/
count_components(net_fin)

comp.list <- components(net_fin)
table(comp.list$csize)

comp1 <- largest_component(net_fin)

###################################################################################################/
# 5. Tilføj netværkseksterne node attributes til netværket ----
###################################################################################################/

comp1 <- add_vertex_attr(graph = comp1, 
                          data = den_fin %>% ungroup() %>% 
                            select(affiliation, sector, type, tags),
                          match_var = "affiliation")

###################################################################################################/
# 6. Komponent visualisering ----
###################################################################################################/

# Two mode plot
# jeg tæller først lige hvor mange der er af hver type (for at bruge det i titlen på plottet)
type <- table(V(net_fin_2m)$type)
ind  <- type[1]
corp <- type[2]


p2m <- net_fin_2m %>% ggraph("fr") +
  geom_edge_link0(color = "grey35", width = 0.3, alpha = 0.3) +
  geom_node_point(aes(size = factor(type), color= type, shape = type)) + 
  ggtitle("Finanssektoren", subtitle = paste0("(persons=",ind, ", organisations=", corp, ")")) + 
  scale_color_discrete(labels = c("individuals", "corporations")) + 
  scale_size_manual(values = c(2, 4), labels = c("individuals", "corporations")) + 
  scale_shape_manual(values = c(4, 19), labels = c("individuals", "corporations")) +   labs(size = "node type", color = "node type", shape = "node type") +
  theme_graph(base_family = "serif")

# Alle virksomheder
p <- net_fin %>% ggraph("kk") +
  geom_edge_link0(color = "grey35", width = 0.3, alpha = 0.3) +
  geom_node_point(size = .5) + ggtitle(paste0("Finanssektoren (n=", vcount(net_fin), ")")) +
  theme_graph(base_family = "serif")

# Største komponent af virksomheder
p1 <- comp1 %>% ggraph("kk") +
  geom_edge_link0(color = "grey35", width = 0.2, alpha = 0.2) +
  geom_node_point(size = 1.5, aes(color = sector)) + ggtitle(paste0("Største komponent (n=", vcount(comp1), ")")) +
  theme_graph(base_family = "serif")


p2m
ggarrange(plotlist = list(p, p1))

###################################################################################################/
# 7. Netværks mål ----
###################################################################################################/

dens     <- edge_density(comp1)
trans    <- transitivity(comp1)
radius   <- radius(comp1)
diameter <- diameter(comp1)

net_description <- c("nb. of nodes" = vcount(comp1),
                     "nb. of edges" = ecount(comp1),
                     "nb. of components" = count_components(comp1),
                     "largest component: nb. of nodes" = vcount(comp1), 
                     "largest component: share of nodes" = vcount(comp1) / vcount(comp1), 
                     "largest component: nb. of edges" = ecount(comp1), 
                     "largest component: share of edges" = ecount(comp1) / ecount(comp1),  
                     "largest component: diameter" = diameter,  
                     "largest component: radius" = radius,
                     "largest component: density" = dens, 
                     "largest component: transitivity" = trans) %>% 
  enframe(name = "Measures", value = "value")

write_xlsx(net_description, "output/store_danske_virksomheder_individer_net_description.xlsx")


###################################################################################################/
# 8.1 Centralitetsmål mv. ----
###################################################################################################/

deg                <-  degree(comp1)
betw               <-  betweenness(comp1, normalized = TRUE)
local_betw         <-  betweenness(comp1, cutoff = 2, normalized = TRUE)
close              <-  closeness(comp1, normalized = TRUE)
eig                <-  eigen_centrality(comp1, directed = FALSE)$vector
core               <-  coreness(comp1)
local_transitivity <- transitivity(comp1, type = "local")
constr             <- constraint(comp1)
brokerage          <- 1 / constr



# Lad os samle de nodespecifikke centralitetsmål for den største komponent
# Her kan I jo tilføje hvad I har brug for:
# 
cent_metrics <- tibble(
  name        = V(comp1)$name,
  degree      = deg, 
  betweenness = betw,
  local_betweenness = local_betw,
  closeness   = close,
  eigen       = eig,
  constraint  = constr,
  brokerage   = 1/ constr,
  coreness    = core,
  local_trans = local_transitivity)

##########/
# Ranking
##########/

cent_metrics <- cent_metrics %>% 
  mutate(degree_rank      = dense_rank(desc(degree)),                                        betw_rank        = dense_rank(desc(betweenness)),
         local_betw_rank  = dense_rank(desc(local_betweenness)),
         closeness_rank   = dense_rank(desc(closeness)),
         brokerage_rank   = dense_rank(desc(brokerage)),
         eigen_rank       = dense_rank(desc(eigen)),
         local_trans_rank = dense_rank(local_trans))

##################/
# Et sammensat rank
##################/
cent_metrics <- cent_metrics %>% mutate(composit_rank = dense_rank(degree_rank + betw_rank + local_betw_rank + closeness_rank + brokerage_rank + eigen_rank))

write_xlsx(cent_metrics, "output/store_danske_virksomheder_individer_centralitymetrics.xlsx")


###################################################################################################/
# 8.2 Tilføj centralitetsmål som attributes ----
###################################################################################################/
comp1 <- add_vertex_attr(graph= comp1, 
                         data = cent_metrics, 
                         match_var = "name")




###############/
# Kliker
###############/
# Kliker er subsets af noder, hvis interne densitet er 1. Dvs et subset af noder hvor alle er forbundne til hinanden. En node kan godt indgå i flere kliker

# Hvor mange kliker er der af forskellig størrelse
cl_count <- clique_size_counts(comp1) 
names(cl_count) <- 1:length(cl_count)
cl_count


# Hvordan finder vi klikerne. funktionen cliques() gemmer alle kliker med min = x, noder
clique <- cliques(comp1, min = 2) 

# Maximal-kliker
# Kliker der ikke kan udvides ved at tilføje en adjacent node. Dvs. kliker, der ikke er sub-kliker i en større klike.

# Hvor mange maximale kliker er der
count_max_cliques(comp1, min = 3) 
count_max_cliques(comp1, min = 4)
count_max_cliques(comp1, min = 5)

# find all of the maximal cliques
max_cliques <- max_cliques(comp1, min = 3)

# maximum clique, den størt mulige maximale klike
table(sapply(max_cliques, length))

p <- clique_plot(comp1, n = 10, mode = "edges")
p

###################################################################################################/
# 10 Community strukturer i netværket ----
# Kan vi finde andre underindelinger af netværket baseret på netværksstrukturen?
###################################################################################################/
# Findes der et mål for om én inddeling af et netværk i grupper er bedre end en anden inddeling?

# Et bud som anvendes i mange community detection algoritmer er at kigge på forholdet mellem edges (ties) internt i de definerede grupper og edges mellem/på tværs af disse grupper. Det kaldes modularitet (eller modularity). 
# 
# Det udregnes ved at sammenholde det faktisk forhold mellem edges *internt* (within) i klynger (modules) og edges *mellem* (between) klynger (modules) med det samme forhold i et random netværk med samme samme antal noder og edges. Modulariteten er således den faktisk andel af within_group_edges minus andelen af within_group_edges i et ækvivalent men tilfældigt netværk.  
# 
# 
# Hvis vi gerne vil finde en klyngestruktur i et netværk kan vi forsøge at inddele netværket i grupper på en måde der *optimerer modulariteten*.
#
# Louvain clustering er en blandt flere algoritmer, der arbejder ud fra den logik. 
# 
# Forsimplet starter alle noder med at være deres egen gruppe og lægges derefter sammen så modulariteten hele tiden bliver stærkere.
# 
# Bemærk at algoritmen IKKE er deterministisk. Dvs. der kan være situationer, hvor den finder (marginalt) forskellige løsninger.  

louvain <- cluster_louvain(comp1, resolution = 1)
names(louvain)

n_distinct(louvain$membership) # antallet af klynger
louvain$membership             # klyngenummer for hver virksomhed i netværket
table(louvain$membership)      # antallet af virksomheder i hver klynge


louvain$modularity             # Algoritmens er iterativ og her kan vi se modulariteten i de forskellige steps

# Er der en sammenhæng mellem sector of clusters?
 
# Først kan vi jo lige se på om organisationer af samme slags er hyppigere forbundet
assortativity_nominal(comp1, types = factor(V(comp1)$sector), directed = F)

# Og dernæst kan vi prøve at tælle op for hver sektor og tjekke fordelingen
cl_sector <- tibble(clusters = louvain$membership, sector = V(comp1)$sector)

share_cl  <- cl_sector %>% group_by(clusters) %>% count(sector) %>% mutate(pct = n/ sum(n), type = "within cluster") %>% select(-n)
share_sec <- cl_sector %>% group_by(sector) %>% count(clusters) %>% mutate(pct = n/ sum(n), type = "of sector") %>% select(-n)

cluster_sector <- bind_rows(share_cl, share_sec)
cluster_sector <- cluster_sector %>% pivot_wider(id_cols = c("sector", "type"), names_from = clusters, values_from = pct) %>% arrange(sector)

write_xlsx(cluster_sector, "output/finance_cluster_sector.xlsx")

###################################################################################################/
# 11. Visualisering af community strukturen ----
# 
###################################################################################################/
# Vi tilføjer klyngemedlemskabet som en node attribute, vi vil gerne have pæne labels så vi tilføjer et nul foran et cifrede tal (det gør vi fordi tal, når de læses som text sorteres anderledes)
tal1 <- c(1:20) %>% as.character()
tal2 <- c(1:20) %>% sprintf("%02d", .)
sort(tal1)
sort(tal2)

V(comp1)$louvain <- sprintf("%02d", louvain$membership)

# Og laver en edge attribute, så vi kan farve klynge-interne edges også
# Det er en funktion jeg har skrevet, som tjekker om noderne i hver ende af en edge har samme værdi på en given attribute (her louvain). Det kan bruges til at give edgen samme farve som de to noder, hvis de er ens. 
comp1 <- edge_attr_from_vertex_attr(comp1, vertex_attr = "louvain")

# Nu kan vi lave et flot plot
comp1 %>% 
  ggraph(layout = "stress") +
  geom_edge_link0(aes(filter=louvain!= "9999", color = louvain), width = 0.65, alpha = 0.2) +
  geom_edge_link0(aes(filter=louvain == "9999"), color = "black", width = 0.65, alpha = 0.1) + 
  geom_node_point(aes(color=louvain), alpha=0.95, size=3) + 
  theme_graph() 


# Lad os kigge lidt på de enkelte klynger i netværket
focus    <- unique(V(comp1)$louvain) %>% sort()
amount   <- focus %>% n_distinct()

# Farvelægning: 
# Option 1: Hvis det er kontinuert variabel man vil farvelægge efter kan viridis functionen være god: scale_color_viridis() eller scale_edge_color_viridis() 
# Option 2: Hvis det er en variabel med kategoier, kan man vælge farveskalaer fra RColorBrewer palettes

# RcolorBrewer 

# Se farverne
display.brewer.all()

# Nu kan vi udvælge de farver vi gerne vil have
mycolors <- colorRampPalette(brewer.pal(10, "Spectral"))(amount)
rows     <- sample(length(mycolors))
mycolors <- mycolors[rows]
names(mycolors) <- focus
klyngeplot <- map(1:length(focus), 
                  function(x) cluster_plot(comp1, x, focus, mycolors, edge_attr = "louvain"))

ggarrange(plotlist = klyngeplot, ncol = 3, nrow = 4) %>% annotate_figure(
  top = text_grob("Clusters in the Danish finance network", family = "serif"),
  bottom = text_grob(paste0("modularity =", round(modularity(louvain),2)), family = "serif", hjust = -1))


#########################################/
# Kan vi beskrive klyngerne meningsfuld?
#########################################/
# Lad os lave et data objekt med klynger, virksomhedsnav, sektor, type og tags
klyngedata <- data.frame(cluster = V(comp1)$louvain, affiliation = V(comp1)$name, sector = V(comp1)$sector, type = V(comp1)$type, tags = V(comp1)$tags)

View(klyngedata)
# Kan vi lave noget statistik?
# Hvis vi nu krydser fx sektor og klynge og laver en chi test, så vi kan kigge på de standardiserede residualer...

sec_tab      <- table(klyngedata$sector, klyngedata$cluster)
sector_chi_t <- chisq.test(sec_tab)
sector_chi_t$stdres

# Hvad med tags ? der er jo også andre tags end finans?
# Først skal vi lige have dem splittet ad...
table(klyngedata$tags)
# det gør vi med separate_longer_delim() den laver en ny række, når der er et ", "
tags_data    <- klyngedata %>% separate_longer_delim(tags, ", ")
table(tags_data$tags) %>% View()

# Vi kan også lige rette FINA til Finance
tags_data <- tags_data %>% mutate(tags = case_when(tags=="FINA"~"Finance", .default = tags))
# Og så skal vi lige sørge for at hver Virksomhed i hvert cluster, kun har det samme tag 1 gang
tags_data <- tags_data %>% distinct(affiliation, tags, cluster)
head(tags_data)

# Nu kan vi lave en tabel på tags og cluster
tags_tab <- table(tags_data$tags, tags_data$cluster)
tags_chi_t <- chisq.test(tags_tab)

stdres <- bind_rows(tags_chi_t$stdres %>% 
                      data.frame() %>%
                      mutate(type = "tag"),
                    sector_chi_t$stdres %>% 
                      data.frame() %>% 
                      mutate(type = "sector"))

colnames(stdres) <- c("tag", "cluster", "z-score", "type")
stdres <- stdres %>% relocate(type, .before = tag)

# Hvis vi kigger på den og husker at stdres > 1.65 er en signifikant 'overrepræsentation' og < - 1.65 en signifikant underrepræsentation..:
# 
stdres %>% arrange(-`z-score`) %>% arrange(cluster) %>% View()

# Lad os prøve at se på hvad der er de mest distinktive tags i hver klynge 
most_distinctive_tags <- stdres %>% 
  filter(type == "tag") %>% 
  group_by(cluster) %>% arrange(-`z-score`) %>% summarise(tag_plus = paste0(head(tag, 3), collapse = " + "))

most_distinctive_sector <- stdres %>% 
  filter(type == "sector") %>% 
  group_by(cluster) %>% arrange(-`z-score`) %>% summarise(sector_plus = paste0(head(tag, 2), collapse = " + "))

distinctive_tag_sector <- left_join(most_distinctive_tags, most_distinctive_sector)

distinctive_tag_sector


#####################################/
# Hvordan er klyngerne forbundne?
#####################################/
# vi kan prøve at lægge alle vertices i de forskellige clusters sammen.
# Vi har tidligere tilføjet nogle edge attribute, der fortæller hvilke klynger noderne i hver ende af en edge tilhører: 
# 
e_data        <- as_data_frame(comp1, "edges") 
View(e_data)

# Interne ties 
edge_data_int <- e_data %>% 
  filter(louvain_a == louvain_b) %>% 
  group_by(louvain_a, louvain_b) %>% 
  summarise(n_edges = n()) %>%  
  rename(name = louvain_a) %>%  # Vi kan nøjes med at beholde navnet på den ene (da de jo er ens!)
  select(-louvain_b)

# Ties imellem
edge_data_bet <- e_data %>% filter(louvain_a != louvain_b)

# Til de interne ties kan vi også lige tilføje antallet af noder, så vi kan beregne densiteten:
edge_data_int$size <- as.vector(table(V(comp1)$louvain))
edge_data_int      <- edge_data_int %>% mutate(density = n_edges / (size*(size-1)) / 2)

# edge_data_bet indeholder nu faktisk et netværk mellem klynger: 
# Det laver vi til et vægtet netværksobjekt (edgevægten er antallet af forbindelser mellem dem)
reduced_net <- edge_data_bet %>% 
  mutate(weight = 1) %>%  # Vi laver en vægt på 1 til alle edges som vi kan summe til sidst
  select(louvain_a, louvain_b, weight) %>% 
  graph_from_data_frame(directed = F) %>% 
  simplify(edge.attr.comb = list(weight ="sum"))

# og tilføjer klyngernes størrelse og densitet som vertex attributes
V(reduced_net)$internal_density <- edge_data_int$density
V(reduced_net)$size             <- edge_data_int$size


# Nu kan vi plotte den reducerede graf:
# 
ggraph(reduced_net, "fr", weights = 1/weight) +
  geom_edge_link0(aes(width = weight), alpha = 0.5) +
  geom_node_point(aes(size = size, color = name)) +
  geom_node_label(aes(label = name, color = name), repel = T) +
  scale_size_continuous(range = c(5, 25)) +
  scale_edge_width_continuous(range = c(2, 5)) +
  labs(color = "Cluster") +
  guides(size = "none") +
  theme_graph()


# Vi kan også undersøge hvilke noder der er mindst constraint i hver klynge
V(comp1)$brokers <- 1/constraint(comp1)
brokers <- data.frame(affiliation = V(comp1)$name, cluster = V(comp1)$louvain, brokers = V(comp1)$brokers)

top_brokers <- brokers %>% group_by(cluster) %>% mutate(broker_rank = dense_rank(desc(brokers))) %>% filter(broker_rank < 3) %>% arrange(cluster)
top_brokers

# Eller hvilken virksomhed der har flest ties 'ud af sin klynge'
e_data_a <- e_data %>% filter(louvain == "9999") %>%  select(node = from, cluster = louvain_a, other = to, louvain) 
e_data_b <- e_data %>% filter(louvain == "9999") %>%  select(node = to, cluster = louvain_b, other = from, louvain) 
external_ties <- bind_rows(e_data_a, e_data_b)
external_ties <- external_ties %>% group_by(cluster, node) %>% summarise(external_ties = n())
external_ties <- external_ties %>% group_by(cluster) %>% mutate(linker_rank = dense_rank(desc(external_ties))) %>% filter(linker_rank < 3)
View(external_ties)



# Ekstra: Andre community detection algoritmer ----
# Der findes andre community detection algoritmer end Louvain som er baseret på samme princip - optimering af modularity. 
# De indeholder de samme elementer: membership og modularity, så de kan indsættes i koden ovenfor: 



leiden_cl <- cluster_leiden(comp1, objective_function = "modularity")
names(leiden_cl)

edge_bet_cl    <- cluster_edge_betweenness(comp1)
names(edge_bet_cl)

fast_greedy_cl <- cluster_fast_greedy(comp1)
names(fast_greedy_cl)

label_prop_cl  <- cluster_label_prop(comp1)
names(label_prop_cl)

leading_eig_cl <- cluster_leading_eigen(comp1)
names(leading_eig_cl)

walktrap_cl    <- cluster_walktrap(comp1)
names(walktrap_cl)

spinglass_cl   <- cluster_spinglass(comp1)
names(spinglass_cl)

infomap_cl     <- cluster_infomap(comp1)
names(infomap_cl)

#  I kan læse lidt her om forholdet mellem forskellige algoritmer
# https://stackoverflow.com/questions/9471906/what-are-the-differences-between-community-detection-algorithms-in-igraph


################################/
# fast_greedy 
###############################/

fast_greedy <- cluster_fast_greedy(comp1)

names(fast_greedy)
table(fast_greedy$membership, louvain$membership)

V(comp1)$fast_greedy <- sprintf("%02d", fast_greedy$membership)

# laver en edge attribute, så vi kan farve klynge-interne edges også
comp1 <- edge_attr_from_vertex_attr(comp1, vertex_attr = "fast_greedy")


amount   <- V(comp1)$fast_greedy %>% n_distinct()
mycolors <- colorRampPalette(brewer.pal(10, "Spectral"))(amount)
rows     <- sample(length(mycolors))
mycolors <- mycolors[rows]

# Nu kan vi lave et flot plot
comp1 %>% 
  ggraph(layout = "stress") +
  geom_edge_link0(aes(filter=fast_greedy!= "9999", color = fast_greedy), width = 0.65, alpha = 0.6) +
  geom_edge_link0(aes(filter=fast_greedy == "9999"), color = "black", width = 0.65, alpha = 0.4) + 
  geom_node_point(aes(color=fast_greedy), alpha=0.95, size=3) + 
  scale_edge_color_manual(values = mycolors) +
  scale_color_manual(values = mycolors) +
  theme_graph() 

focus           <- unique(V(comp1)$fast_greedy) %>% sort()
names(mycolors) <- focus
klyngeplot <- map(1:length(focus), 
                  function(x) cluster_plot(comp1, x, focus, mycolors, edge_attr = "fast_greedy"))

klyngeplot







###################################################################################################/
# 9. Visualiseringer af netværk ----
###################################################################################################/

# lad os lege lidt med visualisering af betweenness centraliteten
comp1 %>% ggraph('stress') +
  geom_edge_link0() + 
  # color betweenness, size degree
  geom_node_point() 





# Det kan være et æstetisk problem!! at noderne bliver plottet i alfabetisk rækkefølge. Hvad nu, hvis vi gerne vil have at de noder vi gerne vil fremhæve, skal ligge øverst.
# Det er der desværre ikke nogen helt nem metode til, men en lille funktion kan hjælpe.

## Først laver vi et fast layout, som vi kan genbruge:
stable_lay <- create_layout(comp1, layout = 'stress')


# Betweenness

p1 <- stable_lay %>% arrange(-betw_rank) %>% # minus foran i arrange, betyder at den plotter i omvendt rækkefølge, dvs. højeste værdier først
  ggraph() +
  geom_edge_link0(data = get_edge_coord(comp1, stable_lay), aes(x= x, y=y, xend=xend, yend = yend), width =.3, alpha = 0.3) + 
  geom_node_point(aes(fill = betweenness, size = degree), color = "black",shape = 21) + 
  scale_size_continuous(range = c(2,10)) + 
  geom_node_label(aes(filter=betw_rank<=10, 
                      label = paste0(betw_rank, ": ", name)), 
                  size = 3, repel = T, force = 25) +
  guides(label = "none", size = "none", color = "none") +
  theme_graph()
p1


# Farvelægning: 
# Option 1: Hvis det er kontinuert variabel man vil farvelægge efter kan viridis functionen være god: scale_color_viridis() eller scale_edge_color_viridis() 
# Option 2: Hvis det er en variabel med kategoier, kan man vælge farveskalaer fra RColorBrewer palettes

# RcolorBrewer 

# Se farverne
display.brewer.all()

# Vælg en farveskala og tilpas den til det antal kategorier, der skal repræsenteres
amount   <- V(comp1)$louvain %>% n_distinct()
mycolors <- colorRampPalette(brewer.pal(10, "Spectral"))(amount)

rows <- sample(length(mycolors))
mycolors <- mycolors[rows]

comp1 %>% 
  ggraph(layout = "stress") +
  geom_edge_link0(aes(filter=louvain!= "9999", color = louvain), width = 0.8, alpha = 0.6) +
  geom_edge_link0(aes(filter=louvain == "9999"), color = "black", width = 0.3, alpha = 0.2) + 
  geom_node_point(aes(color=louvain), alpha=0.95, size=3) + 
  theme_graph(base_family = 'Serif') + 
  scale_edge_color_manual(values = mycolors) +
  scale_color_manual(values = mycolors) 



all_layouts <- c(
  # fra igraph
  'nicely', # vælger automatisk et passende layout
  'stress', # default i ggrapj
  'fr', # fruchterman reingold
  'dh', 
  'drl',
  'gem',
  'graphopt',
  'kk',
  'lgl',
  'mds',
  'sugiyama',
  'bipartite',
  'star',
  'tree')

###################################################################################################

