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
library(igraph)
library(ggraph)

# Download nye functions filer
download.file("https://jacoblunding.quarto.pub/virkstrat2025/functions/networkfunctions.R", "functions/networkfunctions.R")

source("functions/networkfunctions.R")

#############################################################/
# 1. Indlæs data  ----
#############################################################/

den <- read_csv("data/den17-no-nordic-letters.csv")

# subset data til kun at indeholde rækker hvor sector == "Corporation"
den_corp <- 
  den %>% filter(sector == "Corporations")

# subsetter videre data til kun at indeholde linkere, dvs. individer med mere end 1 bestyrelsespost
den_corp <- 
  den_corp %>% 
  group_by(name) %>% 
  mutate(n_memberships = n_distinct(affiliation)) 

den_corp <- den_corp %>% filter(n_memberships >1) 

#############################################################/
# 2. Lav netværksobjektet / grafobjektet ----
#############################################################/
 
biadj    <- xtabs(den_corp, formula = ~name + affiliation, sparse = T)
adj_ind  <- biadj %*% t(biadj)
adj_virk <- t(biadj) %*% biadj

# individ netværket 
gr_ind    <- adj_ind %>% graph_from_adjacency_matrix(mode = "undirected", weighted = TRUE, diag = FALSE) 
# virksomheds netværket 
gr_virk   <- adj_virk %>% graph_from_adjacency_matrix(mode = "undirected", weighted = TRUE, diag = FALSE) 

#################################/
# 3. Kig på komponenter & plot ----
# I det her eksempel går vi videre med
# virksomhedsnetværket: gr_virk
#################################/

# Hvordan ser komponent strukturen ud
complist <- components(gr_virk)
complist$csize
complist$no

# Lad os vælge den største komponent med largest_component funktionen fra Igraph: 

largest_comp_virk <- gr_virk %>% largest_component()

# og lave et simplet plot af hele netværket vs den største komponent
p1 <- gr_virk %>% 
  ggraph(layout='fr') + 
  geom_edge_link0(color='grey', width=0.6, alpha=0.45) + 
  geom_node_point(color='black', alpha=0.6)  + 
  labs(title = paste0("Netværket af virksomheder i \n'Danish Elite Network' (n=", vcount(gr_virk), ")")) +
  theme_graph()  + theme(plot.title = element_text(family = "serif", size = 12))
p2 <- largest_comp_virk %>% 
  ggraph(layout='fr') + 
  geom_edge_link0(color='grey', width=0.6, alpha=0.45) + 
  geom_node_point(color='black', alpha=0.6)  + 
  labs(title = paste0("Den største komponent (n=", vcount(largest_comp_virk),")")) +
  theme_graph() + theme(plot.title = element_text(family = "serif", size = 12))


##################################################/
# 4. Centralitetsmål  ----
# 
#################################################/

star <- make_star(10, mode = "undirected")
V(star)$name <- LETTERS[1:10]
star %>% ggraph("kk") +
  geom_edge_link0() +
  geom_node_label(aes(label = name, size = degree(star)), show.legend = F) +
  scale_size_continuous(range = c(3,6)) +
  theme_graph(base_family = "serif") + labs(title = "Stjernegraf med 10 noder", caption = "Freeman, Linton C. 1979. “Centrality  in Social Networks  Conceptual  Clarification.” Social  Networks 1979(1):215–39.")

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

deg <- degree(largest_comp_virk) 
table(deg)

# Vi tilføjer degreemålet til vores grafobjekt som vertex attribute: der er to måder at gøre det på.
#1)
V(largest_comp_virk)$degree <- deg
#2)
largest_comp_virk <- set_vertex_attr(largest_comp_virk, name = "degree", value = deg)


hist_deg <- deg %>% tibble() %>% 
  ggplot() +
  geom_histogram(aes(x=.), fill = "grey20") + #binwidth definerer, hvor mange kategorier på x-aksen en søje i histogrammet skal 'opsummere'
  scale_y_continuous(breaks = seq(0,100, 10), name = "Antal") + scale_x_continuous(breaks = c(1, seq(5,max(deg), 5)), minor_breaks = seq(0,max(deg), 1), name ="Degree") + theme_minimal(base_family = "serif")

p_deg <- largest_comp_virk %>% 
  ggraph(layout='stress') + 
  geom_edge_link0(color='grey', width=0.6, alpha=0.45) + 
  geom_node_point(aes(color=degree), alpha=0.8)  + 
  labs(title = paste0("Den største komponent (n=", vcount(largest_comp_virk),")"), subtitle = "degree", color = "") +
  theme_graph() + theme(plot.title = element_text(family = "serif", size = 12), plot.subtitle = element_text(family = "serif", size = 12), legend.position = "bottom")

ggarrange(plotlist = list(hist_deg, p_deg), widths = c(1.4,2))


##################################################/
# Eigenvector centrality: 
##################################################/
# Ide: som med degree, det er godt at have mange 'venner', men endnu bedre, hvis disse venner også er populære!
# I praksis: Udregnes hurtigt med 'kompliceret' matematik, eigenvector decomposition, deraf navnet. Intuitionen er: Alle noder starter med en 'vægt' på 1, for hver node tælles summen af deres forbindelsers 'vægt', svarer i første omgang til degree, gentages i flere runder, hvorved noder der er forbundne til velforbundne node, stiger hurtigere (= er mere centrale), skaleres til at være mellem 0 og 1, hvor 0 er isolates og en højeste centralitet.

eig <- eigen_centrality(largest_comp_virk, weights = NA, directed = FALSE)
str(eig)
eig <- eig$vector

V(largest_comp_virk)$eigencentrality <- eig

hist_eig <- eig %>% tibble() %>% 
  ggplot() +
  geom_histogram(aes(x=.), fill = "grey20") + 
  scale_y_continuous(name = "Antal") + scale_x_continuous(breaks = seq(0,max(eig), .1), name ="Eigencentralitet") + theme_minimal(base_family = "serif")

p_eig <- largest_comp_virk %>% 
  ggraph(layout='stress') + 
  geom_edge_link0(color='grey', width=0.6, alpha=0.45) + 
  geom_node_point(aes(color=eigencentrality), alpha=0.8)  + 
  labs(title = paste0("Den største komponent (n=", vcount(largest_comp_virk),")"), subtitle = "Eigencentrality", color = "") +
  theme_graph() + theme(plot.title = element_text(family = "serif", size = 12), plot.subtitle = element_text(family = "serif", size = 12), legend.position = "bottom")

ggarrange(plotlist = list(hist_eig, p_eig), widths = c(1.4,2))


##################################################/
# 4.b Stibaserede centralitetsmål  ----
#################################################/


##################################################/
# Excentricitetscentralitet:
##################################################/ 

# Ide: Et netværk har yderpunkter, dvs. noder der er længst fra hinanden. De siges at ligge i periferien og deres afstand er derfor netværkets Diameter. De noder med den korteste afstand til fjerneste node, kalder vi centrum i netværket, og afstanden til periferien Radius. 
# I praksis: for hver node udregnes dens længste korteste sti til en anden node. Den inverse eccentricitet -> 1/eccentricitet, er således et centralitetsmål, som går fra lav til høj.

ecc <- 1 / eccentricity(largest_comp_virk, weights = NA)
table(ecc)
V(largest_comp_virk)$eccentricity <- ecc

hist_ecc <- ecc %>% tibble() %>% 
  ggplot() +
  geom_histogram(aes(x=.), fill = "grey20") + 
  scale_y_continuous(name = "Antal") + scale_x_continuous(breaks = seq(0,max(ecc), .01), name ="1/Eccentricity") + theme_minimal(base_family = "serif")

p_ecc <- largest_comp_virk %>% 
  ggraph(layout='stress') + 
  geom_edge_link0(color='grey', width=0.6, alpha=0.45) + 
  geom_node_point(aes(color=eccentricity), alpha=0.8)  + 
  labs(title = paste0("Den største komponent (n=", vcount(largest_comp_virk),")"), subtitle = "1/Eccentricity", color = "") +
  theme_graph() + theme(plot.title = element_text(family = "serif", size = 12), plot.subtitle = element_text(family = "serif", size = 12), legend.position = "bottom")

ggarrange(plotlist = list(hist_ecc, p_ecc), widths = c(1.4,2))


##################################################/
# Closeness centralitet: 
##################################################/
# Ide: en central node er en der (i gennemsnit) er tæt på de andre noder. En der hurtigt kan række ud i netværket og derfor har en grad af uafhængighed.
# I praksis: udregnes (for node A) som : antallet af noder udover A selv / summen af A's netværksafstande til alle andre noder. Svarer til den inverse gennemsnitlige afstand til de andre noder. 

clo  <- closeness(largest_comp_virk, weights = NA) 
V(largest_comp_virk)$closeness <- clo

hist_clo <- clo %>% tibble() %>% 
  ggplot() +
  geom_histogram(aes(x=.), fill = "grey20") + 
  scale_y_continuous(name = "Antal") + scale_x_continuous(name ="Closeness") + theme_minimal(base_family = "serif")

p_clo <- largest_comp_virk %>% 
  ggraph(layout='stress') + 
  geom_edge_link0(color='grey', width=0.6, alpha=0.45) + 
  geom_node_point(aes(color=closeness), alpha=0.8)  + 
  labs(title = paste0("Den største komponent (n=", vcount(largest_comp_virk),")"), subtitle = "Closeness", color = "") +
  theme_graph() + theme(plot.title = element_text(family = "serif", size = 12), plot.subtitle = element_text(family = "serif", size = 12), legend.position = "bottom")

ggarrange(plotlist = list(hist_clo, p_clo), widths = c(1.4,2))


##################################################/
# Betweenness centralitet: 
##################################################/
# Ide: En central node er en der er uundværlig for at andre noder, der ikke er direkte forbundne, kan 'nå' hinanden, dvs. én der bygger bro, én gatekeeper osv.
# I praksis: Tæller antallet af shortest paths parvist mellem alle andre noder, der går igennem en given node.

bet <- betweenness(largest_comp_virk, weights = NA)
V(largest_comp_virk)$betweenness <- bet

hist_bet <- bet %>% tibble() %>% 
  ggplot() +
  geom_histogram(aes(x=.), fill = "grey20") + 
  scale_y_continuous(name = "Antal") + scale_x_continuous(name ="Betweenness") + theme_minimal(base_family = "serif")

p_bet <- largest_comp_virk %>% 
  ggraph(layout='stress') + 
  geom_edge_link0(color='grey', width=0.6, alpha=0.45) + 
  geom_node_point(aes(color=betweenness), alpha=0.8)  + 
  labs(title = paste0("Den største komponent (n=", vcount(largest_comp_virk),")"), subtitle = "Betweenness", color = "") +
  theme_graph() + theme(plot.title = element_text(family = "serif", size = 12), plot.subtitle = element_text(family = "serif", size = 12), legend.position = "bottom")

ggarrange(plotlist = list(hist_bet, p_bet), widths = c(1.4,2))

# Et samlet dataobjekt (en 'tibble') med de udvalgte centralitetsmål 
cent_metrics <- as_data_frame(largest_comp_virk, what = "vertices") %>% tibble()

# Centralitetsrank 
  # Det kan være en god ide at lave en rankvariabel for de forskellige mål, som ranker alle noder efter deres centralitet på de forskellige mål. Funktionen dense_rank( ) kombineret med desc( ) [descending] giver os et rank hvor noden med den højeste centralitet bliver nr 1 og de andre noder, 2,3,4,5 osv. jo lavere deres centralitet er.

cent_metrics_rnk <- cent_metrics %>% mutate(across(.cols = -name, .fns = ~dense_rank(desc(.x)), .names = "{.col}_rnk"))


# korrrelation mellem forskellige former for centralitet 
  # med en 'hjemmelavet' plotfunktion fra 'networkfunctions.R' kan vi lave et hurtigt plot, der viser korrelationen mellem de forskellige centralitetsmål.
cent_metrics_norm <- cent_metrics %>% mutate(across(.cols = -name, .fns = ~.x / max(.x))) 
cent_metrics_norm %>% cor_plots(., name_var = "name")

# og vi kan se de parvise korrelationer.
cent_metrics_norm  %>% select(-name) %>% cor(, method = "kendall")

#Konklussion: De er allesammen korrelerede til hinanden i forskellige grad, men ikke perfekt, da de udtrykker forskellige aspekter af hvad det vil sige at være central i et netværk: 
# Degree: 'simpel' popularitet, dvs. hvem har flest forbindelser.
# Closeness: effektiv spreder af information, da høj closeness betyder at resten af netværket er relativt tilgængeligt for denne node. 
# Betweennes: kontrol med information, høj betweenness betyder at en stor del af det, der 'flyder' mellem noder i netværket går gennem denne node.



##################################################/
# 5. Kerne/periferi  struktur ----
##################################################/

# Coreness eller K-core decomposition 
  # En anden måde at tænke centralitet på, med udgangspunkt i netværkets kerne/periferi struktur
  # ide: Første lag K=0: Alle noder; Andet lag K=1 alle noder med < 1 forbindelse slettes; næste lag K=2 alle noder der nu har <2 forbindelser slettes; K=3 alle noder der nu har <3 noder slettes osv. indtil man ikke kan slette noder uden at antallet de reterende noders forbindelser falder....
core <- coreness(largest_comp_virk)
table(core)

# et visualierngs eksempel | bruger en funktion fra networkfunctions.R
coreness_viz(largest_comp_virk, algorithm = 'fr')



# lad os lige tilføje coreness til vores net_metrics data
cent_metrics <- cent_metrics %>% mutate(coreness = core)


# 6. Eksempler på visualiernger af netværk ----

# Visualisering af eccentriciteten 

largest_comp_virk %>% ggraph(layout='stress') + 
  geom_edge_link0(color='grey', width=0.6, alpha=0.35) +
  geom_node_point(aes(color=eccentricity), alpha=0.75, size = 2) + 
  theme_graph() + scale_color_viridis(direction = -1) + labs(color="1/Eccentricity") +
  geom_node_label(aes( filter=name %in% {cent_metrics %>% filter(eccentricity %in% c(min(eccentricity), max(eccentricity))) %>% pull(name)}, label=name), alpha=0.65, size = 3, repel=T, force = 50)

# Visualisering af betweenness 
largest_comp_virk %>% ggraph(layout='stress') + 
  geom_edge_link0(color='grey', width=0.6, alpha=0.35) + 
  geom_node_point(aes(color=betweenness, size = betweenness), alpha=0.75) + 
  theme_graph() + scale_color_viridis(direction = -1) + labs(color="Betweenness") + 
  geom_node_label(aes( filter=name %in% {cent_metrics_rnk %>% filter(betweenness_rnk <= 10) %>% pull(name)}, label=name), alpha=0.65, size = 3, repel=T, force = 50)


ggsave('output/elitedb-graph-betweenness.png', width=30, height=17.5, unit='cm')

# Visualisering af closeness
largest_comp_virk %>% ggraph(layout='stress') + 
  geom_edge_link0(color='grey', width=0.6, alpha=0.35) +
  geom_node_point(aes(color=closeness), alpha=0.75, size = 2) + 
  theme_graph() + scale_color_viridis(direction = -1) + 
  labs(color="Closeness") + 
  geom_node_label(aes( filter=name %in% {cent_metrics_rnk %>% filter(closeness_rnk <= 10) %>% pull(name)}, label=name), alpha=0.65, repel=T,size=3, force = 60)
ggsave('output/elitedb-graph-closeness.png', width=30, height=17.5, unit='cm')


#############################################################################################/
# 7 Centralisering: ----
# Er centraliteten i netværket spredt ud eller koncentreret på få noder
#############################################################################################/
  # udregnes som "summen af differencen mellem centraliteten for den mest centrale node og de andre" divideret med "den teoretiske situation, hvor én node er central og alle andre perifære" For de fleste centralitetsmål er denne teoretiske situation stjernegrafen...

deg_cent  <- centr_degree(largest_comp_virk)$centralization
deg_cent
clo_cent  <- centr_clo(largest_comp_virk)$centralization
clo_cent
betw_cent <- centr_betw(largest_comp_virk)$centralization
betw_cent
eigen_cent <- centr_eigen(largest_comp_virk)$centralization
eigen_cent


stargr <- make_star(vcount(largest_comp_virk), mode = "undirected") 

lay1 <- create_layout(largest_comp_virk, layout = "fr") 

p0 <- stargr %>% ggraph(layout = "star") + 
  geom_edge_link0(edge_width = .1, edge_alpha = 0.4) + 
  geom_node_point(aes(size = degree(stargr), color = degree(stargr))) + 
  scale_size_continuous(range = c(.5,8)) + 
  guides(size = "none", color = "none", alpha = "none") + 
  labs(caption = paste0("Centralization = 1")) + 
  theme_graph()

p1 <- lay1 %>% ggraph() + 
  geom_edge_link0(edge_width = .1, edge_alpha = 0.4) + 
  geom_node_point(aes(size = degree, color = degree, alpha = degree)) + 
  scale_size_continuous(range = c(0.5, 6)) + 
  guides(size = "none", color = "none", alpha = "none") + 
  labs(caption = paste0("Degree centralization = ", round(deg_cent,2))) + 
  theme_graph()

p2 <- lay1 %>% ggraph() + 
  geom_edge_link0(edge_width = .1, edge_alpha = 0.4) + 
  geom_node_point(aes(size = betweenness, color = betweenness, alpha = betweenness)) +
  scale_size_continuous(range = c(0.5, 6)) + 
  guides(size = "none", color = "none", alpha = "none") +
  labs(caption = paste0("Betwenness centralization = ", round(betw_cent,2))) + 
  theme_graph()

p3 <- lay1 %>% ggraph() + 
  geom_edge_link0(edge_width = .1, edge_alpha = 0.4) + 
  geom_node_point(aes(size =eigencentrality, color = eigencentrality, alpha = eigencentrality)) +
  scale_size_continuous(range = c(0.5, 6)) + 
  guides(size = "none", color = "none", alpha = "none") + 
  labs(caption = paste0("Eigencentrality centralization = ", round(eig_cent,2))) + 
  theme_graph()

ggarrange(plotlist = list(p0, p1, p2, p3)) %>% annotate_figure(., top = text_grob("Graph level centralization", family = "serif", size = 12, face = "bold"))