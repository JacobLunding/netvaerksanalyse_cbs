#################################################/
#                                               #
#  Øvelse 7: Netværksvisualisering              #
#                                               #
#################################################/


###################################################################################################/
# 0. Indlæs pakker ----
###################################################################################################/
library(tidyverse)
library(ggraph)
library(igraph)
library(tidygraph)
library(graphlayouts)
library(RColorBrewer)
library(readxl) 
library(writexl)
library(ggpubr)
library(Matrix)



###################################################################################################/
# 1. Læs datafil ----
#
##################################################################################################/
den <- read_csv("data/finans_den17.csv")


###################################################################################################/
# 2. Definerer et netværksobjekt for virksomheder ----
###################################################################################################/

# lav en sparse incidence matrice name x affiliation: 
bi_adj <- xtabs(formula = ~ name + affiliation, 
                   data = den, 
                   sparse = TRUE)

# lav virksomhed x virksomhed adjacency matricen: 
adj_c  <- t(bi_adj) %*% bi_adj

# lav netværks objektet
gr <- graph_from_adjacency_matrix(adjmatrix = adj_c, mode = "undirected") %>% simplify() %>% as_tbl_graph()


###################################################################################################/
# 4. Tilføj netværkseksterne node attributes til netværksobjektet ----
###################################################################################################/

gr <- gr %>% 
  left_join(den %>% select(name = affiliation, type, sector) %>% distinct())


###################################################################################################/
# 5. Netværkets komponenter? ----
###################################################################################################/

gr <- gr %>% 
  mutate(comp = group_components())

gr %>% as_tibble() %>% count(comp)


gr1     <- gr %>% filter(comp == 1)


###################################################################################################/
# 7. Tilføj Netværks mål som attributes til netværksobjektet ----
###################################################################################################/


gr1 <- gr1 %>% 
  mutate(
    # degree
    degree = centrality_degree(),
    degree_rnk = degree %>% desc %>% dense_rank,
    # betweenness
    betweenness = centrality_betweenness(normalized = T),
    betweenness_rnk = betweenness %>% desc %>% dense_rank,
    # local betweenness
    local_betweenness = centrality_betweenness(normalized = T, cutoff = 2),
    local_betweenness_rnk = local_betweenness %>% desc %>% dense_rank,
    # closeness
    closeness = centrality_closeness(normalized = T),
    closeness_rnk = closeness %>% desc %>% dense_rank,
    # brokerage
    brokerage = 1/node_constraint(),
    brokerage_rnk = brokerage %>% desc %>% dense_rank)

#################################################/
# 8. VISUALISERING ----
#################################################/

############################################/
############################################/
############################################/
# Trin i visualisering: 
# node farve: firmtype 
# node størrelse: betweenness
############################################/
############################################/
############################################/
# forskellige layout algoritmer
layouts <- c(
  'stress',
  'kk',
  'fr',
  'nicely',
  'dh',
  'drl',
  'gem',
  'graphopt',
  'lgl',
  'mds')

pl <- map(layouts, function(x)
  gr1 %>% ggraph(layout = x) +
    geom_edge_link0(color='grey', width=0.6, alpha=0.45) + 
    geom_node_point(color='black', alpha=0.6)  + 
    theme_graph() +
    labs(caption = paste0("layout with: ", x))
)

ggarrange(plotlist = pl)

############################################/
# 1: create layout: 'initialize plot'
############################################/
p <- gr1 %>% 
  ggraph(layout = "stress")
          #layout: en af layout algoritmerne fra listen nedenfor. Mest almindelige er nok; fr, kk, nicely, stress 
p

############################################/
# 2a: tilføg geom layers: altså det vi gerne vil 'tegne'
############################################/

# Layer 1 Edges
# geom_edge_* 'tegner' edges:
 
p + geom_edge_link0()

p <- p +
  geom_edge_link0(
    width=0.5, 
    # tykkelsen på edges
    alpha=0.4, 
    # synligheden af edges (fra 0-1, dvs. "fra helt gennemsigtig til slet ikke gennemsigtig")
    color = "grey60")
    # farve på edges
p
    
# Layer 2 Nodes
# geom_node_* 'tegner' nodes:  

p + geom_node_point()
#Lige som med edges kan vi sætte forskellige parametre: size, color, alpha, shape, fill etc.

p + geom_node_point(size = 3)
p + geom_node_point(size = 3, shape = 17)
p + geom_node_point(size = 3, shape = 20, color = "steelblue")
p + geom_node_point(size = 3, shape = 21, fill = "salmon2")
p + geom_node_point(size = 3, shape = 21, fill = "salmon2", color = "steelblue")
p + geom_node_point(size = 3, shape = 21, fill = "salmon2", color = "steelblue", alpha = 0.8)



###########################################/
# 'Mapping aesthetics'
#
###########################################/
# Hvis nu vi gerne vil have at nogle af parametrene skal variere efter en given variabel, så skal vi bruge den funktion i geom'et , der mapper en variabel henover punkterne. mapping = aes()

p <- p + geom_node_point(
    # med aes() [Aesthetic mappings] sætter vi forskellige varierende aesthetics, color, size etc. Her bestemmer tag farven og betweenness størrelsen
    mapping = aes(size = betweenness, color = type),
    # andre aesthetics som vi ikke vil 'mappe' men bare give en fast værdi
    alpha = 0.85) 
p

# Layer 3 labels
############################################/
# geom_node_label 'sætter labels på'  + filter funktionen
############################################/
p <- p + geom_node_label(
    # her laver vi et filter, så vi kun plotter lables for de 5 mest centrale noder
    mapping = aes(filter=betweenness_rnk <= 10, label=name), 
    size=2, 
    repel=TRUE,
    show.legend = FALSE) 

p

# Layer 4
############################################/
# 2b: tilføg ekstra geom layers
############################################/
# Lad os sige vi vil tilføge en lille ekstra markør på de noder vi fremhæver.
p <- p +
  # tilføj et ekstra lille punkt oven i punktet for de 5 mest centrale på betweenness:
  geom_node_point(
    mapping = aes(filter=betweenness_rnk <=10), 
    color = "black", 
    size =0.5,
    # shape! Her kan man med en google søgning "R point shapes" se hvilke former man kan vælge
    shape = 19)
p


############################################/
# 3: Scales.....
# Vi kan være interesseret i at ændre på hvordan størrelserne varierer på de variable aesthetics vi har sat
############################################/

#########/
# Size:
#########/
p <- p + scale_size_continuous(range = c(1,12))
p

#########/
# Colors:
#########/

display.brewer.all()

my_colors <- brewer.pal(5, name = "Set2")
# colorRampPalette(my_colors)(15)
# HVis man har brug for flere farver end skalaen umiddelbart tillader...
  # my_colors <- brewer.pal(9, name = "Spectral") Første tal, 9 indikerer hvor mange farver vi vil hive ud af paletten, 1 til max. 
# Hvis vi vil have flere farver kan colorRampPalette bruges til at lave flere farver. Det gøres ved at man sætter det ønksede antal i en parantes efter funktionen:
# colorRampPalette(my_colors)(n_distinct(V(comp1)$tag)) 

p <- p + 
  scale_color_manual(name = "Type", values = my_colors)
p


############################################/
# 4: Guides / legends
# Vi kan ændre forskellige ting ved vores legends... fx størrelsen på markøren og titlen
p <- p + guides(color = guide_legend(override.aes = list(size = 5), title = "Virksomhedstype"), 
                size = guide_legend(title = "Betweenness centralitet"))
p


############################################/

############################################/
# 5: Tilføj titel, undertitel, caption
p <- p + labs(title = "Figure 1: Den finansielle sektor", 
                subtitle = paste0("n = ", vcount(gr1)),
              caption = "layout with ggraph::stress")
p
############################################/


############################################/
# 6: Theme 
# Ændring af det overorndede 'look' 
p + theme_graph()
# Ændring af den overordnede skrifttype for [næsten] al text i plottet
p <- p + theme_graph(base_family = "serif") # for også at ændre text type i labels, skal man tilbage til geom_node_label og tilføje family = "serif", der også...!!!
##############################################################################/
p

# vi kan overskrive layer 3 (som var vores labels)
p$layers[[3]] <- geom_node_label(mapping = aes(filter=betweenness_rnk <=10, color=type, label=name), 
                                 size=2, repel=TRUE, family = "serif", show.legend = FALSE) 
p

# gem som png 
ggsave('output/elitedb-graph-lektion07_01.png', plot = p, width=30, height=17.5, unit='cm', create.dir = TRUE)
# og/eller gem som pdf
ggsave('output/elitedb-graph-lektion07_01.pdf', plot = p, width=20, height=12.5, unit='cm', create.dir = TRUE)


############################################/
# 7. Et andet eksempel closeness (farver efter kontinuert variabel)
############################################/
# base graph
p1 <- gr1 %>% 
  ggraph(layout = "graphopt") + 
  geom_edge_link0(width=.5, alpha=0.4, color = "grey70") + 
  geom_node_point(mapping = aes(color=closeness, size=degree)) + 
  theme_graph() 
p1
# size scale for en kontinuert variabel
p1 <-  p1 + scale_size_continuous(range=c(2, 10))

# color scale for en kontinuert variabel
p1 + scale_color_viridis() 
mid <- gr1 %>% as_tibble() %>% pull(closeness) %>% mean()
# Vi kan også lave en farvesskala selv: kig på edit(colors())
p1 <- p1 + scale_color_gradient2(low='wheat', mid='lightpink1',  high='magenta', na.value='green', midpoint = mid) 
p1
# gem plot
ggsave('output/elitedb-graph-lektion06_01.png', plot = p1, width=30, height=17.5, unit='cm', create.dir = TRUE)


###############################################/
# Ændring på rækkefølgen der plottes i
# relevant, hvis vi gerne vil fremhæve centralitet
# så er det visuelt logisk at de mest centrale noder
# plottes til sidst, så de ligger øverst.
###############################################/

# Vi kan lave et layout objekt, som vi kan sortere og plotte fra
layout <- create_layout(gr1, "stress")
names(layout)

layout %>% arrange(closeness) %>% 
  ggraph() + 
  geom_edge_link0(width=.5, alpha=0.4) + 
  geom_node_point(mapping = aes(color=closeness, size=degree)) + 
  scale_size_continuous(range=c(2, 8)) +
  scale_color_viridis() +
  theme_graph() 

# Nu har vi fået noderne placret i den rigtige rækkefølge, men det er gået helt galt med edges....
# Der er desværre ikke nogen 'nem' måde at fikse det på, men jeg har skrevet en funktion I kan bruge

source("functions/get_edge_coord.R")

p_clo <- layout %>% arrange(closeness) %>% 
  ggraph() + 
  geom_edge_link0(data = get_edge_coord(gr1, layout), 
                  mapping = aes(x = x, y = y, xend = xend, yend = yend), 
                  width=.5, alpha=0.4) + 
  geom_node_point(mapping = aes(color=closeness, size=degree)) + 
  geom_node_label(mapping = aes(filter = closeness_rnk <= 5, label = name), 
                  family = "serif", repel = T) +
  scale_size_continuous(range=c(2, 8)) +
  scale_color_viridis() +
  theme_graph() 
p_clo

p_bet <- layout %>% arrange(betweenness) %>% 
  ggraph() + 
  geom_edge_link0(data = get_edge_coord(gr1, layout), 
                  mapping = aes(x = x, y = y, xend = xend, yend = yend), 
                  width=.5, alpha=0.4) + 
  geom_node_point(mapping = aes(color=betweenness, size=degree)) + 
  geom_node_label(mapping = aes(filter = betweenness_rnk <= 5, label = name), 
                  family = "serif", repel = T) +
  scale_size_continuous(range=c(2, 8)) +
  scale_color_viridis() +
  theme_graph() 
p_bet

p_brok <- layout %>% arrange(brokerage) %>% 
  ggraph() + 
  geom_edge_link0(data = get_edge_coord(gr1, layout), 
                  mapping = aes(x = x, y = y, xend = xend, yend = yend), 
                  width=.5, alpha=0.4) + 
  geom_node_point(mapping = aes(color=brokerage, size=degree)) + 
  geom_node_label(mapping = aes(filter = brokerage_rnk <= 5, label = name), 
                  family = "serif", repel = T) +
  scale_size_continuous(range=c(2, 8)) +
  scale_color_viridis() +
  theme_graph() 
p_brok

p_lbet <- layout %>% arrange(local_betweenness) %>% 
  ggraph() + 
  geom_edge_link0(data = get_edge_coord(gr1, layout), 
                  mapping = aes(x = x, y = y, xend = xend, yend = yend), 
                  width=.5, alpha=0.4) + 
  geom_node_point(mapping = aes(color=local_betweenness, size=degree)) + 
  geom_node_label(mapping = aes(filter = local_betweenness_rnk <= 5, label = name), 
                  family = "serif", repel = T) +
  scale_size_continuous(range=c(2, 8)) +
  scale_color_viridis() +
  theme_graph() 
p_lbet



###################################################################################################/
# 9. Visualiering af centralitetsmål via ggplot2 ----
###################################################################################################/

# En tibble (dataobjekt) med forskellige centralitetsmål
metrics <- gr1 %>% as_tibble() %>% select(name, type, degree, betweenness, closeness, brokerage)


### HVIS VI VIL SÆTTE LABLE PÅ UDVALGTE PUNKTER
# ggplot har ikke den filter funktion der er i ggraph
# så vi må lave en variable, name2, hvor vi gemmer alle navne, 
metrics$name2 <- metrics$name
# dernæst kan vi sætte alle navne vi IKKE vil se til NA, her hvsi brokerage er under 5.5 og closeness < 0.0076
metrics$name2[metrics$betweenness < 0.05] <- NA
metrics$name2[metrics$brokerage >7] <- NA

library(ggrepel)
# Option 1: to mål overfor hinanden (closeness vs brokerage)
metrics %>% 
ggplot() + 
  geom_point(mapping = aes(x = brokerage, y = betweenness, color=type, size=degree)) + 
  geom_label_repel(mapping = aes(x = brokerage, y = betweenness, label = name2)) +
  scale_color_manual(values = my_colors) +
  scale_x_continuous(breaks = seq(1,max(metrics$brokerage), 1), name = "1/Burts constraint") +
  scale_y_continuous(name = "Betweenness") +
  guides(color = guide_legend(override.aes = list(size = 4), title = "Virksomhedstype"),
         size = guide_legend(title = "Degree centralitet")) +
  theme_bw(base_family = "serif")

# Option 2: et mål delt op på tags
metrics %>% 
ggplot() + 
  geom_density(mapping = aes(x = betweenness, fill=type)) + 
  labs(y='share') + 
  facet_wrap(~type) + 
  scale_fill_manual(values = my_colors) + 
  guides(fill = "none") +
  theme_minimal(base_family = "serif") + 
  theme(strip.text.x = element_text(size = 12, face = "bold")) 










########################################################################/
# BONUS: forskellige mål opdelt på grupper(her tags) ----
########################################################################/

# Gennemsnitsmål baseret på en tags
metrics %>% summarise(n = n_distinct(name), 
                      across(.cols = c("degree", "closeness","betweenness", "brokerage"), 
                             .fns = list("Mean" = mean, "Max" = max, "Min" = min)), 
                      .by = type)

## Forholdet mellem ties indenfor og mellem tags
# vi henter en edgelist ud fra netværksobjektet
e <- get.data.frame(gr1, what = "edges")
# fordi det er undirected vil vi gerne 'fordoble' det så from-to og to-from er med
e <- tibble(from = c(e$from, e$to), 
            to   = c(e$to, e$from))
# dernæst kan vi left_join'e tags på for henholdsvis from variablen og to variablen.
e <- e %>% 
  left_join(., tibble(from = V(comp1)$name, from_tag = V(comp1)$tags)) %>% 
  left_join(., tibble(to = V(comp1)$name, to_tag = V(comp1)$tags))

# og lave en summarise(....., .by = from_tag), hvor vi 1. tæller hvor mange der har tag'et og dernæst hvor mange af hvert tags forbindelser, der er til det samme tag og hvor mange der er til et andet tag.
e %>% summarise(nodes     = n_distinct(from) , ties_within = sum(from_tag == to_tag),
                ties_out  = sum(from_tag != to_tag),
                share_out = ties_out / (ties_within + ties_out), .by = from_tag)




### Two mode plot
# lad os fjerne folk, der kun har en position
del      <- which(rowSums(bi_adj) < 2)
bi_adj   <- bi_adj[-del,]
net2m    <- graph_from_biadjacency_matrix(bi_adj, directed = FALSE, multiple = FALSE) %>% simplify() %>% as_tbl_graph()

net2m    <- net2m %>% mutate(comp = group_components())

layout2m <- create_layout(net2m %>% filter(comp == 1), layout = "kk")
generateRPointShapes()
layout2m %>% ggraph() +
  geom_edge_link0(width = 0.3, alpha = 0.3, color = "grey60") +
  geom_node_point(mapping = aes(shape = type, color = type, size = type), alpha = 0.9) +
  scale_shape_manual(values = c("FALSE" = 4, "TRUE" = 20), labels = c("Individer", "Virksomheder"), name = "") +
  scale_color_manual(values = c("FALSE" = "salmon3", "TRUE" = "steelblue2"), labels = c("Individer", "Virksomheder"), name = "") +
  scale_size_manual(values = c("FALSE" = 2, "TRUE" = 6), labels = c("Individer", "Virksomheder"), name = "") +
  theme_graph(base_family = "serif") + labs(title = "Two-mode plot")
