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
library(graphlayouts)
library(RColorBrewer)
library(readxl) 
library(writexl)
library(ggpubr)
library(Matrix)
source("functions/custom_functions.R")
source("functions/networkfunctions.R")

###################################################################################################/
# 1. Læs datafil ----
##################################################################################################/

den <- read_csv("input/den17-no-nordic-letters.csv")


###################################################################################################/
# 2. Subset og evt. omkod data m.m. ----
###################################################################################################/

# Vi subsetter så sector er enten Corporations eller Foundations (hvis tags så indeholder ordet Corporation) og desuden må type ikke være "Organisation" eller "Netvaerk (VL-gruppe)"
den1 <- den %>% filter((sector == "Corporations" | (sector == "Foundations" & grepl("Corporation", tags))) & !type %in% c("Organisation","Netvaerk (VL-gruppe)"))

# Vi ser på hvilke tags der optræder i det data
show.all.tags(den1)

# Udvælger tags der har med finance at gøre
den2 <- has.tags(den1, c("Finance", "FINA", "Banks", "Pensions", "Insurance", "Venture- og kapitalfonde"), result = "den")

###################################################################################################/
# 3. Definerer et netværksobjekt for virksomheder ----
###################################################################################################/

# lav en sparse incidence matrice name x affiliation: 
incidence <- xtabs(formula = ~ name + affiliation, 
                   data = den2, 
                   sparse = TRUE)

# lav virksomhed x virksomhed adjacency matricen: 
adj_c  <- incidence_to_adjacency(incidence = incidence, mode = c("col"), weigthed = FALSE) # eller: adj_c <- Matrix::t(incidence) %*% incidence

# lav netværks objektet
gr <- graph_from_adjacency_matrix(adjmatrix = adj_c, mode = "undirected") %>% simplify()

###################################################################################################/
# 4. Netværkets komponenter? ----
###################################################################################################/

comp.list <- components(gr)
comp.list$no
table(comp.list$csize)


comp1 <- largest_component(gr)

# plot 
comp1 %>% 
  ggraph(layout='fr') + 
  geom_edge_link0(color='grey', width=0.6, alpha=0.45) + 
  geom_node_point(color='black', alpha=0.6)  + 
  theme_graph()


###################################################################################################/
# 5. Komponent visualisering ----
###################################################################################################/

p <- gr %>% ggraph("fr") +
  geom_edge_link0(color = "grey35", width = 0.3, alpha = 0.3) +
  geom_node_point(size = .5) + ggtitle(paste0("Den finansielle sektor i DK (n=", vcount(gr), ")")) +
  theme_graph(base_family = "serif")
p1 <- comp1 %>% ggraph("fr") +
  geom_edge_link0(color = "grey35", width = 0.3, alpha = 0.3) +
  geom_node_point(size = 1.2) + ggtitle(paste0("Største komponent (n=", vcount(comp1), ")")) +
  theme_graph(base_family = "serif")

ggarrange(plotlist = list(p, p1))

###################################################################################################/
# 6. Tilføj netværkseksterne node attributes til netværksobjektet ----
###################################################################################################/

# Tags 
comp1 <- add_vertrex_attr(graph = comp1, 
                          data = den1 %>% 
                            select(affiliation, tags),
                          match_var = "affiliation")
# omkod tags
V(comp1)$tags <- case_when(grepl("Insurance|Pension", V(comp1)$tags) ~"Insurance&Pension",
                                    grepl("Bank", V(comp1)$tags) ~"Banks",
                                    .default = "Other finance")

###################################################################################################/
# 7. Tilføj Netværks mål som attributes til netværksobjektet ----
###################################################################################################/

# degree
V(comp1)$degree <- degree(comp1)
V(comp1)$degree_rnk <- dense_rank(desc(V(comp1)$degree))
#betweenness 
V(comp1)$betweenness <- betweenness(comp1)
V(comp1)$betweenness_rnk <- dense_rank(desc(V(comp1)$betweenness))
# closeness 
V(comp1)$closeness <-  closeness(comp1)
V(comp1)$closeness_rnk <- dense_rank(desc(V(comp1)$closeness))

#################################################/
# 8. VISUALISERING ----
#################################################/

# forskellige layout algoritmer
layouts <- c(
  'stress',
  'dh',
  'drl',
  'fr',
  'gem',
  'graphopt',
  'kk',
  'lgl',
  'mds',
  'nicely')

pl <- map(layouts, function(x)
  comp1 %>% ggraph(layout = x) +
    geom_edge_link0(color='grey', width=0.6, alpha=0.45) + 
    geom_node_point(color='black', alpha=0.6)  + 
    theme_graph() +
    labs(caption = paste0("layout with: ", x))
    )

ggarrange(plotlist = pl)

# se alle color options i base R
edit(colors())

############################################/
############################################/
############################################/
# Trin i visualisering: with node color as firmtype and node size as degree
############################################/
############################################/
############################################/

############################################/
# 1: create layout: 'initialize plot'
############################################/
p <- comp1 %>% 
  ggraph(
    #layout: en af layout algoritmerne fra listen ovenfor. Mest almindelige er nok; fr, kk, nicely, stress 
    layout = "stress") + 
  # theme 
  theme_graph() 
p


############################################/
# 2a: tilføg geom layers: altså det vi gerne vil 'tegne'
############################################/
p <- p +
# geom_edge_* 'tegner' edges:
  geom_edge_link0(
    # tykkelsen på edges
    width=0.5, 
    # synligheden af edges (fra 0-1, dvs. "fra helt gennemsigtig til slet ikke gennemsigtig")
    alpha=0.4, 
    # farve på edges
    color = "grey60") + 
# geom_node_* 'tegner' nodes:  
  geom_node_point(
    # med aes() [Aesthetic mappings] sætter vi forskellige varierende aesthetics, color, size etc. Her bestemmer tag farven og betweenness størrelsen
    aes(color=tags, size = betweenness),
    # andre aesthetics som vi ikke vil 'mappe' men bare give en fast værdi
    alpha = 0.95) + 
# geom_node_label 'sætter labels på'
  geom_node_label(
    # her laver vi et filter, så vi kun plotter lables for de 5 mest centrale noder
    aes(filter=betweenness_rnk <= 5, color=tags, label=name), 
    size=2, 
    repel=TRUE, family = "serif",
    show.legend = FALSE) 
p

############################################/
# 2b: tilføg ekstra geom layers:
############################################/
# Lad os sige vi vil tilføge en lille ekstra markør på de noder vi fremhæver.
p <- p +
  # add a little dark point to where the labelled nodes are 
  geom_node_point(
    aes(filter=betweenness_rnk <= 5), 
    color = "black", 
    size =0.5,
    # shape! Her kan man med en google søgning "R point shapes" se hvilke former man kan vælge
    shape = 19)
p
############################################/
# 3: Scales.....
############################################/
# Vi kan være interesseret i at ændre på hvordan størrelserne varierer på de variable aesthetics vi har sat
#########/
# Size:
#########/
p <- p + scale_size_continuous(range = c(1,12))
p

# Vi kan også ændre skalaens 'breaks'
range(V(comp1)$betweenness)
p + scale_size_continuous(range = c(1,12), 
                           breaks = c(0,50,100,125,150,175,200,250,350))

#########/
# Colors:
#########/
display.brewer.all()
my_colors <- brewer.pal(3, name = "Set2")
# colorRampPalette(my_colors)(15)
# HVis man har brug for flere farver end skalaen umiddelbart tillader...
  # my_colors <- brewer.pal(9, name = "Spectral") Første tal, 9 indikerer hvor mange farver vi vil hive ud af paletten, 1 til max. 
# Hvis vi vil have flere farver kan colorRampPalette bruges til at lave flere farver. Det gøres ved at man sætter det ønksede antal i en parantes efter funktionen:
# colorRampPalette(my_colors)(n_distinct(V(comp1)$tag)) 

p <- p + 
  scale_color_manual(values = my_colors, labels = c("Banker", "Forsikring og Pension", "Anden finance"))
p


############################################/
# 4: Guides / legends
# Vi kan ændre forskellige ting ved vores legends... fx størrelsen på markøren og titlen
p <- p + guides(color = guide_legend(override.aes = list(size = 5), title = "Virksomhedstype"), 
                size = guide_legend(title = "Betweenness centralitet"))
p
# Alternativ løsning, hvis man vil ændre titel på legends
p <- p + labs(size="Betweenness centralitet", 
                color="Virksomhedstype")
############################################/

############################################/
# 5: Tilføj titel, undertitel, caption
p <- p + labs(title = "Figure 1: Corporate interlocks i den finansielle sektor", 
                subtitle = paste0("n = ", vcount(comp1)),
              caption = "layout with ggraph::stress")
p
############################################/


############################################/
# 6: Ændring af den overordnede skrifttype for [næsten] al text i plottet
p <- p + theme_graph(base_family = "serif") # for også at ændre text type i labels, skal man tilbage til geom_node_label og tilføje family = "serif", der også...!!!
p
############################################/

# gem som png 
ggsave('output/elitedb-graph-lektion07_01.png', plot = p, width=30, height=17.5, unit='cm')
# og/eller gem som pdf
ggsave('output/elitedb-graph-lektion07_01.pdf', plot = p, width=20, height=12.5, unit='cm')


############################################/
# 7. Et andet eksempel closeness (farver efter kontinuert variabel)
############################################/
# base graph
p1 <- comp1 %>% 
  ggraph(layout = "graphopt") + 
  geom_edge_link0(width=.5, alpha=0.4) + 
  geom_node_point(aes(color=closeness, size=degree)) + 
  theme_graph() 
p1
# size scale for en kontinuert variabel
p1 <-  p1 + scale_size_continuous(range=c(2, 10))

# color scale for en kontinuert variabel
p1 + scale_color_viridis() 
# Vi kan også lave en farvesskala selv: kig på edit(colors())
p1 <- p1 + scale_color_gradient2(low='wheat', mid='lightpink1',  high='magenta', na.value='green', midpoint = .0065) 

# gem plot
ggsave('output/elitedb-graph-lektion06_01.png', plot = p1, width=30, height=17.5, unit='cm')












###################################################################################################/
# 9. Visualiering af centralitetsmål via ggplot2 ----
###################################################################################################/

# En tibble (dataobjekt) med forskellige centralitetsmål

metrics <- tibble(
  name =          V(comp1)$name,
  tags =           V(comp1)$tags,
  degree =        degree(comp1),
  betweenness =   betweenness(comp1),
  closeness =     closeness(comp1), 
  brokerage =     1/constraint(comp1) 
)       
### HVIS VI VIL SÆTTE LABLE PÅ UDVALGTE PUNKTER

# vi laver en variable, name2, hvor vi gemmer alla navne, 
metrics$name2 <- metrics$name
# dernæst kan vi sætte alle navne vi IKKE vil se til NA, her hvsi brokerage er under 5.5 og closeness < 0.0076
metrics$name2[metrics$brokerage < 5.5 & metrics$closeness <0.0076] <- NA

# Option 1: to mål overfor hinanden (closeness vs brokerage)
metrics %>% 
ggplot() + 
  geom_point(aes(x = brokerage, y = closeness, color=tags, size=degree)) + 
  geom_label(aes(x = brokerage, y = closeness, label = name2)) +
  scale_color_manual(values=my_colors) +
  scale_x_continuous(breaks = seq(1,max(metrics$brokerage), 1), name = "1/Burts constraint") +
  #scale_y_continuous(name = "Closeness") +
  labs(y = "Closeness") +
  guides(color = guide_legend(override.aes = list(size = 4), title = "Virksomhedstype"),
         size = guide_legend(title = "Degree centralitet")) +
  theme_bw(base_family = "serif")

# Option 2: et mål delt op på tags
metrics %>% 
ggplot() + 
  geom_density(aes(x = brokerage, fill=tags)) + 
  labs(y='share') + 
  facet_wrap(~tags) + 
  scale_fill_manual(values=my_colors) + 
  guides(fill = "none") +
  theme_minimal(base_family = "serif") + 
  theme(strip.text.x = element_text(size = 12, face = "bold")) 

# Option 3: alle mål i en matrice
cor_plots(metrics_table = metrics %>% select(-name, -name2, -tags), smooth_fun = "lm")









########################################################################/
# BONUS: forskellige mål opdelt på grupper(her tags) ----
########################################################################/

# Gennemsnitsmål baseret på en tags
metrics %>% summarise(n = n_distinct(name), 
                      across(.cols = c("degree", "closeness","betweenness", "brokerage"), 
                             .fns = list("Mean" = mean, "Max" = max, "Min" = min)), 
                      .by = tags)

## Forholdet mellem ties indenfor og mellem tags
# vi henter en edgelist ud fra netværksobjektet
e <- get.data.frame(comp1, what = "edges")
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

# lidt a la assortativity
assortativity_nominal(comp1, types = factor(V(comp1)$tags), directed = F)
