# Eksamen i Virksomhedsstrategi i et netværksperspektiv
# Analyse af netværket i Danmark
# Indlæser pakkerne samt custom functions til orbis

install.packages("writexl")
install.packages("ggraph")
install.packages("ggpubr")

library(writexl)
library(stringr)
library(tidyverse)
library(data.table)
library(ggraph)
library(igraph)
library(graphlayouts)
library(RColorBrewer)
library(readxl)
library(ggplot2)
library(tidyverse)
library(readxl)
library(writexl)
library(data.table)
library(Matrix)
library(ggplot2)
library(ggpubr)
library(RColorBrewer)
source("functions/networkfunctions.R")
source("functions/custom_functions.R")

#dataset fra orbis
df_UK <- read_orbisxlsx(path = "data/02_Storbritanien_250.xlsx")

# downloader layouts til graferne
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
  'sugiyama',
  'nicely',
  'bipartite',
  'star',
  'tree',
  'dendrogram',
  'manual',
  'linear',
  'matrix',
  'treemap',
  'circlepack',
  'partition',
  
  'hive'
)

##Companies to remove
# Create a vector of company names to remove from df_UK
companies_to_remove_UK <- c(
  "GLENCORE ENERGY UK LTD.",
  "MOTABILITY OPERATIONS LIMITED",
  "RIO TINTO FINANCE PLC",
  "KELDA EUROBOND CO LIMITED",
  "B.A.T. INTERNATIONAL FINANCE P.L.C.",
  "ZENITH AUTOMOTIVE HOLDINGS LIMITED",
  "FREEPOINT COMMODITIES EUROPE LLP",
  "PORTERBROOK LEASING COMPANY LIMITED",
  "EVERSHOLT RAIL LEASING LIMITED"
)

# Remove rows where rev_rank column matches the names in the list
df_UK <- df_UK[!df_UK$affiliation %in% companies_to_remove_UK, ]


## Vi ændrer en kolonne som funktionen ikke kender til et navn vi bestemmer. Det er de kolonner vi selv har tilføjet men Lasse, Christoph og Jakob ikke har.
## Vi renamer kolonne 20 til "Citybased"
colnames(df_UK)[20] <- "Citybased"



## Vi filtrere role_type så den kun indeholder de værdier vi finder interessante. Det vil sige vi har fjernet regulere medlemmer af en virksomhed der er registret inde på orbis.

df_UK <- df_UK %>% 
  filter(grepl("BoD|SenMan|ExeC|ExeB|SupB", role_type)) %>% 
  distinct(name, affiliation, .keep_all = T) %>% 
  filter(role_status == "Current")

#Lad os til at begynde med reducere vores data til kun aktive/current poster og poster der faktisk er personer:
df_UK <- df_UK %>% 
  filter(person == TRUE & role_status == "Current")

#KOMPONENT
# Incidence matrice
incidence <- xtabs(formula = ~ name + affiliation,
                   data = df_UK,
                   sparse = TRUE)

# Adjacency matrice
adj_c <- Matrix::t(incidence) %*% incidence
# One-mode graf
gr_UK <- graph_from_adjacency_matrix(adj_c, mode = "undirected") %>%
  simplify(remove.multiple = TRUE, remove.loops = TRUE)


# Visualisering af hele one-mode netværket
gr_UK%>%
  ggraph(layout="fr") +
  geom_edge_link0(color = "gray60") +
  geom_node_point(color="red") +
  geom_node_text(aes(label=name), size=1, nudge_y = -0.2) +
  theme_graph() +
  labs(title = "One-mode netværk i Storbritanien",
       subtitle = "Figur 1")+
  theme(plot.title = element_text(hjust = 0.5))

### Fjern alle dem der kun har 1 forbindelse.

#Vi kan lave en ny variabel i vores datasæt, der for hvert individ tæller hvor mange virksomeder, de er knyttet til:
df_UK <- df_UK %>% group_by(name) %>% mutate(n_memberships = n_distinct(affiliation))
df_UK <- df_UK %>% group_by(affiliation) %>% mutate(n_members = n_distinct(name))
df_UK %>% ungroup() %>%  count(n_memberships)

# Lad os slette personer, `n = 1703`, der 'kun' sidder i en enkelt virksomhed, da de alligevel ikke laver nogen forbindelser på tværs af virksomheder i vores netværk. Vi sletter så at sige folk i vores affiliation data, der ikke er 'linkere' (`filter()`) og samtidig sørger vi for at hvert individ kun optræder én gang per virksomhed (`distinct()`) - det kan jo være at nogen har mere end en rolle i samme bestyrelse (datasnavs?).

df_UK <- df_UK %>% 
  filter(n_memberships > 1) %>% 
  distinct(name, affiliation, .keep_all = TRUE)

##vi har fundet ud af hvor mange virksomheder der er i data sættet.
df_UK$affiliation %>% unique() %>% length()

#KOMPONENT
# Incidence matrice
incidence <- xtabs(formula = ~ name + affiliation,
                   data = df_UK,
                   sparse = TRUE)
# Adjacency matrice
adj_c <- Matrix::t(incidence) %*% incidence
# One-mode graf
gr_UK <- graph_from_adjacency_matrix(adj_c, mode = "undirected") %>%
  simplify(remove.multiple = TRUE, remove.loops = TRUE)

# Visualisering af hele one-mode netværket
plot1 <- gr_UK%>%
  ggraph(layout="fr") +
  geom_edge_link0(color = "gray60") +
  geom_node_point(color="red") +
  geom_node_text(aes(label=name), size=1, nudge_y = -0.2) +
  theme_graph() +
  labs(title = "One-mode netværk i Storbritanien",
       subtitle = "Figur 1")+
  theme(plot.title = element_text(hjust = 0.5))
ggsave("figur 1.png", width = 10, height = 8, dpi = 300)


par(mfrow = c(1, 2))


library(igraph)
library(ggraph)
library(ggplot2)
library(patchwork)

plot1 + plot2
# Komponenter & visualisering --------------------------------------------------
# Hvilke komponenter?
complist <- components(gr_UK)
# Dekomposering af grafen
comps <- decompose(gr_UK)
# Index
index <-
  table(complist$membership) %>%
  as_tibble(.name_repair = make.names) %>%
  arrange(desc(n)) %>%
  mutate(X = as.numeric(X)) %>%
  
  pull(1)

# vælger det største komponent
comp1 <- comps[[index[1]]]
# Densitet ---------------------------------------------------------------------
edge_density(gr_UK, loops= FALSE)
# Transitivitet ----------------------------------------------------------------
transitivity(gr_UK)

# Brobyggere
brokerage <- 1- constraint(gr_UK)
mean(brokerage)
# Stilængder -------------------------------------------------------------------
# Gennemsnitlig afstand i den største komponent
mean_distance(gr_UK)
# Diameter ---------------------------------------------------------------------
# Diameter i den største komponent
diameter(gr_UK, directed = FALSE)
x# Den længeste afstand
farthest.nodes(gr_UK, directed = FALSE)



## Lav graf udfra by
name_counts_UK <- table(df_UK$Citybased)

# Get top 5
top_names_UK <- head(sort(name_counts_UK, decreasing = TRUE), 50)

# Print result
print(top_names_UK)

## Lav ny variable til datasættet
df_UK$City_Category <- ifelse(df_UK$Citybased %in% 
                                c("LONDON", "EDINBURGH", "WYTHALL", 
                                  "MANCHESTER", "CHESTER"), 
                              df_UK$Citybased, "OTHER")

library(ggraph)
library(tidygraph)
##Filtrer  

df_UK_City <- df_UK %>% 
  filter(n_memberships > 2)

name_counts_UK <- table(df_UK_City$Citybased)

# Get top 5
top_names_UK <- head(sort(name_counts_UK, decreasing = TRUE), 50)

# Print result
print(top_names_UK)

## Lav ny variable til datasættet
df_UK_City$City_Category <- ifelse(df_UK_City$Citybased %in% 
                                c("LONDON", "EDINBURGH", "PERTH", 
                                  "YORK", "WYTHALL"), 
                              df_UK_City$Citybased, "OTHER")



# Find de 5 mest hyppige
top5_names <- names(sort(name_counts_UK, decreasing = TRUE)[1:5])
print(top5_names)


# Convert dataset to tidygraph format
graph_uk_1 <- as_tbl_graph(df_UK_City)

# Define color palette
city_colors <- c("LONDON" = "red", 
                 "EDINBURGH" = "blue", 
                 "PERTH" = "green", 
                 "YORK" = "purple", 
                 "WYTHALL" = "orange", 
                 "OTHER" = "gray")

# Create network plot
ggraph(graph_uk_1, layout = "fr") +
  geom_edge_link(color = "grey80", alpha = 0.5) +
  geom_node_point(aes(color = City_Category), size = 3) +
  scale_color_manual(values = city_colors) +  
  labs(title = "UK Network: Top 5 Cities + Others") +
  theme_graph(base_family = "Helvetica") +
  theme(legend.title = element_blank(),
        plot.title = element_text(size = 14, face = "bold"))




# Komponenter & visualisering --------------------------------------------------
# Hvilke komponenter?
complist <- components(gr_UK)
# Dekomposering af grafen
comps <- decompose(gr_UK)
# Index
index <-
  table(complist$membership) %>%
  as_tibble(.name_repair = make.names) %>%
  arrange(desc(n)) %>%
  mutate(X = as.numeric(X)) %>%
  pull(1)

# vælger det største komponent
comp1 <- comps[[index[1]]]
# Densitet ---------------------------------------------------------------------
edge_density(comp1, loops= FALSE)
# Transitivitet ----------------------------------------------------------------
transitivity(comp1)
# Brobyggere
brokerage <- 1- constraint(comp1)
mean(brokerage)
# Stilængder -------------------------------------------------------------------
# Gennemsnitlig afstand i den største komponent
mean_distance(comp1)
# Diameter ---------------------------------------------------------------------
# Diameter i den største komponent
diameter(comp1, directed = FALSE)
# Den længeste afstand
farthest.nodes(comp1, directed = FALSE)
# Centralitetsmål ------------------------------------------------------------
# Degree(antal forbindelser) # #betweenness(brobygningscentralitet) #closeness
(tæthedscentralitet) # Eigenvector

# Grupperer først vores centralitetsvariabler i en matrice og kalder den "Metrics"
metrics <- tibble(
  name = names(degree(comp1, mode="all")),
  degree = degree(comp1, mode="all"),
  betweenness = betweenness(comp1, directed=FALSE, weights=NA),
  closeness = closeness(comp1, mode="all", weights=NA),
  eigenvector = eigen_centrality(comp1, directed=FALSE, weights=NA)$vector,
  brokerage= 1-constraint(comp1))

#tilføjer mål til det største komponent
# degree(antal forbindelser)
V(comp1)$degree <- degree(comp1, mode = "all")
#betweenness (brobygningscentralitet)
V(comp1)$betweenness <- betweenness(comp1, directed = FALSE)
# closeness (tæthedscentralitet)
V(comp1)$closeness <- closeness(comp1, mode = "all")














# Centralitetsmål ------------------------------------------------------------
# Degree(antal forbindelser) # #betweenness(brobygningscentralitet) #closeness
(tæthedscentralitet) # Eigenvector

# Grupperer først vores centralitetsvariable i en matrice og kalder den "Metrics"
metrics <- tibble(
  name = names(degree(comp1, mode="all")),
  degree = degree(comp1, mode="all"),
  betweenness = betweenness(comp1, directed=FALSE, weights=NA),
  closeness = closeness(comp1, mode="all", weights=NA),
  eigenvector = eigen_centrality(comp1, directed=FALSE, weights=NA)$vector,
  brokerage= 1-constraint(comp1))

#tilføjer mål til det største komponent
# degree(antal forbindelser)
V(comp1)$degree <- degree(comp1, mode = "all")
#betweenness (brobygningscentralitet)
V(comp1)$betweenness <- betweenness(comp1, directed = FALSE)
# closeness (tæthedscentralitet)
V(comp1)$closeness <- closeness(comp1, mode = "all")
#Visualisering figur 4: Tæthedscentralitet I Danmark
comp1 %>%
  ggraph(layout = "graphopt") +
  geom_edge_link0(width=.5, alpha=0.4) +
  geom_node_point(aes(color=closeness, size=betweenness)) +
  scale_color_viridis() +
  theme_graph() +
  labs(title = "Tæthedscentralitet og brobygningscentralitet i Norge ",
       subtitle = "Figur 3")+
  scale_color_gradient2(low='white', high='red',
                        na.value='green',) +
  theme(plot.title = element_text(hjust = 0.5))+
  geom_node_label(aes(filter=name %in% {metrics %>% filter(closeness> 0.0075) %>% pull(name)}
                      #baseR version would be: net_metrics$name[net_metrics$betweenness_rank < 10]
                      ,label=name), alpha=0.65, size = 2, repel=T, force = 50)+
  geom_node_point(aes( filter=name %in% {metrics %>% filter(closeness> 0.0075) %>%
      pull(name)} #baseR version would be: net_metrics$name[net_metrics$betweenness_rank < 10]
      ,label=name), alpha=0.65, size = 2, repel=T, force = 50, color="blue")+
  labs(color="tæthedscentralitet", size="brogygningscentralitet")
ggsave("figur 3.png", width = 10, height = 8, dpi = 300)

# filter data for at inkluderer de 3 navne med den højeste tæthedscentralitet
top_names <- metrics %>% top_n(3, eigenvector)
# forkorter navnene til de 5 først forbogstaver
top_names$name_forkoertet<- str_sub(top_names$name, end = 6)
# Udregner korreleationskofficienten mellem brokerage og eigenvector
correlation <- cor(metrics$brokerage, metrics$eigenvector)
# figur 6: Forholdet mellem brokerager, eigenvector og tæthedcentralitet

metrics %>%
  ggplot(aes(x = brokerage, y = eigenvector, size=closeness, color = closeness)) +
  geom_point(alpha=0.8)+
  scale_color_gradient(low = "wheat", high = "red") +
  geom_text(data = top_names, aes(label = name_forkoertet), size = 2, nudge_y = -0.05, color =
              "black") +
  geom_point(data = top_names, aes(label = name_forkoertet), size = 1, color = "blue")+
  labs(title = "Forholdet mellem brokerage, eigenvector og tæthedscentralitet i Danmark ",
       subtitle = "Figur 5",
       caption = paste0("Korrelation i mellem eigenvector og brokerage = ", round(correlation, 2))) +
  theme(plot.title = element_text(hjust = 0.5))+
  labs(
    size="tæthedscentralitet",
    color="tæthedscentralitet")

ggsave("figur 6.png", width = 10, height = 8, dpi = 300)

#klynger-----------------------------------------------------------------------
# Udregner modularitet og antal klynger i det største komponent
louvain <- cluster_louvain(comp1)
# Hvor store er klyngerne?
louvain$membership
length(louvain$membership)
table(louvain$membership)
#Tjekker for modulariet i den største klyngee
louvain$modularity
modularity(louvain)

#rangerer vores centralitetmål fra vores matrice(metrics) og rangerer dem og laver en
summereret variabet der hedder sum_rank
a1 <- df_dk %>%
  count(affiliation, sort = TRUE) %>%
  filter(affiliation %in% metrics$name) %>%
  rename(N = n, name = affiliation)

metrics <-
  metrics %>%
  left_join(a1, by = "name") %>%
  select(name, N, everything())

m1 <- c('degree', 'betweenness', 'closeness', 'eigenvector')
for (i in m1) {
  metrics <- metrics %>% arrange(desc(get(i)))
  metrics <- metrics %>% mutate(!!paste0(i, "_rank") := rleid(get(i)))
}
metrics <-
  metrics %>%
  mutate(sum_rank = degree_rank+betweenness_rank+closeness_rank+eigenvector_rank) %>%
  arrange(sum_rank)
#Gemmer metricen i excel og sætter ind i opgaven som bilag
write.xlsx(metrics,"/Users/georggilmartin/Desktop/Vikrsomhedsstrategi/R eksamen
/metrics_hele.xlsx", sheetName="sheet")