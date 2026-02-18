#############################################/
# Session2 øvelse ----
# Indlæs data og lav netværks
#############################################/

library(tidyverse)
library(tidygraph)
library(igraph)
library(ggraph)
library(Matrix)

# 1: Indlæs datasættet "data/Session2_exampledata.csv"
# Husk read_csv()


# 2: Hvilke variable er der i data?
#     - husk glimpse()
# Hvor mange unikke virksomheder er der? Hvor mange personer? Hvormange subbranches og hvor mange i hver?
#     - husk dt %>% count(VARIABLE NAVN) eller dt %>% summarise(n = n_distinct(VARIABELNAVN))

# 3: Lav evt. omkodning, hvis. fx. kan kategorier med få udfald måske slås sammen til en 'other' kategori
#     - husk mutate() og case_when(udsagn~"ny værdi", .default = ?)  
#     == er lig 
#     %in% optræder i c("","","")
#     eksempel: dt %>% mutate(VAR = case_when(VAR %in% c("???", "!!!", "...")~"Other", .default = VAR))


# 3: Lav tre netværksmatricer; 
#      - en bi_adjacency (twomode) med personer og virksomheder
#      - en adjacency med personer
#      - en adjacency med virksomheder
#  Husk: xtabs(......, sparse = T) og %*% samt t()
  


# 4: Lav tre netværksobjekter
##    - brug graph_from_biadjacency_matrix til biadjacency (twomode)
##        directed ?? TRUE eller FALSE, multiple?? TRUE eller FALSE
##    - graph_from_adjacency_matrix til adjacency (onemode)
##        mode ?? directed eller undirected? og diag = FALSE






# 5: Tre visualiseringer
p1 <- gr_bi %>% 
  ggraph("kk") +
  geom_edge_link(aes(edge_alpha = 0.8), color = "gray") +
  geom_node_point(aes(color = type, shape = type, size = type)) +
  geom_node_label(aes(filter = type, label = name, color = type), repel = TRUE, show.legend = FALSE,size = 5, family = "serif") +
  scale_color_manual(values = c("salmon1", "steelblue2"), labels = c("Bestyrelsesmedlem", "Virksomhed"), name = "") +
  scale_shape_manual(values = c(20, 18), labels = c("Bestyrelsesmedlem", "Virksomhed"), name = "") +
  scale_size_manual(values = c(6, 10), labels = c("Bestyrelsesmedlem", "Virksomhed"), name = "") +
  theme_graph(base_family = "serif") + guides(edge_alpha = "none", color = guide_legend(position = "bottom")) +
  ggtitle( "Two-Mode Network (bipartite):", subtitle = "(Bestyrelsesmedlemmer <-> Virksomheder)") + theme(title = element_text(size = 8))
p1


p2 <- gr_ind %>% 
  ggraph("kk") +
  geom_edge_link(aes(edge_alpha = 0.8), color = "gray") +
  geom_node_point(color = "salmon1", size = 6, shape = 20) +
  theme_graph(base_family = "serif") + guides(edge_alpha = "none") +
  ggtitle("Medlems-projektion", subtitle = "(delte bestyrelsmedlemskaber)") + theme(title = element_text(size = 8))
p2


## Her tilføjer vi en variabel med subbranches til netværket....
gr_org  <- gr_org %>% 
  activate(nodes) %>% 
  left_join(dt %>% distinct(affiliation, tech_subbranch), by = c("name" = "affiliation"))

p3 <- gr_org %>%  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = 0.8), color = "gray") +
  geom_node_point(aes(color = tech_subbranch), size = 6, shape =18) +
  geom_node_text(aes(label = name), repel = TRUE, size = 5) +
  theme_graph(base_family = "serif") + guides(edge_alpha = "none") + 
  labs(color = "Subbranch") +
  ggtitle("Virksomheds-projektion", subtitle = "(delte bestyrelsesmedlemmer)") + 
  theme(title = element_text(size = 8))
p3

ggsave(filename = "output/tech_twomode.png", plot = p1, width = 20, height = 20, units = c("cm"))
ggsave(filename = "output/tech_individuals.png", plot = p2, width = 20, height = 20, units = c("cm"))
ggsave(filename = "output/tech_affiliations.png", plot = p3, width = 20, height = 20, units = c("cm"))
