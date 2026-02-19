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
dt <- read_csv("data/Session2_exampledata.csv")

# 2: Hvilke variable er der i data?
glimpse(dt)
# fortæller mig at jeg indlæst et datasæt med 96 rækker og 3 variable. person_name, som er navnene på (fiktive) bestyrelsesmedlemmer i forskellige (fiktive) tech-virksomheder (variablen affiliation). Desuden er der en variabel tech_subbranch som er en inddeling af virksomhederne i en række mere eller mindre fiktive tech områder


# Hvor mange unikke virksomheder er der? Hvor mange personer? Hvormange subbranches og hvor mange i hver?
dt %>% summarise(n_ind = n_distinct(person_name), n_virk = n_distinct(affiliation), n_subbranches = n_distinct(tech_subbranch))
# fortæller mig at der er 79 unikke personer med poster i 20 unikke selskaber, som tilhører 6 forskellige sub_branches
dt %>% group_by(tech_subbranch) %>% summarise(n = n_distinct(affiliation))
# fortæller mig desuden at der er 7 virksomheder der laver AI og 6 der arbejder med Cybersecurity og 4 med Cloud Computing. I de resterende tre kategorier er der kun en virksomhed.

# 3: Lav evt. omkodning, hvis. fx. kan kategorier med få udfald måske slås sammen til en 'other' kategori
# eftersom små kategorier nogen gange kan være irriterende, vil jeg gerne slå dem sammen til én kategori, som jeg kalder other
dt <- dt %>% mutate(tech_subbranch = case_when(tech_subbranch %in% c("BioDigital Interfaces", "Extended Reality Platforms (XR)", "Quantum Edge Computing")~"Other", .default = tech_subbranch))
#resultatet er at vi nu kun har 4 kategorier
dt %>% group_by(tech_subbranch) %>% summarise(n = n_distinct(affiliation))


# 3: Lav tre netværksmatricer; 
#      - en bi_adjacency (twomode) med personer og virksomheder
adj_bi <- xtabs(data = dt, formula = ~person_name + affiliation, sparse =T)
#      - en adjacency med personer
adj_ind <- adj_bi %*% t(adj_bi)
#      - en adjacency med virksomheder
adj_virk <- t(adj_bi) %*% adj_bi
  
# 4: Lav tre netværksobjekter med funktioner fra Igraph
##    - brug graph_from_biadjacency_matrix til biadjacency (twomode)
##        directed ?? TRUE eller FALSE, multiple?? TRUE eller FALSE
net_bi <- graph_from_biadjacency_matrix(adj_bi, directed = FALSE) %>% simplify()
##    - graph_from_adjacency_matrix til adjacency (onemode)
net_ind <- graph_from_adjacency_matrix(adj_ind, mode = "undirected") %>% simplify()
##        mode ?? directed eller undirected? og diag = FALSE
net_virk <- graph_from_adjacency_matrix(adj_virk, mode = "undirected") %>% simplify()
##      I kan lave dem til tidygraph format bagefter med as_tbl_graph()

net_bi   <- as_tbl_graph(net_bi)
net_ind  <- as_tbl_graph(net_ind)
net_virk <- as_tbl_graph(net_virk)



# 5: Tre visualiseringer
p1 <- net_bi %>% 
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

p2 <- net_ind %>% 
  ggraph("kk") +
  geom_edge_link(aes(edge_alpha = 0.8), color = "gray") +
  geom_node_point(color = "salmon1", size = 6, shape = 20) +
  theme_graph(base_family = "serif") + guides(edge_alpha = "none") +
  ggtitle("Medlems-projektion", subtitle = "(delte bestyrelsmedlemskaber)") + theme(title = element_text(size = 8))
p2

## Her tilføjer vi en variabel med subbranches til netværket....
net_virk  <- net_virk %>% 
  activate(nodes) %>% 
  left_join(dt %>% distinct(affiliation, tech_subbranch), by = c("name" = "affiliation"))

p3 <- net_virk %>%  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = 0.8), color = "gray") +
  geom_node_point(aes(color = tech_subbranch), size = 6, shape =18) +
  geom_node_text(aes(label = name), repel = TRUE, size = 5) +
  theme_graph(base_family = "serif") + guides(edge_alpha = "none") + 
  labs(color = "Subbranch") +
  ggtitle("Virksomheds-projektion", subtitle = "(delte bestyrelsesmedlemmer)") + 
  theme(title = element_text(size = 8))




ggsave(filename = "output/tech_twomode.png", plot = p1, width = 20, height = 20, units = c("cm"))
ggsave(filename = "output/tech_individuals.png", plot = p2, width = 20, height = 20, units = c("cm"))
ggsave(filename = "output/tech_affiliations.png", plot = p3, width = 20, height = 20, units = c("cm"))
