##########################
#
#  Øvelse 1: Introduktion til netværksanalyse 
#
###########################


# SETTING UP --------------------------------------------------------------

# Install packages (only execute once) - if there is an error, check troubleshoot panel here https://agamerdinger.com/teaching/virksomhedsstrategi/
# 
#loading the libraries
library("tidyverse")
library(ggraph)
library(igraph)

# loading data | If there is an error, it is probably because of your working directory
den <- read_csv("input/den17-no-nordic-letters.csv")

#select columns name and affiliation
den_name_affil <- den %>% select(name, affiliation, gender) 

#count affiliation 
den %>% count(affiliation, sort= TRUE) %>% head(20)
den %>% count(sector, sort = TRUE)
den %>% count(tags, sort = TRUE)

# check that this does the same thing than this: den %>% group_by(affiliation) %>% summarize(n = n())

#filter dataset den to only include Corporations, then call it den1
den1 <- den %>% filter(sector == "Corporations")


# Making a two-mode network --------------------------------------------------------

# Make an incidence matrix
incidence <- xtabs(formula = ~name + affiliation, data=den1, sparse=TRUE)

# View the incidence matrix 
incidence[1:5,1:5]

# make an adjacency matrix for individuals
adj_i <- incidence %*% Matrix::t(incidence)
adj_i[1:5,1:5]

# make an adjacency matrix for corporations
adj_a <- Matrix::t(incidence) %*% incidence 
adj_a[1:5,1:5]


# make a graph object "gr" from the incidence matrix !!husk bi
gr <- incidence %>% graph_from_biadjacency_matrix(., directed = FALSE)
table(V(gr)$type)

# make a graph from adjacency (gr1 for individuals)
gr1 <- adj_i %>% graph_from_adjacency_matrix(., mode = "undirected") %>% 
  simplify(remove.multiple = TRUE, remove.loops = TRUE)

# make a graph from adjacency (gr2 for corporations)
gr2 <- adj_a %>% graph_from_adjacency_matrix(., mode = "undirected") %>% 
  simplify(remove.multiple = TRUE, remove.loops = TRUE)

# Visualize the network ---------------------------------------------------

# Visualize the two mode graph

gr %>% 
  ggraph(layout = "kk") +
  geom_edge_link0(color = "gray") +
  geom_node_point(aes(color = type, size = type)) +
  scale_size_manual(values = c(0.6,1), guide = "none") +
  scale_color_manual(values=c("red", "blue"), labels=c("individuals", "corporations")) +
  theme_graph() 


# Make and visualize the one-mode network ---------------------------------

# Visualize the new graph for corporations
gr2 %>% 
  ggraph(layout="mds") +
  geom_edge_link0(color = "gray60") +
  geom_node_point() +
  geom_node_text(aes(label=name), size=1) +
  theme_graph()
