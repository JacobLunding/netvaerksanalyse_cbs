###############################################/
#
#  Øvelse 3: Collecting network data
#
###############################################/
### To do...: upload nye filer til Canvas....

###############################################/
# SETTING UP --------------------------------------------------------------
###############################################/

#setting the working directory


#loading the libraries
library(tidyverse)
library(ggraph)
library(graphlayouts)
library(igraph)
library(Matrix)
library(readxl)
# for cleaning functions in orbis


source("functions/custom_functions.R")
source("functions/networkfunctions.R")
###############################################/
# Subsetting den17 dataset by tags ----------------------------------------
###############################################/


# load data 
den  <- read_csv("input/den17-no-nordic-letters.csv")

# subset of corporations that have a valid cvr affiliation
den1 <- den %>% filter(sector == "Corporations" & !is.na(cvr_affiliation))

# what kind of sectors are included in the data set?
sector <- standard.sectors(sets = "Danish")

# How to see all the tags. 
# all tags of the raw data set including tags from other sectors 
show.all.tags(den) 
# all tags of company boards 
show.all.tags(den1) 

# Subsetting den1 by one tag

# suppose we want to get a subset of den1 that includes all names and affiliations with the tag "Banks"
bank <- den1 %>% has.tags(., "Banks", result = "den")

# now, we only want the UNIQUE affiliations 
bank1 <- den1 %>% has.tags(., "Banks", result = "affil")

# and only UNIQUE names
bank2 <- den1 %>% has.tags(., "Banks", result = "name")

# Subsetting den1 by several tags

# we can also subset by several tags, first we make a vector
tags <- c("Banks", "Finance", "FINA", "Pensions", "Insurance")

# this gives us the whole data tibble 
finance <-has.tags(den1, tags, result = "den")


# now, we only want the UNIQUE affiliations 
finance1 <-has.tags(den1, tags, result = "affil")

# and only UNIQUE names
finance2 <-has.tags(den1, tags, result = "name")

# How to find those nodes that are members of two tags? grep (Global Regular Expression search and Print) + l for logical
a1 <- den1 %>% filter(grepl("Bank", tags, ignore.case = TRUE)) %>% pull(name)
b1 <- den1 %>% filter(grepl("Farmin", tags, ignore.case = TRUE)) %>% pull(name)
c1 <- intersect(a1, b1)
# or:
c2 <- den1 %>% group_by(name) %>% filter(any(grepl("Banks", tags)) & any(grepl("Farming", tags))) %>% pull(name) %>% unique()

###############################################/
# Loading graph object ----------------------------------------------------
# From our finance data set
###############################################/

# 1. Creating an incidence matrix and then an adjacency matrix
incidence <- xtabs(data = finance, formula = ~ name + affiliation, sparse = TRUE)

adj_i <- incidence %*% t(incidence)

adj_c <- t(incidence) %*% incidence

# 2. Loading two-mode network "gr" and one-mode networks "gr1" and "gr2"

gr <- graph_from_biadjacency_matrix(incidence, directed = FALSE) %>% 
  simplify(remove.multiple = TRUE, remove.loops = TRUE)

V(gr)$type
# setting types, the two-mode graph object contains to sets of vertices. By default the rows of the input matrix, here individuals will be FALSE and the columns of the input matrix, here corporations, will be TRUE.
# We use the function V() to get to the vertice attributes
V(gr)$type <- ifelse(V(gr)$type == FALSE, "individuals", "corporations")


gr1 <- graph_from_adjacency_matrix(adj_i, mode = "undirected") %>% 
  simplify(remove.multiple = TRUE, remove.loops = TRUE)

gr2 <- graph_from_adjacency_matrix(adj_c, mode = "undirected") %>% 
  simplify(remove.multiple = TRUE, remove.loops = TRUE)



# Visualizing two-mode networks  ---------------------------------------

gr %>% 
  ggraph("fr") +
  # adding edges...
  geom_edge_link0(color = "gray40", edge_alpha = .3, edge_width = .5) +
  # setting size for corporations
  geom_node_point(aes(filter=type=="corporations", color = type, fill = type), alpha = 0.8, size = 4) +
  # setting size for individuals
  geom_node_point(aes(filter=type=="individuals", color = type, fill = type), alpha = 0.8, size = 1.5) +
  # changing the legend content
  scale_color_manual(values = c("salmon2", "steelblue"), 
                     labels = c("corporations", "individuals")) +
  labs(title = "Corporate interlocks in the Danish financial sector (bipartite)", 
       # changing the legend header
       color = "Node types", fill = "Node types") +
  theme_graph()


# Select largest component 
largest_comp <- largest_component(gr)

largest_comp %>% 
  ggraph("fr") +
  # adding edges...
  geom_edge_link0(color = "gray40", edge_alpha = .3, edge_width = .5) +
  # setting size for corporations
  geom_node_point(aes(filter=type=="corporations", color = type, fill = type), alpha = 0.8, size = 4) +
  # setting size for individuals
  geom_node_point(aes(filter=type=="individuals", color = type, fill = type), alpha = 0.8, size = 1.5) +
  # changing the legend content
  scale_color_manual(values = c("salmon2", "steelblue"), 
                     labels = c("corporations", "individuals")) +
  labs(title = "Largest component in the Danish financial sector (bipartite)", 
       # changing the legend header
       color = "Node types", fill = "Node types") +
  theme_graph()

# Adding graph attributes  ---------------------------------------

######################/
# person-person network: example: add gender
######################/
gr1
# make a data.frame with names in the same order(!!) as in the network object and a data.frame with name and gender
add_gender <- data.frame(name = V(gr1)$name)
gender     <- finance %>% distinct(name, gender) # distinct to get only one row per name

# join them together
add_gender <- add_gender %>% left_join(gender, by = "name")
add_gender %>% count(gender, sort = TRUE)

add_gender <- add_gender %>% mutate(gender = case_when(is.na(gender) | gender == "Binominal" ~"Unknown", .default = gender))
# add gender to the graph object : Two ways....

V(gr1)$gender <- add_gender %>% pull(gender)
gr1 <- gr1 %>% set.vertex.attribute(., name = "gender", value = add_gender$gender)

# visualize
gr1 %>% 
  ggraph("fr") +
  geom_edge_link0(color = "gray40", edge_alpha = 0.4, edge_width = .2) +
  geom_node_point(aes(color = gender %>% factor(levels = c("Men", "Women", "Unknown"))), size = 2) + scale_color_manual(values = c("steelblue", "salmon2", "grey24")) +
  labs(title = "Gender in the Danish financial network", color = "Gender") +
  theme_graph()

######################/
# organization-organization network: example: add sector
######################/
gr2

# We use the has.tags function to find the affiliation with the different tags
bank      <- has.tags(den1, "Banks", result = "affil")
fin       <- has.tags(den1, c("Finance", "FINA"), "affil")
pension   <- has.tags(den1, "Pensions", "affil")
insurance <- has.tags(den1, "Insurance", "affil")

# first, let us create a data frame which binds three data frames together into one. We give each node a tag name. 
affil_sector <- rbind(
  
  tibble(name=fin, tag ="finance"), 
  tibble(name=bank, tag ="bank"), 
  tibble(name=pension, tag ="pension"),
  tibble(name=insurance, tag ="insurance")
)

# second, let us make a table
affil_sector <- xtabs(formula = ~ name + tag, data = affil_sector)
# change matrix to tibble format 
affil_sector <- affil_sector %>% as_tibble()

firmtype <- affil_sector %>% filter(n > 0) %>% group_by(name) %>% summarise(type = paste(tag, collapse = " + "))
firmtype %>% count(type, sort = TRUE)

firmtype <- firmtype %>% mutate(type = if_else(type == "finance", "other finance", type)) 
firmtype <- firmtype %>% mutate(type = gsub("(finance \\+ )|( \\+ finance)", "", type))

# Now, let's add the firmtypes to our graph obj .....
add_firmtype <- data.frame(name = V(gr2)$name)
add_firmtype <- add_firmtype %>% left_join(., firmtype, by = "name")

# again two ways of doing it....
V(gr2)$firmtype  <- add_firmtype$type
gr2 <- gr2 %>% set.vertex.attribute(name =  "firmtype", value = add_firmtype$type)

# Select largest component 
comps        <- decompose.graph(gr2)
size         <- comps %>% map(.f = vcount) %>% unlist
largest_comp <- comps[[which.max(size)]]

# To have an ordered attribute
firm_type    <- V(largest_comp)$firmtype %>% factor(levels = c("bank", "insurance", "pension", "other finance", "bank + insurance + pension"))

# visualize
largest_comp %>% 
  ggraph(layout = "fr") + 
  geom_edge_link0(edge_width=.3, edge_alpha=0.4) + 
  geom_node_label(aes(color=firm_type, label=name), size=3, nudge_y=-0.05, repel=TRUE) + 
  geom_node_point(aes(color=firm_type), size=4) + 
  labs(title = "Corporate interlocks in the Danish financial sector, by sector", color = "Sector") +
  theme_graph() 



###############################################/
# Orbis -------------------------------------------------------------------
###############################################/

df <- read_xlsx("input/tobaco_and_alcohol.xlsx", guess_max = 100000, sheet = 2)

df <- df %>% select(`Company name Latin alphabet`, `Country ISO code`, `NACE Rev. 2, core code (4 digits)`, `DMFull name`, `DMUCI (Unique Contact Identifier)`, `DMCurrent or previous`, `DMJob title (in English)`, `DMType of role`, `DMBoard, committee or department`, `DMLevel of responsibility`)

df <- df %>% rename(affiliation = `Company name Latin alphabet`, 
                    country = `Country ISO code`, 
                    sector = `NACE Rev. 2, core code (4 digits)`,
                    name = `DMFull name`, 
                    id = `DMUCI (Unique Contact Identifier)`, 
                    role_status = `DMCurrent or previous`, 
                    role = `DMJob title (in English)`,
                    role_type_abbrev = `DMType of role`,
                    role_type = `DMBoard, committee or department`,
                    role_level = `DMLevel of responsibility`)

df <- df %>% mutate(person = substr(id, 1, 1) == "P", gender = case_when(
  grepl("^mr", name, ignore.case = TRUE) ~ "male",
  grepl("^ms", name, ignore.case = TRUE) ~ "female", 
  .default = "unkown"))

df <- df %>% filter(!is.na(role_status))


df %>% count(role_level, sort = TRUE) %>% View

df <- df %>% 
  mutate(role_level_rec = case_when(
    grepl("Member", role_level, ignore.case = T) ~ "member",
    grepl("executive", role_level, ignore.case = T) ~ "executive",
    grepl("vice (pres|chair)", role_level, ignore.case = T) ~ "vice chairman", 
    grepl("president|chairman", role_level, ignore.case = T) ~ "chairman", 
    .default = "other"))

df         <- df %>% mutate(role_level_rec = case_when(role_level_rec == "other" & 
                                                         (grepl("SenMan", role_type_abbrev, ignore.case = T) | 
                                                            grepl("manager", role_level, ignore.case = T)) ~ "executive",
                                                       role_level_rec == "other" & grepl("chief", role_level, ignore.case = T) ~ "executive",
                                                       .default = role_level_rec))

# Let's subset, with the filter function, to only persons and only current positions and get rid of other in role.
df_current <- df %>% filter(person == TRUE & role_status == "Current" & role_level_rec != "other")

# Let's see the distibution of roles in the data
df_current %>% ungroup() %>% count(role_level_rec, sort = TRUE)

# see how many positions in each company
df_current %>% ungroup() %>% count(affiliation, sort = TRUE)

# share of men and board size per company (will be shown in later exercises too)
gender <- df_current %>% 
  count(affiliation, gender) %>% 
  filter(n > 1) %>% 
  group_by(affiliation) %>% 
  mutate(board_size = sum(n), share = n/board_size) %>% 
  filter(gender == "female") %>% 
  arrange(-share)


ggplot(gender) + geom_histogram(aes(x = share)) + ylab("Company boards") + xlab("Share of women") + theme_bw()


# Let's make a variable in our data with number of board memberships per individual
df_current <- df_current %>% group_by(name) %>% mutate(n_memberships = n())
df_current %>% ungroup() %>%  count(n_memberships)
# Let's delete all boards with only one member
df_current <- df_current %>% filter(n_memberships > 1)

incidence <- xtabs(data = df_current, formula = ~name + affiliation, sparse = T)
adj_c     <- Matrix::t(incidence) %*% incidence

gr <- graph_from_adjacency_matrix(adj_c, mode = "undirected") %>% 
  simplify(remove.multiple = TRUE, remove.loops = TRUE)

# add sector as attribute...
add_sector <- data.frame(affiliation = V(gr)$name)
sector     <- df_current %>% ungroup %>% distinct(affiliation, sector) 

sector     <- sector %>% 
  mutate(sector = substr(sector, 1,2)) %>% 
  distinct() %>% 
  mutate(sector = case_when(
  sector == "12" ~ "Tobacco",
  sector == "11" ~ "Alcohol", 
  .default = NA))

add_sector <- add_sector %>% left_join(., sector, by = "affiliation")
V(gr)$sector <- add_sector$sector
#V(gr)$type <- ifelse(V(gr)$name %in% df_current$name, "individuals", "corporations")
gr <- gr - which(gr %>% degree == 0)

# largest component
largest_comp <- largest_component(gr)

largest_comp %>% 
  ggraph("fr") +
  # adding edges...
  geom_edge_link0(color = "gray40", edge_alpha = 1, edge_width = .6) +
  # adding vertices colored by type...
  #geom_node_point(aes(color = type)) +
  # setting size for corporations
  geom_node_point(aes(color = sector), alpha = 1, size = 3) +
  # setting size for individuals
  # geom_node_point(aes(filter= type=="individuals", color = type), alpha = 1, size = 1) +
  # adding labels for corporations
   geom_node_label(aes(filter = grepl("british american tobacco plc|carlsberg a/S|PHILIP MORRIS INTERNATIONAL INC|HEINEKEN N\\.|diageo p", name, ignore.case = T), label = name, color = sector), size = 3,
                  # no overlap
                  repel = TRUE) +
  # changing the legend content
  scale_color_manual(values = c("salmon2", "steelblue", "grey"), 
                     labels = c("Alcohol", "Tobacco", "NA")) +
  labs(title = "Biggest component of corporate interlocks in alcohol and tobacco sector", 
       # changing the legend header
       color = "Node types") +
  theme_graph()

