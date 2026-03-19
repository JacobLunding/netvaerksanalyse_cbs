
library(tidyverse)
library(readxl)
library(writexl)
library(igraph)
library(tidygraph)
library(ggraph)
library(ggpubr)
library(Matrix)
# Ny pakke at installere
# install.packages("RColorBrewer")
library(RColorBrewer)
source("functions/community_plot.R")
###################################################################################################/
# 1. Læs datafil ----
###################################################################################################/
load("data/Power Elite 2024.Rda")
net.pe24
pe24

net.pe24 <- net.pe24 %>% left_join(pe24)

###################################################################################################/
# 8.1 Centralitetsmål mv. ----
###################################################################################################/

# Lad os samle de nodespecifikke centralitetsmål for den største komponent
# Her kan I jo tilføje hvad I har brug for:
# 
net.pe24 <- net.pe24 %>% mutate(
  degree        = centrality_degree(),
  betweenness   = centrality_betweenness(),
  closeness     = centrality_closeness(),
  eigen         = centrality_eigen(),
  constraint    = node_constraint(),
  brokerage     = 1/ constraint,
  local_trans   = local_transitivity(),
  degree_w      = centrality_degree(weights = 1/weight),
  betweenness_w = centrality_betweenness(weights = 1/weight),
  closeness_w   = centrality_closeness(weights = 1/weight),
  eigen_w       = centrality_eigen(weights = 1/weight),
  constraint_w  = node_constraint(weights = 1/weight),
  brokerage_w   = 1/ constraint_w,
  local_trans_w = local_transitivity(weights = 1/weight))

net.pe24 %>% as_tibble() %>% View()

##############################/
# Homofili / assortativity
##############################/
## Først kan vi jo lige se på om 'ens' er hyppigere forbundet end de 'burde' være ift i et tilfældigt netværk med samme fordeling.
net.pe24 %>% with_graph(tibble(gender_assort = graph_assortativity(women),
                               Sektor_assort = graph_assortativity(Sektor)))

###############/
# Kliker / subgrupper
###############/
# Kliker er subsets af noder, hvis interne densitet er 1. 
# Dvs et subset af noder hvor alle er forbundne til hinanden. En node kan godt indgå i flere kliker.
# Det kiggede vi på tidligere

###################################################################################################/
# Community strukturer i netværket ----
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



# Vi kan begregne modularitet i netværket for en på forhånd given inddeling af noderne:
# fx Køn
# 
net.pe24 %>% with_graph(tibble(
  gender_mod_w = graph_modularity(group = factor(women), weights = 1/weight),
  gender_mod = graph_modularity(group = factor(women))))

# eller Sektor
net.pe24 %>% with_graph(tibble(
  sector_mod_w = graph_modularity(group = factor(Sektor), weights = 1/weight),
  sector_mod = graph_modularity(group = factor(Sektor))))




#################
# Louvain ----
#################

net.pe24 <- net.pe24 %>% mutate(groups = group_louvain(weights = 1/weight))

net.pe24 %>% as_tibble() %>% count(groups)
net.pe24 %>% with_graph(graph_modularity(group = groups, weights = 1/weight))


###################################################################################################/
# Visualisering af community strukturen ----
# 
###################################################################################################/
# Vi tilføjer klyngemedlemskabet som en node attribute, vi vil gerne have pæne labels så vi tilføjer et nul foran et cifrede tal (det gør vi fordi tal, når de læses som text sorteres anderledes)
tal1 <- c(1:20) %>% as.character()
tal2 <- c(1:20) %>% sprintf("%02d", .)
sort(tal1)
sort(tal2)

net.pe24 <- net.pe24 %>% 
  mutate(groups = sprintf("%02d", groups))


# Vi vil gerne kunne farvelægge edges (streger) efter hvilken klynge de to noder den forbinder, tilhører:
# 
# Med nedenstående kode laver vi en edge attribute, så edges mellem to personer fra SAMME community får værdien svarende til denne gruppe, mens edges på tværs af grupper får værdien "9999"
# Den variabel kan vi nemlig bruge, i det følgende plot, så vi kan farve edges internet i gruppper og dermed fremhæve 'gruppe'strukturen
net.pe24 <- net.pe24 %>% 
  activate(edges) %>% 
  mutate(groups_a = .N()$groups[from],
         groups_b = .N()$groups[to],
    groups = case_when(.N()$groups[from] == .N()$groups[to]~.N()$groups[from], .default = "9999")) %>% 
  activate(nodes)

#Vi laver et fast layout med create_layout(), som vi kan genbruge i senere plots, så de kommer til at se ens ud.
layout <- create_layout(net.pe24, "fr", weights = 1/weight)

# Nu kan vi lave et plot af den fundne klyngestruktur
net.pe24 %>% 
  ggraph(layout = layout) +
  geom_edge_link0(aes(filter=groups!= "9999", color = groups, width = 1/weight, alpha = 1/weight), show.legend = FALSE) +
  scale_edge_width_continuous(range = c(0.2,0.6))+
  scale_edge_alpha_continuous(range = c(0.1,0.6))+
  geom_edge_link0(aes(filter=groups == "9999"), color = "grey60", width = 0.1, alpha = 0.2) +
  geom_node_point(aes(fill=groups), color = "black", shape = 21, alpha=0.95, size=3) + 
  theme_graph() 


# Lad os kigge lidt på de enkelte klynger i netværket
focus    <- net.pe24 %>% as_tibble() %>% pull(groups) %>% unique() %>% sort()
amount   <- focus %>% n_distinct()

# Farvelægning: 
# Option 1: Hvis det er kontinuert variabel man vil farvelægge efter kan viridis functionen være god: scale_color_viridis() eller scale_edge_color_viridis() 
# Option 2: Hvis det er en variabel med kategoier, kan man vælge farveskalaer fra RColorBrewer palettes

# RcolorBrewer 

# Se farverne
display.brewer.all()

# Nu kan vi udvælge de farver vi gerne vil have
mycolors <- colorRampPalette(brewer.pal(6, "Set1"))(amount)
rows     <- sample(length(mycolors))
mycolors <- mycolors[rows]
names(mycolors) <- focus

klyngeplot <- map(1:length(focus), 
                  function(x) community_plot(net.pe24, x, clusters = focus, layout = layout, colors = mycolors, edge_attr = "groups"))

mod_score <- net.pe24 %>% with_graph(graph_modularity(as.factor(groups), weights = 1/weight)) %>% round(2)

ggarrange(plotlist = klyngeplot, ncol = 2, nrow = 3) %>% 
  annotate_figure(
  top = text_grob("Clusters in the Danish Power Elite 2024", family = "serif"),
  bottom = text_grob(paste0("modularity score =", mod_score), family = "serif", hjust = -1))








#############################################/
# Kan vi mon beskrive klyngerne meningsfuld?
#############################################/

# Vi kan altså inddele netværket i X communities, som optimerer andelen af within-ties ift. et tilfældigt netværk...
# 
# Vi så før at Sektoropdelingen også havde en moderat modularitets score, så lad os prøve at 


# Og dernæst kan vi prøve at tælle op for hver sektor og tjekke fordelingen
cl_sector <- net.pe24 %>% as_tibble() %>% count(groups, Sektor)

share_cl  <- cl_sector %>% group_by(groups) %>% mutate(pct = n/ sum(n), type = "within cluster") %>% select(-n)
share_sec <- cl_sector %>% group_by(Sektor) %>% mutate(pct = n/ sum(n), type = "of sector") %>% select(-n)

cluster_sector <- bind_rows(share_cl, share_sec)
cluster_sector <- cluster_sector %>% pivot_wider(id_cols = c("Sektor", "type"), names_from = groups, values_from = pct) %>% arrange(Sektor)

write_xlsx(cluster_sector, "output/pe24_cluster_sector.xlsx")



################################################/
# Statistik test af overrepræsentation....
################################################/
################################################
# Lad os lave et data objekt med klynger, virksomhedsnavn, sektor, køn
klyngedata <- net.pe24 %>% as_tibble() %>% 
  select(name, groups, Sektor, Under_undersektor, women) %>% 
  mutate(Sektor_spec = case_when(!is.na(Under_undersektor)~paste0(Sektor, ": ", Under_undersektor), .default = NA))

View(klyngedata)
# Kan vi lave noget statistik?
# Hvis vi nu krydser fx sektor og klynge og laver en chi test, så vi kan kigge på de standardiserede residualer...

sec_tab      <- table(klyngedata$Sektor, klyngedata$groups)
sector_chi_t <- chisq.test(sec_tab)
sector_chi_t$stdres

# Nu kan vi lave en tabel på sundersektor og cluster
sub_sec_tab      <- table(klyngedata$Sektor_spec, klyngedata$groups)
sub_sector_chi_t <- chisq.test(sub_sec_tab)
sub_sector_chi_t$stdres

# Nu kan vi lave en tabel på sundersektor og cluster
women_tab      <- table(klyngedata %>% mutate(women = case_when(women~"Kvinde", .default = "Mand")) %>% pull(women), klyngedata$groups)
women_chi_t <- chisq.test(women_tab)
women_chi_t$stdres




stdres <- bind_rows(women_chi_t$stdres %>% 
                      data.frame() %>%
                      mutate(type = "gender"),
                    sector_chi_t$stdres %>% 
                      data.frame() %>% 
                      mutate(type = "sector"),
                    sub_sector_chi_t$stdres %>% 
                      data.frame() %>% 
                      mutate(type = "sub_sector"))

colnames(stdres) <- c("Value", "cluster", "z-score", "type")
stdres <- stdres %>% relocate(type, .before = "Value")

# Hvis vi kigger på den og husker at stdres > 1.65 er en signifikant 'overrepræsentation' og < - 1.65 en signifikant underrepræsentation..:
# 
stdres %>% arrange(-`z-score`) %>% arrange(cluster) %>% View()

group_description <- stdres %>% 
  arrange(cluster) %>% 
  filter(`z-score` > 1.65) %>% 
  pivot_wider(id_cols = Value, names_from = cluster, values_from = `z-score`, values_fill = NA) 


#####################################/
# Hvordan er klyngerne forbundne?
#####################################/
# vi kan prøve at lægge alle vertices i de forskellige clusters sammen.
# Vi har tidligere tilføjet nogle edge attribute, der fortæller hvilke klynger noderne i hver ende af en edge tilhører: 
# 
net.pe24 <- net.pe24 %>% activate(edges) %>% 
  mutate(group_a = .N()$groups[from],
         group_b = .N()$groups[to],
         name_a = .N()$name[from],
         name_b = .N()$name[to],
         org_a = .N()$Organisation[from],
         org_b = .N()$Organisation[to],)
e_data        <- net.pe24 %>% as_tibble("edges") 


# Interne ties 
edge_data_int <- e_data %>% 
  filter(group_a == group_b) %>%  # Vi kigger kun på ties, der er mellem to noder fra samme community
  group_by(group_a, group_b) %>% 
  summarise(n_edges = n()) %>%  
  rename(name = group_a) %>%  # Vi kan nøjes med at beholde navnet på den ene (da de jo er ens!)
  select(-group_b)

# Til de interne ties kan vi også lige tilføje antallet af noder, så vi kan beregne densiteten:
edge_data_int$size <- net.pe24 %>% as_tibble("nodes") %>% count(groups) %>% pull(n)
edge_data_int      <- edge_data_int %>% mutate(density = n_edges / (size*(size-1)) / 2)

# Ties imellem
edge_data_bet <- e_data %>% filter(group_a != group_b)


# edge_data_bet indeholder nu faktisk et netværk mellem klynger: 
# Det laver vi til et vægtet netværksobjekt (edgevægten er antallet af forbindelser mellem dem)
reduced_net <- edge_data_bet %>% 
  mutate(weight = 1) %>%  # Vi laver en vægt på 1 til alle edges som vi kan summe til sidst
  select(group_a, group_b, weight) %>% 
  graph_from_data_frame(directed = T) %>% 
  simplify(edge.attr.comb = list(weight ="sum"))

bet_ties <- reduced_net %>% as_tbl_graph() %>% 
  activate(edges) %>% 
  mutate(group_a = .N()$name[from], 
         group_b = .N()$name[to]) %>%
  as_tibble("edges")


bet_ties <- bet_ties %>% group_by(group_a) %>% mutate(weight  = weight/sum(weight)) %>% distinct(group_a, group_b, weight)
net      <- bet_ties %>% graph_from_data_frame(directed = T) %>% as_tbl_graph()

ties <- data.frame(e_data %>% count(group_a))
ties$n <- ties$n + e_data %>% count(group_b) %>% pull(n)

# og tilføjer klyngernes størrelse og densitet som vertex attributes
# 
edge_data_int$tota_edges  <- ties$n
edge_data_int$sh_internal <- (edge_data_int$tota_edges - edge_data_int$n_edges) /  edge_data_int$tota_edges

net <- net %>% left_join(edge_data_int %>% select(sh_internal, name))


reduced_net <- reduced_net %>% as_tbl_graph() %>% left_join(edge_data_int %>% select(name, density, size, sh_internal))

# Nu kan vi plotte den reducerede graf:
# 
net <- net %>% activate(edges) %>% 
  mutate(Cluster = .N()$name[from])

ggraph(net, "fr", weights = 1/weight) +
  geom_edge_parallel(aes(, width = weight, color = Cluster, label = round(weight, 2)), alpha = 0.5, arrow = arrow(length = unit(4, 'mm')), end_cap = circle(12, 'mm'), sep = unit(5, "mm")) +
  geom_node_point(aes(size = sh_internal, color = name)) +
  geom_node_label(aes(label = name, color = name), repel = T) +
  scale_size_continuous(range = c(15, 25)) +
  scale_edge_width_continuous(range = c(.5, 3)) +
  labs(color = "Cluster") +
  guides(size = "none", edge_width = "none") +
  theme_graph()


# Eller hvilken person der har flest ties 'ud af sin klynge'
e_data_a <- e_data %>% filter(groups == "9999") %>%  select(node = name_a, cluster = group_a, org = org_a, other = name_b, groups) 
e_data_b <- e_data %>% filter(groups == "9999") %>%  select(node = name_b, cluster = group_b, org = org_b, other = name_a, groups) 
external_ties <- bind_rows(e_data_a, e_data_b)
external_ties <- external_ties %>% group_by(cluster, node, org) %>% summarise(external_ties = n())
external_ties <- external_ties %>% group_by(cluster) %>% mutate(linker_rank = dense_rank(desc(external_ties))) %>% filter(linker_rank < 5)
View(external_ties)













# Ekstra: Andre community detection algoritmer ----
# Der findes andre community detection algoritmer end Louvain som er baseret på samme princip - optimering af modularity. 
# De indeholder de samme elementer: membership og modularity, så de kan indsættes i koden ovenfor: 

net.pe24 <- net.pe24 %>% activate(nodes) %>% mutate(
  leiden_cl = group_leiden(weights = 1/weight),
  #edge_bet_cl = group_edge_betweenness(weights = 1/weight), LANGSOM
  fast_greedy_cl = group_fast_greedy(weights = 1/weight), 
  label_prop_cl = group_label_prop(weights = 1/weight), 
  leading_eigen_cl = group_leading_eigen(weights = 1/weight),
  walktrap_cl = group_walktrap(weights = 1/weight),
  infomap_cl = group_infomap(weights = 1/weight)
  #, spinglass = group_spinglass(weights = 1/weight) LANGSOM
  )


net.pe24 %>% as_tibble("nodes") %>% select(groups, leiden_cl) %>% table()
net.pe24 %>% as_tibble("nodes") %>% select(groups, fast_greedy_cl) %>% table()
net.pe24 %>% as_tibble("nodes") %>% select(groups, label_prop_cl) %>% table()
net.pe24 %>% as_tibble("nodes") %>% select(groups, leading_eigen_cl) %>% table()
net.pe24 %>% as_tibble("nodes") %>% select(groups, walktrap_cl) %>% table()
net.pe24 %>% as_tibble("nodes") %>% select(groups, infomap_cl) %>% table()

#  I kan læse lidt her om forholdet mellem forskellige algoritmer
# https://stackoverflow.com/questions/9471906/what-are-the-differences-between-community-detection-algorithms-in-igraph


