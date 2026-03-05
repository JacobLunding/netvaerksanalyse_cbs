library(tidyverse)
library(readxl)
library(writexl)
library(igraph)
library(tidygraph)
library(ggraph)
library(patchwork)
library(ggpubr)
library(Matrix)

source("functions/read_orbis.R")
source("functions/ego_neighborhoods.R")
source("functions/ego_net_plot.R")

###################################################################################################/
# 1. Indlæs Danish Elitenetworks 2024
# Filtre:
# 1. Branche:			                            branchekode = "Landbrug, jagt, skovbrug og fiskeri"
#                                             tags indeholder "landbrug"
# 2. Personer:                                Personer der er ledere (direktør/formand) i min. én "landbrugs-ting"
# 3. Poster:                                  Folk med mere end én position
# 4. Medlemmer:                               Bestyrelser med mindre end 30 medlemmer
# 
###################################################################################################/
# 
# den <- read_csv("data/danish_elitenetworks2024.csv")
# 
# # Først laver vi en 'er_landbrug' variabel ud fra branchekode og tags
# den <- den %>% 
#   mutate(landbrug = 
#            case_when(
#              affiliation_branche_niveau1 == "Landbrug, jagt, skovbrug og fiskeri" | grepl("landbrug", affiliation_tags, ignore.case = T)~TRUE, 
#              .default = FALSE))
# 
# # Dernæst laver vi en 'er_landmand' variabel, hvor vi for hver person *group_by()* 'spørger' om vedkommende har nogen (any) positioner i min én *any()* landbrugsting, hvor de også er leder.
# den <- den %>% group_by(person_name) %>% mutate(er_landmand = any(landbrug & position_leader)) 
# 
# # Det filter anvender vi og reducerer data til kun at indeholde 'landbrugsledere'
# den_land <- den %>% filter(er_landmand) 
# 
# # Dernæst laver vi to variable; en der tæller hver persons antal medlemskaber og en der tæller hver bestyrelses antal medlemmer
# den_land <- den_land %>% group_by(person_name) %>% mutate(memberships = n_distinct(affiliation_orig_name))
# den_land <- den_land %>% group_by(affiliation_orig_name) %>% mutate(members = n_distinct(person_name))
# 
# # De to variable bruger vi som det næste filter, så vi kun har bestyrelser med mindre end 30 medlemmer og personer med mere end 1 post
# den_land <- den_land %>% filter(members < 30 & memberships > 1 & !grepl("Events eller begivenheder", affiliation_tags)) 






###################################################################################################/
# 1. Indlæs data m.m. ----
###################################################################################################/



pharma_nordic <- read_orbisxlsx("data/nordic_pharma2025.xlsx")
colnames(pharma_nordic)
head(pharma_nordic)
###################################################################################################/
# 2. Omkod data m.m. ----
###################################################################################################/

# Vi laver en ny variabel, executive, som er TRUE hvise role_type indeholder **enten** BoD *eller* ExeB *eller* SenMan *eller* ExeC ELLER role er forskellige ting der indikerer executive level:
pharma_nordic <- pharma_nordic %>% mutate(executive = 
                                            grepl("BoD|ExeB|SenMan|ExeC", role_type) | 
                                            grepl("Director|directeur|Head of|general|Chief|executive|president|Chairman", role, ignore.case =T))

# Vi har også to forskellige variable for personers geografiske oprindelse
pharma_nordic %>% count(person_country, sort = TRUE)
pharma_nordic %>% filter(is.na(person_country)) %>% count(person_countries, sort = TRUE)

pharma_nordic <- pharma_nordic %>% 
  mutate(person_geo = case_when(is.na(person_country)~
                                  str_extract(person_countries, 
                                              "(^.*?(?=;))|(^.*?$)"),
                                .default = person_country))

pharma_nordic %>% count(person_geo, sort = TRUE)

# Omkod også US
pharma_nordic <- pharma_nordic %>% mutate(person_geo = case_when(person_geo == "United States"~"United States of America", .default = person_geo))

pharma_nordic %>% count(person_geo, sort = TRUE)

# Endelig kan vi filtrere på de variable vi ønsker: HER FILTRERER VI IKKE PÅ CURRENT!! 
# dvs at bestyrelser der har udveklet medlemmer historisk også tæller som en forbindelse!

pharma_nordic <- pharma_nordic %>% filter(person == TRUE)
# pharma_nordic <- pharma_nordic %>% filter(role_status == "Current")
pharma_nordic <- pharma_nordic %>% filter(executive == TRUE)
pharma_nordic <- pharma_nordic %>% distinct(name, affiliation, .keep_all = T)

###################################################################################################/
# 2. Definerer et netværksobjekt for virksomheder ----
###################################################################################################/

bi_adj    <- xtabs(pharma_nordic, formula = ~name + affiliation, sparse = T)
adj_affil <- t(bi_adj) %*% bi_adj
net       <- adj_affil %>% graph_from_adjacency_matrix(mode = "undirected") %>% simplify()
net       <- net %>% as_tbl_graph()


###################################################################################################/
# 3. Tilføjer udvalgte virksomhedsvariable til netværksobjektet ----
###################################################################################################/

pharma_nordic <- pharma_nordic %>% group_by(affiliation) %>% mutate(women_share = sum(person_gender == "F", na.rm = T) / sum(person_gender %in% c("M", "F"), na.rm = T))

# først laver vi et datasæt, der kun har én række per virksomhed (distinct(affiliation, .keep_all = TRUE))
variables_to_add <- pharma_nordic %>% 
  arrange(revenue) %>% 
  distinct(affiliation, .keep_all = TRUE) %>% 
  select(affiliation, affiliation_country, guo_name, guo_country, n_employees, revenue, total_assets, women_share)  

# og left_joiner det på netværksobjektet (vi renamer affiliation til name, så det matcher med name i netværksobjektet)
net       <- net %>% left_join(variables_to_add %>% rename(name = affiliation))

###################################################################################################/
# 4. Hurtig visualisering ----
###################################################################################################/


net %>% ggraph() +
  geom_edge_link0(width = 0.3, alpha = 0.2) +
  geom_node_point(aes(color = affiliation_country)) +
  theme_graph()

###################################################################################################/
# 5. Komponenter ----
###################################################################################################/
net       <- net %>% mutate(comp = group_components())

net_c1    <- net %>% filter(comp == 1)

###################################################################################################/
# 6. Netværks mål ----
###################################################################################################/

dens     <- edge_density(net_c1)
trans    <- transitivity(net_c1)
radius   <- radius(net_c1)
diameter <- diameter(net_c1)

net_description <- c("nb. of nodes" = vcount(net), # vcount tæller noder/vertices
                     "nb. of edges" = ecount(net), # ecount tæller edges
                     "nb. of components" = count_components(net), 
                     "largest component: nb. of nodes" = vcount(net_c1), 
                     "largest component: share of nodes" = vcount(net_c1) / vcount(net), 
                     "largest component: nb. of edges" = ecount(net_c1), 
                     "largest component: share of edges" = ecount(net_c1) / ecount(net),  
                     "largest component: diameter" = diameter,  
                     "largest component: radius" = radius,
                     "largest component: density" = dens, 
                     "largest component: transitivity" = trans) %>% enframe(name = "Measures", value = "value")

write_xlsx(net_description, "output/pharma_nordic_example_net_description.xlsx")

############################################################/
# 7. Ego-netværk og lokal transitivitet ----
# åbne vs. lukkede trekanter på ego niveau
############################################################/

# Hvad er et ego netværk
ego_net <- make_ego_graph(net_c1, order = 1, nodes = "LEO PHARMA A/S")[[1]]

ggraph(ego_net) +
  geom_edge_link0() +
  geom_node_point(size = 2) +
  geom_node_label(aes(label = name)) +
  theme_graph()



# Lad os tage nove nordisk som et andet eksempel og så hvordan vi zoomer ind på ego-netværket 'inden i' det store netværk.
ego <- "NOVO NORDISK A/S"
# Ego neighborhoods er en funktion jeg har skrevet som plotter egonetværk i flere nabolag
pl <- ego_neighborhoods(net_c1, 4, ego, labels = FALSE)

ggpubr::ggarrange(plotlist = rev(pl), labels = rev(c("1st neighbourhood", "2nd neighbourhood", "3rd neighbourhood", "4th neighbourhood"))) + labs(caption = ego)


############################################################/
# 8. NYT MÅL: Burt's constraint ----
# Strukturelle huller, brokerage, mål på egonetværks niveau
############################################################/

############################################/
# Netværkslukning, lokal transitivitet
###########################################/

random_nodes <- transitivity(net_c1, type = "local")

random_nodes <- random_nodes[!is.nan(random_nodes) & random_nodes < 1 & random_nodes > 0] 
random_nodes <- sort(random_nodes)
random_nodes <- names(random_nodes)[sort(sample(1:length(random_nodes), 12))]

pl <- ego_net_plot(graph = net_c1, nodes = random_nodes, mode = "transitivity")
p <- ggpubr::ggarrange(plotlist = pl)
p
ggsave(plot = p, filename = "output/network closure.pdf", height = 6, width = 10)

## Constraint:
# Måler i hvilket omfang en nodes forbindelser er redundante
# Måler 'brokerage', Burt's Constraint måler i hvor høj grad en nodes 'venner' (direkte forbindelser) også er forbundet til hinanden - danner en lukket gruppe. I en lukket gruppe er man begrænset (constrained) ift adgangen til 'ny viden' (tænk ekkokammer) fordi alle ens kontakter også er forbundne. En node der har venner 'uden for' sin klike har derfor en lavere constraint og vil oftere være den, der bringer nye ideer/tanker/viden ind i sin klike. Med andre ord, jo mindre "constraint", desto bedre kan en node fungere som en broker over strukturelle huller i netværket.
# En kombination af tre ting påvirker en nodes (ego) constraint:
# 1) c-size: antallet af egos direkte forbindelser (degree)... 
      # lav degree --> højere constraint
# 2) c-density: antallet af forbindelser mellem egos direkte forbindelser... 
      # mange 'alter'forbindelser --> høj constraint
# 3) c-hierarchy: antallet af andre forbindelser som egos forbindelser har... 
      # færre 'ego-eksterne' forbindelser --> høj constraint


# constraint funktioen i Igraph beregner Burt's constraint, som er højere jo mere 'constrained' en node er, dvs. lav constraint = høj brokerage. 

#####################################/
# Burts metode !!
######################################/
ego_constr <- make_ego_graph(net_c1)
names(ego_constr) <- net_c1 %>% as_tibble() %>% pull(name)
constr_burt <- imap(ego_constr, .f = ~constraint(.x, .y))
constr_burt <- constr_burt %>% unlist()


#Igraph/tidygraph måden
constr    <- constraint(net_c1)
# det vil vi ofte gerne 'vende om' så det bliver et mål for brokerage evne.
brokerage <- 1 / constr


###################################################################################################/
# 9 Centralitetsmål mv. ----
###################################################################################################/

# Lad os samle de nodespecifikke centralitetsmål for den største komponent
net_c1 <- net_c1 %>% mutate(
                                  # Dem vi kender fra tidligere....:
                            degree      = centrality_degree(),
                            betweenness = centrality_betweenness(normalized = TRUE),
                            closeness   = centrality_closeness(normalized = TRUE),
                            eigencentr  = centrality_eigen(),
                            coreness    = node_coreness(),
                            local_trans = local_transitivity(),
                                  # Nogle nye
                                  # local betweenness beregner betweenness i et lokalt område omkring ego, 
                                  # cutoff 2 betyder at vi kun kigger på 2nd neighbourhood..
                            local_betweenness  = centrality_betweenness(normalized = TRUE, cutoff = 2), 
                            constraint_igraph  = node_constraint(),
                            brokerage_igraph   = 1/constraint_igraph,
                            constraint_burt    = constr_burt,
                            brokerage_burt     = 1/constraint_burt)
                            
  
net_c1 <- net_c1 %>% mutate(degree_rank  = dense_rank(desc(degree)),
                            betw_rank    = dense_rank(desc(betweenness)),
                            local_betw_rank  = dense_rank(desc(local_betweenness)),
                            closeness_rank   = dense_rank(desc(closeness)),
                            brokerage_igraph_rank   = dense_rank(desc(brokerage_igraph)),
                            brokerage_burt_rank   = dense_rank(desc(brokerage_burt)),
                            eigen_rank       = dense_rank(desc(eigencentr)),
                            coreness_rank    = dense_rank(desc(coreness)),
                            local_trans_rank = dense_rank(local_trans))

centr_metrics <- net_c1 %>% as_tibble() 

centr_metrics %>% group_by(affiliation_country) %>% 
  summarise(n_comp =n(),
            mean_degree = mean(degree),
            mean_betw   = mean(betweenness),
            mean_betw_local = mean(local_betweenness),
            mean_close   = mean(closeness),
            mean_brokerage = mean(brokerage_igraph))

write_xlsx(centr_metrics, "output/pharma_nordic_example_centralitymetrics.xlsx")

###################################################################################################/
# 10 Example: Constraint vs. closure/transitivity vs. local betweenness----
###################################################################################################/

### lad os udvælge nogle forskellige virksomheder fra brokerage fordelingen: nr 1, 5, 10, 15, 20, 30 fx
### 
### 

brokers <- centr_metrics %>% 
  arrange(brokerage_igraph_rank) %>% 
  filter(brokerage_igraph_rank %in% c(1,20, 40, 60)) %>% 
  distinct(brokerage_igraph_rank, .keep_all = T) %>% 
  pull(name)
brokers

pl <- ego_net_plot(graph = net_c1, nodes = brokers, mode = "constraint")

p <- ggpubr::ggarrange(plotlist = pl, common.legend = T, legend = "none")
p
ggsave(plot = p, filename = "output/constraint_brokerage.pdf", height = 6, width = 8)


###################################################################################################/
# 11. Visualiseringer af netværk ----
###################################################################################################/

# Tidygraph!!! tidygraph pakken laver igraph objektet om, så man kan arbejde med netværket ligesom vi gør med andre data objekter (mutate, arrange, filter osv.).. Her vil jeg fx gerne sortere (arrange) netværket, så den plotter noderne i en bestemt rækkefølge..


# Degree
p0 <- net_c1 %>% as_tbl_graph() %>% arrange(degree) %>% ggraph("stress") +
  geom_edge_link0(width =.3, alpha = 0.3) +
  geom_node_point(aes(color = degree, size = degree)) + 
  scale_size_continuous(range = c(2,5)) + 
  geom_node_label(aes(filter=degree_rank<=5, label = paste0(degree_rank, ": ", name)), size = 3, repel = T, force = 25) +
  guides(label = "none", size = "none") +
  theme_graph(base_family = "serif")

# Betweenness
p1 <- net_c1 %>% as_tbl_graph() %>% arrange(betweenness) %>% ggraph("stress") +
  geom_edge_link0(width =.3, alpha = 0.3) +
  geom_node_point(aes(color = betweenness, size = betweenness)) + 
  scale_size_continuous(range = c(2,5)) + 
  geom_node_label(aes(filter=betw_rank<=5, label = paste0(betw_rank, ": ", name)), size = 3, repel = T, force = 25) +
  guides(label = "none", size = "none") +
  theme_graph(base_family = "serif")

# local betweenness
p1.1 <- net_c1 %>% as_tbl_graph() %>% arrange(local_betweenness) %>% ggraph("stress") +
  geom_edge_link0(width =.3, alpha = 0.3) +
  geom_node_point(aes(color = local_betweenness, size = local_betweenness)) + 
  scale_size_continuous(range = c(2,5)) + 
  geom_node_label(aes(filter=local_betw_rank<=5, label = paste0(local_betw_rank, ": ", name)), size = 3, repel = T, force = 25) +
  guides(label = "none", size = "none") +
  theme_graph(base_family = "serif")


# closeness
p2<- net_c1 %>% as_tbl_graph() %>% arrange(closeness) %>% ggraph("stress") +
  geom_edge_link0(width =.3, alpha = 0.3) +
  geom_node_point(aes(color = closeness, size = closeness)) + 
  scale_size_continuous(range = c(2,5)) + 
  geom_node_label(aes(filter=closeness_rank<=5, label = paste0(closeness_rank, ": ", name)), size = 3, repel = T, force = 25) +
   guides(label = "none", size = "none") +
  theme_graph(base_family = "serif")

# brokerage
p3<- net_c1 %>% as_tbl_graph() %>% arrange(brokerage_burt) %>% ggraph("stress") +
  geom_edge_link0(width =.3, alpha = 0.3) +
  geom_node_point(aes(color = brokerage_burt, size = degree)) + 
  scale_size_continuous(range = c(2,5)) + 
  geom_node_label(aes(filter=brokerage_burt_rank<=5, label = paste0(brokerage_burt_rank, ": ", name)), size = 3, repel = T, force = 25) +
  guides(label = "none", size = "none") +
  theme_graph(base_family = "serif")


p <- ggarrange(plotlist = list(p0, p1, p2, p3)) %>%  annotate_figure(., top = paste0("Største komponent (n=", vcount(net_c1), ") top-5:"))

ggsave("output/pharma_nordic_example_net_plots.pdf", plot = p, width = 15, height = 10)


###################################################################################################/
# 12. Assortativity (assortative mixing) ----
# homophili i netværk: 
###################################################################################################/

# Det kan være interessant at vide om noder der er ens på forskellige egenskaber er forbundet med hinanden. For at svare på det spørgsmål kan man udregne det der kaldes et netværks Assortativity koefficent, der som en korrelations koefficient kan antage værdier mellem -1 og 1. En høj assortativity koefficient betyder at ensartede noder (på den udvalgte egenskab) i højere grad er forbundne, mens en negativ koefficient (gående mod -1) betyder at der omvendt er tale at forskelligartede noder tendere mod at have forbindelser. En assortatity koefficient på 0 beskriver et netværk, hvor der ingen korrelation (hverken positiv eller negativ) er mellem en bestemt egenskab og tendensen til at danne forbindelser.

# Assortativity for kontinuerte variable----

# Funktion assortativity() bruges til at beregne assortativity for kontinuerte variable
# Funktionen assortativity_nominal() bruges til at beregne netværksassortativity for kategorielle variable

# Vi kan beregne assortativity for degree. Altså, tenderer velforbundne noder mod at være forbundne. "Populær tiltrækker populær".

# Assortativity degree udregnet vha igraph
assortativity(net_c1, values = degree(net_c1))


# I mange tilfælde vil det være interessant at beregne assortativity på netværks-eksterne egenskaber. Vi kunne fx. tage antal ansatte, omsætning og assets: er store virksomheder tilbøjelige til at skabe forbindelser til andre store virksomheder:

    # først sætter vi lige missing (NA) til 0 på antal ansatte, omsætning og assets
    # Ændrer til mean i stedet for o
# En kategoriel variabel vi kunne kigge på er country. 
# En anden kategoriel variable kunne være Global Ultimate Owner (guo):
# Selskaber der ikke er en del af en koncern har NA på guo, der sætter vi navnet ind i stedet


net_c1 <- net_c1 %>% mutate(
  n_employees = replace_na(n_employees, 0),
  revenue = replace_na(revenue, 0),
  revenue = case_when(revenue<0~0, .default = revenue),
  total_assets = replace_na(total_assets, 0),
  guo_name = case_when(is.na(guo_name)~name, .default = guo_name))


# Assortativity score udregnet i tidygraph!!
assort <- net_c1 %>% with_graph(tibble(employees  = graph_assortativity(attr = n_employees),             # Kontinuert
                                           revenue    = graph_assortativity(attr = revenue),             # Kontinuert
                                           assets     = graph_assortativity(attr = total_assets),        # Kontinuert 
                                           gender     = graph_assortativity(attr = women_share),         # Kontinuert 
                                           country    = graph_assortativity(attr = affiliation_country), # Nominiel
                                           guo        = graph_assortativity(attr = guo_name)))           # Nominiel

assort

