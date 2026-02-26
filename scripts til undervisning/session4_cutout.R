##################################################/
# 5. Kerne/periferi  struktur ----
##################################################/

##################################################/
# Excentricitetscentralitet:
##################################################/ 

# Ide: Et netværk har yderpunkter, dvs. noder der er længst fra hinanden. De siges at ligge i periferien og deres afstand er derfor netværkets Diameter. De noder med den korteste afstand til fjerneste node, kalder vi centrum i netværket, og afstanden til periferien Radius. 
# I praksis: for hver node udregnes dens længste korteste sti til en anden node. 
# Den inverse eccentricitet -> 1/eccentricitet, er således et centralitetsmål, som går fra lav til høj.

gr_virk <- gr_virk %>% 
  activate(nodes) %>% 
  mutate(eccentricity_rev = 1/node_eccentricity())

ecc <- gr_virk %>% as_tibble(active = "nodes") %>% select(c25_virk, eccentricity_rev) %>% group_by(c25_virk) %>% summarise(mean(eccentricity_rev))

hist_ecc <- ecc %>% tibble() %>% 
  ggplot() +
  geom_histogram(aes(x=.), fill = "grey20") + 
  scale_y_continuous(name = "Antal") + scale_x_continuous(breaks = seq(0,max(ecc), .01), name ="1/Eccentricity") + theme_minimal(base_family = "serif")

p_ecc <- gr_virk %>% 
  ggraph(layout='stress') + 
  geom_edge_link0(color='grey', width=0.6, alpha=0.45) + 
  geom_node_point(aes(color=eccentricity_rev), alpha=0.8)  + 
  labs(title = paste0("Den største komponent (n=", vcount(gr_virk),")"), subtitle = "1/Eccentricity", color = "") +
  theme_graph() + theme(plot.title = element_text(family = "serif", size = 12), plot.subtitle = element_text(family = "serif", size = 12), legend.position = "bottom")

ggarrange(plotlist = list(hist_ecc, p_ecc), widths = c(1.4,2))