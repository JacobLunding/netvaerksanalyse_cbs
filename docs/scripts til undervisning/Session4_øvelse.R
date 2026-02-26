##############  #Lektion03 ################
# 
# Virksomhedsstrategi i et netværksperspektiv 
# Centralitetsmål - øvelse
#
###########################################

# 1. Indlæs (library) nødvendige pakker: ----
# vi skal bruge tidyverse, som altid
# igraph til grafobjekter og centralitetsmål
# ggraph og ggplot2 og ggpubr til grafplots
# og "functions/networkfunctions.R" som skal sources
library(tidyverse)
library(tidygraph)
library(igraph)
library(ggraph)
library(ggplot2)
library(ggpubr)
library(Matrix)
library(readxl)
source("functions/read_orbis.R")

##################################/
# Vælg et af følgende datasæt 
# ELLER
# indlæs noget andet
##################################/

#Nordisk pharma
download.file("https://jacoblunding.github.io/netvaerksanalyse_cbs/data/nordic_pharma2025.xlsx", "data/nordic_pharma2025.xlsx")
dt <- read_orbisxlsx("data/nordic_pharma2025.xlsx")
#Europæiske biler
download.file("https://jacoblunding.github.io/netvaerksanalyse_cbs/data/Cardata.xlsx", "data/Cardata.xlsx")
dt <- read_orbisxlsx("data/Cardata.xlsx")
#Nordisk Elektricitet
download.file("https://jacoblunding.github.io/netvaerksanalyse_cbs/data/nordic_electricity.xlsx", "data/nordic_electricity.xlsx")
dt <- read_orbisxlsx("data/nordic_electricity.xlsx")
#Shipping world-wide
download.file("https://jacoblunding.github.io/netvaerksanalyse_cbs/data/shipping.xlsx", "data/shipping.xlsx")
dt <- read_xlsx("data/shipping.xlsx")

# Antal poster per individ
dt <- dt %>% 
  group_by(person_id) %>% 
  mutate(n_memberships = n_distinct(affiliation)) %>% 
  ungroup()
# Antal individer per bestyrelse
dt <- dt %>% 
  group_by(affiliation) %>%
  mutate(n_members = n_distinct(person_id)) %>% 
  ungroup()

## Byg filter overvej hvilke kriterier I vil bruge
dt <- dt %>% 
  filter(role_status == "Current") %>% 
  filter(person)


##################################/
# NETVÆRK
##################################/

bi_adj <- xtabs(data = dt, formula = ~name + affiliation)

# Hvilket netværk vil I lave, individer eller virksomheder?
# 
# 


##################################/
# ANALYSE
# beregn forskellige centralitetsmål
##################################/

