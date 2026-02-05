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


# 2. Indlæs data ----
  # brug igen pharma datasættet, som ligger i input


# 3. subset datasættet ----
 # så vi kun kigger på virksomheder med mere end 1 i boardet og hvor data om omsætningen (affil_rev) ikke er na, og kun personer, der har poster i mere end en koncern:
    # til det skal vi først:
        # lave en ny variabel n_ind som tæller [n_distinct] unikke navne for hver affiliation
        # lave en ny variabel n_guo som tæler [n_distinct] unikke koncern navne [guo] for hver person
        # 

# 4. Lav et individ x individ netværk ----


# 5. Se på grafens komponentstruktur og lav et nyt grafobjekt med den størte komponent ----



# 6. Udregn forskellige centralitets mål ----
  # og gem dem i et data objekt, en 'tibble()'


# 7. Tilføj en rankvariable for hver af centralitetsmålene og lav en samlet centralitets-rank


# 8. Åben jeres tibble med centralitetsmål i Vieweren ----
  # ... og kig lidt på hvilke virksomheder der ligger i toppen på de forskellige centralitetsmål
  




# 9. Tilføj centralitetsmålene til netværksobjektet for den størte komponent og lav forskellige visualiseringer. ---- 
  # sæt color og size på noderne efter målene..


# 10. EKSTRAOPGAVE: Prøv at lave en simpel regression mellem betweenness og omsætning:

