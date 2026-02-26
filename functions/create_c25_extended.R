c25 <- read_xlsx("data/RP-CVR.xlsx")
den2 <- read_xlsx("data/c25_data_final.xlsx", sheet = 2)
den <- left_join(den2, c25 %>% mutate(c25 = "yes", CVR = as.character(CVR)) %>% select(cvr = CVR, c25))
den <- den %>% filter(person_virksomhed == "person" & !Rolle %in% c("Legale ejere", "Likvidator", "(Suppleant)", "Stiftere", "Tilsynsråd"))
den <- den %>% filter(!(Rolle == "Reelle ejere" & Andet != "Er reel ejer som bestyrelsesmedlem"))
den %>% count(Rolle, Andet) %>% View()

write_csv(den, "data/C25_extended.csv")
