require(igraph)
require(tidyverse)

bet_2mN <- function(x) {
  t <- data.frame(type = V(x)$type, bet =betweenness(x, normalized = F, directed = F))
  n <- bind_rows(t %>% count(type) %>% pivot_wider(names_from = type, values_from = n) %>%
                   mutate(type = FALSE) %>% rename(own = `FALSE`, other = `TRUE`),
                 t %>% count(type) %>% pivot_wider(names_from = type, values_from = n) %>%
                   mutate(type = TRUE) %>% rename(own = `TRUE`, other = `FALSE`))
  t <- left_join(t, n)
  t %>% mutate(bet_2mN = case_when(own>other~bet / (2*(own-1)*(other-1)),
                                   own <= other~bet / ((0.5*other*(other-1)) +
                                                         (0.5*(own-1)*(own-2)) +
                                                         ((own-1)*(other-2))))) %>% pull(bet_2mN)
}
clo_2mN <- function(x) {
  t <- data.frame(type = V(x)$type, clo = 1/closeness(x, normalized = FALSE))
  n <- bind_rows(t %>% count(type) %>% pivot_wider(names_from = type, values_from = n) %>%
                   mutate(type = FALSE) %>% rename(own = `FALSE`, other = `TRUE`),
                 t %>% count(type) %>% pivot_wider(names_from = type, values_from = n) %>%
                   mutate(type = TRUE) %>% rename(own = `TRUE`, other = `FALSE`))
  t <- left_join(t, n)
  t %>% mutate(clo_2mN = (other+(2*own)-2) / clo) %>% pull(clo_2mN)
}
deg_2mN <- function(x) {
  t <- data.frame(type = V(x)$type, deg = degree(x))
  n <- bind_rows(t %>% count(type) %>% pivot_wider(names_from = type, values_from = n) %>%
                   mutate(type = FALSE) %>% rename(own = `FALSE`, other = `TRUE`),
                 t %>% count(type) %>% pivot_wider(names_from = type, values_from = n) %>%
                   mutate(type = TRUE) %>% rename(own = `TRUE`, other = `FALSE`))
  t <- left_join(t, n)
  t %>% mutate(deg_2mN = deg / other ) %>% pull(deg_2mN)
}
