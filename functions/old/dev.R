read_orbisxlsx <- function(path = "path") {
  
  require(dplyr)
  require(readxl)
  # loading data and retrieving names
  df <- suppressWarnings(readxl::read_xlsx(path, sheet = 2, guess_max = 10000000))

  df <- df %>% mutate(across(matches("Appointment|Resignation"), .fns = ~case_when(str_detect(.x, "^[0-9]{4}$") & !is.na(.x)~ymd(paste0(.x, "-01-01")),
            str_detect(.x,  "^[0-9]{4}-[0-9]{2}-[0-9]{2}")  & !is.na(.x)~ymd(.x),
            str_detect(.x, "^[0-9]{5,}$") & !is.na(.x)~ ymd("1899-12-30")+ days(as.numeric(.x)),
            .default = NA))) %>% suppressWarnings()
  
  names <- df %>% names() 
  
  # renaming cols (here adapting for the fact that "\r" can sometimes be included)
  selected_vars <- c(
    "name" = grep("full name", names, ignore.case = T, value = T),
    "affiliation" = grep("company name", names, ignore.case = T, value = T),
    "role" = grep("job title", names, ignore.case = T, value = T),
    "role_type" = grep("DMBoard", names, ignore.case = T, value = T),
    "role_type_abbrev" = grep("DMType", names, ignore.case = T, value = T),
    "role_level" = grep("DMLevel", names, ignore.case = T, value = T),
    "appointment" = grep("appointment", names, ignore.case = T, value = T),
    "resignation" = grep("resignation", names, ignore.case = T, value = T),
    "role_status" = grep("current", names, ignore.case = T, value = T),
    "id" = grep("unique contact identifier", names, ignore.case = T, value = T),
    "sector" = grep("NACE", names, ignore.case = T, value = T),                  
    "revenue" = grep("operating revenue", names, ignore.case = T, value = T),
    "n_employees" = grep("number of employees", names, ignore.case = T, value = T),
    "csh_name" = grep("CSH - Name", names, ignore.case = T, value = T),
    "csh_country" = grep("CSH - Country ISO code", names, ignore.case = T, value = T),
    "guo_name" = grep("GUO - Name", names, ignore.case = T, value = T),
    "guo_country" = grep("GUO - Country ISO code", names, ignore.case = T, value = T),
    "duo_name" = grep("DUO - Name", names, ignore.case = T, value = T),
    "duo_country" = grep("DUO - Country ISO code", names, ignore.case = T, value = T)
  )
  
  df1 <- df %>% 
    # select and rename of what is of interest
    select(any_of(selected_vars)) %>% 
    # get rid of na's in title
    filter(!is.na(role)) %>% 
    mutate(across(matches("n_employees|revenue"), ~ na_if(., "n.a.") %>% as.numeric(.)), 
           # create gender variable 
           gender = case_when(
             grepl("^mr", name, ignore.case = TRUE) ~ "male",
             grepl("^ms", name, ignore.case = TRUE) ~ "female", 
             .default = NA), person = substr(id, 1, 1) == "P") # %>% 
  # get rid of duplicates
  # distinct(affiliation, id, .keep_all = TRUE)
  
  return(df1)
}



remotes::install_github("schochastics/roughnet")

c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3")
library(roughnet)
library(igraph)
library(graphlayouts)

g <- make_graph(c("A", "B", "A", "C"))
V(g)$shape <- c("rectangle", "circle", "circle")
V(g)$fill <- c("#E41A1C", "#377EB8",  "#377EB8")
V(g)$fillstyle <- c("hachure")
V(g)$color <- "black"
V(g)$size <- 30
V(g)$stroke <- 2
E(g)$color <- "#AEAEAE"
roughnet(g) 

