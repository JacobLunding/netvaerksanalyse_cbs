show_tags <- function(den, level = 1) {

all_tags <- den %>% distinct(affiliation, affiliation_tags) %>% pull(affiliation_tags) %>% str_split("; ") %>% unlist() %>% tibble() 
colnames(all_tags) <- "name"
all_tags <- all_tags %>% separate_wider_delim(name, delim = "_", names_sep = "_", too_few = "align_start")
  
if(level == 1){  
return(all_tags %>% rename(level1 = name_1) %>% count(level1))
}
if(level == 2){
return(all_tags %>% mutate(level2 = paste0(name_1, "_", name_2)) %>% filter(!grepl("_NA_|_NA$", level2)) %>% count(level2))
}
if(level == 3){
return(all_tags %>% mutate(level3 = paste0(name_1, "_", name_2, "_", name_3)) %>% filter(!grepl("_NA_|_NA$", level3)) %>% count(level3))
}
if(level == 4){
return(all_tags %>% mutate(level4 = paste0(name_1, "_", name_2, "_", name_3, "_", name_4)) %>% filter(!grepl("_NA_|_NA$", level4)) %>% count(level4))
}
}