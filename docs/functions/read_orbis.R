read_orbisxlsx <- function(path = "path", resultsheet = 2) {
  
  require(dplyr)
  require(readxl)
  # loading data and retrieving names
  df <- suppressWarnings(readxl::read_xlsx(path, guess_max = 10000000, sheet = resultsheet))
  df <- df %>% select(-any_of("...1"))
  df <- df %>% mutate(across(matches("Appointment|Resignation"), .fns = ~case_when(str_detect(.x, "^[0-9]{4}$") & !is.na(.x)~ymd(paste0(.x, "-01-01")),
                                                                                   str_detect(.x,  "^[0-9]{4}-[0-9]{2}-[0-9]{2}")  & !is.na(.x)~ymd(.x),
                                                                                   str_detect(.x, "^[0-9]{5,}$") & !is.na(.x)~ ymd("1899-12-30")+ days(as.numeric(.x)),
                                                                                   .default = NA))) %>% suppressWarnings()
  
  names <- df %>% names() 
  
  # renaming cols (here adapting for the fact that "\r" can sometimes be included)
  selected_vars <- c(
    "name" = grep("full name", names, ignore.case = T, value = T),
    "person_id" = grep("unique contact identifier", names, ignore.case = T, value = T),
    "person_gender" = grep("DMGender", names, ignore.case = T, value = T),
    "person_country" = grep("DMCountry$", names, ignore.case = T, value = T),
    "person_countries" = grep("DMCountry/.*? nationality", names, ignore.case = T, value = T),
    "affiliation" = grep("company name", names, ignore.case = T, value = T),
    "affiliation_id" = grep("^BvD ID number$", names, ignore.case = T, value = T),
    "affiliation_country" = grep("^Country ISO code", names, ignore.case = T, value = T),
    "role" = grep("job title.*? eng", names, ignore.case = T, value = T),
    "board_type" = grep("DMBoard", names, ignore.case = T, value = T),
    "role_type" = grep("DMType", names, ignore.case = T, value = T),
    "role_level" = grep("DMLevel", names, ignore.case = T, value = T),
    "appointment" = grep("appointment", names, ignore.case = T, value = T),
    "resignation" = grep("resignation", names, ignore.case = T, value = T),
    "role_status" = grep("current", names, ignore.case = T, value = T),
    "sector" = grep("NACE Rev\\. 2", names, ignore.case = T, value = T),                  
    "revenue" = grep("operating revenue", names, ignore.case = T, value = T),
    "total_assets" = grep("total assets", names, ignore.case = T, value = T),
    "n_employees" = grep("number of employees", names, ignore.case = T, value = T),
    "csh_id" = grep("CSH - BvD ID number", names, ignore.case = T, value = T),
    "csh_orbis_id" = grep("CSH - Orbis ID number", names, ignore.case = T, value = T),
    "csh_name" = grep("CSH - Name", names, ignore.case = T, value = T),
    "csh_sector" = grep("CSH - NACE", names, ignore.case = T, value = T),
    "csh_country" = grep("CSH - Country ISO code", names, ignore.case = T, value = T),
    "subsid_id" = grep("SUB - BvD ID number", names, ignore.case = T, value = T),
    "subsid_orbis_id" = grep("SUB - Orbis ID number", names, ignore.case = T, value = T),
    "subsid_name" = grep("SUB - Name", names, ignore.case = T, value = T),
    "subsid_sector" = grep("SUB - NACE", names, ignore.case = T, value = T),
    "subsid_country" = grep("SUB - Country ISO code", names, ignore.case = T, value = T),
    "guo_id" = grep("GUO - BvD ID number", names, ignore.case = T, value = T),
    "guo_orbis_id" = grep("GUO - Orbis ID number", names, ignore.case = T, value = T),
    "guo_ucid" = grep("GUO - UCI", names, ignore.case = T, value = T),
    "guo_name" = grep("GUO - Name", names, ignore.case = T, value = T),
    "guo_sector" = grep("GUO - NACE", names, ignore.case = T, value = T),
    "guo_country" = grep("GUO - Country ISO code", names, ignore.case = T, value = T),
    "duo_id" = grep("DUO - BvD ID number", names, ignore.case = T, value = T),
    "duo_orbis_id" = grep("DUO - Orbis ID number", names, ignore.case = T, value = T),
    "duo_name" = grep("DUO - Name", names, ignore.case = T, value = T),
    "duo_sector" = grep("DUO - NACE", names, ignore.case = T, value = T),
    "duo_country" = grep("DUO - Country ISO code", names, ignore.case = T, value = T)
  )
  
  df1 <- df %>% 
    # select and rename of what is of interest
    select(any_of(c(selected_vars, names[!names %in% selected_vars])))
  
  df1 <- df1  %>% 
    # get rid of na's in title
    filter(!is.na(role)) %>% 
    mutate(person = substr(person_id, 1, 1) == "P") # %>% 
  # get rid of duplicates
  # distinct(affiliation, id, .keep_all = TRUE)
  
  if(is.character(df1$n_employees)) {
    df1 <- df1 %>% mutate(across(matches("n_employees"), ~ na_if(., "n.a.") %>% as.numeric(.)))
  }
  if(is.character(df1$revenue)) {
    df1 <- df1 %>% mutate(across(matches("revenue"), ~ na_if(., "n.a.") %>% as.numeric(.)))
  }
  if(is.character(df1$assets)) {
    df1 <- df1 %>% mutate(across(matches("assets"), ~ na_if(., "n.a.") %>% as.numeric(.)))
  }
  
  df1 <- df1 %>% group_by(affiliation) %>% 
    mutate(across(any_of(matches("csh_|duo_|guo_|subsid_")), .fns = ~first(.x))) 
  
  
  
  changes <- data.frame(orbis_var = as.vector(selected_vars[selected_vars %in% names]), new_var = names(selected_vars[selected_vars %in% names]))
  
  nch <- nchar(changes$orbis_var) 
  tab <- nchar(changes$orbis_var) %>% max()
  message(c("Orbis variable names updated: \n\n", paste0(gsub("\n", " ", changes$orbis_var), strrep(" ", tab-nch),"=> ", changes$new_var, "\n"), "\n\nNew variables added: \n", "person {TRUE/FALSE}" ))
  return(df1 %>% ungroup())
}
