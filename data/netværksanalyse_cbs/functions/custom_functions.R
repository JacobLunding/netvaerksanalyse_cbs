
###############################
#                              
#  custom functions for virksomhedsstrategi kurs            
#                              
###############################

# Author: Alexander Gamerdinger
# Version: 2023-02-20

# Libraries 

#===========================================================================

##############  #afsnit #################
# tag funktioner fra Anton Graus "eliter" pakke  
# alle variabelnavne sat til lowercase,
# ellers intet ændret
# github: https://github.com/antongrau/eliter/blob/master/R/tags.R
#########################################

has.tags       <- function(den, tags, result = c("affil", "name", "den") , silent = FALSE, mode = c("and", "or")){
  
  find.in.tag.list       <- function(tag.list, tag) unlist(lapply(tag.list, function(x, tag) any(x == tag), tag = tag))
  
  result.args            <- c("affil", "name", "den")
  affil.name             <- as.character(den$affiliation)
  tag.list               <- strsplit(as.character(den$tags),  ", ")
  
  # Remove white space noise
  tags                   <- trimws(tags)
  # tag.list               <- lapply(tag.list, trimws)  This was extremely slow
  
  # Find tags
  sector.match           <- do.call("cbind", lapply(tags, find.in.tag.list, tag.list = tag.list))
  colnames(sector.match) <- tags
  
  if(mode == "or"){
  tag.affils             <- unique(affil.name[which(rowSums(sector.match) >= 1)])
  }else{
  tag.affils             <- unique(affil.name[which(rowSums(sector.match) == length(tags))])  
  }
  
  # Return amount of matches per tag
  if (identical(silent, FALSE)) {
    how.many.per.tag     <- cbind("Matched positions" = colSums(sector.match, na.rm = T))
    print(how.many.per.tag)
    cat("\n", "\n")
  }
  
  # Results
  
  if (match.arg(result, result.args) == "affil") {
    return(as.character(tag.affils))
  }
  
  if (match.arg(result, result.args) == "name") {
    return(as.character(unique(den$name[den$affiliation %in% tag.affils])))
  }
  
  if (match.arg(result, result.args) == "den") {
    droplevels(den[den$affiliation %in% tag.affils,])
  }
}

#' show.all.tags
#' 
#' Displays a matrix with all the tags in den.
#' 
#' @param den a affiliation edgelist
#' @return a matrix with all tags and their frequencies
#' @export
#' @examples
#' data(den)
#' show.all.tags(den)

show.all.tags   <- function(den){
  tags                <- as.character(den$tags)
  tags.split          <- unlist(strsplit(tags, ", "))
  tags.positions      <- table(tags.split)
  tags.affiliations   <- tags[duplicated(den$affiliation) == FALSE]
  tags.affiliations   <- table(trimws(unlist(strsplit(tags.affiliations, ","))))
  cbind(Positions = tags.positions, affiliations = tags.affiliations)
}


#' Convert list of tags to sectors
#'
#' @param den a den class object
#' @param sector.tags a list of tags
#' @param other The other category, if FALSE, it is omitted
#' @param silent if FALSE the number of matched positions and affiliations is printed
#' @param mutually.exclusive if TRUE the produced sectors are mutually exclusive 
#' @param sector.membership if TRUE a data.frame with a mutually exclusive sector memberships vector is returned
#'
#' @return a list of den objects
#' @export
#'
#' @examples
#' data(den)
#' sectors        <- standard.sectors("Danish")
#' tags.to.sectors(den, sectors)
#' tags.to.sectors(den, sectors, mutually.exclusive = TRUE, sector.membership = TRUE)

tags.to.sectors <- function(den, sector.tags, other = "Other", silent = FALSE, mutually.exclusive = FALSE, sector.membership = FALSE){
  
  # Match tags
  list.dens            <- lapply(sector.tags, has.tags, den = den, result = "den", silent = silent)
  
  # Other category
  if (other != FALSE) {
    affil.names        <- lapply(list.dens, getElement, "affiliation")
    affil.names        <- unique(unlist(affil.names, use.names = F))
    other.affils       <- setdiff(unique(den$affiliation), affil.names)
    den.other          <- droplevels(den[den$affiliation %in% other.affils,])
    l                  <- length(list.dens)
    list.dens[[l + 1]] <- den.other
    names(list.dens)[l + 1] <- other
  }
  
  # Mutually exclusive
  if (identical(mutually.exclusive, TRUE)) {
    affil.names                 <- lapply(list.dens, getElement, "affiliation")
    affil.names                 <- sort(as.character(unique(unlist(affil.names, use.names = F))))
    
    sector.edge                 <- list()
    for (i in 1:length(list.dens)) sector.edge[[i]]  <- data.frame("affiliation" = unique(as.character(list.dens[[i]]$affiliation)), "sector" = names(list.dens[i]))
    sector.edge                 <- do.call("rbind", sector.edge)
    incidence.sector            <- xtabs(~ affiliation + sector, sector.edge, sparse = T)
    
    membership.sector           <- vector(mode = "logical", length = nrow(incidence.sector))
    for (i in 1:ncol(incidence.sector)) membership.sector[incidence.sector[, i] == 1] <- colnames(incidence.sector)[i]
    
    exclusive.names             <- split(affil.names, f = membership.sector)
    for (i in 1:length(list.dens)) list.dens[[i]] <- list.dens[[i]][list.dens[[i]]$affiliation %in% exclusive.names[[i]] ,  , drop = TRUE]
  }
  
  # Out  
  if (sector.membership == TRUE & mutually.exclusive == TRUE) {
    mem.out     <- data.frame("affiliation" = rownames(incidence.sector), "sector" = factor(membership.sector, levels = names(list.dens), ordered = TRUE)) 
    mem.out     <- mem.out[order(mem.out$affiliation), ]
    return(mem.out)
    
  }else{
    return(list.dens)
  }
}

#' Standard sector tags
#'
#' Collections of tags that roughly correspond to sectors.
#'
#' @param sets a character vector with the sets of sector tags
#'
#' @return a list of tags
#' @export
#'
#' @examples
#' data(den)
#' sectors      <- standard.sectors()
#' sectors$Danish
#' 
#' standard.sectors("English")

standard.sectors <- function(sets = c("Danish", "English", "4 sectors")){
  
  set.list                           <- list()
  
  # Danish and detailed set
  list.tags                           <- list()
  list.tags$Erhvervsliv               <- c("Corporation")
  list.tags$Arbejdsgivere             <- c("Business association", "Employers association")
  list.tags$"Fagforeninger"           <- c("Unions", "Standsforening", "A-kasse", "Union controlled")
  list.tags$"Kultur og medier"        <- c("Culture", "Design", "Media", "Journalists", "MEFO", "Libraries", "Language")
  list.tags$"Videnskab og uddannelse" <- c("Science", "Education", "Universities") 
  list.tags$Stat                      <- c("State administration", "Ministry", "State corporation", "Military", "Public leaders", "Commission", "Politics", "Parliament", "State business") # FEJL - State business
  list.tags$Politik                   <- c("Politics", "Parliament", "Political party", "City council")
  list.tags$Sundhed                   <- c("Health", "Patients", "MEDI", "Medicine", "Doctors")
  list.tags$Fritid                    <- c("Recreation and sports", "Sports", "Recreation")
  list.tags$Fonde                     <- c("Foundation", "Charity")
  list.tags$"Fødevarer"               <- c("Farming", "Food", "Fishing", "LEVN", "Forestry")
  
  set.list$"Danish"                   <- list.tags
  
  
  # English and detailed set
  list.tags                           <- list()
  list.tags$Corporations              <- c("Corporation")
  list.tags$"Employers"               <- c("Business association", "Employers association")
  list.tags$"Unions"                  <- c("Unions", "Standsforening", "A-kasse", "Union controlled")
  list.tags$"Culture and media"       <- c("Culture", "Design", "Media", "Journalists", "MEFO", "Libraries", "Language")
  list.tags$"Science and education"   <- c("Science", "Education", "Universities", "Museums")
  list.tags$State                     <- c("State administration", "Ministry", "State corporation", "Military", "Public leaders", "Commission", "Politics", "Parliament", "State business") # FEJL - State business
  list.tags$Politics                  <- c("Politics", "Parliament","Political party", "City council")
  list.tags$Health                    <- c("Health", "Patients", "MEDI", "Medicine", "Doctors")
  list.tags$Recreation                <- c("Recreation and sports", "Sports", "Recreation")
  list.tags$Foundation                <- c("Foundation", "Charity")
  list.tags$"Foods"                   <- c("Farming", "Food", "Fishing", "LEVN", "Forestry")
  
  set.list$"English"                  <- list.tags
  
  # 4 sectors
  
  list.tags                           <- list()
  list.tags$"State and politics"      <- c("State administration", "Ministry", "State corporation", "Military", "Public leaders", "Commission", "Politics", "Parliament", "State business", "Politics", "Parliament","Political party", "City council") # FEJL - State business
  list.tags$"Business"                <- c("Corporation", "Business association", "Employers association")
  list.tags$"Unions"                  <- c("Unions", "Standsforening", "A-kasse", "Union controlled")
  list.tags$"Science and culture"     <- c("Culture", "Design", "Media", "Journalists", "MEFO", "Libraries", "Language", "Science", "Education", "Universities", "Museums")
  
  set.list$"4 sectors"                <- list.tags
  
  set.list                              <- set.list[sets]
  if (length(set.list) == 1) set.list   <- set.list[[1]]
  set.list
}



#' Roles and position on the basis of sectors
#'
#' sectors.to.role constructs a role or position variable on the basis of sectors.
#' If the sectors are mutually exclusive it constructs a single named vector.
#' If a person has roles in more than one sector it is the last sector in the list of sectors that counts.
#'
#' @param den a den class object, see \link{as.den} with a ROLE variable
#' @param list.dens a list of den objects that corresponds to the sectors
#' @param role the set of roles
#' @param other either FALSE or the label for the "Other" category.
#' @param mutually.exclusive if TRUE sectors are mututally exclusive
#'
#' @return either a matrix or a named vector
#' @export
#'
#' @examples
#' data(den)
#' den                 <- as.den(den)
#' list.dens           <- tags.to.sectors(den, standard.sectors("Danish"))
#' sectors.to.role(den, list.dens, mutually.exclusive = TRUE)
sectors.to.role        <- function(den, list.dens, role = c("Chief executive", "Executive"), other = "Other", mutually.exclusive = FALSE){
  
  den.role             <- den[den$ROLE %in% role,]
  list.dens            <- lapply(list.dens, function(x) droplevels(x[x$ROLE %in% role,]))
  
  # Other category
  if (other != FALSE) {
    affil.names             <- lapply(list.dens, getElement, "affiliation")
    affil.names             <- unique(unlist(affil.names, use.names = F))
    other.affils            <- setdiff(unique(den.role$affiliation), affil.names)
    den.other               <- droplevels(den.role[den.role$affiliation %in% other.affils,])
    l                       <- length(list.dens)
    list.dens[[l + 1]]      <- den.other
    names(list.dens)[l + 1] <- other
  }
  
  # Check for empty sectors
  list.dens            <- list.dens[!sapply(list.dens, nrow) == 0]
  
  # Mutually exclusive
  ind.names                 <- lapply(list.dens, getElement, "name")
  ind.names                 <- sort(as.character(unique(unlist(ind.names, use.names = FALSE))))
  
  sector.edge                 <- list()
  for (i in 1:length(list.dens)) sector.edge[[i]]  <- data.frame("name" = unique(as.character(list.dens[[i]]$name)), "sector" = names(list.dens[i]))
  sector.edge                 <- do.call("rbind", sector.edge)
  incidence.sector            <- xtabs(~ name + sector, sector.edge, sparse = TRUE)
  incidence.sector            <- incidence.sector[order(rownames(incidence.sector)),]
  
  membership.sector           <- vector(mode = "logical", length = nrow(incidence.sector))
  for (i in 1:ncol(incidence.sector)) membership.sector[incidence.sector[, i] == 1] <- colnames(incidence.sector)[i]
  
  
  if (identical(mutually.exclusive, FALSE)) { 
    return(incidence.sector)  }else{
      mem.out                    <- factor(membership.sector, levels = names(list.dens), ordered = TRUE)
      names(mem.out)             <- rownames(incidence.sector)
      
      return(mem.out)
    }
}









