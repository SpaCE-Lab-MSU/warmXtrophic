# TITLE:          Herbivory functions
# AUTHORS:        Kara Dobson
# COLLABORATORS:  Phoebe Zarnetske, Mark Hammond, Moriah Young
# DATA INPUT:     
# DATA OUTPUT:    Script containing functions for cleaning herbivory data
# PROJECT:        warmXtrophic
# DATE:           December, 2020

# remove certain columns
remove_col <- function(df,name){
  vec <- which(names(df) %in% name)
  df = df[,-vec]
  return(df)
}

# check that there are no species misspellings
spp_name <- function(df){
  spp <- unique(sort(df[["Species"]]))
  return(spp)
}

# check that there are no site name misspellings
site_name <- function(df){
  spp <- unique(sort(df[["Site"]]))
  return(spp)
}

# change site names
change_site <- function(df){
  df$Site[df$Site == "KBS"] <- "kbs"
  df$Site[df$Site == "UMBS"] <- "umbs"
  df$Site[df$Site == "umbs "] <- "umbs"
  df$Site[df$Site == "Uu"] <- "umbs"
  return(df)
}

# change date format
change_date <- function(df){
  df[["Date"]] <- as.Date(df[["Date"]],format="%m/%d/%Y")
  return(df)
}

