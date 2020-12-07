# TITLE:          Plant composition functions
# AUTHORS:        Kara Dobson
# COLLABORATORS:  Phoebe Zarnetske, Mark Hammond, Moriah Young
# DATA INPUT:     
# DATA OUTPUT:    Script containing functions for cleaning plant comp data
# PROJECT:        warmXtrophic
# DATE:           December, 2020

# remove certain columns
remove_col <- function(df,name){
  vec <- which(names(df) %in% name)
  df = df[,-vec]
  return(df)
}

# change data format
change_date <- function(df){
  df[["Date"]] <- as.Date(df[["Date"]],format="%m/%d/%Y")
  return(df)
}

# check that there are no species mispellings
spp_name <- function(df){
  spp <- unique(sort(df[["Species"]]))
  return(spp)
}

# change species names
change_spp <- function(df){
  df$Species[df$Species == "Rubsp"] <- "Rusp"
  df$Species[df$Species == "Sosp"] <- "Spsp"
  df$Species[df$Species == "Syal"] <- "Syla"
  df$Species[df$Species == "phpr"] <- "Phpr"
  return(df)
}

