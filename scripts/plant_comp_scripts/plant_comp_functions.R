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

# change date format
change_date <- function(df){
  df[["Date"]] <- as.Date(df[["Date"]],format="%m/%d/%Y")
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

# change species names
change_spp <- function(df){
  df$Species[df$Species == "Rubsp"] <- "Rusp"
  df$Species[df$Species == "Sosp"] <- "Spsp"
  df$Species[df$Species == "Syal"] <- "Syla"
  df$Species[df$Species == "phpr"] <- "Phpr"
  df$Species[df$Species == "Elrre"] <- "Elre"
  df$Species[df$Species == "Bown"] <- "Brown"
  df$Species[df$Species == "Brown "] <- "Brown"
  df$Species[df$Species == "popr"] <- "Popr"
  df$Species[df$Species == "Lepidium campestre"] <- "Leca"
  df$Species[df$Species == "Ramu"] <- "Romu"
  df$Species[df$Species == "Anspp"] <- "Ansp"
  df$Species[df$Species == "Smooth_oat"] <- "Arel"
  df$Species[df$Species == "Bare"] <- "Bare_Ground"
  df$Species[df$Species == "Bare Groud"] <- "Bare_Ground"
  df$Species[df$Species == "Bare Ground "] <- "Bare_Ground"
  df$Species[df$Species == "Bare Ground"] <- "Bare_Ground"
  df$Species[df$Species == "Bare Groud "] <- "Bare_Ground"
  return(df)
}

# change site names
change_site <- function(df){
  df$Site[df$Site == "KBS"] <- "kbs"
  df$Site[df$Site == "UMBS"] <- "umbs"
  df$Site[df$Site == "umbs "] <- "umbs"
  df$Site[df$Site == "Uu"] <- "umbs"
  return(df)
}

