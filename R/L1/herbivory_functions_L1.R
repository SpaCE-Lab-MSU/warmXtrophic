# TITLE:          Herbivory functions
# AUTHORS:        Kara Dobson
# COLLABORATORS:  Phoebe Zarnetske, Mark Hammond, Moriah Young
# DATA INPUT:     
# DATA OUTPUT:    Script containing functions for cleaning herbivory data
# PROJECT:        warmXtrophic
# DATE:           December, 2020


# change columns to lowercase so dataframes can be merged
lowercase <- function(df){
  names(df) <- tolower(names(df))
  return(df)
}

# change column names + remove needed columns
change_col_names <- function(df){
  colnames(df) <- sub("eaten", "p_eaten", colnames(df))
  colnames(df) <- sub("damage", "p_damage", colnames(df))
  colnames(df) <- sub("id", "date", colnames(df))
  return(df)
}

# change date format
change_date <- function(df){
  df[["date"]] <- as.Date(df[["date"]],format="%m/%d/%Y")
  return(df)
}

# remove certain columns
remove_col <- function(df,name){
  vec <- which(names(df) %in% name)
  df = df[,-vec]
  return(df)
}

# check that there are no species misspellings
spp_name <- function(df){
  spp <- unique(sort(df[["species"]]))
  return(spp)
}

# check that there are no site name misspellings
site_name <- function(df){
  spp <- unique(sort(df[["site"]]))
  return(spp)
}

# removing species
remove_spp <- function(df){
        df <- df[!grepl("Assp",df$species),]
        df <- df[!grepl("Romu",df$species),]
        df <- df[!grepl("Daca",df$species),]
        df <- df[!grepl("Assy",df$species),]
        df <- df[!grepl("Besp",df$species),]
        df <- df[!grepl("Frve",df$species),]
        df <- df[!grepl("Acru",df$species),]
        df <- df[!grepl("Amla",df$species),]
        df <- df[!grepl("Hipi",df$species),]
        df <- df[!grepl("Syla",df$species),]
        df <- df[!grepl("Vaan",df$species),]
        return(df)
}

# change site names
change_spp <- function(df){
  df$species[df$species == "Sora"] <- "Soca"
  return(df)
}

# change site names
change_site <- function(df){
  df$site[df$site == "UMBS"] <- "umbs"
  df$site[df$site == "KBS"] <- "kbs"
  return(df)
}

