# TITLE:          Plant composition functions
# AUTHORS:        Kara Dobson, Pat Bills, Phoebe Zarnetske
# COLLABORATORS:  Mark Hammond, Moriah Young
# DATA INPUT:     a L0 dataframe with warmXtrophic observational data
# DATA OUTPUT:    Script containing functions for cleaning plant comp data
# PROJECT:        warmXtrophic
# DATE:           December, 2020; updated April 2022

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

# OPTIONAL: remove species that are very rare in the data
remove_spp <- function(df){
        df <- df[!grepl("Alpe",df$Species),]
        df <- df[!grepl("Apca",df$Species),]
        df <- df[!grepl("Brin",df$Species),]
        df <- df[!grepl("Juni",df$Species),]
        df <- df[!grepl("Plla",df$Species),]
        df <- df[!grepl("Romu",df$Species),]
        df <- df[!grepl("Stme",df$Species),]
        df <- df[!grepl("Umsp",df$Species),]
        df <- df[!grepl("Ulsp",df$Species),]
        df <- df[!grepl("Spsp",df$Species),]
        df <- df[!grepl("Acru",df$Species),]
        df <- df[!grepl("Oebi",df$Species),]
        df <- df[!grepl("Besp",df$Species),]
        df <- df[!grepl("Prse",df$Species),]
        df <- df[!grepl("Quru",df$Species),]
        df <- df[!grepl("Prpe",df$Species),]
        df <- df[!grepl("Veth",df$Species),]
        df <- df[!grepl("Anma",df$Species),]
        df <- df[!grepl("Vaan",df$Species),]
        df <- df[!grepl("Gnul",df$Species),]
        df <- df[!grepl("Trdu",df$Species),]
        df <- df[!grepl("Alpe",df$Species),]
        df <- df[!grepl("Sypi",df$Species),]
        df <- df[!grepl("Hipi",df$Species),]
        df <- df[!grepl("Popsp",df$Species),]
        df <- df[!grepl("Acsy",df$Species),]
        df <- df[!grepl("Lampu",df$Species),]
        df <- df[!grepl("Lapu",df$Species),]
        df <- df[!grepl("Sysa",df$Species),]
        df <- df[!grepl("WILD_RASP?",df$Species),]
        return(df)
}

# change species names
change_spp <- function(df){
  df$Species[df$Species == "Rubsp"] <- "Rusp"
  df$Species[df$Species == "Rufl"] <- "Rusp"
  df$Species[df$Species == "Rual"] <- "Rusp"
  df$Species[df$Species == "Hica"] <- "Hisp"
  df$Species[df$Species == "Assy"] <- "Assp"
  df$Species[df$Species == "Poco"] <- "Posp"
  df$Species[df$Species == "popr"] <- "Popr"
  df$Species[df$Species == "Popr"] <- "Posp"
  df$Species[df$Species == "Trre"] <- "Trsp"
  df$Species[df$Species == "Trpr"] <- "Trsp"
  df$Species[df$Species == "Sogi"] <- "Sosp"
  df$Species[df$Species == "Sone"] <- "Sosp"
  df$Species[df$Species == "Sohi"] <- "Sosp"
  df$Species[df$Species == "Syal"] <- "Syla"
  df$Species[df$Species == "phpr"] <- "Phpr"
  df$Species[df$Species == "Elrre"] <- "Elre"
  df$Species[df$Species == "Lepidium campestre"] <- "Leca"
  df$Species[df$Species == "Evening Primrose"] <- "Oebi"
  df$Species[df$Species == "Milkweed"] <- "Assp"
  df$Species[df$Species == "Red Maple"] <- "Acru"
  df$Species[df$Species == "unknown dicot"] <- "Unknown_Dicot"
  df$Species[df$Species == "Unknown 5"] <- "Unknown"
  df$Species[df$Species == "UNKNOWN 5"] <- "Unknown"
  df$Species[df$Species == "UNKNOWN_7"] <- "Unknown"
  df$Species[df$Species == "UKNOWN_Grass"] <- "Unknown_Grass"
  df$Species[df$Species == "unknown_grass"] <- "Unknown_Grass"
  df$Species[df$Species == "Unknown_grass"] <- "Unknown_Grass"
  df$Species[df$Species == "UNKNOWN_GRASS"] <- "Unknown_Grass"
  df$Species[df$Species == "UNKNOWN_GRASS2"] <- "Unknown_Grass"
  df$Species[df$Species == "UNKNOWN_GRASS3"] <- "Unknown_Grass"
  df$Species[df$Species == "UNK_Forb"] <- "Unknown_Forb"
  df$Species[df$Species == "UNK_Grass"] <- "Unknown_Grass"
  df$Species[df$Species == "UNK_grass"] <- "Unknown_Grass"
  df$Species[df$Species == "Unk Grass"] <- "Unknown_Grass"
  df$Species[df$Species == "uUk Grass"] <- "Unknown_Grass"
  df$Species[df$Species == "UNKNOWN_PLANT1"] <- "Unknown"
  df$Species[df$Species == "UNK_1"] <- "Unknown"
  df$Species[df$Species == "UNKNOWN1"] <- "Unknown"
  df$Species[df$Species == "UNK"] <- "Unknown"
  df$Species[df$Species == "Unknown_shrub"] <- "Unknown_Shrub"
  df$Species[df$Species == "UNK_Shrub2"] <- "Unknown_Shrub"
  df$Species[df$Species == "Shrub/tree"] <- "Unknown_Shrub_Tree"
  df$Species[df$Species == "UKNOWN_sp"] <- "Unknown"
  df$Species[df$Species == "Unknown."] <- "Unknown"
  df$Species[df$Species == "UNKNOWN!"] <- "Unknown"
  df$Species[df$Species == "UNK_2"] <- "Unknown"
  df$Species[df$Species == "Grass"] <- "Unknown_Grass"
  df$Species[df$Species == "Thin_Blade_Grass"] <- "Unknown_Thin_Blade_Grass"
  df$Species[df$Species == "Wide bladed grass"] <- "Unknown_Wide_Blade_Grass"
  df$Species[df$Species == "Chickweed"] <- "Stme"
  df$Species[df$Species == "Animal"] <- "Animal_Disturbance"
  df$Species[df$Species == "Groundhog hole/sand"] <- "Animal_Disturbance"
  df$Species[df$Species == "Aster sp"] <- "Asun"
  df$Species[df$Species == "Aster"] <- "Asun"
  df$Species[df$Species == "UNK_Aster"] <- "Asun"
  df$Species[df$Species == "UNK_Assy"] <- "Assp"
  df$Species[df$Species == "Dogbane"] <- "Apsp"
  df$Species[df$Species == "Ramu"] <- "Romu"
  df$Species[df$Species == "moss"] <- "Moss"
  df$Species[df$Species == "Anspp"] <- "Ansp"
  df$Species[df$Species == "Smooth_oat"] <- "Arel"
  df$Species[df$Species == "Bown"] <- "Brown"
  df$Species[df$Species == "Brown "] <- "Brown"
  df$Species[df$Species == "Bare"] <- "Bare_Ground"
  df$Species[df$Species == "Bare Groud"] <- "Bare_Ground"
  df$Species[df$Species == "Bareground"] <- "Bare_Ground"
  df$Species[df$Species == "Bare Ground "] <- "Bare_Ground"
  df$Species[df$Species == "Bare Ground"] <- "Bare_Ground"
  df$Species[df$Species == "Bare Groud "] <- "Bare_Ground"
  df$Species[df$Species == "Total Live"] <- "Live"
  df$Species[df$Species == "Total No Live"] <- "Nonlive"
  df$Species[df$Species == "LItter"] <- "Litter"
  df$Species[df$Species == "Standing Dead"] <- "Vert_Litter"
  df$Species[df$Species == "Total "] <- "Total"
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

