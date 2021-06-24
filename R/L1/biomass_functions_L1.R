# TITLE:          Biomass - ANPP functions
# AUTHORS:        Moriah Young
# COLLABORATORS:  Phoebe Zarnetske, Mark Hammond, Kara Dobson
# DATA INPUT:     
# DATA OUTPUT:    Script containing functions for cleaning biomass - ANPP data
# PROJECT:        warmXtrophic
# DATE:           June, 2020


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

# check that there are no double plot IDs
plot_check <- function(df){
        ID <- unique(sort(df[["Plot"]]))
        return(ID)
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
        df$Species[df$Species == "Ramu"] <- "Romu"
        df$Species[df$Species == "Anspp"] <- "Ansp"
        df$Species[df$Species == "Smooth_oat"] <- "Arel"
        df$Species[df$Species == "Bown"] <- "Brown"
        df$Species[df$Species == "Brown "] <- "Brown"
        df$Species[df$Species == "Bare"] <- "Bare_Ground"
        df$Species[df$Species == "Bare Groud"] <- "Bare_Ground"
        df$Species[df$Species == "Bare Ground "] <- "Bare_Ground"
        df$Species[df$Species == "Bare Ground"] <- "Bare_Ground"
        df$Species[df$Species == "Bare Groud "] <- "Bare_Ground"
        return(df)
}