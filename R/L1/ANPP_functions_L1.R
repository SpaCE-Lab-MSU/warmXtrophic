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
        spp <- unique(sort(df[["species"]]))
        return(spp)
}

# check that there are no site name misspellings
site_name <- function(df){
        spp <- unique(sort(df[["site"]]))
        return(spp)
}

# check that there are no double plot IDs
plot_check <- function(df){
        ID <- unique(sort(df[["plot"]]))
        return(ID)
}

# change species names
change_spp <- function(df){
        df$species[df$species == "Rubsp"] <- "Rusp"
        df$species[df$species == "Bubsp"] <- "Rusp"
        df$species[df$species == "Hipr"] <- "Hisp"
        df$species[df$species == "Assy"] <- "Assp"
        df$species[df$species == "Trpr"] <- "Trsp"
        df$species[df$species == "Toxrad"] <- "Tora"
        df$species[df$species == "Syal"] <- "Syla"
        df$species[df$species == "Lowsp"] <- "Losp"
        df$species[df$species == "Elrre"] <- "Elre"
        df$species[df$species == "Ramu"] <- "Romu"
        df$species[df$species == "Soil"] <- "Bare_Ground"
        df$species[df$species == "Bareground"] <- "Bare_Ground"
        df$species[df$species == "Evening_Primrose"] <- "Oebi"
        df$species[df$species == "Lichen"] <- "Ulsp"
        df$species[df$species == "Hieracium"] <- "Hisp"
        df$species[df$species == "Solidago"] <- "Sosp"
        df$species[df$species == "Capedi"] <- "Cape"
        df$species[df$species == "Moss"] <- "Umsp"
        df$species[df$species == "Red Maple"] <- "Acru"
        df$species[df$species == "Bareground_sand"] <- "Bare_Ground"
        df$species[df$species == "Groundhog_hole_sand"] <- "Groundhog"
        df$species[df$species == "Surface_Litter"] <- "Litter"
        return(df)
}

# change site names
change_site <- function(df){
        df$site[df$site == "kba"] <- "kbs"
        df$site[df$site == "UMBS"] <- "umbs"
        return(df)
}
