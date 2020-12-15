# TITLE:          Phenology functions
# AUTHORS:        Kara Dobson and Moriah Young 
# COLLABORATORS:  Phoebe Zarnetske, Mark Hammond
# DATA INPUT:     
# DATA OUTPUT:    Script containing functions for cleaning phenology data
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

#function to change column names
change_colnames <- function(df){
        names(df)[names(df) == "site"] <- "Site"
        names(df)[names(df) == "date"] <- "Date"
        names(df)[names(df) == "plot"] <- "Plot"
        names(df)[names(df) == "species"] <- "Species"
        names(df)[names(df) == "action"] <- "Action"
        return(df)
}

# change species names
change_spp <- function(df){
        df$Species[df$Species == "Aspi "] <- "Aspi"
        df$Species[df$Species == "Daca "] <- "Daca"
        df$Species[df$Species == "Aca"] <- "Daca"
        df$Species[df$Species == "Hype "] <- "Hype"
        df$Species[df$Species == "Soca "] <- "Soca"
        df$Species[df$Species == "Acmi "] <- "Acmi"
        df$Species[df$Species == "ruac"] <- "Ruac"
        df$Species[df$Species == "Ruace"] <- "Ruac"
        df$Species[df$Species == "Cape "] <- "Cape"
        df$Species[df$Species == "cape"] <- "Cape"
        df$Species[df$Species == "Vaan "] <- "Vaan"
        df$Species[df$Species == "Popr "] <- "Popr"
        df$Species[df$Species == "Popre"] <- "Popr"
        df$Species[df$Species == "Porp"] <- "Popr"
        df$Species[df$Species == "PHpr"] <- "Phpr"
        df$Species[df$Species == "Piau"] <- "Hiau"
        df$Species[df$Species == "Des"] <- "Desp"
        df$Species[df$Species == "rual"] <- "Rual"
        df$Species[df$Species == "HIsp"] <- "Hisp"
        df$Species[df$Species == "Hipr"] <- "Hica"
        df$Species[df$Species == "Dach"] <- "Daca"
        df$Species[df$Species == "Drin"] <- "Brin"
        df$Species[df$Species == "Dafl"] <- "Dagl"
        df$Species[df$Species == "Agre"] <- "Elre"
        df$Species[df$Species == "unknown"] <- "Unknown"
        return(df)
}

change_site <- function(df){
        df$Site[df$Site == "KBS"] <- "kbs"
        df$Site[df$Site == "umbs "] <- "umbs"
        df$Site[df$Site == "kbs "] <- "kbs"
        return(df)
}

