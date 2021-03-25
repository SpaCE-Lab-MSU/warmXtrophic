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

# check that there are no weird dates
date_check <- function(df){
        date <- unique(sort(df[["Date"]]))
        return(date)
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
        df$Species[df$Species == "Hipr"] <- "Hisp"
        df$Species[df$Species == "Dach"] <- "Daca"
        df$Species[df$Species == "Drin"] <- "Brin"
        df$Species[df$Species == "Dafl"] <- "Dagl"
        df$Species[df$Species == "Agre"] <- "Elre"
        df$Species[df$Species == "unknown"] <- "Unknown"
        return(df)
}

# remove species
remove_spp <- function(df){
        df <- df[!grepl("Alpe",df$Species),]
        df <- df[!grepl("Apca",df$Species),]
        df <- df[!grepl("Brin",df$Species),]
        df <- df[!grepl("Desp",df$Species),]
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
        #df <- df[!grepl("Prpe",df$Species),]
        df <- df[!grepl("Veth",df$Species),]
        df <- df[!grepl("Anma",df$Species),]
        df <- df[!grepl("Vaan",df$Species),]
        df <- df[!grepl("Gnul",df$Species),]
        df <- df[!grepl("Trdu",df$Species),]
        df <- df[!grepl("Unknown",df$Species),]
        #df <- df[!grepl("Alpe",df$Species),]
        #df <- df[!grepl("Sypi",df$Species),]
        #df <- df[!grepl("Hipi",df$Species),]
        #df <- df[!grepl("Popsp",df$Species),]
        return(df)
}

# change species names
change_spp_2 <- function(df){
        df$Species[df$Species == "Rufl"] <- "Rusp"
        df$Species[df$Species == "Popsp"] <- "Posp"
        df$Species[df$Species == "Trre"] <- "Trsp"
        df$Species[df$Species == "Trpr"] <- "Trsp"
        df$Species[df$Species == "Poco"] <- "Posp"
        df$Species[df$Species == "Popr"] <- "Posp"
        df$Species[df$Species == "Hipi"] <- "Hisp"
        df$Species[df$Species == "Hica"] <- "Hisp"
        df$Species[df$Species == "Hiau"] <- "Hisp"
        df$Species[df$Species == "Sogi"] <- "Sosp"
        df$Species[df$Species == "Sone"] <- "Sosp"
        df$Species[df$Species == "Sohi"] <- "Sosp"
        df$Species[df$Species == "Assy"] <- "Assp"
        df$Species[df$Species == "Rual"] <- "Rusp"
        return(df)
}

# change site names
change_site <- function(df){
        df$Site[df$Site == "KBS"] <- "kbs"
        df$Site[df$Site == "umbs "] <- "umbs"
        df$Site[df$Site == "kbs "] <- "kbs"
        df$Site[df$Site == "UMBS"] <- "umbs"
        return(df)
}

# change dates
change_date <- function(df){
        df$Date[df$Date == "8/9/0207"] <- "8/9/2017"
        df$Date[df$Date == "6/1/1017"] <- "6/1/2017"
        df$Date[df$Date == "7/11/1017"] <- "7/11/2017"
        df$Date[df$Date == "7/14/2011"] <- "7/14/2017"
        df$Date[df$Date == "7/14/20117"] <- "7/14/2017"
        df$Date[df$Date == "6/23/3017"] <- "6/23/2017"
        df$Date[df$Date == "6/30/3017"] <- "6/30/2017"
        df$Date[df$Date == "7/23/3017"] <- "7/23/2017"
        return(df)
}
#change_date <- function(df){
#        df$Date[df$Date == "0207-08-09"] <- "2017-08-09"
#        df$Date[df$Date == "1017-06-01"] <- "2017-06-01"
#        df$Date[df$Date == "1017-07-11"] <- "2017-07-11"
#        df$Date[df$Date == "2011-07-14"] <- "2017-07-14"
#        df$Date[df$Date == "3017-06-23"] <- "2017-06-23"
#        df$Date[df$Date == "3017-06-30"] <- "2017-06-30"
#        df$Date[df$Date == "3017-07-23"] <- "2017-07-23"
#        return(df)
#}
