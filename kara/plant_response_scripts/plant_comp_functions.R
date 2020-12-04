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
