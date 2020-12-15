# change column names
column_names <- function(df){
  colnames(df) <- sub("^Overstory.PAR", "Overstory", colnames(df))
  colnames(df) <- sub("^Understory.*R$", "Understory_1", colnames(df))
  colnames(df) <- sub("^Understory.PAR.\\d", "Understory_2", colnames(df))
  colnames(df) <- sub("^PAR", "Understory_2", colnames(df))
  colnames(df) <- sub("^Above_Biomass", "Overstory", colnames(df))
  colnames(df) <- sub("^Ground_1", "Understory_1", colnames(df))
  colnames(df) <- sub("^Gound_2", "Understory_2", colnames(df))
  colnames(df) <- sub("^Date", "Date_Time", colnames(df))
  return(df)
}

# remove columns
remove_column <- function(df,name){
  vec <- which(names(df) %in% name)
  df = df[,-vec]
  return(df)
}

# change date format to POSIX
change_POSIX <- function(df){
  df[["Date_Time"]] <- as.POSIXct(df[["Date_Time"]],tryFormats = c("%m/%d/%Y"), tz="UTC")
  return(df)
}
