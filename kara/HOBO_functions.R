change_names <- function(df){
  colnames(df) <- sub("^Date.Time..GMT.0\\d.00", "Date_Time", colnames(df))
  colnames(df) <- sub("^Temp.*warmed_air_1m.", "Temp_F_XP_air_1m", colnames(df))
  colnames(df) <- sub("^Intensity.*warmed_light_1..", "Intensity_lum_ft_XP_light_1m", colnames(df))
  df = subset(df, select = -c(X.))
  return(df)
}

change_POSIX <- function(df){
  df[["Date_Time"]] <- as.POSIXct(df[["Date_Time"]],format="%m/%d/%y %I:%M:%S %p", tz="UTC")
  return(df)
  }

