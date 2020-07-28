#Change column names for pendant cleanup
change_pend_names <- function(df){
  colnames(df) <- sub("^Date.Time..GMT.0\\d.00", "Date_Time", colnames(df))
  colnames(df) <- sub("^Temp.*warmed_air_1m.", "Temp_F_XP_air_1m", colnames(df))
  colnames(df) <- sub("^Intensity.*warmed_light_1..", "Intensity_lum_ft_XP_light_1m", colnames(df))
  df = subset(df, select = -c(X.))
  return(df)
  }

#Change column names for paired sensor cleanup
change_pair_names <- function(df){
  colnames(df) <- sub("^X\\dU_warmed_soil_5cm", "XU_warmed_soil_temp_5cm", colnames(df))
  colnames(df) <- sub("^X\\dU_ambient_soil_5cm", "XU_ambient_soil_temp_5cm", colnames(df))
  colnames(df) <- sub("^X\\dH_warmed_soil_moisture_5cm", "XH_warmed_soil_moisture_5cm", colnames(df))
  colnames(df) <- sub("^X\\dH_ambient_soil_moisture_5cm", "XH_ambient_soil_moisture_5cm", colnames(df))
  colnames(df) <- sub("^X\\dH_warmed_air_1m", "XH_warmed_air_1m", colnames(df))
  colnames(df) <- sub("^X\\dH_warmed_RH_1m", "XH_warmed_RH_1m", colnames(df))
  colnames(df) <- sub("^X\\dH_ambient_air_1m", "XH_ambient_air_1m", colnames(df))
  colnames(df) <- sub("^X\\dH_ambient_RH_1m", "XH_ambient_RH_1m", colnames(df))
  colnames(df) <- sub("^X\\dU_warmed_air_10cm", "XU_warmed_air_10cm", colnames(df))
  colnames(df) <- sub("^X\\dU_ambient_air_10cm", "XU_ambient_air_10cm", colnames(df))
  colnames(df) <- sub("^Date.Time..GMT.0\\d.00", "Date_Time", colnames(df))
  colnames(df) <- sub("^Water.Content.*warmed_soil_moisture_5cm.", "XH_warmed_soil_moisture_5cm", colnames(df))
  colnames(df) <- sub("^Water.Content.*ambient_soil_moisture_5cm.", "XH_ambient_soil_moisture_5cm", colnames(df))
  colnames(df) <- sub("^Temp.*warmed_air_1m.", "XH_warmed_air_1m", colnames(df))
  colnames(df) <- sub("^Temp.*ambient_air_1m.", "XH_ambient_air_1m", colnames(df))
  colnames(df) <- sub("^RH.*warmed_RH_1m.", "XH_warmed_RH_1m", colnames(df))
  colnames(df) <- sub("^RH.*ambient_RH_1m.", "XH_ambient_RH_1m", colnames(df))
  colnames(df) <- sub("^Temp.*warmed_air_10cm.", "XU_warmed_air_10cm", colnames(df))
  colnames(df) <- sub("^Temp.*warmed_soil_5cm.", "XU_warmed_soil_temp_5cm", colnames(df))
  colnames(df) <- sub("^Temp.*ambient_soil_5cm.", "XU_ambient_soil_temp_5cm", colnames(df))
  colnames(df) <- sub("^Temp.*ambient_air_10cm.", "XU_ambient_air_10cm", colnames(df))
  return(df)
}

#Change 2017, 2018, and 2019 dataframes to celsius for paired sensors
f_to_c <- function(df){
  df[["XH_warmed_air_1m"]] <- fahrenheit.to.celsius(df[["XH_warmed_air_1m"]])
  df[["XH_warmed_RH_1m"]] <- fahrenheit.to.celsius(df[["XH_warmed_RH_1m"]])
  df[["XH_ambient_air_1m"]] <- fahrenheit.to.celsius(df[["XH_ambient_air_1m"]])
  df[["XH_ambient_RH_1m"]] <- fahrenheit.to.celsius(df[["XH_ambient_RH_1m"]])
  df[["XU_warmed_air_10cm"]] <- fahrenheit.to.celsius(df[["XU_warmed_air_10cm"]])
  df[["XU_warmed_soil_temp_5cm"]] <- fahrenheit.to.celsius(df[["XU_warmed_soil_temp_5cm"]])
  df[["XU_ambient_air_10cm"]] <- fahrenheit.to.celsius(df[["XU_ambient_air_10cm"]])
  df[["XU_ambient_soil_temp_5cm"]] <- fahrenheit.to.celsius(df[["XU_ambient_soil_temp_5cm"]])
  return(df)
}

#Change 2020 dataframe to celsius for pendant sensors
f_to_c2 <- function(df){
  df[["Temp_F_XP_air_1m"]] <- fahrenheit.to.celsius(df[["Temp_F_XP_air_1m"]])
  return(df)
}

#Change date format to POSIX format
change_POSIX <- function(df){
  df[["Date_Time"]] <- as.POSIXct(df[["Date_Time"]],tryFormats = c("%m/%d/%y %I:%M:%S %p", "%m/%d/%Y %H:%M"), tz="UTC")
  return(df)
  }

#Adding in pendant_ID column
add_name_cols <- function(l){
  for(i in seq_along(l)){
    l[[i]]$Pendant_ID <- gsub("^pend(.{2,3})_.*$", "\\1", names(l)[i])
  }
  return(l)
}

#Remove columns from paired sensors
remove_col <- function(df,name){
  vec <- which(names(df) %in% name)
  df = df[,-vec]
  return(df)
}
