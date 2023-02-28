#Change column names for pendant cleanup - kbs
change_pend_names <- function(df){
  colnames(df) <- sub("^Date.Time..GMT.0\\d.00", "Date_Time", colnames(df))
  colnames(df) <- sub("^Date.Time..GMT..0\\d00", "Date_Time", colnames(df))
  colnames(df) <- sub("^X04.*\\d\\d", "Date_Time", colnames(df))
  colnames(df) <- sub("^Temp.*warmed_air_1m.", "Temp_F_XP_air_1m", colnames(df))
  colnames(df) <- sub("^Temp....C.", "Temp_F_XP_air_1m", colnames(df))
  colnames(df) <- sub("^X\\d\\d.\\d\\d", "Temp_F_XP_air_1m", colnames(df))
  colnames(df) <- sub("^Intensity.*warmed_light_1..", "Intensity_lum_ft_XP_light_1m", colnames(df))
  colnames(df) <- sub("^Intensity....lux.", "Intensity_lum_ft_XP_light_1m", colnames(df))
  colnames(df) <- sub("^X\\d\\d\\d\\d\\d.\\d\\d", "Intensity_lum_ft_XP_light_1m", colnames(df))
  return(df)
}

#Change column names for pendant cleanup - umbs
change_pend_names_umbs <- function(df){
        colnames(df) <- sub("^Date.Time..GMT.0\\d.00", "Date_Time", colnames(df))
        colnames(df) <- sub("^Date.Time..GMT..0\\d00", "Date_Time", colnames(df))
        colnames(df) <- sub("^X12.*\\d\\d", "Date_Time", colnames(df))
        colnames(df) <- sub("^X05.*\\d\\d", "Date_Time", colnames(df))
        colnames(df) <- sub("^Temp.*warmed_air_1m.", "Temp_F_XP_air_1m", colnames(df))
        colnames(df) <- sub("^Temp....C.", "Temp_F_XP_air_1m", colnames(df))
        colnames(df) <- sub("^X\\d\\d.\\d\\d", "Temp_F_XP_air_1m", colnames(df))
        colnames(df) <- sub("^X.\\d.\\d\\d", "Temp_F_XP_air_1m", colnames(df))
        colnames(df) <- sub("^X\\d.\\d\\d", "Temp_F_XP_air_1m", colnames(df))
        colnames(df) <- sub("^Intensity.*warmed_light_1..", "Intensity_lum_ft_XP_light_1m", colnames(df))
        colnames(df) <- sub("^Intensity....lux.", "Intensity_lum_ft_XP_light_1m", colnames(df))
        colnames(df) <- sub("^X\\d\\d\\d\\d\\d.\\d\\d", "Intensity_lum_ft_XP_light_1m", colnames(df))
        return(df)
}

# change column names for pendant cleanup again - just fixing the mistakes made from the random characters above
change_pend_names2 <-  function(df){
        colnames(df) <- sub("^Temp_F_XP_air_1m.*\\d", "Intensity_lum_ft_XP_light_1m", colnames(df))
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
  df[["XH_ambient_air_1m"]] <- fahrenheit.to.celsius(df[["XH_ambient_air_1m"]])
  df[["XU_warmed_air_10cm"]] <- fahrenheit.to.celsius(df[["XU_warmed_air_10cm"]])
  df[["XU_warmed_soil_temp_5cm"]] <- fahrenheit.to.celsius(df[["XU_warmed_soil_temp_5cm"]])
  df[["XU_ambient_air_10cm"]] <- fahrenheit.to.celsius(df[["XU_ambient_air_10cm"]])
  df[["XU_ambient_soil_temp_5cm"]] <- fahrenheit.to.celsius(df[["XU_ambient_soil_temp_5cm"]])
  return(df)
}

# Change 2020 dataframe to celsius for pendant sensors
f_to_c2 <- function(df){
  df[["Temp_F_XP_air_1m"]] <- fahrenheit.to.celsius(df[["Temp_F_XP_air_1m"]])
  return(df)
}

#Change date format to POSIX format
change_POSIX <- function(df){
  df[["Date_Time"]] <- as.POSIXct(df[["Date_Time"]],tryFormats = c("%m/%d/%y %I:%M:%S %p",
                                                                   "%m/%d/%Y %H:%M",
                                                                   "%m/%d/%y %H:%M",
                                                                   "%m/%d/%Y %I:%M:%S",
                                                                   "%m/%d/%Y %H:%M:%S",
                                                                   "%F %H:%M:%S"), tz="UTC")
  return(df)
}

#Adding in pendant_ID column
add_name_cols <- function(l){
  for(i in seq_along(l)){
    l[[i]]$Pendant_ID <- gsub("^pend(.{2,3})_.*$", "\\1", names(l)[i])
  }
  return(l)
}

# Adding column for plot ID for pendants - KBS
plot_ID_kbs <- function(df){
        df[["Plot"]] = 0
        df[["Plot"]][df[["Pendant_ID"]] == '4P'] = "D2"
        df[["Plot"]][df[["Pendant_ID"]] == '5P'] = "A2"
        df[["Plot"]][df[["Pendant_ID"]] == '6P'] = "B2"
        df[["Plot"]][df[["Pendant_ID"]] == '7P'] = "C4"
        df[["Plot"]][df[["Pendant_ID"]] == '8P'] = "D3"
        df[["Plot"]][df[["Pendant_ID"]] == '9P'] = "A4"
        df[["Plot"]][df[["Pendant_ID"]] == '10P'] = "A5"
        df[["Plot"]][df[["Pendant_ID"]] == '11P'] = "C6"
        df[["Plot"]][df[["Pendant_ID"]] == '12P'] = "D6"
        return(df)
}

# Adding column for plot ID for pendants - UMBS
plot_ID_umbs <- function(df){
        df[["Plot"]] = 0
        df[["Plot"]][df[["Pendant_ID"]] == '4P'] = "C1"
        df[["Plot"]][df[["Pendant_ID"]] == '5P'] = "A2"
        df[["Plot"]][df[["Pendant_ID"]] == '6P'] = "B2"
        df[["Plot"]][df[["Pendant_ID"]] == '7P'] = "B3"
        df[["Plot"]][df[["Pendant_ID"]] == '8P'] = "D3"
        df[["Plot"]][df[["Pendant_ID"]] == '9P'] = "A4"
        df[["Plot"]][df[["Pendant_ID"]] == '10P'] = "B6"
        df[["Plot"]][df[["Pendant_ID"]] == '11P'] = "C6"
        df[["Plot"]][df[["Pendant_ID"]] == '12P'] = "D6"
        return(df)
}

#Remove columns from paired sensors
remove_col <- function(df,name){
  vec <- which(names(df) %in% name)
  df = df[,-vec]
  return(df)
}

# removing outliers from paired sensors
#remove_outliers = function(df){
#  is.na(df[["XU_warmed_soil_temp_5cm"]]) <- df[["XU_warmed_soil_temp_5cm"]] >= 40
#  is.na(df[["XU_ambient_soil_temp_5cm"]]) <- df[["XU_ambient_soil_temp_5cm"]] >= 40
#  is.na(df[["XU_warmed_soil_temp_5cm"]]) <- df[["XU_warmed_soil_temp_5cm"]] <= -40
#  is.na(df[["XU_ambient_soil_temp_5cm"]]) <- df[["XU_ambient_soil_temp_5cm"]] <= -40
#  is.na(df[["XH_warmed_soil_moisture_5cm"]]) <- df[["XH_warmed_soil_moisture_5cm"]] <= 0
#  is.na(df[["XH_ambient_soil_moisture_5cm"]]) <- df[["XH_ambient_soil_moisture_5cm"]] <= 0
#  is.na(df[["XH_ambient_air_1m"]]) <- df[["XH_ambient_air_1m"]] <= -30
#  is.na(df[["XH_warmed_air_1m"]]) <- df[["XH_warmed_air_1m"]] <= -30
#  is.na(df[["XH_warmed_RH_1m"]]) <- df[["XH_warmed_RH_1m"]] <= -30
#  is.na(df[["XH_ambient_RH_1m"]]) <- df[["XH_ambient_RH_1m"]] <= -30
#  is.na(df[["XU_warmed_air_10cm"]]) <- df[["XU_warmed_air_10cm"]] >= 49
#  is.na(df[["XU_ambient_air_10cm"]]) <- df[["XU_ambient_air_10cm"]] >= 49
#  is.na(df[["XU_warmed_air_10cm"]]) <- df[["XU_warmed_air_10cm"]] <= -30
#  is.na(df[["XU_ambient_air_10cm"]]) <- df[["XU_ambient_air_10cm"]] <= -30
#  return(df)
#}

remove_outliers = function(df){
        # first removing common error values
        is.na(df) <- df == -888.88
        is.na(df) <- df == 537.327
        is.na(df) <- df == 537.33
        is.na(df) <- df == 316.524
        is.na(df) <- df == 304.306
        # then removing data > 2SD
        df[["XU_warmed_soil_temp_5cm"]] <- replace(df[["XU_warmed_soil_temp_5cm"]],abs(scale(df[["XU_warmed_soil_temp_5cm"]]))>2,NA)
        df[["XU_ambient_soil_temp_5cm"]] <- replace(df[["XU_ambient_soil_temp_5cm"]],abs(scale(df[["XU_ambient_soil_temp_5cm"]]))>2,NA)
        df[["XH_warmed_soil_moisture_5cm"]] <- replace(df[["XH_warmed_soil_moisture_5cm"]],abs(scale(df[["XH_warmed_soil_moisture_5cm"]]))>2,NA)
        df[["XH_ambient_soil_moisture_5cm"]] <- replace(df[["XH_ambient_soil_moisture_5cm"]],abs(scale(df[["XH_ambient_soil_moisture_5cm"]]))>2,NA)
        df[["XH_ambient_air_1m"]] <- replace(df[["XH_ambient_air_1m"]],abs(scale(df[["XH_ambient_air_1m"]]))>2,NA)
        df[["XH_warmed_air_1m"]] <- replace(df[["XH_warmed_air_1m"]],abs(scale(df[["XH_warmed_air_1m"]]))>2,NA)
        df[["XH_warmed_RH_1m"]] <- replace(df[["XH_warmed_RH_1m"]],abs(scale(df[["XH_warmed_RH_1m"]]))>2,NA)
        df[["XH_ambient_RH_1m"]] <- replace(df[["XH_ambient_RH_1m"]],abs(scale(df[["XH_ambient_RH_1m"]]))>2,NA)
        df[["XU_warmed_air_10cm"]] <- replace(df[["XU_warmed_air_10cm"]],abs(scale(df[["XU_warmed_air_10cm"]]))>2,NA)
        df[["XU_ambient_air_10cm"]] <- replace(df[["XU_ambient_air_10cm"]],abs(scale(df[["XU_ambient_air_10cm"]]))>2,NA)
        return(df)
}
        
        
        