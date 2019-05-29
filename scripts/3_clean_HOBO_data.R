######################################
# clean L0 HOBO data
# created by NKL May 29, 2019
#####################################

rm(list = ls())

# Check for and install required packages
for (package in c('tidyverse', 'googledrive', 'googlesheets')) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package)
    library(package, character.only=T)
  }
}


#check UMBS near plot A6 to see if ground squirrel activity has intrefered with sensors:
dl3H <- read.csv("~/Google Drive File Stream/My Drive/warmXtrophic/data/L0/UMBS/sensor_data/2019/5_14_2019/UMBS_3H_5142019.csv", stringsAsFactors=F, skip=1)

dl3U <- read.csv("~/Google Drive File Stream/My Drive/warmXtrophic/data/L0/UMBS/sensor_data/2019/5_14_2019/UMBS_3U_5142019.csv", stringsAsFactors=F, skip=1)

#clean up colnames
dl3H <- dl3H %>%
	dplyr::select(-"X.") %>%
	dplyr::rename(Date_Time = Date.Time..GMT.04.00,
				  Soil_moist_warmed_5A = Water.Content..m..m...LGR.S.N..10736966..SEN.S.N..10736055..LBL..3H_warmed_soil_moisture_5cm.,
				  Soil_moist_ambient_6A = Water.Content..m..m...LGR.S.N..10736966..SEN.S.N..10736058..LBL..3H_ambient_soil_moisture_5cm.,
				  Air_1m_warmed_5A = Temp...F..LGR.S.N..10736966..SEN.S.N..10737458..LBL..3H_warmed_air_1m.,
				  RH_warmed_5A = RH.....LGR.S.N..10736966..SEN.S.N..10737458..LBL..3H_warmed_RH_1m.,
				  Air_1m_ambient_6A = Temp...F..LGR.S.N..10736966..SEN.S.N..10737459..LBL..1H_ambient_air_1m.,
				  RH_ambient_6A = RH.....LGR.S.N..10736966..SEN.S.N..10737459..LBL..1H_ambient_RH_1m.)


dl3U <- dl3U %>%
	dplyr::select(-c("X.", "Bad.Battery..LGR.S.N..10737619.", "Good.Battery..LGR.S.N..10737619.", "Host.Connected..LGR.S.N..10737619.","End.Of.File..LGR.S.N..10737619.")) %>%
	dplyr::rename(Date_Time = Date.Time..GMT.04.00,
				  Air_10cm_warmed_5A = Temp...F..LGR.S.N..10737619..SEN.S.N..10737619..LBL..3U_warmed_air_10cm.,
				  Soil_temp_warmed_5A = Temp...F..LGR.S.N..10737619..SEN.S.N..10737619..LBL..3U_warmed_soil_5cm.,
				  Soil_temp_ambient_6A = Temp...F..LGR.S.N..10737619..SEN.S.N..10737619..LBL..3U_ambient_soil_5cm.,
				  Air_10cm_ambient_6A = Temp...F..LGR.S.N..10737619..SEN.S.N..10737619..LBL..3U_ambient_air_10cm.)

#covert Date_Time to date format				  		  
dl3H$Date_Time <-  as.POSIXct(dl3H$Date_Time, format = "%m/%d/%y %I:%M:%S %p")
str(dl3H)
dl3U$Date_Time <-  as.POSIXct(dl3U$Date_Time, format = "%m/%d/%y %I:%M:%S %p")
str(dl3U)

#plot recent months soil moisture:
pdf(file = "~/Desktop/ground_squirrel.pdf", height = 6, width = 9)
par(mfrow = c(2,1))
par(mar = c(2,4,3,2))
plot(dl3H$Date_Time[dl3H$Date_Time > "2018-09-01"], dl3H$Soil_moist_warmed_5A[dl3H$Date_Time > "2018-09-01"],type = "l", col = "red", ylab = "Soil moisture", xlab = "Date (2018-2019)", xaxt = "n")
points(dl3H$Date_Time[dl3H$Date_Time > "2018-09-01"], dl3H$Soil_moist_ambient_6A[dl3H$Date_Time > "2018-09-01"],type = "l",col = "blue")
legend("bottomright", legend = c("warmed (5A)", "ambient (6A)"), col = c("red", "blue"), lty=1, cex = 0.5)

#plot recent months soil moisture:
par(mar = c(5,4,0,2))
plot(dl3U$Date_Time[dl3U$Date_Time > "2018-09-01"], dl3U$Soil_temp_warmed_5A[dl3U$Date_Time > "2018-09-01"],type = "l", col = "red", ylab = "Soil temperature (C)", xlab = "Date (2018-2019)")
points(dl3U$Date_Time[dl3U$Date_Time > "2018-09-01"], dl3U$Soil_temp_ambient_6A[dl3U$Date_Time > "2018-09-01"],type = "l",col = "blue")
legend("bottomright", legend = c("warmed (5A)", "ambient (6A)"), col = c("red", "blue"), lty=1, cex = 0.5)
dev.off()

