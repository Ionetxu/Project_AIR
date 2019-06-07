# Initial analysis
library(readr)
library(dplyr)
library(tidyr)
library(purrr)
library(lubridate)
library(ggplot2)
library(stringr)
library(knitr)
library(xts)
library(zoo)
library(gridExtra)
library(fpp2)
library(RcppRoll)
library(kableExtra)
library(imputeTS)
options(knitr.table.format = "html")

airPM10 <- read_csv('/Users/ione/Desktop/Project_AIR/data/airpm10.csv')
View(airPM10)
dim(airPM10)
summary(airPM10)
str(airPM10)

# Giving new column names
airPM10 <- airPM10 %>% dplyr::rename(measurement_code='CODI MESURAMENT',
                            pollutant='CONTAMINANT',
                            station_code = 'CODI ESTACIÓ',
                            station_name = 'NOM ESTACIÓ',
                            latitude = 'LATITUD',
                            longitude = 'LONGITUD',
                            unit = 'UNITATS',
                            year = 'ANY',
                            month = 'MES',
                            day = 'DIA',
                            dt = 'DATA',
                            time = 'HORA',
                            value = 'VALOR')
head(airPM10)
station_dict <- data.frame(
  station_code = c(3,4,39,42,43,44,50,54,56,57,58),
  station_alias = c("St.Gervasi", "Poblenou", "Sagrera","Sants", "Eixample",
                    "Gracia-Sant Gervasi","Ciutatella","Torre Girona",
                    "Parc Vall Hebron","Palau Reial",
                    "Observatori Fabra")
)

#We join the station dictionary to the airNO2 dataframe to add new station names
head(airPM10$station_alias)
airPM10 <- airPM10 %>% left_join(station_dict, by = 'station_code')


#Convert Time column in better format concatenating minutes and seconds
#Take out a space of time column
str_squish(airPM10$time)
airPM10$time <- paste(airPM10$time,":00:00",sep = "")
#Times that are 24:00:00 transform them into 0:00:00 of next day as R doesnt like the 24h format.
#I will try to do this with lubridate:
head(print(airPM10$time))

#Going to include the time with the date in a new column dt
airPM10$dt <- with(airPM10, ymd(airPM10$dt) + hms(time))
#Convert into POSIXct because Dplyr doesnt support POSIXlt
airPM10$dt <- as.POSIXct(airPM10$dt)
head(print(airNO2$dt))
#We drop columns that we don't need:
airPM10 <- dplyr::select(airPM10, -c("station_name", "time"))
summary(airPM10)

#Let's analyze the data by filtering by station:


Poblenou_PM10 <- airPM10 %>% filter(station_code == 4)
Sants_PM10 <- airPM10 %>% filter(station_code == 42)
Eixample_PM10 <- airPM10 %>% filter(station_code == 43)
Gracia_PM10 <- airPM10 %>% filter(station_code == 44)
Ciutatella_PM10 <- airPM10 %>% filter(station_code == 50)
Torre_girona_PM10 <- airPM10 %>% filter(station_code == 54)
Vall_hebron_PM10 <- airPM10 %>% filter(station_code == 56)
Palau_reial_PM10 <- airPM10 %>% filter(station_code == 57)
Observ_fabra_PM10 <- airPM10 %>% filter(station_code == 58)

#Poblenou
Poblenou_PM10_plt <- ggplot(Poblenou_PM10, aes(x = dt, y = value)) +
  geom_point(alpha = 0.5) +
  geom_smooth(color = "grey", alpha = 0.2) +
  geom_hline(yintercept = 50, linetype="dashed", colour = "red")+
  scale_x_datetime(date_breaks = "2 years",date_labels = "%Y") +
  labs( x = "Time", y = "PM10 (µg/m3)", title = "PM10(µg/m3) - Poblenou")
Poblenou_PM10_plt
#We see that we have only data from years 2004,2005, half 2006, 2007-2010, and 2016, 2017, half 2018.

#Sants
Sants_PM10_plt <- ggplot(Sants_PM10, aes(x = dt, y = value)) +
  geom_point(alpha = 0.5) +
  geom_smooth(color = "grey", alpha = 0.2) +
  geom_hline(yintercept = 50, linetype="dashed", colour = "red")+
  scale_x_datetime(date_breaks = "2 years",date_labels = "%Y") +
  labs( x = "Time", y = "PM10 (µg/m3)", title = "PM10(µg/m3) - Sants")
Sants_PM10_plt
#We only have data from 2002 to 2004, and 2005 to 2009

#Eixample
Eixample_PM10_plt <- ggplot(Eixample_PM10, aes(x = dt, y = value)) +
  geom_point(alpha = 0.5) +
  geom_smooth(color = "grey", alpha = 0.2) +
  geom_hline(yintercept = 50, linetype="dashed", colour = "red")+
  scale_x_datetime(date_breaks = "1 year",date_labels = "%Y") +
  labs( x = "Time", y = "PM10 (µg/m3)", title = "PM10(µg/m3) - Eixample")
Eixample_PM10_plt
#We have data from 2004 to 2007, and 2011 to 2013, and 2014 to 2019

#Gracia
Gracia_PM10_plt <- ggplot(Gracia_PM10, aes(x = dt, y = value)) +
  geom_point(alpha = 0.5) +
  geom_smooth(color = "grey", alpha = 0.2) +
  geom_hline(yintercept = 50, linetype="dashed", colour = "red")+
  scale_x_datetime(date_breaks = "1 year",date_labels = "%Y") +
  labs( x = "Time", y = "PM10 (µg/m3)", title = "PM10(µg/m3) - Gracia")
Gracia_PM10_plt
#We have data from 2004 to 2007, and 2018 to 2010, and 2016 to 2019

# There is no data for Ciutatella

#Torre_girona
Torre_girona_PM10_plt <- ggplot(Torre_girona_PM10, aes(x = dt, y = value)) +
  geom_point(alpha = 0.5) +
  geom_smooth(color = "grey", alpha = 0.2) +
  geom_hline(yintercept = 50, linetype="dashed", colour = "red")+
  scale_x_datetime(date_breaks = "1 year",date_labels = "%Y") +
  labs( x = "Time", y = "PM10 (µg/m3)", title = "PM10(µg/m3) - Torre girona")
Torre_girona_PM10_plt
#Data from 2016 to 2019

#There is no data for PM10 in Vall_hebron

#Palau_reial
Palau_reial_PM10_plt <- ggplot(Palau_reial_PM10, aes(x = dt, y = value)) +
  geom_point(alpha = 0.5) +
  geom_smooth(color = "grey", alpha = 0.2) +
  geom_hline(yintercept = 50, linetype="dashed", colour = "red")+
  scale_x_datetime(date_breaks = "1 year",date_labels = "%Y") +
  labs( x = "Time", y = "PM10 (µg/m3)", title = "PM10(µg/m3) - Palau reial")
Palau_reial_PM10_plt
#Data from 2016 to 2019

#Observ_fabra
Observ_fabra_PM10_plt <- ggplot(Observ_fabra_PM10, aes(x = dt, y = value)) +
  geom_point(alpha = 0.5) +
  geom_smooth(color = "grey", alpha = 0.2) +
  geom_hline(yintercept = 50, linetype="dashed", colour = "red")+
  scale_x_datetime(date_breaks = "1 year",date_labels = "%Y") +
  labs( x = "Time", y = "PM10 (µg/m3)", title = "PM10(µg/m3) - Observatori Fabra")
Observ_fabra_PM10_plt
#Data only from end of 2018 and 2019

#I'm going to discard Vall Hebron, Ciutatella,Sagrera,St Gervasi from the study.


#Missing values management - package imputeTS

#Eixample:
#Going to create a TS object with frequency= 24 (assumption=hourly measurements with 1 day seasonality)

Eixample_PM10_2014_2018 <- Eixample_PM10 %>% filter(year >=2014, year <= 2018)
Eixample_PM10_2014_2018_ts <- ts(Eixample_PM10_2014_2018[,10], start = c(2014, 1), frequency = 24)

plotNA.distributionBar(Eixample_PM10_2014_2018_ts, breaks = 12)
plotNA.gapsize(Eixample_PM10_2014_2018_ts)
statsNA(Eixample_PM10_2014_2018_ts)

imp_2014_2018_PM10_Eixample_intp <- na.interpolation(Eixample_PM10_2014_2018_ts)
Eixample_PM10_2014_2018 <- Eixample_PM10_2014_2018 %>% mutate(imp_2014_2018_PM10_Eixample_intp)
plotNA.imputations(x.withNA = Eixample_PM10_2014_2018_ts, x.withImputations = imp_2014_2018_PM10_Eixample_intp)

head(Eixample_PM10_2014_2018)

write.csv(Eixample_PM10_2014_2018, "/Users/ione/Desktop/Project_AIR/Data/Eixample_PM10.csv", row.names = F)

#I will also prepare data for Tableau, joining PM10 and NO2 with no NA imputations:
Air_NO2_PM10 <- rbind(airNO2_1,airPM10)
summary(Air_NO2_PM10)
head(Air_NO2_PM10)

View(Air_NO2_PM10)
#write.csv(Air_NO2_PM10, "/Users/ione/Desktop/Project_AIR/Data/Air_NO2_PM10.csv", row.names = F)
Air_NO2_PM10_weather <- merge(Air_NO2_PM10,Weather_bcn, by ="dt")
summary(Air_NO2_PM10_weather)
#write.csv(Air_NO2_PM10_weather, "/Users/ione/Desktop/Project_AIR/Data/Air_NO2_PM10_weather.csv", row.names = F)

