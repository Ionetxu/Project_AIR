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


airNO2 <- read_csv('/Users/ione/Desktop/Project_AIR/data/airNO2.csv')
View(airNO2)
dim(airNO2)
summary(airNO2)
str(airNO2)

# Giving new column names
airNO2 <- airNO2 %>% rename(measurement_code='CODI MESURAMENT',
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
head(airNO2)
unique(airNO2$station_name)
unique(airNO2$station_code)
airNO2 %>% filter(airNO2$station_code=='39') %>% .$station_name
airNO2 %>% filter(airNO2$station_code=='56') %>% .$station_name

#Are stations 3 and 44 the same? ( St Gervasi vs Gracia & St Gervasi)
airNO2 %>% filter(airNO2$station_code=='3') %>% .$longitude
airNO2 %>% filter(airNO2$station_code=='44') %>% .$longitude

#They are not the same stations

#Changing station names and fixing typos (two different names 
#"Barcelona (Gràcia - Sant Gervasi)'/"Barcelona (Gracia - Sant Gervasi)
#for same station #44 

station_dict <- data.frame(
  station_code = c(3,4,39,42,43,44,50,54,56,57,58),
  station_alias = c("St.Gervasi", "Poblenou", "Sagrera","Sants", "Eixample",
                    "Gracia-Sant Gervasi","Ciutatella","Torre Girona",
                    "Parc Vall Hebron","Palau Reial",
                    "Observatori Fabra")
)

#We join the station dictionary to the airNO2 dataframe to add new station names

airNO2 <- airNO2 %>% left_join(station_dict, by = 'station_code')
summary(airNO2)

#Convert Time column in better format concatenating minutes and seconds
#Take out a space of time column
str_squish(airNO2$time)
airNO2$time <- paste(airNO2$time,":00:00",sep = "")
head(print(airNO2$time))

#Going to include the time with the date in a new column dt


airNO2$dt <- with(airNO2, ymd(airNO2$dt) + hms(time))
#Convert into POSIXct because Dplyer doesnt support POSIXlt

airNO2$dt <- as.POSIXct(airNO2$dt)
head(print(airNO2$dt))
#We drop columns that we don't need - measurement-code and station name & sort columns
airNO2_1 <- dplyr::select(airNO2, -c("measurement_code", "station_name"))
summary(airNO2_1)

airNO2_1 %>% select('pollutant','station_code','station_alias','latitude',
                 'longitude','year','month','day','dt','time','value')

#Let's analyze the data by measurement station to see completeness


St_gervasi_NO2 <- airNO2_1 %>% filter(station_code == 3)
Poblenou_NO2 <- airNO2_1 %>% filter(station_code == 4)
Sagrera_NO2 <- airNO2_1 %>% filter(station_code == 39)
Sants_NO2 <- airNO2_1 %>% filter(station_code == 42)
Eixample_NO2 <- airNO2_1 %>% filter(station_code == 43)
Gracia_NO2 <- airNO2_1 %>% filter(station_code == 44)
Ciutatella_NO2 <- airNO2_1 %>% filter(station_code == 50)
Torre_girona_NO2 <- airNO2_1 %>% filter(station_code == 54)
Vall_hebron_NO2 <- airNO2_1 %>% filter(station_code == 56)
Palau_reial_NO2 <- airNO2_1 %>% filter(station_code == 57)
Observ_fabra_NO2 <- airNO2_1 %>% filter(station_code == 58)

#Let's do some initial plots by station:
#St Gervasi 
St_Gervasi_NO2_plt <- ggplot(St_gervasi_NO2, aes(x = as.Date(dt), y = value)) + 
  geom_point(alpha = 0.5) +
  geom_smooth(color = "grey", alpha = 0.2) +
  labs( x = "Time", y = "NO2 (µg/m3)", title = "NO2(µg/m3) - St Gervasi")

#Only data from 1991 to 1997 - not interesting?

#Poblenou
Poblenou_NO2_plt <- ggplot(Poblenou_NO2, aes(x = dt, y = value)) + 
  geom_point(alpha = 0.5) +
  geom_smooth(color = "grey", alpha = 0.2) +
  scale_x_datetime(breaks='1 year',labels = date_format_tz( "%y")) +
  labs( x = "Time", y = "NO2 (µg/m3)", title = "NO2(µg/m3) - Poblenou")
Poblenou_NO2_plt 
#Good data from 1991 to 2019, with breaks in between

#Sagrera
Sagrera_NO2_plt <- ggplot(Sagrera_NO2, aes(x = dt, y = value)) + 
  geom_point(alpha = 0.5) +
  geom_smooth(color = "grey", alpha = 0.2) +
  scale_x_datetime(breaks='1 year',labels = date_format_tz( "%y")) +
  labs( x = "Time", y = "NO2 (µg/m3)", title = "NO2 (µg/m3)- Sagrera")
#Only data from 1993 to 2002

#Sants
Sants_NO2_plt <- ggplot(Sants_NO2, aes(x = dt, y = value)) + 
  geom_point(alpha = 0.5) +
  geom_smooth(color = "grey", alpha = 0.2) +
  scale_x_datetime(breaks='1 year',labels = date_format_tz( "%y")) +
  labs( x = "Time", y = "NO2 (µg/m3)", title = "NO2(µg/m3) - Sants")
#Data from 1995 to 2019
#Eixample
Eixample_NO2_plt <- ggplot(Eixample_NO2, aes(x = dt, y = value)) + 
  geom_point(alpha = 0.5) +
  geom_smooth(color = "grey", alpha = 0.2) +
  scale_x_datetime(breaks='1 year',labels = date_format_tz( "%y")) +
  labs( x = "Time", y = "NO2 (µg/m3)", title = "NO2(µg/m3) - Eixample")
#Data from 1995 to 2019
#Gracia
Gracia_NO2_plt <- ggplot(Gracia_NO2, aes(x = dt, y = value)) + 
  geom_point(alpha = 0.5) +
  geom_smooth(color = "grey", alpha = 0.2) +
  scale_x_datetime(breaks='1 year',labels = date_format_tz( "%y")) +
  labs( x = "Time", y = "NO2 (µg/m3)", title = "NO2(µg/m3) - Gracia")
#Good data from 1995 to 2019
#Ciutatella
Ciutatella_NO2_plt <- ggplot(Ciutatella_NO2, aes(x = dt, y = value)) + 
  geom_point(alpha = 0.5) +
  geom_smooth(color = "grey", alpha = 0.2) +
  scale_x_datetime(breaks='1 year',labels = date_format_tz( "%y")) +
  labs( x = "Time", y = "NO2 (µg/m3)", title = "NO2(µg/m3) - Ciutatella")
#Data from 2004 to 2019

#Torre_girona
Torre_girona_NO2_plt <- ggplot(Torre_girona_NO2, aes(x = dt, y = value)) + 
  geom_point(alpha = 0.5) +
  geom_smooth(color = "grey", alpha = 0.2) +
  scale_x_datetime(breaks='1 year',labels = date_format_tz( "%y")) +
  labs( x = "Time", y = "NO2 (µg/m3)", title = "NO2(µg/m3) - Torre Girona")
#Data from 2006 to 2019
#Vall_hebron
Vall_hebron_NO2_plt <- ggplot(Vall_hebron_NO2, aes(x = dt, y = value)) + 
  geom_point(alpha = 0.5) +
  geom_smooth(color = "grey", alpha = 0.2) +
  scale_x_datetime(breaks='1 year',labels = date_format_tz( "%y")) +
  labs( x = "Time", y = "NO2 (µg/m3)", title = "NO2(µg/m3) - Vall Hebron")
#Data only for 2010-2011??

#Palau_reial
Palau_reial_NO2_plt <- ggplot(Palau_reial_NO2, aes(x = dt, y = value)) + 
  geom_point(alpha = 0.5) +
  geom_smooth(color = "grey", alpha = 0.2) +
  scale_x_datetime(breaks='1 year',labels = date_format_tz( "%y")) +
  labs( x = "Time", y = "NO2 (µg/m3)", title = "NO2(µg/m3) - Palau Reial")
#Data from 2011 to 2019
#Observ_fabra
Observ_fabra_NO2_plt <- ggplot(Observ_fabra_NO2, aes(x = dt, y = value)) + 
  geom_point(alpha = 0.5) +
  geom_smooth(color = "grey", alpha = 0.2) +
  scale_x_datetime(breaks='1 year',labels = date_format_tz( "%y")) +
  labs( x = "Time", y = "NO2 (µg/m3)", title = "NO2(µg/m3) - Observatori Fabra")
#Data from 2018 to 2019

#I'm going to discard Sagrera and St Gervasi from the study

#Going to analyse the missing values by station

#First I am going to plot a subset of data for Poblenou station:

# Define Start and end times for the subset as POSICXct objects
startTime <- as.POSIXct("2019-03-01 10:00:00",tz="UTC")
endTime <- as.POSIXct("2019-03-20 10:00:00",tz="UTC")

# create a start and end time R object
start.end <- c(startTime,endTime)
start.end

#I have to format with time zone as otherwise ggplot2 doesnt deal with original date format
date_format_tz <- function(format = "%Y-%m-%d", tz = "UTC") {
  function(x) format(x, format, tz=tz)
}
View(Poblenou_NO2)
Poblenou_NO2_subset_plt <- ggplot(Poblenou_NO2, aes(x = as.POSIXct(dt), y = value)) + 
  geom_line(alpha = 0.5) +
  labs( x = "Time", y = "NO2 (µg/m3)", title = "NO2(µg/m3) - Poblenou NO2 Subset")+
  geom_smooth(color = "grey", alpha = 0.2) +
  coord_cartesian( ylim = c(0, 120))+
  scale_x_datetime(limits=start.end,breaks='24 hours',labels = date_format_tz( "%d\n%H:%M", tz="UTC"))
  
Poblenou_NO2_subset_plt

Sants_NO2_subset_plt <- ggplot(Sants_NO2, aes(x = as.POSIXct(dt), y = value)) + 
  geom_line(alpha = 0.5) +
  labs( x = "Time", y = "NO2 (µg/m3)", title = "NO2(µg/m3) - Poblenou NO2 Subset")+
  geom_smooth(color = "grey", alpha = 0.2) +
  coord_cartesian( ylim = c(0, 120))+
  scale_x_datetime(limits=start.end,breaks='12 hours',labels = date_format_tz( "%d\n%H:%M", tz="UTC"))

Sants_NO2_subset_plt

Eixample_NO2_subset_plt <- ggplot(Eixample_NO2, aes(x = as.POSIXct(dt), y = value)) + 
  geom_line(alpha = 0.5) +
  labs( x = "Time", y = "NO2 (µg/m3)", title = "NO2(µg/m3) - Poblenou NO2 Subset")+
  geom_smooth(color = "grey", alpha = 0.2) +
  coord_cartesian( ylim = c(0, 150))+
  scale_x_datetime(limits=start.end,breaks='12 hours',labels = date_format_tz( "%d\n%H:%M", tz="UTC"))

Eixample_NO2_subset_plt

Gracia_NO2_subset_plt <- ggplot(Gracia_NO2, aes(x = as.POSIXct(dt), y = value)) + 
  geom_line(alpha = 0.5) +
  labs( x = "Time", y = "NO2 (µg/m3)", title = "NO2(µg/m3) - Poblenou NO2 Subset")+
  geom_smooth(color = "grey", alpha = 0.2) +
  coord_cartesian( ylim = c(0, 125))+
  scale_x_datetime(limits=start.end,breaks='12 hours',labels = date_format_tz( "%d\n%H:%M", tz="UTC"))

Gracia_NO2_subset_plt

Ciutatella_NO2_subset_plt <- ggplot(Ciutatella_NO2, aes(x = as.POSIXct(dt), y = value)) + 
  geom_line(alpha = 0.5) +
  labs( x = "Time", y = "NO2 (µg/m3)", title = "NO2(µg/m3) - Poblenou NO2 Subset")+
  geom_smooth(color = "grey", alpha = 0.2) +
  coord_cartesian( ylim = c(0, 125))+
  scale_x_datetime(limits=start.end,breaks='12 hours',labels = date_format_tz( "%d\n%H:%M", tz="UTC"))

Ciutatella_NO2_subset_plt
#There are no measurements taken between 1am and 10am systematically in any station, avoiding rush hour in the morning. 
# It seems there is a peak every morning around 9-10am


#Missing values management - package imputeTS

#Going to create a TS object with assumption frequency= 24 (hourly measurements with 1 day )
#There is a bit gap of data in 2013-2014, so I will first try to do the imput between 2014-01-01 and 2017-12-31,
#as there is a gap of data in 2018 as well. 

Poblenou_NO2_2014_1 <- Poblenou_NO2 %>% filter(year ==2014 & month == 1)

Poblenou_NO2_2014_ts <- ts(Poblenou_NO2_2014_1[,11], start = c(2014, 1), frequency = 24)

#Let's plot the NA distribution with bars as it's a large file
plotNA.distributionBar(Poblenou_NO2_2014_ts, breaks = 31)
plotNA.gapsize(Poblenou_NO2_2014_ts)
statsNA(Poblenou_NO2_2014_ts)

#NO2 monthly evolution during 2014 in Poblenou station
Poblenou_NO2_subset_2014_plt <- ggplot(Poblenou_NO2_2014_1, aes(x = dt, y = value)) + 
  geom_line(alpha = 0.5) +
  labs( x = "Time", y = "NO2 (µg/m3)", title = "NO2(µg/m3) - Poblenou NO2 January 2014 without imputations")+
  geom_smooth(color = "grey", alpha = 0.2) +
  coord_cartesian( ylim = c(0, 150))+
  scale_x_datetime(breaks='1 day',labels = date_format_tz( "%d"))
Poblenou_NO2_subset_2014_plt

#I am going to try imputing values by kalman algorithm:

imp <- na.kalman(Poblenou_NO2_2014_ts)

#Plot of real values with imputations with kalman algorithm:
plotNA.imputations(x.withNA = Poblenou_NO2_2014_ts, x.withImputations = imp)



