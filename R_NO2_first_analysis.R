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


airNO2 <- read_csv('/Users/ione/Desktop/Project_AIR/data/airNO2.csv')
View(airNO2)

#Filtering data by measuring station
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
                                    date_time_utc = 'DATA',
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


#We drop columns that we don't need - measurement-code and station name & sort columns
airNO2_1 <- dplyr::select(airNO2, -c("measurement_code", "station_name"))
summary(airNO2_1)

airNO2_1 %>% select('pollutant','station_code','station_alias','latitude',
                 'longitude','year','month','day','date_time_utc','time','value')

#Let's analyze the data by measurement station to see completeness

data_completeness <- airNO2_1 %>% group_by(station_code, year = year(date_time_utc))

summarise_all(funs(round(sum(!is.na(.))/n(), 2)))  # We obtain the proportion of 'not NAs'

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
ggplot(St_gervasi_NO2, aes(x = date_time_utc, y = value)) + 
  geom_point(alpha = 0.5) +
  geom_smooth(color = "grey", alpha = 0.2) +
  scale_colour_gradientn(colours = terrain.colors(10)) +
  theme(legend.position = c(0.3, 0.9),
        legend.background = element_rect(colour = "transparent", fill = NA), legend.direction = "horizontal")+
  labs( x = "Time", y = "NO2 (µg/m3)", title = "NO2(µg/m3) - St Gervasi")

#Only data from 1991 to 1997 - not interesting?

#Poblenou
ggplot(Poblenou_NO2, aes(x = date_time_utc, y = value)) + 
  geom_point(alpha = 0.5) +
  geom_smooth(color = "grey", alpha = 0.2) +
  scale_colour_gradientn(colours = terrain.colors(10)) +
  theme(legend.position = c(0.3, 0.9),
        legend.background = element_rect(colour = "transparent", fill = NA), legend.direction = "horizontal")+
  labs( x = "Time", y = "NO2 (µg/m3)", title = "NO2(µg/m3) - Poblenou")
#Good data from 1991 to 2019, with breaks in between

#Sagrera
ggplot(Sagrera_NO2, aes(x = date_time_utc, y = value)) + 
  geom_point(alpha = 0.5) +
  geom_smooth(color = "grey", alpha = 0.2) +
  scale_colour_gradientn(colours = terrain.colors(10)) +
  theme(legend.position = c(0.3, 0.9),
        legend.background = element_rect(colour = "transparent", fill = NA), legend.direction = "horizontal")+
  labs( x = "Time", y = "NO2 (µg/m3)", title = "NO2 (µg/m3)- Sagrera")
#Only data from 1993 to 2002

#Sants
ggplot(Sants_NO2, aes(x = date_time_utc, y = value)) + 
  geom_point(alpha = 0.5) +
  geom_smooth(color = "grey", alpha = 0.2) +
  scale_colour_gradientn(colours = terrain.colors(10)) +
  theme(legend.position = c(0.3, 0.9),
        legend.background = element_rect(colour = "transparent", fill = NA), legend.direction = "horizontal")+
  labs( x = "Time", y = "NO2 (µg/m3)", title = "NO2(µg/m3) - Sants")
#Data from 1995 to 2019
#Eixample
ggplot(Eixample_NO2, aes(x = date_time_utc, y = value)) + 
  geom_point(alpha = 0.5) +
  geom_smooth(color = "grey", alpha = 0.2) +
  scale_colour_gradientn(colours = terrain.colors(10)) +
  theme(legend.position = c(0.3, 0.9),
        legend.background = element_rect(colour = "transparent", fill = NA), legend.direction = "horizontal")+
  labs( x = "Time", y = "NO2 (µg/m3)", title = "NO2(µg/m3) - Eixample")
#Data from 1995 to 2019
#Gracia
ggplot(Gracia_NO2, aes(x = date_time_utc, y = value)) + 
  geom_point(alpha = 0.5) +
  geom_smooth(color = "grey", alpha = 0.2) +
  scale_colour_gradientn(colours = terrain.colors(10)) +
  theme(legend.position = c(0.3, 0.9),
        legend.background = element_rect(colour = "transparent", fill = NA), legend.direction = "horizontal")+
  labs( x = "Time", y = "NO2 (µg/m3)", title = "NO2(µg/m3) - Gracia")
#Good data from 1995 to 2019
#Ciutatella
ggplot(Ciutatella_NO2, aes(x = date_time_utc, y = value)) + 
  geom_point(alpha = 0.5) +
  geom_smooth(color = "grey", alpha = 0.2) +
  scale_colour_gradientn(colours = terrain.colors(10)) +
  theme(legend.position = c(0.3, 0.9),
        legend.background = element_rect(colour = "transparent", fill = NA), legend.direction = "horizontal")+
  labs( x = "Time", y = "NO2 (µg/m3)", title = "NO2(µg/m3) - Ciutatella")
#Data from 2004 to 2019

#Torre_girona
ggplot(Torre_girona_NO2, aes(x = date_time_utc, y = value)) + 
  geom_point(alpha = 0.5) +
  geom_smooth(color = "grey", alpha = 0.2) +
  scale_colour_gradientn(colours = terrain.colors(10)) +
  theme(legend.position = c(0.3, 0.9),
        legend.background = element_rect(colour = "transparent", fill = NA), legend.direction = "horizontal")+
  labs( x = "Time", y = "NO2 (µg/m3)", title = "NO2(µg/m3) - Torre Girona")
#Data from 2006 to 2019
#Vall_hebron
ggplot(Vall_hebron_NO2, aes(x = date_time_utc, y = value)) + 
  geom_point(alpha = 0.5) +
  geom_smooth(color = "grey", alpha = 0.2) +
  scale_colour_gradientn(colours = terrain.colors(10)) +
  theme(legend.position = c(0.3, 0.9),
        legend.background = element_rect(colour = "transparent", fill = NA), legend.direction = "horizontal")+
  labs( x = "Time", y = "NO2 (µg/m3)", title = "NO2(µg/m3) - Vall Hebron")
#Data only for 2010-2011??

#Palau_reial
ggplot(Palau_reial_NO2, aes(x = date_time_utc, y = value)) + 
  geom_point(alpha = 0.5) +
  geom_smooth(color = "grey", alpha = 0.2) +
  scale_colour_gradientn(colours = terrain.colors(10)) +
  theme(legend.position = c(0.3, 0.9),
        legend.background = element_rect(colour = "transparent", fill = NA), legend.direction = "horizontal")+
  labs( x = "Time", y = "NO2 (µg/m3)", title = "NO2(µg/m3) - Palau Reial")
#Data from 2011 to 2019
#Observ_fabra
ggplot(Observ_fabra_NO2, aes(x = date_time_utc, y = value)) + 
  geom_point(alpha = 0.5) +
  geom_smooth(color = "grey", alpha = 0.2) +
  scale_colour_gradientn(colours = terrain.colors(10)) +
  theme(legend.position = c(0.3, 0.9),
        legend.background = element_rect(colour = "transparent", fill = NA), legend.direction = "horizontal")+
  labs( x = "Time", y = "NO2 (µg/m3)", title = "NO2(µg/m3) - Observatori Fabra")
#Data from 2018 to 2019

#I'm going to discard Sagrera and St Gervasi from the study

#Going to analyse the missing values by station


