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

install.packages("bookdown")


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
#Times that are 24:00:00 transform them into 0:00:00 of next day as R doesnt like the 24h format.
#I will try to do this with lubridate:
head(print(airNO2$time))


#Going to include the time with the date in a new column dt
airNO2$dt <- with(airNO2, ymd(airNO2$dt) + hms(time))
#Convert into POSIXct because Dplyer doesnt support POSIXlt
airNO2$dt <- as.POSIXct(airNO2$dt)
head(print(airNO2$dt))
#We drop columns that we don't need - measurement-code and station name & sort columns
#I take out time because 24h is not able to interpret correctly
airNO2_1 <- dplyr::select(airNO2, -c("measurement_code", "station_name", "time"))
summary(airNO2_1)

#Let's analyze the data by plotting them by station

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

str(Eixample_NO2)

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
Sants_NO2_plt
#Data from 1995 to 2019
#Eixample
Eixample_NO2_plt <- ggplot(Eixample_NO2, aes(x = dt, y = value)) +
  geom_point(alpha = 0.5) +
  geom_hline(yintercept = 200, linetype="dashed", colour = "red")+
  geom_hline(yintercept = 40, linetype="dashed", colour = "red")+
  geom_smooth(color = "grey", alpha = 0.2) +
  scale_x_datetime(breaks='1 year',labels = date_format_tz( "%y")) +
  labs( x = "Year", y = "NO2 (µg/m3)", title = "NO2(µg/m3) - NO2 evolution in Eixample station")
Eixample_NO2_plt

#Data from 1995 to 2019
#Gracia
Gracia_NO2_plt <- ggplot(Gracia_NO2, aes(x = dt, y = value)) +
  geom_point(alpha = 0.5) +
  geom_smooth(color = "grey", alpha = 0.2) +
  scale_x_datetime(breaks='1 year',labels = date_format_tz( "%y")) +
  labs( x = "Time", y = "NO2 (µg/m3)", title = "NO2(µg/m3) - Gracia")
Gracia_NO2_plt
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
endTime <- as.POSIXct("2019-03-08 10:00:00",tz="UTC")

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
  labs( x = "Time", y = "NO2 (µg/m3)", title = "NO2(µg/m3) - Poblenou NO2")+
  geom_smooth(color = "grey", alpha = 0.2) +
  coord_cartesian( ylim = c(0, 120))+
  scale_x_datetime(limits=start.end,breaks='24 hours',labels = date_format_tz( "%b\n%d", tz="UTC"))

Poblenou_NO2_subset_plt

Sants_NO2_subset_plt <- ggplot(Sants_NO2, aes(x = as.POSIXct(dt), y = value)) +
  geom_line(alpha = 0.5) +
  labs( x = "Time", y = "NO2 (µg/m3)", title = "NO2(µg/m3) - NO2 levels in March 2019 in Sants station")+
  geom_smooth(color = "grey", alpha = 0.2) +
  coord_cartesian( ylim = c(0, 120))+
  scale_x_datetime(limits=start.end,breaks='1 day',labels = date_format_tz( "%b\n%d"))

Sants_NO2_subset_plt

Eixample_NO2_subset_plt <- ggplot(Eixample_NO2, aes(x = as.POSIXct(dt), y = value)) +
  geom_line(alpha = 0.5) +
  geom_hline(yintercept = 40, linetype="dashed", colour = "red")+
  labs( x = "Time", y = "NO2 (µg/m3)", title = "NO2(µg/m3) - NO2 levels in March 2019 in Eixample station")+
  geom_smooth(color = "grey", alpha = 0.2) +
  coord_cartesian( ylim = c(0, 150))+
  scale_x_datetime(limits=start.end,breaks='24 hours',labels = date_format_tz( "%b\n%d"))

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
Poblenou_NO2_2014_ts_1 <- ts(Poblenou_NO2_2014_1[,11], start = c(2014, 1), frequency = 24)
plotNA.distributionBar(Poblenou_NO2_2014_ts_1, breaks = 31)
plotNA.gapsize(Poblenou_NO2_2014_ts_1)
statsNA(Poblenou_NO2_2014_ts_1)

Poblenou_NO2_subset_2014_plt_1 <- ggplot(Poblenou_NO2_2014_1, aes(x = dt, y = value)) +
  geom_line(alpha = 0.5) +
  labs( x = "Time", y = "NO2 (µg/m3)", title = "NO2(µg/m3) - Poblenou NO2 Jan 2014 without imputations")+
  geom_smooth(color = "grey", alpha = 0.2) +
  coord_cartesian( ylim = c(0, 150))+
  scale_x_datetime(breaks='1 day',labels = date_format_tz( "%d"))
Poblenou_NO2_subset_2014_plt_1

#We are going to apply the NA analysis to whole 2014 year:

Poblenou_NO2_2014 <- Poblenou_NO2 %>% filter(year ==2014)
Poblenou_NO2_2014_ts <- ts(Poblenou_NO2_2014[,11], start = c(2014, 1), frequency = 24)
#Let's plot the NA distribution with bars as it's a large file
plotNA.distributionBar(Poblenou_NO2_2014_ts, breaks = 12)
plotNA.gapsize(Poblenou_NO2_2014_ts)
statsNA(Poblenou_NO2_2014_ts)

#NO2 monthly evolution during 2014 in Poblenou station
Poblenou_NO2_subset_2014_plt <- ggplot(Poblenou_NO2_2014, aes(x = dt, y = value)) +
  geom_line(alpha = 0.5) +
  labs( x = "Time", y = "NO2 (µg/m3)", title = "NO2(µg/m3) - Poblenou NO2 2014 without imputations")+
  geom_smooth(color = "grey", alpha = 0.2) +
  coord_cartesian( ylim = c(0, 150))+
  scale_x_datetime(breaks='1 month',labels = date_format_tz( "%b %y"))
Poblenou_NO2_subset_2014_plt

#I am going to try imputing values by mean algorithm:
imp_2014_1_NO2_Poblenou_mean <- na.mean(Poblenou_NO2_2014_ts_1)
imp_2014_NO2_Poblenou_mean <- na.mean(Poblenou_NO2_2014_ts)
#Plot of real values with imputations with mean algorithm:
plotNA.imputations(x.withNA = Poblenou_NO2_2014_ts_1, x.withImputations = imp_2014_1_NO2_Poblenou_mean)
plotNA.imputations(x.withNA = Poblenou_NO2_2014_ts, x.withImputations = imp_2014_NO2_Poblenou_mean)

#I am going to try imputing values by Weighted Moving Average algorithm:
imp_2014_1_NO2_Poblenou_ma <- na.ma(Poblenou_NO2_2014_ts_1)
imp_2014_NO2_Poblenou_ma <- na.ma(Poblenou_NO2_2014_ts)
#Plot of real values with imputations with mean algorithm:
plotNA.imputations(x.withNA = Poblenou_NO2_2014_ts_1, x.withImputations = imp_2014_1_NO2_Poblenou_ma)
plotNA.imputations(x.withNA = Poblenou_NO2_2014_ts, x.withImputations = imp_2014_NO2_Poblenou_ma)

#I am going to try imputing values by Last Observation Carried Forward algorithm:
imp_2014_1_NO2_Poblenou_locf <- na.locf(Poblenou_NO2_2014_ts_1)
imp_2014_NO2_Poblenou_locf <- na.locf(Poblenou_NO2_2014_ts)
#Plot of real values with imputations with mean algorithm:
plotNA.imputations(x.withNA = Poblenou_NO2_2014_ts_1, x.withImputations = imp_2014_1_NO2_Poblenou_locf)
plotNA.imputations(x.withNA = Poblenou_NO2_2014_ts, x.withImputations = imp_2014_NO2_Poblenou_locf)

#I am going to try imputing values by kalman algorithm:
imp_2014_1_NO2_Poblenou_kalman <- na.kalman(Poblenou_NO2_2014_ts_1)
imp_2014_NO2_Poblenou_kalman <- na.kalman(Poblenou_NO2_2014_ts)
#Plot of real values with imputations with kalman algorithm:
plotNA.imputations(x.withNA = Poblenou_NO2_2014_ts_1, x.withImputations = imp_2014_1_NO2_Poblenou_kalman)
plotNA.imputations(x.withNA = Poblenou_NO2_2014_ts, x.withImputations = imp_2014_NO2_Poblenou_kalman)
#Some imputed values are negative, which is not a good outcome.

#I am going to try imputing values by interpolation algorithm:
imp_2014_1_NO2_Poblenou_intp <- na.interpolation(Poblenou_NO2_2014_ts_1)
imp_2014_NO2_Poblenou_intp <- na.interpolation(Poblenou_NO2_2014_ts)
#Plot of real values with imputations with interpolation algorithm:
plotNA.imputations(x.withNA = Poblenou_NO2_2014_ts_1, x.withImputations = imp_2014_1_NO2_Poblenou_intp)
plotNA.imputations(x.withNA = Poblenou_NO2_2014_ts, x.withImputations = imp_2014_NO2_Poblenou_intp)

#Autocorrelation of time-series plot:
ggAcf(imp_2014_NO2_Poblenou_intp)
#The maximum autocorrelation occurs at lag=1. We are going to apply a Ljung-Box test to discard is
#white noise time series
Box.test(imp_2014_NO2_Poblenou_intp, lag=1, type = "Ljung")

#Because p-value < 2,2 e-16, we see there is some seasonality and it's not a completely random series

#Now I am going to prepare CSV files for time-series forecasting. Things to do:
#*Imputing NA values bt interpolation for 4 years (2014,2015,2016,2017, and maybe 2018)
#*Doing it for all stations except for St_Gervasi and Sagrera:
#St_gervasi_NO2, Poblenou_NO2, Sagrera_NO2, Sants_NO2, Eixample_NO2, Gracia_NO2, Ciutatella_NO2, Torre_girona_NO2
#Palau_reial_NO2
#Doing it also for PM10

#Imputation of NA values for Poblenou - NO2 values from 2014 to 2018

Poblenou_NO2_2014_2017 <- Poblenou_NO2 %>% filter(year >=2014, year <= 2017)
Poblenou_NO2_2014_2017_ts <- ts(Poblenou_NO2_2014_2017[,11], start = c(2014, 1), frequency = 24)

imp_2014_2017_NO2_Poblenou_intp <- na.interpolation(Poblenou_NO2_2014_2017_ts)
plotNA.imputations(x.withNA = Poblenou_NO2_2014_2017_ts, x.withImputations = imp_2014_2017_NO2_Poblenou_intp)

autoplot(imp_2014_2017_NO2_Poblenou_intp)
autoplot(Poblenou_NO2_2014_2017_ts)

Poblenou_NO2_2014_2017 <- Poblenou_NO2_2014_2017 %>% mutate(imp_2014_2017_NO2_Poblenou_intp)
str(Poblenou_NO2_2014_2017)

write.csv(Poblenou_NO2_2014_2017, "/Users/ione/Desktop/Project_AIR/Data/Poblenou_NO2.csv", row.names = F)

#I am going to write the new dataframe with the ts element with new imputed values:
#SANTS:
Sants_NO2_2014_2017 <- Sants_NO2 %>% filter(year >=2014, year <= 2017)
Sants_NO2_2014_2017_ts <- ts(Sants_NO2_2014_2017[,11], start = c(2014, 1), frequency = 24)
imp_2014_2017_NO2_Sants_intp <- na.interpolation(Sants_NO2_2014_2017_ts)
plotNA.imputations(x.withNA = Sants_NO2_2014_2017_ts, x.withImputations = imp_2014_2017_NO2_Sants_intp)
write.csv(Sants_NO2_2014_2017, "/Users/ione/Desktop/Project_AIR/Data/Sants_NO2.csv", row.names = F)



#I am going to use Eixample data for forecasting purposes:
#I am going to subset 3 training datasets:
Eixample_NO2_2014_2018 <- Eixample_NO2 %>% filter(year >=2014, year <= 2018)
Eixample_NO2_2018 <- Eixample_NO2 %>% filter(year == 2018)
Eixample_NO2_2018_09 <- Eixample_NO2 %>% filter(year == 2018 & month == 09)


Eixample_NO2_2014_2018_ts <- ts(Eixample_NO2_2014_2018[,10], start = c(2014, 1), frequency = 24)
Eixample_NO2_2018_ts <- ts(Eixample_NO2_2018[,10], start = c(2018, 1), frequency = 24)
Eixample_NO2_2018_09_ts <- ts(Eixample_NO2_2014_2018[,10], frequency = 24)

Eixample_NO2_2014_2018_year_ts <- ts(Eixample_NO2_2014_2018[,11], start = c(2014, 1), frequency = 8760)
Eixample_NO2_ts <- ts(Eixample_NO2[,10],start = c(1995,1), frequency= 8760)

plotNA.distributionBar(Eixample_NO2_2014_2018_ts, breaks = 12)
plotNA.distributionBar(Eixample_NO2_2018_ts, breaks = 12)
plotNA.distributionBar(Eixample_NO2_2018_09_ts, breaks = 9)

plotNA.gapsize(Eixample_NO2_2014_2018_ts)
statsNA(Eixample_NO2_2014_2018_ts)

imp_2014_2018_NO2_Eixample_intp <- na.interpolation(Eixample_NO2_2014_2018_ts)
imp_2018_NO2_Eixample_intp <- na.interpolation(Eixample_NO2_2018_ts)
imp_2018_09_NO2_Eixample_intp <- na.interpolation(Eixample_NO2_2018_09_ts)


Eixample_NO2_2014_2018 <- Eixample_NO2_2014_2018 %>% mutate(imp_2014_2018_NO2_Eixample_intp)
Eixample_NO2_2018 <- Eixample_NO2_2018 %>% mutate(imp_2018_NO2_Eixample_intp)
Eixample_NO2_2018_09 <- Eixample_NO2_2018_09 %>% mutate(imp_2018_09_NO2_Eixample_intp)

plotNA.imputations(x.withNA = Eixample_NO2_2014_2018_ts, x.withImputations = imp_2014_2018_NO2_Eixample_intp)

#Dataframes:
write.csv(Eixample_NO2_2018, "/Users/ione/Desktop/Project_AIR/Data/Eixample_NO2_2018.csv", row.names = F)
write.csv(Eixample_NO2_2014_2018, "/Users/ione/Desktop/Project_AIR/Data/Eixample_NO2_2014_2018.csv", row.names = F)
write.csv(Eixample_NO2_2018_09, "/Users/ione/Desktop/Project_AIR/Data/Eixample_NO2_2018_09.csv", row.names = F)
write.csv(Eixample_NO2, "/Users/ione/Desktop/Project_AIR/Data/Eixample_NO2.csv", row.names = F)
#TS structures:
write.csv(imp_2014_2018_NO2_Eixample_intp, "/Users/ione/Desktop/Project_AIR/Data/Eixample_NO2_all_ts.csv", row.names = F)
write.csv(imp_2018_NO2_Eixample_intp, "/Users/ione/Desktop/Project_AIR/Data/Eixample_NO2_2018_ts.csv", row.names = F)
write.csv(imp_2018_09_NO2_Eixample_intp, "/Users/ione/Desktop/Project_AIR/Data/Eixample_NO2_2018_09_ts.csv", row.names = F)

#Analysis of the seasonality of NO2 evolution in Eixample:

ggseasonplot(Eixample_NO2_2014_2018_year_ts, year.labels=TRUE, year.labels.left=TRUE) +
  ylab("NO2") +
  ggtitle("Seasonal plot: NO2 in Eixample")

#I am going to use Gracia data for forecasting purposes:
#Gracia:
Gracia_NO2_2014_2018 <- Gracia_NO2 %>% filter(year >=2014, year <= 2018)
Gracia_NO2_2014_2018_ts <- ts(Gracia_NO2_2014_2018[,11], start = c(2014, 1), frequency = 24)
plotNA.distributionBar(Gracia_NO2_2014_2018_ts, breaks = 12)
plotNA.gapsize(Gracia_NO2_2014_2018_ts)
statsNA(Gracia_NO2_2014_2018_ts)
imp_2014_2018_NO2_Gracia_intp <- na.interpolation(Gracia_NO2_2014_2018_ts)

Gracia_NO2_2014_2018 <- Gracia_NO2_2014_2018 %>% mutate(imp_2014_2018_NO2_Gracia_intp)
plotNA.imputations(x.withNA = Gracia_NO2_2014_2018_ts, x.withImputations = imp_2014_2018_NO2_Gracia_intp)
write.csv(Gracia_NO2_2014_2018, "/Users/ione/Desktop/Project_AIR/Data/Gracia_NO2_2018.csv", row.names = F)
write.csv(imp_2014_2018_NO2_Gracia_intp, "/Users/ione/Desktop/Project_AIR/Data/Gracia_NO2_ts.csv", row.names = F)
