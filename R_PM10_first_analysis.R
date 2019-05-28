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

airPM10 <- read_csv('/Users/ione/Desktop/Project_AIR/data/airpm10.csv')
View(airPM10)
dim(airPM10)
summary(airPM10)
str(airPM10)

# Giving new column names
airPM10 <- airPM10 %>% rename(measurement_code='CODI MESURAMENT',
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
