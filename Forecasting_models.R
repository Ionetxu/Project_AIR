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
library(ggfortify)

##For forecasting purposes, we are going to try do it only for Eixample station ,as it has the higher
##pollution levels, completeness of data as well as less number of NA-s. 

Eixample_ts <- read_csv('/Users/ione/Desktop/Project_AIR/data/Eixample_NO2_ts.csv')
View(Eixample_ts )
dim(Eixample_ts)
summary(Eixample_ts )
str(Eixample_ts )

##Let's give ts format to the dataframe we have stored in the csv file previously:
Eixample <- ts(Eixample_ts, start = c(2014, 1), frequency = 24)
##Let's plot the ts to see how it looks:
autoplot(Eixample)


