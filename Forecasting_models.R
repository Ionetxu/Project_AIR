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
summary(Eixample)

train1 <- window(Eixample, end = c(2018, 11))
#Let's do an autocorrelation plot to see the lag
ggAcf(train1)

#Augmented Dickey-Fuller Test
adf.test(train1, alternative = "stationary")
Pacf(train1)
#No differencing required?

#Let's try to decompose the data:
decomp = stl(Eixample)
autoplot(decomp)

##NAIVE METHOD:
fc1 <- snaive(train1, h = 24)
accuracy(fc1, Eixample)
autoplot(fc1)
checkresiduals(fc1)

##HOLT METHOD:
fcholt <- holt(train1, h=24)
# Look at fitted model using summary()
summary(fcholt)
# Plot the forecasts
autoplot(fcholt)
# Check that the residuals look like white noise
checkresiduals(fcholt)
accuracy(fcholt, Eixample)

#Holt-Winters METHOD:
fhw <- hw(train1, seasonal = "additive", h = 24)
# Check if residuals look like white noise
checkresiduals(fhw)
autoplot(fhw)
accuracy(fhw, Eixample)


#Automatic forecasting with exponential smoothing - ETS model
# Function to return ETS forecasts
fitets <- ets(train1)
checkresiduals(fitets)
fitets %>% forecast(h = 24) %>% autoplot()

#ARIMA
fit <- auto.arima(train1)
# Summarize the fitted model
summary(fit)
# Plot 2-year forecasts
fit %>% forecast(h = 24) %>% autoplot()

#ARIMA with FOURIER

#ARIMA WITH WEATHER

#TBATS model


