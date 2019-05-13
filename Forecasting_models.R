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

#Let's read the csv we prepared previously in the first analysis script.

Eixample <- read_csv('/Users/ione/Desktop/Project_AIR/data/Eixample_NO2_ts.csv')
Eixample_NO2_2014_2018 <- read_csv('/Users/ione/Desktop/Project_AIR/data/Eixample_NO2_2018.csv')
View(Eixample_NO2_2014_2018 )
dim(Eixample)
summary(Eixample )
str(Eixample )

##Because the ts is again in dataframe format,let's transform it again to ts:

Eixample_NO2_ts <- ts(Eixample_NO2_2014_2018[,11], start = c(2014, 1), frequency = 24)
Eixample_NO2_year_ts <- ts(Eixample_NO2_2014_2018[,11], start = c(2014, 1), frequency = 8760)
Eixample_ts <- ts(Eixample, start = c(2014, 1), frequency = 24)
##Let's plot the ts to see how it looks:
autoplot(Eixample_ts)

#For some reason it doesnt work when I give yearly seasonality:
autoplot(Eixample_year_ts)

#AUTOREGRESSION METHODS:

#Let's analyze the data to see what is the trend, seasonality, and what is the 
#autocorrelation level or the linear relationship between lagged values of our time series.

#We are going to plot the autocorrelation coefficients to show the autocorrelation function or ACF. 
#The plot is also known as a correlogram.

ggAcf(Eixample_ts)
ggAcf(Eixample_year_ts)

#We observe that in fact, we have a case of multiple seasonality, with peaks in lag=24 but also in 
#lag=8760, meaning we have a daily season but also a yearly season. 
 
#We also have a trend, because the autocorrelations for small lags are large and positive, and observations nearby in time are also nearby in size. 
#Our time series tend to have positive values that slowly decrease as the lags increase.
# And because data are seasonal, the autocorrelations will be larger for the seasonal lags (at multiples of the seasonal frequency) than for other lags.
#When data are both trended and seasonal, you see a combination of these effects. 
# The lags decrease because of the trend, and they have a “scalloped” shape due to the seasonality, 
# in lag=24 and lag=8760 and multiples.


# DATA PREPARATION: TRAIN and TEST 

#To evaluate the model, we are going to generate multiple training sets, and see what works the best.
#Train: 2014-Jan to 2018-Noc, Test: All month of Dic 2018
#Train: Sept 2018-Nov 2018, Test: last 3 days in 2018-Dec


train1 <- subset(Eixample, end=length(Eixample)-31*24)
autoplot(train1)
train2 <- subset(Eixample, start = 2018, end = length(Eixample) - 3*24)
autoplot(train2)







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


