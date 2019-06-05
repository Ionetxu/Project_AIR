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
library(urca)
library(forecast)
library(seasonal)

##For forecasting purposes, we are going to try do it only for Eixample station ,as it has the higher
##pollution levels and completeness of data.

#Let's read the csv we prepared previously in the first analysis script.

Eixample_NO2_2014_2018 <- read_csv('/Users/ione/Desktop/Project_AIR/data/Eixample_NO2_2014_2018.csv')
Eixample_NO2_2018 <- read_csv('/Users/ione/Desktop/Project_AIR/data/Eixample_NO2_2018.csv')
Eixample_NO2_2018_09 <- read_csv('/Users/ione/Desktop/Project_AIR/data/Eixample_NO2_2018_09.csv')
View(Eixample_NO2_2014_2018 )
dim(Eixample)
summary(Eixample )
str(Eixample )

##Because the ts is again in dataframe format,let's transform it again to ts:
Eixample_NO2_ts <- ts(Eixample_NO2_2014_2018[,10], frequency = 24)
Eixample_NO2_2018_ts <- ts(Eixample_NO2_2018[,10], frequency = 24)
Eixample_NO2_2018_09_ts <- ts(Eixample_NO2_2018_09[,10], frequency = 24)

##Let's plot the ts to see how it looks:
autoplot(Eixample_NO2_ts)
autoplot(Eixample_NO2_2018_ts)
autoplot(Eixample_NO2_2018_09_ts)

#We need to input missing values. We will do it by using interpolation method:
plotNA.distributionBar(Eixample_NO2_ts, breaks = 12)
plotNA.gapsize(Eixample_NO2_ts)
statsNA(Eixample_NO2_ts)

plotNA.distributionBar(Eixample_NO2_2018_ts , breaks = 12)
plotNA.gapsize(Eixample_NO2_2018_ts )
statsNA(Eixample_NO2_2018_ts )

plotNA.distributionBar(Eixample_NO2_2018_09_ts , breaks = 12)
plotNA.gapsize(Eixample_NO2_2018_09_ts )
statsNA(Eixample_NO2_2018_09_ts )

#NA imputation by interpolation method:

imp_2014_2018_NO2_Eixample_intp <- na.interpolation(Eixample_NO2_ts)
imp_2018_NO2_Eixample_intp <- na.interpolation(Eixample_NO2_2018_ts)
imp_2018_09_NO2_Eixample_intp <- na.interpolation(Eixample_NO2_2018_09_ts)

#Let's plot the new ts-s with the na interpolation:
plotNA.imputations(x.withNA = Eixample_NO2_ts, x.withImputations = imp_2014_2018_NO2_Eixample_intp)
plotNA.imputations(x.withNA = Eixample_NO2_2018_ts, x.withImputations = imp_2018_NO2_Eixample_intp)
plotNA.imputations(x.withNA = Eixample_NO2_2018_09_ts, x.withImputations = imp_2018_09_NO2_Eixample_intp)

#Decomposition of time series

#I will decompose the month long time period as an additive time series
Eixample_NO2_Comp <- decompose(imp_2018_09_NO2_Eixample_intp)
plot(Eixample_NO2_Comp)


#AUTOREGRESSION METHODS:

#Let's analyze the data to see what is the trend, seasonality, and what is the
#autocorrelation level or the linear relationship between lagged values of our time series.

#We are going to plot the autocorrelation coefficients to show the autocorrelation function or ACF.
#The plot is also known as a correlogram.

ggAcf(imp_2014_2018_NO2_Eixample_intp)
ggAcf(imp_2018_NO2_Eixample_intp)
ggAcf(imp_2018_09_NO2_Eixample_intp)

#We observe that we have at least one seasonality with peaks in lag=24 and multiples.
#We also have a trend, because the autocorrelations for small lags are large and positive, and observations nearby in time are similar size that decrease
#as the lags increase.
# The lags decrease because of the trend, and they have a “scalloped” shape due to the seasonality,
# in lag=24 and multiples.


# DATA PREPARATION: TRAIN and TEST

#To evaluate the model, we are going to generate 3 training sets, and see what works best.
#Train1: 2014-01 to 2018-11, Test: 2018-12
#Train2: 2018-01 to 2018-11, Test: 2018-12
#Train3: 2018-09-1 to 2018-09-27, Test: 2018-09-28 to 2018-09-30
train1 <- subset(imp_2014_2018_NO2_Eixample_intp, end=length(imp_2014_2018_NO2_Eixample_intp)-31*24)
autoplot(train1)
train2 <- subset(imp_2018_NO2_Eixample_intp, end = length(imp_2018_NO2_Eixample_intp) - 31*24)
autoplot(train2)
train3 <- subset(imp_2018_09_NO2_Eixample_intp, end = length(imp_2018_09_NO2_Eixample_intp) - 3*24)
autoplot(train3)

#We are going to start creating a baseline with some simple forecasting methods like naive,
#seasonal naive, and average methods, and we are going to compare them.

#Average method

#For train1 dataset, with 4 year data, we are going to forecast 3 days
fcavg1 <- meanf(train1, h=24)
autoplot(fcavg1)
summary(fcavg1)
checkresiduals(fcavg1)
acavg1 <- accuracy(fcavg1,imp_2014_2018_NO2_Eixample_intp)
acavg1

fcavg2 <- meanf(train2, h=24)
autoplot(fcavg2)
checkresiduals(fcavg2)
acavg2 <- accuracy(fcavg2,imp_2018_NO2_Eixample_intp)
acavg2

fcavg3 <- meanf(train3, h=24)
autoplot(fcavg3)
checkresiduals(fcavg3)
acavg3 <- accuracy(fcavg3,imp_2018_09_NO2_Eixample_intp)
acavg3

##Seasonal Naïve METHOD:
fcsn1 <- snaive(train1, h = 24)
checkresiduals(fcsn1)
acsnm1 <- accuracy(fcsn1,imp_2014_2018_NO2_Eixample_intp)

fcsn2 <- snaive(train2, h = 24)
autoplot(fcsn2 )
summary(fcsn2)
checkresiduals(fcsn2)
accuracy(fcsn2,imp_2018_NO2_Eixample_intp)

fcsn3 <- snaive(train3, h = 24)
autoplot(fcsn3)
summary(fcsn3)
checkresiduals(fcsn3)
accuracy(fcsn3,imp_2018_09_NO2_Eixample_intp)

#If we compare the accuracy of all these methods and training datasets with different sizes, the
#best RMSE so far has been with Seasonal Naïve method with training data of 1 month.
#See the table with all results.

#We are going to apply some exponential smoothing forecasting methods.
#Forecasts produced using exponential smoothing methods are weighted averages of past observations,
#with the weights decaying exponentially as the observations get older.
#In other words, the more recent the observation the higher the associated weight.


#Holt-Winters METHOD:
fhw1 <- hw(train1, seasonal = "additive", h = 24)
# Look at fitted model using summary()
summary(fhw1)
# Plot the forecasts
autoplot(fhw1)
# Check that the residuals look like white noise
checkresiduals(fhw1)
#Calculate the accuracy of the model
accuracy(fhw1, imp_2014_2018_NO2_Eixample_intp)

fhw2 <- hw(train2, seasonal = "additive", h = 24)
autoplot(fhw2)
summary(fhw2)
checkresiduals(fhw2)
accuracy(fhw2, imp_2018_NO2_Eixample_intp)


fhw3 <- hw(train3, seasonal = "additive", h = 24)
autoplot(fhw3)
summary(fhw3)
checkresiduals(fhw3)
accuracy(fhw3, imp_2018_09_NO2_Eixample_intp)


#Automatic forecasting with exponential smoothing - ETS model
#An alternative to estimating the parameters by minimising the sum of squared errors is to maximise the “likelihood”.
#The likelihood is the probability of the data arising from the specified model.
#Thus, a large likelihood is associated with a good model.
#For an additive error model, maximising the likelihood (assuming normally distributed errors) gives
#the same results as minimising the sum of squared errors.
#However, different results will be obtained for multiplicative error models.


# Function to return ETS forecasts
fitets1 <- ets(train1)
e1 <- fitets1 %>% forecast(h = 24) %>% accuracy(imp_2014_2018_NO2_Eixample_intp)
e1
#With 4 years training, it gives an ETS(M,N,M) model with no white noise(p-value < 2.2e-16)

fitets2 <- ets(train2)
checkresiduals(fitets2)
fitets2 %>% forecast(h =24) %>% autoplot()
e2 <- fitets3 %>% forecast(h = 24) %>% accuracy(imp_2018_NO2_Eixample_intp)
e2
#With 11 months training, it gives an ETS(M,N,A) model with no white noise (p-value < 2.2e-16)

fitets3 <- ets(train3)
summary(fitets3)
fitets3 %>% forecast(h = 24) %>% autoplot()
e3 <- fitets3 %>% forecast(h = 24) %>% accuracy(imp_2018_09_NO2_Eixample_intp)
e3
#RMSE = 9.452193
#With one month training, it gives an ETS(M, Ad, M) model with no white noise (p-value = 7.387e-10)

#The forecast doesnt look too good, the time-series might be a bit more complex than it seems with
#multiple seasonalities ( day, week, year).

#ARIMA MODELS

#A stationary time series is one whose properties do not depend on the time at which the series is observed.
#So time series with trends, or with seasonality, are not stationary,
#and white noise series are stationary
#Stationary time series don't have a predictable pattern in the long term.

#Kwiatkowski-Phillips-Schmidt-Shin (KPSS) test (Kwiatkowski, Phillips, Schmidt, & Shin, 1992).
#In this test, the null hypothesis is that the data are stationary, and we look for evidence
#that the null hypothesis is false.
#Consequently, small p-values (e.g., less than 0.05) suggest that differencing is required.

train1 %>% ur.kpss() %>% summary()
train2 %>% ur.kpss() %>% summary()
train3 %>% ur.kpss() %>% summary()

#Value of test-statistic is: 1.9162 so we can discard that it's a stationary time series, no
#differencing is required.
ndiffs(train1)
ndiffs(train2)
ndiffs(train3)

#This function tells us that differencing is required for train1.

#This other function tells us if we need stationary differencing:
nsdiffs(train1)
nsdiffs(train2)
nsdiffs(train3)
#The result is 0 so no need for seasonal differencing (D=0).
#To implement a seasonal ARIMA model, we need to determine parameters {p,d,q}{P,D,Q}

#The data are clearly non-stationary, with some seasonality, so we will first take
#a seasonal difference.
#The seasonally differenced data also appear to be non-stationary,
#so we take an additional first difference,


train1 %>% diff(lag=24) %>% ggtsdisplay()
train1 %>% diff(lag=24) %>% diff() %>% ggtsdisplay()

#Our aim now is to find an appropriate ARIMA model based on the ACF and PACF.
#The significant spike at lag 24 in the ACF suggests a seasonal MA(1) component (Q=1). The
#spikes in lag 24 for ACF I will include a non-seasonal AR component p=3
#We will try arima(3,1,1)(3,0,1)

fit <- Arima(train1, order=c(3,1,1), seasonal=c(3,0,1),method="CSS")
summary(fit)
checkresiduals(fit)
#There is bit spike in lag=23 in ACF so the AR component should be bigger. We'll try a model
#with p=4, P=4 and q=0, Q=0

fitarima1 <- Arima(train1, order=c(4,1,0), seasonal=c(4,0,0),method="CSS")
summary(fitarima1)
checkresiduals(fitarima1)
ar1 <- fitarima1 %>% forecast(h = 24) %>% accuracy(imp_2014_2018_NO2_Eixample_intp)
ar1
#RMSE=18.52

fitarima2 <- Arima(train1, order=c(5,1,0), seasonal=c(5,0,0),method="CSS")
summary(fitarima2)
checkresiduals(fitarima2)
ar2 <- fitarima2 %>% forecast(h = 24) %>% accuracy(imp_2014_2018_NO2_Eixample_intp)
ar2
#RMSE=18.02

fitarima3 <- Arima(train1, order=c(6,1,0), seasonal=c(6,0,0),method="CSS")
summary(fitarima3)
checkresiduals(fitarima3)
ar3 <- fitarima3 %>% forecast(h = 24) %>% accuracy(imp_2014_2018_NO2_Eixample_intp)
ar3
#RMSE=19.40

fitarima4 <- Arima(train1, order=c(7,1,0), seasonal=c(7,0,0),method="CSS")
summary(fitarima4)
checkresiduals(fitarima4)
ar4 <- fitarima4 %>% forecast(h = 24) %>% accuracy(imp_2014_2018_NO2_Eixample_intp)
ar4
#RMSE=17.12

fitautoarima1 <- auto.arima(train1)
summary(fitautoarima1)
checkresiduals(fitautoarima1)
#ARIMA(5,1,0)(2,0,0)[24]
#AICc=315571.7
fitautoarima1 %>% forecast( h=24) %>% autoplot()
a1 <- fitautoarima1 %>% forecast(h = 24) %>% accuracy(imp_2014_2018_NO2_Eixample_intp)
a1
#RMSE=18.928269

fitautoarima2 <- auto.arima(train2)
#ARIMA(1,0,1)(2,0,0)[24] with non-zero mean
#AICc=58782.28
summary(fitautoarima2)
checkresiduals(fitautoarima2)
fitautoarima2 %>% forecast( h=24) %>% autoplot()
a2 <- fitautoarima2 %>% forecast(h = 24) %>% accuracy(imp_2018_NO2_Eixample_intp)
a2
#RMSE=27.989931

fitautoarima3 <- auto.arima(train3)
summary(fitautoarima3)
checkresiduals(fitautoarima3)
#ARIMA(1,0,0)(0,0,1)[24] with non-zero mean
#AICc=4688.08
fitautoarima3 %>% forecast(h=24) %>% autoplot()
a3 <- fitautoarima3 %>% forecast(h = 24) %>% accuracy(imp_2018_09_NO2_Eixample_intp)
a3
#RMSE = 12.34

#Complex seasonality -
#With pollution data, I think we have a case of multiple seasonality with daily, weekly and yearly
#seasons. To deal with these we should adapt our models
#If the time series is relatively short so that only one type of seasonality is present,
#then it will be possible to use one of the single-seasonal methods like ETS or a seasonal ARIMA model.
#But when the time series is long enough so that multiple seasonal periods appear, it will be necessary to use STL,
#dynamic harmonic regression or TBATS.

#TBATS model
train1 %>% tbats() -> fit_tbats
summary(fit_tbats)
fctbats1<- forecast(fit_tbats, h=24)
residuals(fctbats1)
checkresiduals


train2 %>% tbats() -> fit_tbats2
summary(fit_tbats2)
fc2 <- forecast(fit_tbats2, h=24)
autoplot(fc2)

train3 %>% tbats() -> fit_tbats3
fc3 <- forecast(fit_tbats3, h=24)
autoplot(fc3)
accuracy(fc3,imp_2018_09_NO2_Eixample_intp)
