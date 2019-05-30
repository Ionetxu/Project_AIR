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
library(hydroTSM)
library(tidyquant)
library(reshape)
library(ggpubr)
library(openair)
library(data.table)
require('data.table')
library(robust)

# Data Exploration

#We are going to try to understand more the data and get more insights. There are multiple
#questions I have about the data and I am going to try find the answers by using some additional datasets
#like event calendar dates, weather and health data.

#I will try to formulate questions or hypotesis and find answers from the data.


#1.What is the trend & seasonality of the NO2 and PM10 pollutants in Eixample in the last 10 years? And in Gracia?
#Let's start by reading data of NO2 and PM10 pollutants in Eixample and Gracia.

Eixample_NO2 <- read_csv('/Users/ione/Desktop/Project_AIR/data/Eixample_NO2_2014_2018.csv')
head(Eixample_NO2)
summary(Eixample_NO2)

names(Eixample_NO2)[names(Eixample_NO2) == "imp_2014_2018_NO2_Eixample_intp"] <- "NO2"
summary(Eixample_NO2)

#I am going to upload PM10 data:
Eixample_PM10 <- read_csv('/Users/ione/Desktop/Project_AIR/data/Eixample_PM10.csv')
head(Eixample_PM10)
summary(Eixample_PM10)

Eixample_PM10 <- Eixample_PM10 %>% dplyr::rename(PM10='imp_2014_2018_PM10_Eixample_intp')
summary(Eixample_PM10)


#I am going to do the following analysis for NO2 in Eixample.
#Daily average, min and max --> to see weekly seasonality
#Monthly average, min and max --> yearly seasonality
#Yearly average, min and max


#Let's calculate the min, max, mean and median of each day:

stat_fun <- function(x) c(min = min(x), max = max(x), mean = mean(x), median = median(x))
Eixample_NO2_day <- Eixample_NO2 %>%
  tq_transmute(select     = NO2,
               mutate_fun = apply.daily,
               FUN        = stat_fun)
summary(Eixample_NO2_day)

Eixample_PM10_day <- Eixample_PM10 %>%
  tq_transmute(select     = PM10,
               mutate_fun = apply.daily,
               FUN        = stat_fun)
summary(Eixample_PM10_day)


# NOT SURE ABOUT using XTS object. REVIEW
#Use as.xts to generate an xts object of average daily NO2 data:
Eixample_NO2_day_xts <- as.xts((Eixample_NO2_day[,-1]), order.by = as.Date(Eixample_NO2_day$dt))
head(Eixample_NO2_day_xts)
plot.zoo(Eixample_NO2_day_xts["2018-01"], plot.type = "single", xy.labels = c("Time", "NO2_daily_values"))
autoplot(Eixample_NO2_day_xts["2018-01"], facets = FALSE)

#Let's plot the daily average:

ggplot(Eixample_NO2_day, aes(x = as.Date(dt), y = mean)) +
  geom_line(alpha = 0.5) +
  geom_smooth(color = "grey", alpha = 0.2) +
  scale_x_date(breaks='1 year') +
  labs( x = "Time", y = "NO2 (µg/m3)", title = "NO2 (µg/m3) daily- Eixample")

ggplot(Eixample_PM10_day, aes(x = as.Date(dt), y = mean)) +
  geom_line(alpha = 0.5) +
  geom_smooth(color = "grey", alpha = 0.2) +
  scale_x_date(breaks='1 year') +
  labs( x = "Time", y = "PM10 (µg/m3)", title = "PM10 (µg/m3) daily- Eixample")

#Let's plot the daily median:
ggplot(Eixample_PM10_day, aes(x = as.Date(dt), y = median)) +
  geom_line(alpha = 0.5) +
  geom_smooth(color = "grey", alpha = 0.2) +
  scale_x_date(breaks='1 year') +
  labs( x = "Time", y = "PM10 (µg/m3)", title = "PM10 (µg/m3) daily- Eixample")

#Not really very representative to see if there is a weekly seasonality. Let's analyze monthly data.

Eixample_NO2_month <- Eixample_NO2 %>%
  tq_transmute(select     = NO2,
               mutate_fun = apply.monthly,
               FUN        = stat_fun)
summary(Eixample_NO2_month)

Eixample_PM10_month <- Eixample_PM10 %>%
  tq_transmute(select     = PM10,
               mutate_fun = apply.monthly,
               FUN        = stat_fun)
summary(Eixample_PM10_month)

#Not sure about XTS object. REVIEW:
Eixample_NO2_month_xts <- as.xts((Eixample_NO2_month[,-1]), order.by = as.Date(Eixample_NO2_month$dt))
summary(Eixample_NO2_month_xts)
autoplot(Eixample_NO2_month_xts$mean)


ggplot( data =Eixample_NO2_month , aes(x = as.Date(dt), y = mean)) +
  geom_line(alpha = 0.5) +
  labs( x = "Time", y = "NO2 (µg/m3)", title = "NO2(µg/m3) - Eixample NO2 monthly avg")+
  geom_smooth(color = "grey", alpha = 0.2) +
  scale_x_date(breaks='6 months', date_labels = "%m-%Y")

ggplot(data =Eixample_PM10_month ,aes(x = as.Date(dt), y = mean)) +
  geom_line(alpha = 0.5) +
  labs( x = "Time", y = "PM10 (µg/m3)", title = "NO2(µg/m3) - Eixample PM10 monthly avg")+
  geom_smooth(color = "grey", alpha = 0.2) +
  scale_x_date(breaks='6 months', date_labels = "%m-%Y")

#For NO2 I don't see a clear seasonality but the trend seems slightly positive.
#Therefore the pollutant NO2 levels have not improved significantly since 2014.

#For PM10 the seasonality is even more difficult to see, and there is not visible trend.


#3. What is the average yearly amount of NO2 in each station in Barcelona by year from 2014 to 2018? Does it pass the limit in any station?

Eixample_NO2_year <- Eixample_NO2 %>%
  tq_transmute(select     = NO2,
               mutate_fun = apply.yearly,
               FUN        = stat_fun)
View(Eixample_NO2_year)
#The limit recommended by EU standards is not met in any year (average yearly limit of 40µg/m3).

#4.What is the average yearly amount of PM10 in each station in Barcelona by year from 2014 to 2018? Does it pass the limit in any station?
Eixample_PM10_year <- Eixample_PM10 %>%
  tq_transmute(select     = PM10,
               mutate_fun = apply.yearly,
               FUN        = stat_fun)
View(Eixample_PM10_year)
#All years the yearly average limits set by EU are complied (average yearly limit of 40µg/m3).

#5.How many times does NO2 pass the max hourly limit per year (concentration of hourly 200 µg/m3
# more than 18 times?

head(sort(Eixample_NO2$NO2, decreasing = TRUE),10)
sum(Eixample_NO2$NO2 > 200)

#There are no values that are higher than 200 µg/m3 en Eixample, so limits are complied.

#6.How many times does PM10 pass the max hourly limit per year concentration of daily 50 µg/m3
#more than 35 times a year?

Eixample_PM10_day_2014 <- Eixample_PM10_day %>% filter( dt <= "2014-12-31")
Eixample_PM10_day_2015 <- Eixample_PM10_day %>% filter( dt >= "2015-01-01" & dt <= "2015-12-31")
Eixample_PM10_day_2016 <- Eixample_PM10_day %>% filter( dt >= "2016-01-01" & dt <= "2016-12-31")
Eixample_PM10_day_2017 <- Eixample_PM10_day %>% filter( dt >= "2017-01-01" & dt <= "2017-12-31")
Eixample_PM10_day_2018 <- Eixample_PM10_day %>% filter( dt >= "2018-01-01" & dt <= "2018-12-31")

Eixample_PM10_day_2014 %>% summarize(n_cases = sum(Eixample_PM10_day_2014$mean >= 50))
Eixample_PM10_day_2015 %>% summarize(n_cases = sum(Eixample_PM10_day_2015$mean >= 50))
Eixample_PM10_day_2016 %>% summarize(n_cases = sum(Eixample_PM10_day_2016$mean >= 50))
Eixample_PM10_day_2017 %>% summarize(n_cases = sum(Eixample_PM10_day_2017$mean >= 50))
Eixample_PM10_day_2018 %>% summarize(n_cases = sum(Eixample_PM10_day_2018$mean >= 50))


#In 2014 there were 22 cases with PM10 > 50
#In 2015 there were 33 cases with PM10 > 50
#In 2016 there were 16 cases with PM10 > 50
#In 2017 there were 20 cases with PM10 > 50
#In 2018 there were 20 cases with PM10 > 50

#The concentrations of PM10 are complying with the EU standard limits


#7.What is the max day of the week with higher avg NO2, PM10 pollution normally? And the minimum?
Eixample_NO2_day[order(Eixample_NO2_day$max, decreasing = TRUE),]
#Analyze the days


Eixample_PM10_day[order(Eixample_PM10_day$max, decreasing = TRUE),]
#I have analyzed what are the high concentrations for PM10, and these are the conclusions:
#From top 10 values, 5 of them are from 24th June (Sant Joan). Including the highest observation with value 1167 on 2017-06-24.
#Second highest PM10 value since 2014, happend on the 7th June 2015, when Barcelona won its
#5th Champions league against Juventus.
#Third value and fourth value, 2015-10-03 and 2015-10-04, are part of a weekend, not sure why the high values.
#Tenth value on the 2014-11-30, not sure why.

#8.Is there any difference between weekday and weekend?


#9.What is the month with higher average pollution NO2, PM10 during the year? And the minimum?

#10.What is the most polluted station? And the least?

#11.What is the relationship with weather and pollution? What is the component with more correlation?

#Let's load the data from Raval- zoo in Barcelona (EMA = X4). I'm going to use the weather data from
#Raval to compare it with pollution measured in Eixample.

Weather_bcn <- read_csv('/Users/ione/Desktop/Project_AIR/data/Jlerchundi_X4_14-19.csv')
summary(Weather_bcn)
head(Weather_bcn)
tail(Weather_bcn)

#Data column is in format "1/1/2014 1:00", and it's a character, so I'll change it to be same as
#the pollution format.

names(Weather_bcn)[names(Weather_bcn) == "DATA (T.U.)"] <- "dt"
names(Weather_bcn)[names(Weather_bcn) == "DV10"] <- "wd"
names(Weather_bcn)[names(Weather_bcn) == "VV10"] <- "ws"
Weather_bcn$dt <- parse_date_time(Weather_bcn$dt, "dmy HM", truncated = 3)
head(Weather_bcn)
#I am going to assume these data covers main city area of Barcelona, and I am going to compare it with
#data in Eixample.I am going to join the Eixample data with weather in Raval with dt field.

Eixample_NO2_weather <- merge(Eixample_NO2,Weather_bcn,by="dt" )
summary(Eixample_NO2_weather)

Eixample_PM10_weather <- merge(Eixample_PM10,Weather_bcn,by="dt" )
summary(Eixample_PM10_weather)


#I am going to study the correlations between variables. I will first create a correlation matrix:

cormat <- round(cor(Eixample_NO2_weather),2)

#It doesnt work because all features must be numeric.I will also choose variables that are interesting.

sapply(Eixample_NO2_weather, is.numeric)
Eixample_NO2_weather_num_data <- Eixample_NO2_weather[, sapply(Eixample_NO2_weather, is.numeric)]
head(Eixample_NO2_weather_num_data)
Eixample_NO2_weather_cor <- dplyr::select(Eixample_NO2_weather_num_data, -c("station_code", "latitude", "longitude","year","month","day","value"))
head(Eixample_NO2_weather_cor)
sum(is.na(Eixample_NO2_weather_cor))
#I have 87 values that are NA-s. I need to correct them otherwise the cor matrix won't work.
Eixample_NO2_weather_cor_NA <-Eixample_NO2_weather_cor[complete.cases(Eixample_NO2_weather_cor), ]
sum(is.na(Eixample_NO2_weather_cor_NA))
#Now I will calculate the correlation matrix with only numeric values:
cormat_NO2 <- round(cor(Eixample_NO2_weather_cor_NA),2)
head(cormat_NO2)

#Looking at the data, the only variable that have some correlation with the NO2 values
#in Eixample are Wind speed ( with correlation coefficient of -0.31 and 0.32 for max speed), Wind direction
# with coefficient of -0.15, and atmospheric pressure with positive coefficient of 0.2. I am going to plot some graphs to see
# this relationships a bit further.

#I will do the same for PM10 data:
sapply(Eixample_PM10_weather, is.numeric)
Eixample_PM10_weather_num_data <- Eixample_PM10_weather[, sapply(Eixample_PM10_weather, is.numeric)]
head(Eixample_PM10_weather_num_data)
Eixample_PM10_weather_cor <- dplyr::select(Eixample_PM10_weather_num_data, -c("station_code", "latitude", "longitude","year","month","day","value"))
head(Eixample_PM10_weather_cor)
sum(is.na(Eixample_PM10_weather_cor))
#I have 87 values that are NA-s. I need to correct them otherwise the cor matrix won't work.
Eixample_PM10_weather_cor_NA <-Eixample_PM10_weather_cor[complete.cases(Eixample_PM10_weather_cor), ]
sum(is.na(Eixample_PM10_weather_cor_NA))
#Now I will calculate the correlation matrix with only numeric values:
cormat_PM10 <- round(cor(Eixample_PM10_weather_cor_NA),2)
head(cormat_PM10)

#It looks like the correlations between PM10 and weather factors are different. Temperature is
#what is more correlated with PM10 values with 0.19 cor coefficient, and then atmospheric P with 0,10
# Wind direction cor coefficient of -0.12, and wind speed coef -0.08.

#First we are going to reshape the matrix so that we can plot using ggplot. HEATMAP:
melted_cormat_NO2 <- melt(cormat_NO2)
head(melted_cormat_NO2)

melted_cormat_PM10 <- melt(cormat_PM10)
head(melted_cormat_PM10)

ggplot(data = melted_cormat_NO2, aes(x=X1, y=X2, fill=value)) +
  geom_tile()
ggplot(data = melted_cormat_PM10, aes(x=X1, y=X2, fill=value)) +
  geom_tile()


# Correlation Heatmaps

ggplot(data = melted_cormat_NO2, aes(X1, X2, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white",
                       midpoint = 0, limit = c(-1,1), space = "Lab",
                       name="Pearson\nCorrelation") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, vjust = 1,
                                   size = 12, hjust = 1))+
  coord_fixed()


ggplot(data = melted_cormat_PM10, aes(X1, X2, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white",
                       midpoint = 0, limit = c(-1,1), space = "Lab",
                       name="Pearson\nCorrelation") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, vjust = 1,
                                   size = 12, hjust = 1))+
  coord_fixed()

#The heatplot doesnt show what I wanted, so I will now try to plot the variables with more correlation,
# the NO2 pollution with wind speed & direction as well as Atmospheric pressure.

ggscatter(Eixample_NO2_weather_cor_NA, x = "NO2", y = "P",
          add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "pearson", alpha = 0.1,
          xlab = "NO2 (µg/m3)", ylab = "Atmospheric pressure (mbar)", title = "NO2 and P relationship in Eixample")

#In the plot we can see that the NO2 levels are slightly higher with higher P.

#Let'a analyze the relationship between NO2 levels and Wind speed.
ggscatter(Eixample_NO2_weather_cor_NA, x = "NO2", y = "ws",
          add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "pearson", alpha = 0.1,
          xlab = "NO2 (µg/m3)", ylab = "Wind speed (km/h)", title = "NO2 and Wind speed relationship in Eixample")
#This graph tells us that when the wind is intense, the pollution tends to be lower. The lower the wind
#speed, the higher the pollution.
head(Eixample_NO2_weather_cor_NA)
#Let's analyze the effect of the wind direction to NO2 levels.
ggscatter(Eixample_NO2_weather_cor_NA, x = "NO2", y = "wd",
          add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "pearson", alpha = 0.1,
          xlab = "NO2 (µg/m3)", ylab = "Wind direction", title = "NO2 and Wind direction relationship in Eixample")
#This graph doesnt tell us much so we are going to see other type to discover with what wind direction
#we will have more NO2 pollution.
windRose(Eixample_NO2_weather_cor_NA, ws = "ws", wd = "wd")
percentileRose( mydata = Eixample_NO2_weather_cor_NA, wd = "wd", pollutant = "NO2", mean=TRUE, key.footer = "percentile")

#According to this graph, we observe that when the wind is NW direction and higher speed,the pollution is best,
#while when the wind is SE and low speed,the pollution is highest. I understand this looking at the
#geography of the city, where south east is the ocean, and the pollution can scape when the wind
#is NW, but when the wind is SE, the mountains hold the smog on the city.

#We will do the analysis for PM10. For PM10 I have seen the most influencing factor is Temperature.
# I am going to plot a scatter plot to see their relationship.

ggscatter(Eixample_PM10_weather_cor_NA, x = "PM10", y = "T",
          add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "pearson", alpha = 0.1,
          xlab = "PM10 (µg/m3)", ylab = "Temperature", title = "PM10 and T relationship in Eixample") +
          xlim(c(0,200)) #taking out outliers
#When we have eliminated the outliers ( PM10>200), the correlation with Temperature is a bit higher (0,23)

ggscatter(Eixample_PM10_weather_cor_NA, x = "PM10", y = "P",
          add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "pearson", alpha = 0.1,
          xlab = "PM10 (µg/m3)", ylab = "Temperature", title = "PM10 and P relationship in Eixample") +
  xlim(c(0,200)) #taking out outliers
#When we have eliminated the outliers ( PM10>200), the correlation coef is 0.15 with Atmospheric Pressure.
ggscatter(Eixample_PM10_weather_cor_NA, x = "PM10", y = "P",
          add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "pearson", alpha = 0.1,
          xlab = "PM10 (µg/m3)", ylab = "Temperature", title = "PM10 and P relationship in Eixample") +
  xlim(c(0,200)) #taking out outliers
#When we have eliminated the outliers ( PM10>200), the correlation coef is 0.15 with Atmospheric Pressure.

#We are going to analyse the effect of the wind in PM10:

windRose(Eixample_PM10_weather_cor_NA, ws = "ws", wd = "wd")
percentileRose( mydata = Eixample_PM10_weather_cor_NA, wd = "wd", pollutant = "PM10", mean=TRUE, key.footer = "percentile")

#Very similar effect of the wind direction with NO2 and PM10. Pollution increases with South East wind direction,
#and decreases with NW direction.

#12.Are hospitalizations with respiratory issues affected by peaks of pollution?

health_resp <- read_csv('/Users/ione/Desktop/Project_AIR/data/Respiratory_2014-2017.csv',, locale = locale(encoding = "latin1"))
summary(health_resp)
head(health_resp)

names(health_resp)[names(health_resp) == "dia"] <- "day"
names(health_resp)[names(health_resp) == "mes"] <- "month"
names(health_resp)[names(health_resp) == "any"] <- "year"
names(health_resp)[names(health_resp) == "Diagnòstic Principal"] <- "Diagnosis"
names(health_resp)[names(health_resp) == "Contactes d'hospitalització d'aguts (altes AH)"] <- "Hospitalizations"

head(health_resp)
str(health_resp)


#Month names are strings in catalan and the system doesn't understand them when parsing into date format.
#I will translate the month names and transform into date format:

health_resp$month[health_resp$month == "Gener"] <- "January"
health_resp$month[health_resp$month == "Febrer"] <- "February"
health_resp$month[health_resp$month == "Març"] <- "March"
health_resp$month[health_resp$month == "Abril"] <- "April"
health_resp$month[health_resp$month == "Maig"] <- "May"
health_resp$month[health_resp$month == "Juny"] <- "June"
health_resp$month[health_resp$month == "Juliol"] <- "July"
health_resp$month[health_resp$month == "Agost"] <- "August"
health_resp$month[health_resp$month == "Setembre"] <- "September"
health_resp$month[health_resp$month == "Octubre"] <- "October"
health_resp$month[health_resp$month == "Novembre"] <- "November"
health_resp$month[health_resp$month == "Desembre"] <- "December"
View(health_resp)

health_resp$dt <- paste(health_resp$year, health_resp$month, health_resp$day, sep="-") %>% ymd() %>% as.Date()
head(health_resp)

#I am going to compare these data with NO2 daily average values to see if there is any correlations.
#First I will do the analysis for respiratory issues and then same for heart issues.
#Also, I will subset the health data to have only 2014 to 2018 data and convert the dt of NO2 and PM10 to Date format.

Eixample_NO2_day$dt <- as.Date(Eixample_NO2_day$dt)
Eixample_PM10_day$dt <- as.Date(Eixample_PM10_day$dt)

health_resp_2014 <- health_resp %>% filter(year >=2014, year <= 2018)
head(health_resp_2014)
tail(health_resp_2014)
str(health_resp_2014)

#We only have health data from 2014 to 2018 so we will limit the pollution data accordingly.
Eixample_NO2_day <- Eixample_NO2_day %>% filter ( dt <= "2017-12-31")
tail(Eixample_NO2_day)
Eixample_PM10_day <- Eixample_PM10_day %>% filter ( dt <= "2017-12-31")
tail(Eixample_PM10_day)

#I am going to perform the join of health and NO2 data:
Eixample_NO2_resp <- merge(Eixample_NO2_day,health_resp_2014,by="dt" )
head(Eixample_NO2_resp)

#Similarly I will join PM10 and health data with respiratory issues:
Eixample_PM10_resp <- merge(Eixample_PM10_day,health_resp_2014,by="dt" )
head(Eixample_PM10_resp)

#I am going to transform the data for the correlation plot.I need to aggregate the data
#by dt,but I need different aggregation types for each column, avg for NO2/PM10 and sum for hospitalizations:

df_NO2 <- data.table(Eixample_NO2_resp)
df.NO2_resp <- df[, list(NO2=mean(mean), Hospitalizations_resp=sum(Hospitalizations)),
             by=dt]
df.NO2_resp

df_PM10 <- data.table(Eixample_PM10_resp)
df_PM10_resp <- df_PM10[, list(PM10=mean(mean), Hospitalizations_resp=sum(Hospitalizations)),
             by=dt]
df_PM10_resp


#I am going to do a normality test for pearson correlation tests:

# Shapiro-Wilk normality test for NO2
shapiro.test(df.NO2_resp$NO2) # => W = 0.97615, p-value = 7.869e-15

# Shapiro-Wilk normality test for Hospitalizations
shapiro.test(df.NO2_resp$Hospitalizations_resp) # => W = 0.98142, p-value = 8.661e-13

#Now I will perform a Pearson correlation test:

res <- cor.test(df.NO2_resp$NO2, df.NO2_resp$Hospitalizations_resp,
                method = "pearson")
res

#Correlation coef is 0.5

# Shapiro-Wilk normality test for PM10:

shapiro.test(df_PM10_resp$PM10) # => W = 0.71939, p-value < 2.2e-16

#Now I will perform a Pearson correlation test:

res2 <- cor.test(df_PM10_resp$PM10, df_PM10_resp$Hospitalizations_resp,
                method = "pearson")
res2

#Correlation coeficcient is 0.025, which is extremely low. My hypothesis is that the
#outliers are corrupting the result.I am going to leave the outliers out for the correlation analysis.

#I will try to calculate a robust covariance matrix between PM10 and respiration issues, and
#compare it with a classic covariance matrix:


cov_PM10_resp_classic <- covClassic(cbind(df_PM10_resp$PM10,df_PM10_resp$Hospitalizations_resp), corr = TRUE)
cov_PM10_resp_classic #Estimate of correlation of 0.02524
cov_PM10_resp_rob <- covRob(cbind(df_PM10_resp$PM10,df_PM10_resp$Hospitalizations_resp), corr = TRUE)
cov_PM10_resp_rob #Estimate of correlation of 0.09628
plot(cov_PM10_resp_classic)
plot(cov_PM10_resp_rob)

#Therefore the relationship between PM10 and hospitalizations for respiratory issues is not too correlated.


#Regarding NO2 and hospitalizations caused for respiratory issues:
ggscatter(df.NO2_resp, x = "NO2", y = "Hospitalizations_resp",
          add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "pearson", alpha = 0.1,
          xlab = "NO2 (µg/m3)", ylab = "Hospitalizations", title = "NO2 and hospitalizations respiratory issues in Eixample")


#It looks like there is a positive correlation between NO2 levels and hospitalizations.
#If we plot the values with the time:
ggplot(df.NO2_resp, aes(x =dt)) +
  geom_line(aes(y = NO2, colour = "NO2")) +
  coord_cartesian(xlim=c(as.Date("2014-01-01"),as.Date("2014-01-16"))) +
  geom_line(aes(y = Hospitalizations_resp, colour = "Hospitalizations")) +
  labs( x = "Time", y = "Hospitalizations", title = "NO2(µg/m3) - Respiratory issues in Eixample - week") +
  scale_x_date(date_breaks = "1 day", date_labels = "%a")

ggplot(df.NO2_resp, aes(x =dt)) +
      geom_line(aes(y = NO2, colour = "NO2")) +
      coord_cartesian(xlim=c(as.Date("2014-01-01"),as.Date("2014-01-31"))) +
      geom_line(aes(y = Hospitalizations_resp, colour = "Hospitalizations")) +
      labs( x = "Time", y = "Hospitalizations", title = "NO2(µg/m3) - Respiratory issues in Eixample - month") +
      scale_x_date(date_breaks = "1 week", date_minor_breaks = "1 day", date_labels = "%b %d")

ggplot(df.NO2_resp, aes(x =dt)) +
  geom_line(aes(y = NO2, colour = "NO2")) +
  coord_cartesian(xlim=c(as.Date("2014-01-01"),as.Date("2014-12-31"))) +
  geom_line(aes(y = Hospitalizations_resp, colour = "Hospitalizations")) +
  labs( x = "Time", y = "Hospitalizations", title = "NO2(µg/m3) - Respiratory issues in Eixample - year") +
  scale_x_date(date_breaks = "1 month", date_minor_breaks = "1 week", date_labels = "%b")


#So we conclude that there is some positive correlation between respiratory issues and
#NO2

#13.Are hospitalizations with heart issues affected by pollution?
#I am going to do the same analysis fpr heart issues:
health_heart <- read_csv('/Users/ione/Desktop/Project_AIR/data/Heart_2014-2017.csv', locale = locale(encoding = "latin1"))
summary(health_heart)
head(health_heart)

names(health_heart)[names(health_heart) == "dia"] <- "day"
names(health_heart)[names(health_heart) == "mes"] <- "month"
names(health_heart)[names(health_heart) == "any"] <- "year"
names(health_heart)[names(health_heart) == "Diagnòstic Principal"] <- "Diagnosis"
names(health_heart)[names(health_heart) == "Contactes d'hospitalització d'aguts (altes AH)"] <- "Hospitalizations"

head(health_heart)
str(health_heart)

#We are now going to transform the data for the correlation plot.We need to aggregate the data
#by dt so we have 1 value of pollution observation by 1 value of hospitalizations per day.
#Because we need different aggregation types for each column, avg for NO2 and sum for hospitalizations:


#Month names are strings in catalan and the system doesn't understand them when parsing into date format.
#I will translate the month names and transform into date format:

health_heart$month[health_heart$month == "Gener"] <- "January"
health_heart$month[health_heart$month == "Febrer"] <- "February"
health_heart$month[health_heart$month == "Març"] <- "March"
health_heart$month[health_heart$month == "Abril"] <- "April"
health_heart$month[health_heart$month == "Maig"] <- "May"
health_heart$month[health_heart$month == "Juny"] <- "June"
health_heart$month[health_heart$month == "Juliol"] <- "July"
health_heart$month[health_heart$month == "Agost"] <- "August"
health_heart$month[health_heart$month == "Setembre"] <- "September"
health_heart$month[health_heart$month == "Octubre"] <- "October"
health_heart$month[health_heart$month == "Novembre"] <- "November"
health_heart$month[health_heart$month == "Desembre"] <- "December"
View(health_heart)

health_heart$dt <- paste(health_heart$year, health_heart$month, health_heart$day, sep="-") %>% ymd() %>% as.Date()
head(health_heart)

Eixample_NO2_heart <- merge(Eixample_NO2_day,health_heart,by="dt" )
head(Eixample_NO2_heart)

#We are now going to transform the data for the correlation plot.We need to aggregate the data
#by dt so we have 1 value of pollution observation by 1 value of hospitalizations per day.
#Because we need different aggregation types for each column, avg for NO2 and sum for hospitalizations:

df2 <- data.table(Eixample_NO2_heart)
df2.out <- df[, list(NO2=mean(mean), Hospitalizations_heart=sum(Hospitalizations)),
             by=dt]
df2.out


#Now I will perform a Pearson correlation test between NO2 and hospitalizations because heart issues:

res <- cor.test(df2.out$NO2, df2.out$Hospitalizations_heart,
                method = "pearson")
res

ggplot(df2.out, aes(x =NO2 , y = Hospitalizations_heart)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", color = "grey", alpha = 0.2) +
  labs( x = "NO2(µg/m3)", y = "Hospitalizations", title = "NO2(µg/m3) - Cardiac issues in Eixample")

ggscatter(df2.out, x = "NO2", y = "Hospitalizations_heart",
          add = "reg.line",
          conf.int = TRUE,
          add.params = list(color = "blue",
                            fill = "lightgray") ) +
  stat_cor(method = "pearson", label.x = 3, label.y = 65)  # Add correlation coefficient

#If we plot the values with the time:
ggplot(df2.out, aes(x =dt)) +
  geom_line(aes(y = NO2, colour = "NO2")) +
  coord_cartesian(xlim=c(as.Date("2014-01-01"),as.Date("2014-01-16"))) +
  geom_line(aes(y = Hospitalizations_heart, colour = "Hospitalizations")) +
  labs( x = "Time", y = "Hospitalizations", title = "NO2(µg/m3) - Cardiac issues in Eixample - week") +
  scale_x_date(date_breaks = "1 day", date_labels = "%a")

ggplot(df2.out, aes(x =dt)) +
  geom_line(aes(y = NO2, colour = "NO2")) +
  coord_cartesian(xlim=c(as.Date("2014-01-01"),as.Date("2014-01-31"))) +
  geom_line(aes(y = Hospitalizations_heart, colour = "Hospitalizations")) +
  labs( x = "Time", y = "Hospitalizations", title = "NO2(µg/m3) - Cardiac issues in Eixample - month") +
  scale_x_date(date_breaks = "1 week", date_minor_breaks = "1 day", date_labels = "%b %d")

ggplot(df2.out, aes(x =dt)) +
  geom_line(aes(y = NO2, colour = "NO2")) +
  coord_cartesian(xlim=c(as.Date("2014-01-01"),as.Date("2014-12-31"))) +
  geom_line(aes(y = Hospitalizations_heart, colour = "Hospitalizations")) +
  labs( x = "Time", y = "Hospitalizations", title = "NO2(µg/m3) - Cardiac issues in Eixample - year") +
  scale_x_date(date_breaks = "1 month", date_minor_breaks = "1 week", date_labels = "%b")



#14.Is MWC affecting to pollution?
#15.How are public transport strikes affecting to pollution?
#16.How are taxi strikes affecting to pollution?

#17.Is a BCN football match affecting to pollution?

#18.How is air traffic influencing pollution?
#19.How is port activity influencing pollution?


