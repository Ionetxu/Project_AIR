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

#I am going to do the following analysis for NO2 in Eixample by using xts objects from zoo package.
#Daily average - to see weekly seasonality
#Monthly average --> yearly seasonality
#Yearly average --> trend


#Let's calculate the min, max and mean of each day:

stat_fun <- function(x) c(min = min(x), max = max(x), mean = mean(x))
Eixample_NO2_day <- Eixample_NO2 %>%
  tq_transmute(select     = NO2,
               mutate_fun = apply.daily,
               FUN        = stat_fun)
summary(Eixample_NO2_day)

# Use as.xts to generate an xts object of average daily NO2 data:
Eixample_NO2_day_xts <- as.xts((Eixample_NO2_day[,-1]), order.by = as.Date(Eixample_NO2_day$dt))
head(Eixample_NO2_day_xts)

#Let's plot a subset of the data weekly:
plot.zoo(Eixample_NO2_day_xts["2018-01"], plot.type = "single", xy.labels = c("Time", "NO2_daily_values"))
ggplot(Eixample_NO2_day, aes(x = dt, y = mean)) +
  geom_line(alpha = 0.5) +
  geom_smooth(color = "grey", alpha = 0.2) +
  scale_x_datetime(breaks='1 year') +
  labs( x = "Time", y = "NO2 (µg/m3)", title = "NO2 (µg/m3) daily- Eixample")

autoplot(Eixample_NO2_day_xts["2018-01"], facets = FALSE)
#Not really very representative to see if there is a weekly seasonality. Let's analyze monthly data.

Eixample_NO2_month <- Eixample_NO2 %>%
  tq_transmute(select     = NO2,
               mutate_fun = apply.monthly,
               FUN        = stat_fun)
summary(Eixample_NO2_month)
Eixample_NO2_month_xts <- as.xts((Eixample_NO2_month[,-1]), order.by = as.Date(Eixample_NO2_month$dt))
summary(Eixample_NO2_month_xts)
autoplot(Eixample_NO2_month_xts$mean)

ggplot( aes(x = as.Date(dt), y = mean), data =Eixample_NO2_month ) +
  geom_line(alpha = 0.5) +
  labs( x = "Time", y = "NO2 (µg/m3)", title = "NO2(µg/m3) - Eixample NO2 monthly avg")+
  geom_smooth(color = "grey", alpha = 0.2) +
  scale_x_date(breaks='6 months')
#I don't see a clear seasonality and the trend seems slightly positive, but not very clear trend from
#2014 to 2019. Therefore the pollutant NO2 levels have not improved significantly since 2014,
#since applying the strategic plan to fight pollution.


#2. What is the trend of the NO2 and PM10 pollutants in Barcelona overall in the last 10 years?
#See plots from initial analysis - no need for this
#3. What is the average yearly amount of NO2 in each station in Barcelona by year from 2014 to 2018? Does it pass the limit in any station?

Eixample_NO2_year <- Eixample_NO2 %>%
  tq_transmute(select     = NO2,
               mutate_fun = apply.yearly,
               FUN        = stat_fun)
summary(Eixample_NO2_year)
#We see that all values are over 40 µg/m3 which is the yearly average limit value recommended
#by WHO and is followed by European Union recommendations.
View(Eixample_NO2_year)
ggplot(Eixample_NO2_year[-6,],aes(x=as.Date(dt),y=mean)) +
  geom_line(alpha = 0.5)

#4.What is the average yearly amount of PM10 in each station in Barcelona by year from 2014 to 2018? Does it pass the limit in any station?
#5.How many times does NO2 pass the max hourly limit per year and per station? 0
#6.How many times does PM10 pass the max hourly limit per year and per station?
#7.What is the max day of the week with higher avg NO2, PM10 pollution normally? And the minimum?
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
cormat <- round(cor(Eixample_NO2_weather_cor_NA),2)
head(cormat)

#Looking at the data, the only variable that have some correlation with the NO2 values
#in Eixample are Wind speed ( with correlation coefficient of -0.31 and 0.32 for max speed), Wind direction
# with coefficient of -0.15, and atmospheric pressure with positive coefficient of 0.2. I am going to plot some graphs to see
# this relationships a bit further.

#First we are going to reshape the matrix so that we can plot using ggplot. HEATMAP:
melted_cormat <- melt(cormat)
head(melted_cormat)

ggplot(data = melted_cormat, aes(x=X1, y=X2, fill=value)) +
  geom_tile()

#Let's improve the heatmap a bit more:
# Get lower triangle of the correlation matrix
get_lower_tri<-function(cormat){
  cormat[upper.tri(cormat)] <- NA
  return(cormat)
}
# Get upper triangle of the correlation matrix
get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}
reorder_cormat <- function(cormat){
  # Use correlation between variables as distance
  dd <- as.dist((1-cormat)/2)
  hc <- hclust(dd)
  cormat <-cormat[hc$order, hc$order]
}
cormat <- reorder_cormat(cormat)
upper_tri <- get_upper_tri(cormat)
upper_tri


melted_cormat <- melt(upper_tri, na.rm = TRUE)

# Heatmap

ggplot(data = melted_cormat, aes(X1, X2, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white",
                       midpoint = 0, limit = c(-1,1), space = "Lab",
                       name="Pearson\nCorrelation") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, vjust = 1,
                                   size = 12, hjust = 1))+
  coord_fixed()

head(Eixample_NO2_weather_cor_NA)
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
#Also, I will subset the health data to have only 2014 to 2018 data and convert the dt of NO2 to Date format.

Eixample_NO2_day$dt <- as.Date(Eixample_NO2_day$dt)
head(Eixample_NO2_day)

health_resp_2014 <- health_resp %>% filter(year >=2014, year <= 2018)
head(health_resp_2014)
tail(health_resp_2014)
str(health_resp_2014)

#We only have health data from 2014 to 2018 so we will limit the pollution data accordingly.
Eixample_NO2_day <- Eixample_NO2_day %>% filter ( dt <= "2017-12-31")
tail(Eixample_NO2_day)

#I am going to perform the join of health and NO2 data:

Eixample_NO2_health <- merge(Eixample_NO2_day,health_resp_2014,by="dt" )
head(Eixample_NO2_health)

#We are now going to transform the data for the correlation plot.We need to aggregate the data
#by dt so we have 1 value of pollution observation by 1 value of hospitalizations per day.
#Because we need different aggregation types for each column, avg for NO2 and sum for hospitalizations:

df <- data.table(Eixample_NO2_health)
df.out <- df[, list(NO2=mean(mean), Hospitalizations_resp=sum(Hospitalizations)),
             by=dt]
df.out

View(Eixample_NO2_health)

#It seems the data is correct, so we are going to plot them.

ggplot(df.out, aes(x =NO2 , y = Hospitalizations_resp)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", color = "grey", alpha = 0.2) +
  labs( x = "NO2(µg/m3)", y = "Hospitalizations", title = "NO2(µg/m3) - Respiratory issues in Eixample")

#It looks like there is a positive correlation between NO2 levels and hospitalizations.
#13.Are hospitalizations with heart issues affected by pollution?

#15.How are public transport strikes affecting to pollution?
#16.How are taxi strikes affecting to pollution?

#17.Is a BCN football match affecting to pollution?
#18.How is port activity influencing pollution?

#19.How is air traffic influencing pollution?

#20.How is bicing influencing pollution?


