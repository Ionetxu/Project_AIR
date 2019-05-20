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

names(Eixample_NO2)[names(Eixample_NO2) == "imp_2014_2018_NO2_Eixample_intp"] <- "value_intp"
summary(Eixample_NO2)

#I am going to do the following analysis for NO2 in Eixample by using xts objects from zoo package.
#Daily average - to see weekly seasonality
#Monthly average --> yearly seasonality
#Yearly average --> trend


#Let's calculate the min, max and mean of each day:

stat_fun <- function(x) c(min = min(x), max = max(x), mean = mean(x))
Eixample_NO2_day <- Eixample_NO2 %>%
  tq_transmute(select     = value_intp,
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
  tq_transmute(select     = value_intp,
               mutate_fun = apply.monthly,
               FUN        = stat_fun)
summary(Eixample_NO2_month)
Eixample_NO2_month_xts <- as.xts((Eixample_NO2_month[,-1]), order.by = as.Date(Eixample_NO2_month$dt))
summary(Eixample_NO2_month_xts)
autoplot(Eixample_NO2_month_xts$mean)

ggplot( aes(x = as.Date(dt), y = mean), data =Eixample_NO2_month ) +
  geom_point(alpha = 0.5) +
  labs( x = "Time", y = "NO2 (µg/m3)", title = "NO2(µg/m3) - Eixample NO2 monthly avg")+
  geom_smooth(color = "grey", alpha = 0.2) +
  scale_x_date(breaks='6 months')
#I don't see a clear seasonality and the trend seems slightly positive, but not very clear trend from
#2014 to 2019. Therefore the pollutant NO2 levels have not improved significantly since 2014,
#since applying the strategic plan to fight pollution.


#2. What is the trend of the NO2 and PM10 pollutants in Barcelona overall in the last 10 years?
#See plots from before, they are slightly negative trend.
#3. What is the average yearly amount of NO2 in each station in Barcelona by year from 2014 to 2018? Does it pass the limit in any station?

Eixample_NO2_year <- Eixample_NO2 %>%
  tq_transmute(select     = value_intp,
               mutate_fun = apply.yearly,
               FUN        = stat_fun)
summary(Eixample_NO2_year)
#We see that all values are over 40 µg/m3 which is the yearly average limit value recommended
#by WHO and is followed by European Union recommendations.
View(Eixample_NO2_year)
ggplot(Eixample_NO2_year[-6,],aes(x=as.Date(dt),y=mean)) +
  geom_line(alpha = 0.5) +
  hline(inter)


#4.What is the average yearly amount of PM10 in each station in Barcelona by year from 2014 to 2018? Does it pass the limit in any station?
#5.How many times does NO2 pass the max hourly limit per year and per station?
#6.How many times does PM10 pass the max hourly limit per year and per station?
#7.What is the max day of the week with higher avg NO2, PM10 pollution normally? And the minimum?
#8.Is there any difference between weekday and weekend?
#9.What is the month with higher average pollution NO2, PM10 during the year? And the minimum?
#10.What is the most polluted station? And the least?

#11.What is the relationship with weather and pollution? What is the component with more correlation?
#12.What are the conditions with bad pollution in BCN? And conditions for good pollution?

#13.Are hospitalizations with respiratory issues affected by peaks of pollution?
#14.Are hospitalizations with heart issues affected by peaks of pollution?

#15.How are public transport strikes affecting to pollution?
#16.How are taxi strikes affecting to pollution?

#17.Is a BCN football match affecting to pollution?
#18.How is port activity influencing pollution?

#19.How is air traffic influencing pollution?

#20.How is bicing influencing pollution?


