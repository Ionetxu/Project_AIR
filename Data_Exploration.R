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

# Data Exploration

#We are going to try to understand more the data and get more insights. There are multiple
#questions I have about the data and I am going to try find the answers by using some additional datasets
#like event calendar dates, weather and health data.

#I will try to formulate questions or hypotesis and find answers.
#1.What is the trend of the NO2 and PM10 pollutants in Eixample in the last 10 years? And in Gracia?
#2. What is the trend of the NO2 and PM10 pollutants in Barcelona in the last 10 years?
#3. What is the average yearly amount of NO2 in each station in Barcelona by year from 2014 to 2018? Does it pass the limit in any station?
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
#15.How are public transpory strikes affecting to pollution?
#16.How are taxi strikes affecting to pollution?
#17.Is a BCN football match affecting to pollution?
#18.How is port activity influencing pollution?
#19.How is air traffic influencing pollution?
#20.How is bicing influencing pollution?


