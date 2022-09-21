library(tidyverse)
library(janitor)
library(lubridate)
library(scales)
# Data Source:
# https://divvy-tripdata.s3.amazonaws.com/index.html
rm(list = ls())
dir("D:/_Data_",full.names = T)
#Loading Dataframes
df1=read.csv("D:/_Data_/202004-divvy-tripdata.csv")
df2=read.csv("D:/_Data_/202005-divvy-tripdata.csv")
df3=read.csv("D:/_Data_/202006-divvy-tripdata.csv")
df4=read.csv("D:/_Data_/202007-divvy-tripdata.csv")
df5=read.csv("D:/_Data_/202008-divvy-tripdata.csv")
df6=read.csv("D:/_Data_/202009-divvy-tripdata.csv")
df7=read.csv("D:/_Data_/202010-divvy-tripdata.csv")
df8=read.csv("D:/_Data_/202011-divvy-tripdata.csv")
df9=read.csv("D:/_Data_/202012-divvy-tripdata.csv")
df10= read.csv("D:/_Data_/202101-divvy-tripdata.csv")
df11= read.csv("D:/_Data_/202102-divvy-tripdata.csv")
df12= read.csv("D:/_Data_/202103-divvy-tripdata.csv")
#Combining Dataframes, Removing empty rows and columns
bikeriders = rbind(df1,df2,df3,df4,df5,df6,df7,df8,df9,df10,df11,df12)
#Compare numbers before and after janitor to see how many rows were eliminated
dim(bikeriders)
bikeriders = janitor::remove_empty(bikeriders,which = c("cols"))
bikeriders = janitor::remove_empty(bikeriders,which = c("rows"))
dim(bikeriders)
#Converting date and timestamps
bikeriders$started_at = lubridate::ymd_hms(bikeriders$started_at)
bikeriders$ended_at = lubridate::ymd_hms(bikeriders$ended_at)
#Creating hour field
bikeriders$start_hour = lubridate::hour(bikeriders$started_at)
bikeriders$end_hour = lubridate::hour(bikeriders$ended_at)
#Creating date field
bikeriders$start_date = as.Date(bikeriders$started_at)
bikeriders$end_date = as.Date(bikeriders$ended_at)
#Creating days passed column
bikeriders$days_passed <-difftime(bikeriders$end_date,bikeriders$start_date,
                                  units=c("days"))
#Creating a ride time(hours) column
bikeriders$ride_duration = bikeriders$end_hour-bikeriders$start_hour+
  as.numeric(difftime(bikeriders$end_date,bikeriders$start_date), units="hours")

bikeriders %>% count(start_hour, sort = T) %>% 
  ggplot() + geom_line(aes(x = start_hour,y=n))+
  scale_y_continuous(labels = comma) + labs(title = "Ride Count vs. Start Hour",
                                            x="Start Hour",y="Ride Count")

