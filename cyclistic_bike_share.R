require(readxl)
require(tidyverse)
library(ggplot2)
library(tidyverse)
library(janitor)
library(lubridate)
library(scales)
library(dplyr)
rm(list = ls())
# Data Source:
# https://divvy-tripdata.s3.amazonaws.com/index.html
# Source:
# https://rpubs.com/LMunyan/363306
setwd("D:/_Data_")

# create a list of the files from your target directory
file_list <- list.files(path="D:/_Data_")
 
# initiate a blank data frame, each iteration of the loop will append the data 
# from the given file to this variable
bikeriders <- data.frame()

#had to specify columns to get rid of the total column
for (i in 1:length(file_list)){
  temp_data <- read.csv(file_list[i]) #each file will be read in, specify which 
# columns you need read in to avoid any errors
  bikeriders <- rbind(bikeriders, temp_data) #for each iteration, bind the new 
# data to the building dataset
}


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

bikeriders %>% 
  group_by(ride_duration) %>%
  count %>%
  filter(n > 1000) %>%
  ggplot(aes(x = factor(ride_duration), y = n)) + 
  geom_bar(stat = "identity") + labs(x = "Ride Hours",y = "Ride Count",
                                     title = "Ride Duration Count in Hours")


