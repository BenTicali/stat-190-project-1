rm(list = ls())

library(ggplot2)
library(lubridate)
library(tidyverse)
library(data.table)
library(dplyr)
library(pROC)
library(RColorBrewer)
library(reshape2)
library(randomForest)
library(rpart)
library(rpart.plot)

# function to read in sensor data

read_sensor_data = function(folder, start_index) {
  
  files = list.files(folder)
  
  df = fread(paste0(folder, "/", files[start_index]), header = FALSE)
  
  for (x in (start_index+1):length(files)) {
    
    dt = paste0(folder, "/", files[x])
    
    new = fread(dt, header=FALSE)
    
    df = rbind(df, new)
  }
  
  return(df)
}


##  Active Power Cleaning ----

### I manually split up the Active Power raw data into 4 separate folders 
###   to make reading in the data more achievable
active_power1 = read_sensor_data("Class Data/Active Power1", 2)
colnames(active_power1) = c("Turbine_id", "Time_Stamp", "Date", 'Measurement', 'Type')
active_power1$Time_Stamp = ymd_hms(active_power1$Time_Stamp)
active_power1$interval = round_date(active_power1$Time_Stamp, unit = "10 minutes")
active_power1 <- active_power1 %>% group_by(Turbine_id, interval) %>% summarize(power = mean(Measurement))
fwrite(active_power1, file = 'C:/Users/owenm/Downloads/STAT 190/Class Data/Active_Power1.csv')



active_power2 = read_sensor_data("Class Data/Active Power2", 1)
colnames(active_power2) = c("Turbine_id", "Time_Stamp", "Date", 'Measurement', 'Type')
active_power2$Time_Stamp = ymd_hms(active_power2$Time_Stamp)
active_power2$interval = round_date(active_power2$Time_Stamp, unit = "10 minutes")
active_power2 <- active_power2 %>% group_by(Turbine_id, interval) %>% summarize(power = mean(Measurement))
fwrite(active_power2, file = 'C:/Users/owenm/Downloads/STAT 190/Class Data/Active_Power2.csv')



active_power3 = read_sensor_data("Class Data/Active Power3", 1)
colnames(active_power3) = c("Turbine_id", "Time_Stamp", "Date", 'Measurement', 'Type')
active_power3$Time_Stamp = ymd_hms(active_power3$Time_Stamp)
active_power3$interval = round_date(active_power3$Time_Stamp, unit = "10 minutes")
active_power3 <- active_power3 %>% group_by(Turbine_id, interval) %>% summarize(power = mean(Measurement))
fwrite(active_power3, file = 'C:/Users/owenm/Downloads/STAT 190/Class Data/Active_Power3.csv')



active_power4 = read_sensor_data("Class Data/Active Power4", 1)
colnames(active_power4) = c("Turbine_id", "Time_Stamp", "Date", 'Measurement', 'Type')
active_power4$Time_Stamp = ymd_hms(active_power4$Time_Stamp)
active_power4$interval = round_date(active_power4$Time_Stamp, unit = "10 minutes")
active_power4 <- active_power4 %>% group_by(Turbine_id, interval) %>% summarize(power = mean(Measurement))
fwrite(active_power4, file = 'C:/Users/owenm/Downloads/STAT 190/Class Data/Active_Power4.csv')


## I ran the above sections at different times, and then read all minimized data frames back in

active_power1 <- read.csv("Class Data/Active Power/Active Power All/Active_Power1.csv")
active_power2 <- read.csv("Class Data/Active Power/Active Power All/Active_Power2.csv")
active_power3 <- read.csv("Class Data/Active Power/Active Power All/Active_Power3.csv")
active_power4 <- read.csv("Class Data/Active Power/Active Power All/Active_Power4.csv")

active_power1$interval = ymd_hms(active_power1$interval)
active_power2$interval = ymd_hms(active_power2$interval)
active_power3$interval = ymd_hms(active_power3$interval)
active_power4$interval = ymd_hms(active_power4$interval)



active_power_merged = rbind(active_power1, active_power2, active_power3, active_power4)
active_power_merged <- active_power_merged %>% group_by(Turbine_id, interval) %>% summarize(power = mean(power))


fwrite(active_power_merged, file = 'C:/Users/owenm/Downloads/STAT 190/Class Data/Active_Power_New.csv')



## Other data cleaning ----


windspeed = read_sensor_data("Class Data/Windspeed", 2)
colnames(windspeed) = c("Turbine_id", "Time_Stamp", "Date", 'Measurement', "Type")
windspeed$Time_Stamp = ymd_hms(windspeed$Time_Stamp)
windspeed$interval = round_date(windspeed$Time_Stamp, unit = "10 minutes")
windspeed <- windspeed %>% group_by(Turbine_id, interval) %>% summarize(wind_speed = mean(Measurement))
fwrite(windspeed, file = 'C:/Users/owenm/Downloads/STAT 190/Class Data/Windspeed.csv')


list_df = list(active_power, windspeed)
aggregated <- list_df %>% reduce(full_join, by = c("Turbine_id", "interval"))

fwrite(aggregated, file = 'C:/Users/owenm/Downloads/STAT 190/Class Data/Windspeed_ActivePower.csv')



gearbox_oil_temp = read_sensor_data("Class Data/Gearbox Oil Temperature", 2)
colnames(gearbox_oil_temp) = c("Turbine_id", "Time_Stamp", "Date", 'Measurement', 'Type')
gearbox_oil_temp$Time_Stamp = ymd_hms(gearbox_oil_temp$Time_Stamp)
gearbox_oil_temp$interval = round_date(gearbox_oil_temp$Time_Stamp, unit = "10 minutes")
gearbox_oil_temp <- gearbox_oil_temp %>% group_by(Turbine_id, interval) %>% summarize(gearbox_oil_temperature = mean(Measurement))
fwrite(gearbox_oil_temp, file = 'C:/Users/owenm/Downloads/STAT 190/Class Data/Gearbox_Oil_Temp.csv')



## combining the active power/windspeed data frame to include the gearbox oil temp data
list_df = list(aggregated, gearbox_oil_temp)
new_aggregated <- list_df %>% reduce(full_join, by = c("Turbine_id", "interval"))
fwrite(new_aggregated, file = 'C:/Users/owenm/Downloads/STAT 190/Class Data/GearboxOilTemp_Power_Windspeed.csv')



generator_rpm = read_sensor_data("Class Data/Generator RPM", 2)
colnames(generator_rpm) = c("Turbine_id", "Time_Stamp", "Date", 'Measurement', 'Type')
generator_rpm$Time_Stamp = ymd_hms(generator_rpm$Time_Stamp)
generator_rpm$interval = round_date(generator_rpm$Time_Stamp, unit = "10 minutes")
generator_rpm <- generator_rpm %>% group_by(Turbine_id, interval) %>% summarize(generator_rpm_ = mean(Measurement))

fwrite(generator_rpm, file = 'C:/Users/owenm/Downloads/STAT 190/Class Data/Generator_RPM.csv')



hydraulic_pressure = read_sensor_data("Class Data/Hydraulic Pressure", 2)
colnames(hydraulic_pressure) = c("Turbine_id", "Time_Stamp", "Date", 'Measurement', 'Type')
hydraulic_pressure$Time_Stamp = ymd_hms(hydraulic_pressure$Time_Stamp)
hydraulic_pressure$interval = round_date(hydraulic_pressure$Time_Stamp, unit = "10 minutes")
hydraulic_pressure <- hydraulic_pressure %>% group_by(Turbine_id, interval) %>% summarize(hydraulic_pressure_ = mean(Measurement))

fwrite(hydraulic_pressure, file = 'C:/Users/owenm/Downloads/STAT 190/Class Data/Hydraulic_Pressure.csv')




gearbox_hs_bearing_temp_1 = read_sensor_data("Class Data/Gearbox HS Bearing Temp Part 1", 2)
colnames(gearbox_hs_bearing_temp_1) = c("Turbine_id", "Time_Stamp", "Date", 'Measurement', 'Type')
gearbox_hs_bearing_temp_1$Time_Stamp = ymd_hms(gearbox_hs_bearing_temp_1$Time_Stamp)
gearbox_hs_bearing_temp_1$interval = round_date(gearbox_hs_bearing_temp_1$Time_Stamp, unit = "10 minutes")
gearbox_hs_bearing_temp_1 <- gearbox_hs_bearing_temp_1 %>% group_by(Turbine_id, interval) %>% summarize(gearbox_hs_bearing_temp_ = mean(Measurement))


gearbox_hs_bearing_temp_2 = read_sensor_data("Class Data/Gearbox HS Bearing Temp Part 2", 1)
colnames(gearbox_hs_bearing_temp_2) = c("Turbine_id", "Time_Stamp", "Date", 'Measurement', 'Type')
gearbox_hs_bearing_temp_2$Time_Stamp = ymd_hms(gearbox_hs_bearing_temp_2$Time_Stamp)
gearbox_hs_bearing_temp_2$interval = round_date(gearbox_hs_bearing_temp_2$Time_Stamp, unit = "10 minutes")
gearbox_hs_bearing_temp_2 <- gearbox_hs_bearing_temp_2 %>% group_by(Turbine_id, interval) %>% summarize(gearbox_hs_bearing_temp_ = mean(Measurement))


gearbox_hs_bearing_temp_3 = read_sensor_data("Class Data/Gearbox HS Bearing Temp Part 3", 1)
colnames(gearbox_hs_bearing_temp_3) = c("Turbine_id", "Time_Stamp", "Date", 'Measurement', 'Type')
gearbox_hs_bearing_temp_3$Time_Stamp = ymd_hms(gearbox_hs_bearing_temp_3$Time_Stamp)
gearbox_hs_bearing_temp_3$interval = round_date(gearbox_hs_bearing_temp_3$Time_Stamp, unit = "10 minutes")
gearbox_hs_bearing_temp_3 <- gearbox_hs_bearing_temp_3 %>% group_by(Turbine_id, interval) %>% summarize(gearbox_hs_bearing_temp_ = mean(Measurement))


gearbox_hs_bearing_temp_all = rbind(gearbox_hs_bearing_temp_1, gearbox_hs_bearing_temp_2, gearbox_hs_bearing_temp_3)
# running this line below to average out measurements for potentially identical intervals across different sections of the data
gearbox_hs_bearing_temp_all <- gearbox_hs_bearing_temp_all %>% group_by(Turbine_id, interval) %>% summarize(gearbox_hs_bearing_temp_ = mean(gearbox_hs_bearing_temp_))

fwrite(gearbox_hs_bearing_temp_all, file = 'C:/Users/owenm/Downloads/STAT 190/Class Data/Gearbox_HS_Bearing_Temp_All.csv')




gearbox_ims_bearing_1 = read_sensor_data("Class Data/Gearbox IMS Bearing 1", 2)
colnames(gearbox_ims_bearing_1) = c("Turbine_id", "Time_Stamp", "Date", 'Measurement', 'Type')
gearbox_ims_bearing_1$Time_Stamp = ymd_hms(gearbox_ims_bearing_1$Time_Stamp)
gearbox_ims_bearing_1$interval = round_date(gearbox_ims_bearing_1$Time_Stamp, unit = "10 minutes")
gearbox_ims_bearing_1 <- gearbox_ims_bearing_1 %>% group_by(Turbine_id, interval) %>% summarize(gearbox_ims_bearing_ = mean(Measurement))


gearbox_ims_bearing_2 = read_sensor_data("Class Data/Gearbox IMS Bearing 2", 2)
colnames(gearbox_ims_bearing_2) = c("Turbine_id", "Time_Stamp", "Date", 'Measurement', 'Type')
gearbox_ims_bearing_2$Time_Stamp = ymd_hms(gearbox_ims_bearing_2$Time_Stamp)
gearbox_ims_bearing_2$interval = round_date(gearbox_ims_bearing_2$Time_Stamp, unit = "10 minutes")
gearbox_ims_bearing_2 <- gearbox_ims_bearing_2 %>% group_by(Turbine_id, interval) %>% summarize(gearbox_ims_bearing_ = mean(Measurement))


gearbox_ims_bearing_all = rbind(gearbox_ims_bearing_1, gearbox_ims_bearing_2)
gearbox_ims_bearing_all <- gearbox_ims_bearing_all %>% group_by(Turbine_id, interval) %>% summarize(gearbox_ims_bearing_ = mean(gearbox_ims_bearing_))

fwrite(gearbox_ims_bearing_all, file = 'C:/Users/owenm/Downloads/STAT 190/Class Data/Gearbox_IMS_Bearing_All.csv')


active_power <- read.csv("Class Data/10 Minute Interval/Active_Power.csv")
windspeed <- read.csv("Class Data/10 Minute Interval/Windspeed.csv")
gearbox_oil_temp <- read.csv("Class Data/10 Minute Interval/Gearbox_Oil_Temp.csv")
generator_rpm <- read.csv("Class Data/10 Minute Interval/Generator_RPM.csv")
hydraulic_pressure <- read.csv("Class Data/10 Minute Interval/Hydraulic_Pressure.csv")
gearbox_hs_bearing_temp <- read.csv("Class Data/10 Minute Interval/Gearbox_HS_Bearing_Temp_All.csv")
gearbox_ims_bearing <- read.csv("Class Data/10 Minute Interval/Gearbox_IMS_Bearing_All.csv")

# when loading back in dataframes, interval field is converted to a character.
# before making the final aggregated dataset (outside of work_order_scrubbed and FCTS),
# we will convert all interval fields back to ymd_hms for proper full join to occur.

active_power$interval = ymd_hms(active_power$interval)
windspeed$interval = ymd_hms(windspeed$interval)
gearbox_oil_temp$interval = ymd_hms(gearbox_oil_temp$interval)
generator_rpm$interval = ymd_hms(generator_rpm$interval)
hydraulic_pressure$interval = ymd_hms(hydraulic_pressure$interval)
gearbox_hs_bearing_temp$interval = ymd_hms(gearbox_hs_bearing_temp$interval)
gearbox_ims_bearing$interval = ymd_hms(gearbox_ims_bearing$interval)


list_df = list(active_power, windspeed, gearbox_oil_temp, generator_rpm, hydraulic_pressure, gearbox_hs_bearing_temp, gearbox_ims_bearing)
aggregated <- list_df %>% reduce(full_join, by = c("Turbine_id", "interval"))
fwrite(aggregated, file = 'C:/Users/owenm/Downloads/STAT 190/Class Data/Aggregated_Data.csv')

agg2 <- aggregated[-c(which(duplicated(aggregated[c('Turbine_id', 'interval')]))),]
### 0; that's great!



ambient_temp = read_sensor_data("ambient_temperature", 2)
colnames(ambient_temp) = c("Turbine_id", "Time_Stamp", "Date", 'Measurement', 'Type')
ambient_temp$Time_Stamp = ymd_hms(ambient_temp$Time_Stamp)
ambient_temp$interval = round_date(ambient_temp$Time_Stamp, unit = "10 minutes")
ambient_temp <- ambient_temp %>% group_by(Turbine_id, interval) %>% summarize(ambient_temp = mean(Measurement))

fwrite(ambient_temp, file = 'C:/Users/owenm/Downloads/STAT 190/Class Data/10 Minute Interval/Ambient_Temp.csv')

agg <- read.csv("Class Data/Aggregated_Data.csv")
agg$interval = ymd_hms(agg$interval)


list_df = list(agg, ambient_temp)
new <- list_df %>% reduce(full_join, by = c("Turbine_id", "interval"))
fwrite(new, file = 'C:/Users/owenm/Downloads/STAT 190/Class Data/All_Data.csv')




## Data Exploration ----
agg$month <- format(as.Date(agg$interval, format="%d/%m/%Y"),"%m")

agg <- 

agg <- read.csv("Class Data/Aggregated_Data.csv")
agg$interval = ymd_hms(agg$interval)


ggplot(data = agg) +
  geom_point(aes(x = month, y = wind_speed)) +
  theme_bw() +
  facet_wrap(~Turbine_id)



## Ben graph
ggplot(data = subset(agg, Turbine_id == "Turbine 7" & hydraulic_pressure_ > 200)) + 
  geom_point(aes(x = power, y = hydraulic_pressure_)) +
  theme_bw()

ggplot(data = subset(agg, Turbine_id == "Turbine 7" & hydraulic_pressure_ > 200), aes(x=power, y = hydraulic_pressure_)) + 
  geom_bin2d() +
  scale_fill_gradient(low = "yellow", high = "darkblue", trans = "log10")



## my graph
ggplot(data = agg) + 
  geom_point(aes(x = wind_speed, y = ambient_temp, color = Turbine_id)) +
  theme_bw()

ggplot(data = subset(agg, wind_speed < 30)) + 
  geom_point(aes(x = wind_speed, y = ambient_temp, color = Turbine_id)) +
  theme_bw()


ggplot(data = subset(agg, wind_speed < 30)) + 
  geom_point(aes(x = wind_speed, y = ambient_temp), alpha = 0.25) +
  theme_bw() +
  facet_wrap(~Turbine_id)



## density plots??










