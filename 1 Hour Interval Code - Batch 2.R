# clearing work space
rm(list = ls())

# adding any potentially necessary packages
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

# function to aggregate data that is separated in folders
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


## wrangling new data batch (1 hour) ----

windspeed = read_sensor_data("Class Data/Wind Turbine Data Batch 2 - Windspeed/Windspeed", 2)
colnames(windspeed) = c("Turbine_id", "Time_Stamp", "Date", 'Measurement', "Type")
windspeed$Time_Stamp = ymd_hms(windspeed$Time_Stamp)
windspeed$interval = round_date(windspeed$Time_Stamp, unit = "1 hour")
windspeed <- windspeed %>% group_by(Turbine_id, interval) %>% summarize(avg_windspeed = mean(Measurement), 
                                                                        max_windspeed = max(Measurement), 
                                                                        min_windspeed = min(Measurement))
fwrite(windspeed, file = 'C:/Users/owenm/Downloads/STAT 190/Class Data/1 Hour Interval - Batch 2/Windspeed.csv')




gearbox_oil_temp = read_sensor_data("Class Data/Wind Turbine Data Batch 2 - Gearbox Oil/Gearbox Oil", 2)
colnames(gearbox_oil_temp) = c("Turbine_id", "Time_Stamp", "Date", 'Measurement', 'Type')
gearbox_oil_temp$Time_Stamp = ymd_hms(gearbox_oil_temp$Time_Stamp)
gearbox_oil_temp$interval = round_date(gearbox_oil_temp$Time_Stamp, unit = "1 hour")
gearbox_oil_temp <- gearbox_oil_temp %>% group_by(Turbine_id, interval) %>% summarize(avg_gearbox_oil_temp = mean(Measurement), 
                                                                                      max_gearbox_oil_temp = max(Measurement), 
                                                                                      min_gearbox_oil_temp = min(Measurement))
fwrite(gearbox_oil_temp, file = 'C:/Users/owenm/Downloads/STAT 190/Class Data/1 Hour Interval - Batch 2/Gearbox Oil Temp.csv')





generator_rpm = read_sensor_data("Class Data/Wind Turbine Data Batch 2 - Generator RPM/Generator RPM", 2)
colnames(generator_rpm) = c("Turbine_id", "Time_Stamp", "Date", 'Measurement', 'Type')
generator_rpm$Time_Stamp = ymd_hms(generator_rpm$Time_Stamp)
generator_rpm$interval = round_date(generator_rpm$Time_Stamp, unit = "1 hour")
generator_rpm <- generator_rpm %>% group_by(Turbine_id, interval) %>% summarize(avg_generator_rpm = mean(Measurement),
                                                                                max_generator_rpm = max(Measurement),
                                                                                min_generator_rpm = min(Measurement))
fwrite(generator_rpm, file = 'C:/Users/owenm/Downloads/STAT 190/Class Data/1 Hour Interval - Batch 2/Generator RPM.csv')





hydraulic_pressure = read_sensor_data("Class Data/Wind Turbine Data Batch 2 - Hydraulic Pressure/Hydraulic Pressure", 2)
colnames(hydraulic_pressure) = c("Turbine_id", "Time_Stamp", "Date", 'Measurement', 'Type')
hydraulic_pressure$Time_Stamp = ymd_hms(hydraulic_pressure$Time_Stamp)
hydraulic_pressure$interval = round_date(hydraulic_pressure$Time_Stamp, unit = "1 hour")
hydraulic_pressure <- hydraulic_pressure %>% group_by(Turbine_id, interval) %>% summarize(avg_hydraulic_pressure = mean(Measurement),
                                                                                          max_hydraulic_pressure = max(Measurement),
                                                                                          min_hydraulic_pressure = min(Measurement))
fwrite(hydraulic_pressure, file = 'C:/Users/owenm/Downloads/STAT 190/Class Data/1 Hour Interval - Batch 2/Hydraulic Pressure.csv')






ambient_temp = read_sensor_data("Class Data/Wind Turbine Data Batch 2 - Ambient Temperature/Ambient Temperature", 2)
colnames(ambient_temp) = c("Turbine_id", "Time_Stamp", "Date", 'Measurement', 'Type')
ambient_temp$Time_Stamp = ymd_hms(ambient_temp$Time_Stamp)
ambient_temp$interval = round_date(ambient_temp$Time_Stamp, unit = "1 hour")
ambient_temp <- ambient_temp %>% group_by(Turbine_id, interval) %>% summarize(avg_ambient_temp = mean(Measurement),
                                                                              max_ambient_temp = max(Measurement),
                                                                              min_ambient_temp = min(Measurement))
fwrite(ambient_temp, file = 'C:/Users/owenm/Downloads/STAT 190/Class Data/1 Hour Interval - Batch 2/Ambient Temperature.csv')





active_power = read_sensor_data("Class Data/Wind Turbine Data Batch 2 - Active Power/Active Power", 2)
colnames(active_power) = c("Turbine_id", "Time_Stamp", "Date", 'Measurement', 'Type')
active_power$Time_Stamp = ymd_hms(active_power$Time_Stamp)
active_power$interval = round_date(active_power$Time_Stamp, unit = "1 hour")
active_power <- active_power %>% group_by(Turbine_id, interval) %>% summarize(avg_active_power = mean(Measurement),
                                                                              max_active_power = max(Measurement),
                                                                              min_active_power = min(Measurement))
fwrite(active_power, file = 'C:/Users/owenm/Downloads/STAT 190/Class Data/1 Hour Interval - Batch 2/Active Power.csv')






gearbox_hs_bearing_temp = read_sensor_data("Class Data/Wind Turbine Data Batch 2 - Gearbox HS Bearing/Gearbox HS Bearing", 2)
colnames(gearbox_hs_bearing_temp) = c("Turbine_id", "Time_Stamp", "Date", 'Measurement', 'Type')
gearbox_hs_bearing_temp$Time_Stamp = ymd_hms(gearbox_hs_bearing_temp$Time_Stamp)
gearbox_hs_bearing_temp$interval = round_date(gearbox_hs_bearing_temp$Time_Stamp, unit = "1 hour")
gearbox_hs_bearing_temp <- gearbox_hs_bearing_temp %>% group_by(Turbine_id, interval) %>% summarize(avg_gearbox_hs_bearing_temp = mean(Measurement),
                                                                                                    max_gearbox_hs_bearing_temp = max(Measurement),
                                                                                                    min_gearbox_hs_bearing_temp = min(Measurement))
fwrite(gearbox_hs_bearing_temp, file = 'C:/Users/owenm/Downloads/STAT 190/Class Data/1 Hour Interval - Batch 2/Gearbox HS Bearing Temp.csv')






gearbox_ims_bearing_1 = read_sensor_data("Class Data/Wind Turbine Data Batch 2 - Gearbox IMS Bearing 1/Gearbox IMS Bearing 1", 2)
colnames(gearbox_ims_bearing_1) = c("Turbine_id", "Time_Stamp", "Date", 'Measurement', 'Type')
gearbox_ims_bearing_1$Time_Stamp = ymd_hms(gearbox_ims_bearing_1$Time_Stamp)
gearbox_ims_bearing_1$interval = round_date(gearbox_ims_bearing_1$Time_Stamp, unit = "1 hour")
gearbox_ims_bearing_1 <- gearbox_ims_bearing_1 %>% group_by(Turbine_id, interval) %>% summarize(avg_gearbox_ims_bearing_1 = mean(Measurement),
                                                                                                max_gearbox_ims_bearing_1 = max(Measurement),
                                                                                                min_gearbox_ims_bearing_1 = min(Measurement))
fwrite(gearbox_ims_bearing_1, file = 'C:/Users/owenm/Downloads/STAT 190/Class Data/1 Hour Interval - Batch 2/Gearbox IMS Bearing 1.csv')





gearbox_ims_bearing_2 = read_sensor_data("Class Data/Wind Turbine Data Batch 2 - Gearbox IMS Bearing 2/Gearbox IMS Bearing 2", 2)
colnames(gearbox_ims_bearing_2) = c("Turbine_id", "Time_Stamp", "Date", 'Measurement', 'Type')
gearbox_ims_bearing_2$Time_Stamp = ymd_hms(gearbox_ims_bearing_2$Time_Stamp)
gearbox_ims_bearing_2$interval = round_date(gearbox_ims_bearing_2$Time_Stamp, unit = "1 hour")
gearbox_ims_bearing_2 <- gearbox_ims_bearing_2 %>% group_by(Turbine_id, interval) %>% summarize(avg_gearbox_ims_bearing_2 = mean(Measurement),
                                                                                                max_gearbox_ims_bearing_2 = max(Measurement),
                                                                                                min_gearbox_ims_bearing_2 = min(Measurement))
fwrite(gearbox_ims_bearing_2, file = 'C:/Users/owenm/Downloads/STAT 190/Class Data/1 Hour Interval - Batch 2/Gearbox IMS Bearing 1.csv')





fault = read_sensor_data("Class Data/Wind Turbine Data Batch 2 - Fault Status Codes/Fault Status Codes", 2)
colnames(fault) = c("Turbine_id", "interval", "Date", 'Fault_Code', 'Fault_Code2', 'Error_Description', 'Error_Origin')

fault$isfailure = ifelse(fault$Error_Description %in% c('Grid Filter Current Overload',
                                                        'Hs-Gen Gearbearing Superheated',
                                                        'Ups-Failure',
                                                        'Ups Bypass Error',
                                                        'Gear Oil Temperature High',
                                                        'Gear Oil Pressure Too High/Low',
                                                        'Ims-Gen Gearbearing Temp Too High',
                                                        'Gearoil Level Too Low',
                                                        'Converter Tripped, Auto Start',
                                                        'Mainbreaker Cut Out',
                                                        'Grid Filter Current Overload',
                                                        'Osc. In Gen Speed',
                                                        'Slip Ring Error',
                                                        'Genrpm/Srsg Speed Error'),1,0)


fault = subset(fault, select=c(Turbine_id, Date, isfailure))
fault = fault %>% group_by(Turbine_id, Date) %>% summarize(fault = max(isfailure))
fwrite(fault, file = 'C:/Users/owenm/Downloads/STAT 190/Class Data/1 Day Interval - Batch 2/Fault Codes Time Series.csv')




list_df = list(active_power, windspeed, gearbox_oil_temp, generator_rpm, 
               hydraulic_pressure, gearbox_hs_bearing_temp, gearbox_ims_bearing_1,gearbox_ims_bearing_2, ambient_temp, fault)
aggregated <- list_df %>% reduce(full_join, by = c("Turbine_id", "interval"))

aggregated$fault = ifelse(is.na(aggregated$fault),0,aggregated$fault)

fwrite(aggregated, file = 'C:/Users/owenm/Downloads/STAT 190/Class Data/Batch2_1Hour.csv')