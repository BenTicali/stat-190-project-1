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



## DATA CLEANING (10 minutes) -----
# Creating appropriate columns and merging files

windspeed = read_sensor_data("Class Data/Windspeed", 2)
colnames(windspeed) = c("Turbine_id", "Time_Stamp", "Date", 'Measurement', "Type")
windspeed$Time_Stamp = ymd_hms(windspeed$Time_Stamp)
windspeed$interval = round_date(windspeed$Time_Stamp, unit = "10 minutes")
windspeed <- windspeed %>% group_by(Turbine_id, interval) %>% summarize(wind_speed = mean(Measurement))



gearbox_oil_temp = read_sensor_data("Class Data/Gearbox Oil Temperature", 2)
colnames(gearbox_oil_temp) = c("Turbine_id", "Time_Stamp", "Date", 'Measurement', 'Type')
gearbox_oil_temp$Time_Stamp = ymd_hms(gearbox_oil_temp$Time_Stamp)
gearbox_oil_temp$interval = round_date(gearbox_oil_temp$Time_Stamp, unit = "10 minutes")
gearbox_oil_temp <- gearbox_oil_temp %>% group_by(Turbine_id, interval) %>% summarize(gearbox_oil_temperature = mean(Measurement))



generator_rpm = read_sensor_data("Class Data/Generator RPM", 2)
colnames(generator_rpm) = c("Turbine_id", "Time_Stamp", "Date", 'Measurement', 'Type')
generator_rpm$Time_Stamp = ymd_hms(generator_rpm$Time_Stamp)
generator_rpm$interval = round_date(generator_rpm$Time_Stamp, unit = "10 minutes")
generator_rpm <- generator_rpm %>% group_by(Turbine_id, interval) %>% summarize(generator_rpm_ = mean(Measurement))



hydraulic_pressure = read_sensor_data("Class Data/Hydraulic Pressure", 2)
colnames(hydraulic_pressure) = c("Turbine_id", "Time_Stamp", "Date", 'Measurement', 'Type')
hydraulic_pressure$Time_Stamp = ymd_hms(hydraulic_pressure$Time_Stamp)
hydraulic_pressure$interval = round_date(hydraulic_pressure$Time_Stamp, unit = "10 minutes")
hydraulic_pressure <- hydraulic_pressure %>% group_by(Turbine_id, interval) %>% summarize(hydraulic_pressure_ = mean(Measurement))


ambient_temp = read_sensor_data("Class Data/Ambient Temp", 2)
colnames(ambient_temp) = c("Turbine_id", "Time_Stamp", "Date", 'Measurement', 'Type')
ambient_temp$Time_Stamp = ymd_hms(ambient_temp$Time_Stamp)
ambient_temp$interval = round_date(ambient_temp$Time_Stamp, unit = "10 minutes")
ambient_temp <- ambient_temp %>% group_by(Turbine_id, interval) %>% summarize(ambient_temp = mean(Measurement))



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



active_power1 = read_sensor_data("Class Data/Active Power1", 2)
colnames(active_power1) = c("Turbine_id", "Time_Stamp", "Date", 'Measurement', 'Type')
active_power1$Time_Stamp = ymd_hms(active_power1$Time_Stamp)
active_power1$interval = round_date(active_power1$Time_Stamp, unit = "10 minutes")
active_power1 <- active_power1 %>% group_by(Turbine_id, interval) %>% summarize(power = mean(Measurement))



active_power2 = read_sensor_data("Class Data/Active Power2", 1)
colnames(active_power2) = c("Turbine_id", "Time_Stamp", "Date", 'Measurement', 'Type')
active_power2$Time_Stamp = ymd_hms(active_power2$Time_Stamp)
active_power2$interval = round_date(active_power2$Time_Stamp, unit = "10 minutes")
active_power2 <- active_power2 %>% group_by(Turbine_id, interval) %>% summarize(power = mean(Measurement))



active_power3 = read_sensor_data("Class Data/Active Power3", 1)
colnames(active_power3) = c("Turbine_id", "Time_Stamp", "Date", 'Measurement', 'Type')
active_power3$Time_Stamp = ymd_hms(active_power3$Time_Stamp)
active_power3$interval = round_date(active_power3$Time_Stamp, unit = "10 minutes")
active_power3 <- active_power3 %>% group_by(Turbine_id, interval) %>% summarize(power = mean(Measurement))



active_power4 = read_sensor_data("Class Data/Active Power4", 1)
colnames(active_power4) = c("Turbine_id", "Time_Stamp", "Date", 'Measurement', 'Type')
active_power4$Time_Stamp = ymd_hms(active_power4$Time_Stamp)
active_power4$interval = round_date(active_power4$Time_Stamp, unit = "10 minutes")
active_power4 <- active_power4 %>% group_by(Turbine_id, interval) %>% summarize(power = mean(Measurement))


active_power_merged = rbind(active_power1, active_power2, active_power3, active_power4)
active_power_merged <- active_power_merged %>% group_by(Turbine_id, interval) %>% summarize(power = mean(power))



fault = read_sensor_data("Class Data/Fault Codes Time Series", 2)
colnames(fault) = c("Turbine_id", "interval", "Date", 'Fault_Code', 'OEM_Status_Code', 'Error_Description', 'Error_Origin')

fault$isfailure = ifelse(fault$Error_Description %in% c('Backup Battery Error', 
                                                        'Extreme Hand Terminal Activity',
                                                        'Ft1 Sonic Wind Sensor Error',
                                                        'Inline (Aft) Pressure Sensor Error',
                                                        'Primary Wind Vane Failure',
                                                        'Surge Arrestor Varistor Error',
                                                        'Ambient Temp Sensor Error',
                                                        'Avilight, Lamp Failure',
                                                        'Frt Detected',
                                                        'Gearoil Filter Error, Off-Line',
                                                        'HubCheck Valve Fail – Waiting',
                                                        'Inline (Bef) Pressure Sensor Error',
                                                        'Secondary Wind Vane Failure',
                                                        'Ice Detection: Low Torque',
                                                        'Sec.Wind Vane Blocked',
                                                        'Windspeed Too High To Operate',
                                                        'Grd. Inv. Comm Error Autoreset',
                                                        'Grid Filter Current Overload',
                                                        'Dc Fuse Blown',
                                                        'Fuse Blown, Grid Filter',
                                                        'Grd. Inv. Communication Error',
                                                        'Grid Filter Res Temp Error',
                                                        'Hyd Oil Level Error',
                                                        'Pitchhydraulics Superheated',
                                                        'Hs-Gen Gearbearing Superheated',
                                                        'HubCheck valve fail – Stopped',
                                                        'Pitch Valve Calibrate Fail',
                                                        'Hub Pressuresswitch Error',
                                                        'Hub: Blade A Valve Error',
                                                        'Hub: Blade B Valve Error',
                                                        'Hub: Blade C Valve Error',
                                                        'Overspeed Hcu',
                                                        'Valve Failures During Hubcheck',
                                                        'Brake (Gen) Temperature Error',
                                                        'Blown Yaw Brake Fuse',
                                                        'Too Many Yaw Conv. Errors',
                                                        'Yaw Converter Error',
                                                        'Yaw Fail Waiting',
                                                        'Ups-Failure',
                                                        'Ups Bypass Error'),1,0)


fault = subset(fault, select=c(Turbine_id, interval, isfailure))
fault$interval = ymd_hms(fault$interval)
fault$interval = round_date(fault$interval, unit = "10 minutes")
fault = fault %>% group_by(Turbine_id, interval) %>% summarize(fault = max(isfailure))


list_df = list(active_power_merged, windspeed, gearbox_oil_temp, generator_rpm, 
               hydraulic_pressure, gearbox_hs_bearing_temp_all, gearbox_ims_bearing_all, ambient_temp, fault)
aggregated <- list_df %>% reduce(full_join, by = c("Turbine_id", "interval"))


aggregated$fault = ifelse(is.na(aggregated$fault),0,aggregated$fault)

fwrite(aggregated, file = 'C:/Users/owenm/Downloads/STAT 190/Class Data/Aggregated_Data_10Min.csv')





## DATA CLEANING (1 day)  ----

windspeed = read_sensor_data("Class Data/Windspeed", 2)
colnames(windspeed) = c("Turbine_id", "Time_Stamp", "Date", 'Measurement', "Type")
windspeed$Time_Stamp = ymd_hms(windspeed$Time_Stamp)
windspeed$interval = round_date(windspeed$Time_Stamp, unit = "1 day")
windspeed <- windspeed %>% group_by(Turbine_id, interval) %>% summarize(wind_speed = mean(Measurement))



gearbox_oil_temp = read_sensor_data("Class Data/Gearbox Oil Temperature", 2)
colnames(gearbox_oil_temp) = c("Turbine_id", "Time_Stamp", "Date", 'Measurement', 'Type')
gearbox_oil_temp$Time_Stamp = ymd_hms(gearbox_oil_temp$Time_Stamp)
gearbox_oil_temp$interval = round_date(gearbox_oil_temp$Time_Stamp, unit = "1 day")
gearbox_oil_temp <- gearbox_oil_temp %>% group_by(Turbine_id, interval) %>% summarize(gearbox_oil_temperature = mean(Measurement))



generator_rpm = read_sensor_data("Class Data/Generator RPM", 2)
colnames(generator_rpm) = c("Turbine_id", "Time_Stamp", "Date", 'Measurement', 'Type')
generator_rpm$Time_Stamp = ymd_hms(generator_rpm$Time_Stamp)
generator_rpm$interval = round_date(generator_rpm$Time_Stamp, unit = "1 day")
generator_rpm <- generator_rpm %>% group_by(Turbine_id, interval) %>% summarize(generator_rpm_ = mean(Measurement))



hydraulic_pressure = read_sensor_data("Class Data/Hydraulic Pressure", 2)
colnames(hydraulic_pressure) = c("Turbine_id", "Time_Stamp", "Date", 'Measurement', 'Type')
hydraulic_pressure$Time_Stamp = ymd_hms(hydraulic_pressure$Time_Stamp)
hydraulic_pressure$interval = round_date(hydraulic_pressure$Time_Stamp, unit = "1 day")
hydraulic_pressure <- hydraulic_pressure %>% group_by(Turbine_id, interval) %>% summarize(hydraulic_pressure_ = mean(Measurement))


ambient_temp = read_sensor_data("Class Data/Ambient Temp", 2)
colnames(ambient_temp) = c("Turbine_id", "Time_Stamp", "Date", 'Measurement', 'Type')
ambient_temp$Time_Stamp = ymd_hms(ambient_temp$Time_Stamp)
ambient_temp$interval = round_date(ambient_temp$Time_Stamp, unit = "1 day")
ambient_temp <- ambient_temp %>% group_by(Turbine_id, interval) %>% summarize(ambient_temp = mean(Measurement))





gearbox_hs_bearing_temp_1 = read_sensor_data("Class Data/Gearbox HS Bearing Temp Part 1", 2)
colnames(gearbox_hs_bearing_temp_1) = c("Turbine_id", "Time_Stamp", "Date", 'Measurement', 'Type')
gearbox_hs_bearing_temp_1$Time_Stamp = ymd_hms(gearbox_hs_bearing_temp_1$Time_Stamp)
gearbox_hs_bearing_temp_1$interval = round_date(gearbox_hs_bearing_temp_1$Time_Stamp, unit = "1 day")
gearbox_hs_bearing_temp_1 <- gearbox_hs_bearing_temp_1 %>% group_by(Turbine_id, interval) %>% summarize(gearbox_hs_bearing_temp_ = mean(Measurement))


gearbox_hs_bearing_temp_2 = read_sensor_data("Class Data/Gearbox HS Bearing Temp Part 2", 1)
colnames(gearbox_hs_bearing_temp_2) = c("Turbine_id", "Time_Stamp", "Date", 'Measurement', 'Type')
gearbox_hs_bearing_temp_2$Time_Stamp = ymd_hms(gearbox_hs_bearing_temp_2$Time_Stamp)
gearbox_hs_bearing_temp_2$interval = round_date(gearbox_hs_bearing_temp_2$Time_Stamp, unit = "1 day")
gearbox_hs_bearing_temp_2 <- gearbox_hs_bearing_temp_2 %>% group_by(Turbine_id, interval) %>% summarize(gearbox_hs_bearing_temp_ = mean(Measurement))


gearbox_hs_bearing_temp_3 = read_sensor_data("Class Data/Gearbox HS Bearing Temp Part 3", 1)
colnames(gearbox_hs_bearing_temp_3) = c("Turbine_id", "Time_Stamp", "Date", 'Measurement', 'Type')
gearbox_hs_bearing_temp_3$Time_Stamp = ymd_hms(gearbox_hs_bearing_temp_3$Time_Stamp)
gearbox_hs_bearing_temp_3$interval = round_date(gearbox_hs_bearing_temp_3$Time_Stamp, unit = "1 day")
gearbox_hs_bearing_temp_3 <- gearbox_hs_bearing_temp_3 %>% group_by(Turbine_id, interval) %>% summarize(gearbox_hs_bearing_temp_ = mean(Measurement))


gearbox_hs_bearing_temp_all = rbind(gearbox_hs_bearing_temp_1, gearbox_hs_bearing_temp_2, gearbox_hs_bearing_temp_3)
# running this line below to average out measurements for potentially identical intervals across different sections of the data
gearbox_hs_bearing_temp_all <- gearbox_hs_bearing_temp_all %>% group_by(Turbine_id, interval) %>% summarize(gearbox_hs_bearing_temp_ = mean(gearbox_hs_bearing_temp_))




gearbox_ims_bearing_1 = read_sensor_data("Class Data/Gearbox IMS Bearing 1", 2)
colnames(gearbox_ims_bearing_1) = c("Turbine_id", "Time_Stamp", "Date", 'Measurement', 'Type')
gearbox_ims_bearing_1$Time_Stamp = ymd_hms(gearbox_ims_bearing_1$Time_Stamp)
gearbox_ims_bearing_1$interval = round_date(gearbox_ims_bearing_1$Time_Stamp, unit = "1 day")
gearbox_ims_bearing_1 <- gearbox_ims_bearing_1 %>% group_by(Turbine_id, interval) %>% summarize(gearbox_ims_bearing_ = mean(Measurement))


gearbox_ims_bearing_2 = read_sensor_data("Class Data/Gearbox IMS Bearing 2", 2)
colnames(gearbox_ims_bearing_2) = c("Turbine_id", "Time_Stamp", "Date", 'Measurement', 'Type')
gearbox_ims_bearing_2$Time_Stamp = ymd_hms(gearbox_ims_bearing_2$Time_Stamp)
gearbox_ims_bearing_2$interval = round_date(gearbox_ims_bearing_2$Time_Stamp, unit = "1 day")
gearbox_ims_bearing_2 <- gearbox_ims_bearing_2 %>% group_by(Turbine_id, interval) %>% summarize(gearbox_ims_bearing_ = mean(Measurement))


gearbox_ims_bearing_all = rbind(gearbox_ims_bearing_1, gearbox_ims_bearing_2)
gearbox_ims_bearing_all <- gearbox_ims_bearing_all %>% group_by(Turbine_id, interval) %>% summarize(gearbox_ims_bearing_ = mean(gearbox_ims_bearing_))





active_power1 = read_sensor_data("Class Data/Active Power1", 2)
colnames(active_power1) = c("Turbine_id", "Time_Stamp", "Date", 'Measurement', 'Type')
active_power1$Time_Stamp = ymd_hms(active_power1$Time_Stamp)
active_power1$interval = round_date(active_power1$Time_Stamp, unit = "1 day")
active_power1 <- active_power1 %>% group_by(Turbine_id, interval) %>% summarize(power = mean(Measurement))



active_power2 = read_sensor_data("Class Data/Active Power2", 1)
colnames(active_power2) = c("Turbine_id", "Time_Stamp", "Date", 'Measurement', 'Type')
active_power2$Time_Stamp = ymd_hms(active_power2$Time_Stamp)
active_power2$interval = round_date(active_power2$Time_Stamp, unit = "1 day")
active_power2 <- active_power2 %>% group_by(Turbine_id, interval) %>% summarize(power = mean(Measurement))



active_power3 = read_sensor_data("Class Data/Active Power3", 1)
colnames(active_power3) = c("Turbine_id", "Time_Stamp", "Date", 'Measurement', 'Type')
active_power3$Time_Stamp = ymd_hms(active_power3$Time_Stamp)
active_power3$interval = round_date(active_power3$Time_Stamp, unit = "1 day")
active_power3 <- active_power3 %>% group_by(Turbine_id, interval) %>% summarize(power = mean(Measurement))



active_power4 = read_sensor_data("Class Data/Active Power4", 1)
colnames(active_power4) = c("Turbine_id", "Time_Stamp", "Date", 'Measurement', 'Type')
active_power4$Time_Stamp = ymd_hms(active_power4$Time_Stamp)
active_power4$interval = round_date(active_power4$Time_Stamp, unit = "1 day")
active_power4 <- active_power4 %>% group_by(Turbine_id, interval) %>% summarize(power = mean(Measurement))


active_power_merged = rbind(active_power1, active_power2, active_power3, active_power4)
active_power_merged <- active_power_merged %>% group_by(Turbine_id, interval) %>% summarize(power = mean(power))


fault = read_sensor_data("Class Data/Fault Codes Time Series", 2)
colnames(fault) = c("Turbine_id", "interval", "Date", 'Fault_Code', 'OEM_Status_Code', 'Error_Description', 'Error_Origin')

fault$isfailure = ifelse(fault$Error_Description %in% c('Backup Battery Error', 
                                                        'Extreme Hand Terminal Activity',
                                                        'Ft1 Sonic Wind Sensor Error',
                                                        'Inline (Aft) Pressure Sensor Error',
                                                        'Primary Wind Vane Failure',
                                                        'Surge Arrestor Varistor Error',
                                                        'Ambient Temp Sensor Error',
                                                        'Avilight, Lamp Failure',
                                                        'Frt Detected',
                                                        'Gearoil Filter Error, Off-Line',
                                                        'HubCheck Valve Fail – Waiting',
                                                        'Inline (Bef) Pressure Sensor Error',
                                                        'Secondary Wind Vane Failure',
                                                        'Ice Detection: Low Torque',
                                                        'Sec.Wind Vane Blocked',
                                                        'Windspeed Too High To Operate',
                                                        'Grd. Inv. Comm Error Autoreset',
                                                        'Grid Filter Current Overload',
                                                        'Dc Fuse Blown',
                                                        'Fuse Blown, Grid Filter',
                                                        'Grd. Inv. Communication Error',
                                                        'Grid Filter Res Temp Error',
                                                        'Hyd Oil Level Error',
                                                        'Pitchhydraulics Superheated',
                                                        'Hs-Gen Gearbearing Superheated',
                                                        'HubCheck valve fail – Stopped',
                                                        'Pitch Valve Calibrate Fail',
                                                        'Hub Pressuresswitch Error',
                                                        'Hub: Blade A Valve Error',
                                                        'Hub: Blade B Valve Error',
                                                        'Hub: Blade C Valve Error',
                                                        'Overspeed Hcu',
                                                        'Valve Failures During Hubcheck',
                                                        'Brake (Gen) Temperature Error',
                                                        'Blown Yaw Brake Fuse',
                                                        'Too Many Yaw Conv. Errors',
                                                        'Yaw Converter Error',
                                                        'Yaw Fail Waiting',
                                                        'Ups-Failure',
                                                        'Ups Bypass Error'),1,0)


fault = subset(fault, select=c(Turbine_id, interval, isfailure))
fault$interval = ymd_hms(fault$interval)
fault$interval = round_date(fault$interval, unit = "1 day")
fault = fault %>% group_by(Turbine_id, interval) %>% summarize(fault = max(isfailure))



list_df = list(active_power_merged, windspeed, gearbox_oil_temp, generator_rpm, 
               hydraulic_pressure, gearbox_hs_bearing_temp_all, gearbox_ims_bearing_all, ambient_temp, fault)
aggregated <- list_df %>% reduce(full_join, by = c("Turbine_id", "interval"))

aggregated$fault = ifelse(is.na(aggregated$fault),0,aggregated$fault)

fwrite(aggregated, file = 'C:/Users/owenm/Downloads/STAT 190/Class Data/Aggregated_Data_1Day.csv')



# Some of this is duplicate data, but all of the code below is pasted together for ease of understanding

merged_data$isfailure = ifelse(merged_data$Error_Description %in% c('Grid Filter Current Overload',
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


merged_data$failuretype = case_when(merged_data$Error_Description == 'Grid Filter Current Overload' ~ 'Grid Filter Current Overlad',
                                    merged_data$Error_Description == 'Hs-Gen Gearbearing Superheated' ~ 'Hs-Gen Gearbearing Superheated',
                                    merged_data$Error_Description == 'Ups-Failure' ~ 'Ups_Failure',
                                    merged_data$Error_Description == 'Ups Bypass Error' ~ 'Ups Bypass Error',
                                    merged_data$Error_Description == 'Gear Oil Temperature High' ~ 'Gear Oil Temperature High',
                                    merged_data$Error_Description == 'Gear Oil Pressure Too High/Low' ~ 'Gear Oil Pressure Too High/Low',
                                    merged_data$Error_Description == 'Ims-Gen Gearbearing Temp Too High' ~ 'Ims-Gen Gearbearing Temp Too High',
                                    merged_data$Error_Description == 'Gearoil Level Too Low' ~ 'Gearoil Level Too Low',
                                    merged_data$Error_Description == 'Converter Tripped, Auto Start' ~ 'Converter Tripped, Auto Start',
                                    merged_data$Error_Description == 'Mainbreaker Cut Out' ~ 'Mainbreaker Cut Out',
                                    merged_data$Error_Description == 'Grid Filter Current Overload' ~ 'Grid Filter Current Overload',
                                    merged_data$Error_Description == 'Osc. In Gen Speed' ~ 'Osc. In Gen Speed',
                                    merged_data$Error_Description == 'Slip Ring Error' ~ 'Slip Ring Error',
                                    merged_data$Error_Description == 'Genrpm/Srsg Speed Error' ~ 'Genrpm/Srsg Speed Error',
                                    TRUE ~ "Not an Error")

fault = subset(merged_data, select=c(Turbine_id, interval, isfailure, failuretype))
summary(fault)

fault$interval = ymd_hms(fault$interval)
fault$interval = round_date(fault$interval, unit = "10 minutes")
fault = fault %>% group_by(Turbine_id, interval, failuretype) %>% summarize(fault = max(isfailure))
summary(fault)


agg <- read.csv("Aggregated_Data.csv")
agg$interval = ymd_hms(agg$interval)


list_df = list(agg, fault)
new <- list_df %>% reduce(full_join, by = c("Turbine_id", "interval"))
fwrite(new, file = 'C:/Users/benja/OneDrive - Drake University/Drake University Semester 8/STAT 190/clean_data/Aggregated_Data_Fault.csv')

new$fault = ifelse(is.na(new$fault),0,new$fault)
new$failuretype = ifelse(is.na(new$failuretype),'Not an Error',new$failuretype)


#This is the end of the code block that goes all together


















