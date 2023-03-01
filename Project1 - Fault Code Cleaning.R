rm(list = ls())
#load packages
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


# read in data
file_list = list.files("raw_data/Fault Codes Time Series/")
merged_data = data.frame()

for(i in file_list) {
   file_path = paste0("raw_data/Fault Codes Time Series/", i, sep = "")
   if(file.info(file_path)$size > 0){
      temp_dataset = read.csv(file_path, header = FALSE)
      merged_data = rbind(merged_data, temp_dataset)
   }
}

colnames(merged_data) = c("Turbine_id", "interval", "Date", 'Fault_Code', 'OEM_Status_Code', 'Error_Description', 'Error_Origin')

merged_data$isfailure = ifelse(merged_data$Error_Description %in% c('Backup Battery Error', 
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


fault = subset(merged_data, select=c(Turbine_id, interval, isfailure))
summary(fault)

fault$interval = ymd_hms(fault$interval)
fault$interval = round_date(fault$interval, unit = "10 minutes")
fault = fault %>% group_by(Turbine_id, interval) %>% summarize(fault = max(isfailure))
summary(fault)


agg <- read.csv("clean_data/Aggregated_Data.csv")
agg$interval = ymd_hms(agg$interval)


list_df = list(agg, fault)
new <- list_df %>% reduce(full_join, by = c("Turbine_id", "interval"))
fwrite(new, file = 'C:/Users/benja/OneDrive - Drake University/Drake University Semester 8/STAT 190/clean_data/Aggregated_Data_Fault.csv')
