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

merged_data$isfailure = ifelse(merged_data$Error_Description %in% c('Amb Temp Extended Operation',
                                                             'Geniv: 139 U-Phase Sharing',
                                                             'Geniv: 251 Delta 2 dib +15V',
                                                             'Grdinv: 1 Interlock',
                                                             'Grdinv: 133 Sfe Mains Freq.',
                                                             'Grdinv: 183 Unident. Smps1',
                                                             'Grdinv: 189 Unident. Smps3',
                                                             'Grdinv: Undiagnosed,Delta3',
                                                             'Grdinv: 241 Delta 1 Dib +15v',
                                                             'Grdinv: 261 Delta 3 Dib +15v',
                                                             'Grdinv: 324 Ov Trip1 - Instant', 
                                                             'Hub Module Powerdown Detected',
                                                             'Hyd. For Crane/Cover Activated',
                                                             'Io Module 4 Powerdown Detected',
                                                             'Local, Customer / Guest Visit',
                                                             'Manual Idle Stop',
                                                             'Manual Stop',
                                                             'Many Wind Vane Activations',
                                                             'Mcb Cleaning Ended',
                                                             'Pitch A Tracking During Operation',
                                                             'Pitch B Tracking During Stop',
                                                             'Pitch Functionality Check',
                                                             'Pitch Pawl A Feedb. Operation', 
                                                             'Pitch Pawl C Feedb. Operation',
                                                             'Remote Stop - Owner',
                                                             'Stop for Powerdown',
                                                             'Tor. Converter Fan 1 (Tower)',
                                                             'Turbine In Local Operation',
                                                             'Generator High Speed Waiting',
                                                             'Geninv: 258 Delta 2 Dib Td V',
                                                             'Grdinv: 268 Delta 3 Dib Td V',
                                                             'Local, Ad-Hoc / Repair Work',
                                                             'Local, Scheduled Service Work',
                                                             'Manual Idle Stop - Yawing',
                                                             'Many Anemometer Activations',
                                                             'Mcb Cleaning In Progresss',
                                                             'Pitch A Tracking During Stop',
                                                             'Pitch C Tracking During Stop',
                                                             'Pitch Pawl B Feedb. Operation',
                                                             'Remote Stop - Oem',
                                                             'Stopped, Untwisting Cables',
                                                             'Tor. Converter Coolant Heater',
                                                             'Tor. Converter Fan 2 (Tower)',
                                                             'Tower Vent. Tor',
                                                             'Twrfreq Outside Allowed Window',
                                                             'Yaw Limit Sensor Activated',
                                                             'Converter Trip, External',
                                                             'Converter Tripped, Auto Start',
                                                             'Timeout Dc-Circuit Charging',
                                                             'Service Stop Hyd/Gear Pump/Fan',
                                                             'Pitch Pawl A Feedb. Stop',
                                                             'Accumulator A Check: Timeout',
                                                             'Accumulator B Check: Timeout',
                                                             'Accumulator C Check: Timeout',
                                                             'Emergency Switch Activated'),0,1)

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





