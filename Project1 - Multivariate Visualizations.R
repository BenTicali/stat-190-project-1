rm(list = ls())
#load packages
library(tidyverse)
library(lubridate)

# read in aggregated data
agg = read.csv("clean_data/Aggregated_Data_Fault.csv", stringsAsFactors = TRUE)

# convert interval variable to date-time
agg$interval = ymd_hms(agg$interval)

agg$fault = ifelse(is.na(agg$fault),0,agg$fault)
View(agg)

#####
#Multivariate Plots w/ power
ggplot(data = subset(agg, Turbine_id == 'Turbine 7')) +
   geom_point(aes(x = power, y = gearbox_oil_temperature)) +
   facet_wrap(~fault) +
   theme_bw()

ggplot(data = agg) + 
   geom_point(aes(x = power, y = generator_rpm_, color = fault)) +
   theme_bw()

ggplot(data = subset(agg, Turbine_id == 'Turbine 7'), aes(x = power, y = generator_rpm_)) + 
   geom_bin2d() +
   scale_fill_gradient(low="pink",high="maroon",trans="log10")

ggplot(data = subset(agg, Turbine_id == 'Turbine 7' & hydraulic_pressure_ > 200)) + 
   geom_point(aes(x = power, y = hydraulic_pressure_, color=fault), alpha = .5) +
   theme_bw()

ggplot(data = subset(agg, Turbine_id == 'Turbine 7' & hydraulic_pressure_ > 200), aes(x = power, y = hydraulic_pressure_)) + 
   geom_bin2d() +
   scale_fill_gradient(low="yellow",high="darkblue",trans="log10")

ggplot(data = subset(agg, agg$Turbine_id == "Turbine 7")) + 
   geom_point(aes(x = power, y = gearbox_hs_bearing_temp_)) +
   theme_bw()

ggplot(data = subset(agg, agg$Turbine_id == "Turbine 10")) +
   geom_point(aes(x = power, y = gearbox_ims_bearing_, color = fault)) +
   theme_bw()

ggplot(data = subset(agg, Turbine_id == 'Turbine 7'), aes(x = power, y = gearbox_ims_bearing_)) + 
   geom_bin2d() +
   scale_fill_gradient(low="yellow",high="darkblue",trans="log10")

