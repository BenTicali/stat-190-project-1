rm(list = ls())

#load packages
library(tidyverse)
library(lubridate)

# read in work order data
wo <- read.csv("work order scrubbed.csv")
head(wo)

# please don't attach data

str(wo)

colnames(wo)


View(wo)

#convert start and finish columns to date objects (currently, they are character)
wo$wo_start_date <- ymd_hms(wo$wo_start_date)

wo$wo_finish_date <- ymd_hms(wo$wo_finish_date)

g = ggplot(data = wo) + geom_bar(aes(wo_start_date)) + theme_bw()
g

colnames(wo)
for(i in c(colnames(wo))) {
  print(unique(wo$i))
}

unique(wo$wo_start_date)





# Merge all files from Fault Code Time Series
file_list = list.files("Fault Codes Time Series/")
merged_data = data.frame()



for(i in file_list) {
  file_path = paste0("Fault Codes Time Series/", i, sep = "")
  if(file.info(file_path)$size > 0){
    temp_dataset = read.csv(file_path, header = FALSE)
    merged_data = rbind(merged_data, temp_dataset)
  }
}

colnames(merged_data) = c("Turbine_id", "Time_Stamp", "Date", 'Fault_Code', 'OEM_Status_Code', 'Error_Description', 'Error_Origin')

merged_data$Time_Stamp = ymd_hms(merged_data$Time_Stamp)
merged_data$Date = ymd(merged_data$Date)

merged_data %>% filter(Error_Origin != "Informational") %>% ggplot() + 
  geom_bar(aes(x = Turbine_id)) + facet_wrap(~ Error_Origin)

table(merged_data$Error_Origin)



sorted <- merged_data[order(merged_data$Time_Stamp),]



events <- sorted[-c(which(duplicated(sorted[c('Turbine_id', 'Date', 'Fault_Code')]))),]

events$Date2 <- ymd(events$Date)

difftime(events$Date[1], events$Date[2], units = 'days')

as.numeric(events$Date[1] - events$Date[2])[1]

# sort by turbine, date (order of line by line)

# order events by turbine, date, fault code
events2 <- events[order(events$Turbine_id, events$Date, events$Fault_Code),]
events2$time_difference = NA

# 
for(i in 2:length(events2$Date)){
  if(events2$Turbine_id[i] == events2$Turbine_id[i-1] && events2$Fault_Code[i] == events2$Fault_Code[i-1]){
    events2$time_difference[i] = as.numeric(difftime(events2$Date[i], events2$Date[i-1], 'days'))
  }
  else events2$time_difference[i] = 0
}

events2$time_difference[1] = 0

table(events2$time_difference)
events2 <- events2 %>% filter(time_difference != 1)

# Order Events2 by turbine id and time stamp
events2 <- events2[order(events2$Turbine_id, events2$Time_Stamp),]

# See what the unique informational error descriptions are
events2_info <- events2 %>% filter(Error_Origin == 'Informational')
table(events2_info$Error_Description)


# Bar charts of event count by Turbine for each Error Origin (minus Informational)
events2 %>% filter(Error_Origin != 'Informational') %>%
  ggplot() + geom_bar(aes(x = Turbine_id)) + facet_wrap(~ Error_Origin) + 
  theme(axis.text.x = element_text(angle = 90)) + ggtitle("Count of Error Origin, grouped by error origin")


events2 %>% filter(Error_Origin != 'Informational') %>%
  ggplot() + geom_bar(aes(x = Error_Origin)) + facet_wrap(~ Turbine_id) + 
  theme(axis.text.x = element_text(angle = 90)) + ggtitle("Count of Error Origin, grouped by turbine")



gearbox_1_files = list.files("Gearbox HS Bearing Temp Part 1/")
gearbox_2_files = list.files("Gearbox HS Bearing Temp Part 2/")
gearbox_3_files = list.files("Gearbox HS Bearing Temp Part 3/")
gearbox_merged = data.frame()



for(i in gearbox_1_files) {
  file_path = paste0("Gearbox HS Bearing Temp Part 1/", i, sep = "")
  if(file.info(file_path)$size > 0){
    temp_dataset = read.csv(file_path, header = FALSE)
    gearbox_merged = rbind(gearbox_merged, temp_dataset)
  }
}

for(i in gearbox_2_files) {
  file_path = paste0("Gearbox HS Bearing Temp Part 2/", i, sep = "")
  if(file.info(file_path)$size > 0){
    temp_dataset = read.csv(file_path, header = FALSE)
    gearbox_merged = rbind(gearbox_merged, temp_dataset)
  }
}

for(i in gearbox_3_files) {
  file_path = paste0("Gearbox HS Bearing Temp Part 3/", i, sep = "")
  if(file.info(file_path)$size > 0){
    temp_dataset = read.csv(file_path, header = FALSE)
    gearbox_merged = rbind(gearbox_merged, temp_dataset)
  }
}

gearbox_4_files = list.files("Gearbox Oil Temperature/")

for(i in gearbox_4_files) {
  file_path = paste0("Gearbox Oil Temperature/", i, sep = "")
  if(file.info(file_path)$size > 0){
    temp_dataset = read.csv(file_path, header = FALSE)
    gearbox_merged = rbind(gearbox_merged, temp_dataset)
  }
}

colnames(gearbox_merged) = c("Turbine_id", "Time_Stamp", "Date", "Temp", "Type")

table(gearbox_merged$Type)

bearing_temp = gearbox_merged %>% filter(Type == "Generator Bearing DE")
oil_temp = gearbox_merged %>% filter(Type == "Gearbox Oil")

gearbox_merged2 <- gearbox_merged[-c(which(duplicated(gearbox_merged[c('Turbine_id', 'Date', 'Temp', 'Type')]))),]

#write.csv(gearbox_merged2, )
#write.csv(events2, )

# read in csv's that have already been modified, so I don't have to run the code to produce them everytime
gearbox_merged2 <- read.csv("gearbox_merged2.csv")
events2 <- read.csv("events2.csv")


gearbox_max_min <- gearbox_merged2 %>% group_by(Turbine_id, Date, Type) %>% summarize(max_temp = max(Temp), 
                                                 min_temp = min(Temp)) %>%
  mutate(spread = max_temp - min_temp)


