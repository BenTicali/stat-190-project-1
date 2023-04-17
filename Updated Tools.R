# THESE ARE JUST TWO BLOCKS OF CODE THAT AID OTHER FILES. IT DOES NOT INCLUDE
# ANY OTHER STEPS OF THE PROJECT
# The first block removes duplicates of the same fault code pinging for a series of consecutive intervals
# The second block merges the aggregated data with the next work order that occurs and creates a time until work order variable

#------------------------------Remove duplicates of fault code pinging for consecutive intervals---------------####
# order fault data by turbine, date, and fault code
fault <- events[order(events$Turbine_id, events$Date, events$Fault_Code),]
# create new time difference variable to help remove duplicates
fault$time_difference = NA


# The first occurence of a fault will have a 0 for time difference
for(i in 2:length(fault$Date)){
  # take time difference of two lines if they are the same fault code and same turbine id
  if(fault$Turbine_id[i] == fault$Turbine_id[i-1] && fault$Fault_Code[i] == fault$Fault_Code[i-1]){
    fault$time_difference[i] = as.numeric(difftime(fault$Date[i], fault$Date[i-1], 'days'))
  }
  # new faults or turbines (different from previous row) will start again at 0
  else fault$time_difference[i] = 0 # 0 does not matter because it will not be used for removal
}

# set first line time difference to 0
fault$time_difference[1] = 0

# remove duplicates on the same day (only keep first occurrence of certain fault on a day)
fault <- fault[!duplicated(fault[c("Turbine_id", "Date", "Fault_code"),])]
 
# remove duplicates of the same fault rolling into the next day and every consecutive day
# example: on the first line where the same fault rolls over into the next day, the time difference from the previous 
#   line will be 1. Every other occurence on the same day was already removed, but the next day where it rolled over
#   will also have a 1. Thus, remove all 1's so the only thing that stands was the first occurence in the entire series
#   of intervals.
fault <- fault %>% filter(time_difference != 1)




#---------------------- merge agg data with work order and find time until next work order -----------------------------------####

# rename column so you can merge on a common variable
wo <- wo %>% rename("Turbine_id" = "location_id")

# get rid of unneccesary columns from work order set
wo <- subset(wo, select = c("wo_start_date", "wo_finish_date", "OrderNo", "Order_Info", "Turbine_id", "component_type"))

# order agg and wo by turbine id and interval
agg <- agg[order(agg$Turbine_id, agg$interval)]
wo <- wo[order(wo$Turbine_id, wo$interval)]


# merge the two datasets on turbine id
# this will  expand the number of rows to every occurence of a work order for the turbine in the agg row
  # will reduce this down next
agg <- merge(x = agg, y = wo, by = c("Turbine_id"), all.x = TRUE)

# make sure the dates are in the same format
agg$date = ymd(agg$date)
agg$wo_start_date = ymd(agg$wo_start_date)
agg$wo_finish_date <- ymd(agg$wo_finish_date)

# get rid of rows where the work order starts before the date of the measurements
agg <- agg %>% filter(date <= wo_start_date)

# already ordered to first occurence from eariler
# get rid of duplicated turbine_id and interval (which will get rid of anything that isn't the first work order
#   occuring on or after the date of the measurements for that row)
agg <- agg[!duplicated(agg[c("Turbine_id","interval")]),]

# create time until work order start variable (days)
agg$time_until_wo <- NA

# loop through to apply the diff time function since it didn't work without a loop
for (i in c(1:nrow(agg))){
  agg$time_until_wo[i] <- as.numeric(difftime(agg$wo_start_date[i], agg$date[i], 'days'))
}
