rm(list = ls())


library(tidyverse)
library(lubridate)


final_data <- read.csv("Aggregated_Data_Fault_Before_If.csv")

final_data$interval <- ymd_hms(final_data$interval)
final_data$date = as.Date(final_data$interval)

# ggplot(data = final_data) +
#   geom_boxplot(aes(x = , y = value, fill = km_cluster)) +
#   facet_wrap(~variable, scales = "free") +
#   scale_fill_brewer(palette = "Dark2") +
#   ggtitle("K-means Clusters")


qplot(factor(0),power, data = final_data, geom = 'boxplot')


pdf("boxplots.pdf")

plot_boxes <- function(var) {
  var = sym(var)
  ggplot(data = final_data) +
    geom_boxplot(aes(x = Turbine_id, y = !!var, fill = Turbine_id)) + xlab("Turbine id") +
    ggtitle(paste("Boxplot of", var, "by Turbine id", sep = " ")) +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
}

pdf("boxplots.pdf")
plot_boxes("power")
plot_boxes("wind_speed")
plot_boxes("gearbox_oil_temperature")
plot_boxes("generator_rpm_")
plot_boxes("hydraulic_pressure_")
plot_boxes("gearbox_hs_bearing_temp_")
plot_boxes("gearbox_ims_bearing_")
plot_boxes("ambient_temp")
dev.off()

wo2 <- wo %>% filter(wo_start_date > "2023-06-04", location_id == "Turbine 14")
nrow(wo2)

library(magrittr)

final_data2 = final_data[order(final_data$Turbine_id, final_data$date),]

wo3 <- wo[order(wo$location_id, wo$wo_start_date),]

wo2 <- wo3
final_data2$time_until_wo <- NA

wo2$wo_start_date <- ymd(wo2$wo_start_date)
final_data2$date <- ymd(final_data2$date)


for(i in c(1:nrow(final_data2))) {
  if(final_data2$date[i] < "2020-03-01") {
    # final_data2$time_until_wo[i] = NA -- already set to NA
  }
  else {
    if(wo2 %>% filter(location_id == final_data2$Turbine_id[i], wo_start_date >= final_data2$date[i]) %>%
       nrow() == 0) {#select(wo_start_date) %>% slice_head() %>% pluck(1) %>% length() == 0) {
      # Nothing -- already set to NA (means there are no work orders after this day)
    }
    else {
      temp <- wo2 %>% filter(location_id == final_data2$Turbine_id[i], wo_start_date >= final_data2$date[i]) %>%
        select(wo_start_date) %>% slice_head() %>% pluck(1)
      t <- as.numeric(difftime(temp, final_data2$date[i], 'days'))
      final_data2$time_until_wo[i] <- t
    }
  }
  
}

table(final_data2$time_until_wo)



# wind_speed
final_data2 %>% filter(time_until_wo < 30, wind_speed >= 0) %>% group_by(time_until_wo) %>% 
  summarise(mean_ = mean(wind_speed)) %>% as.data.frame() %>% ggplot() +
  geom_line(aes(x = time_until_wo, y = mean_)) + xlab("days until work order") +
  scale_x_reverse(lim = c(30,0)) + ylab("mean wind speed")

# function to graph the mean of each measurement x numbers of days before a work order

plot_days_until <- function(feature) {
  feature <- sym(feature)
  final_data2 %>% filter(time_until_wo < 31, !!feature >= 0) %>% group_by(Turbine_id, time_until_wo) %>%
    summarise(mean_ = mean(!!feature)) %>% as.data.frame() %>% ggplot() +
    geom_line(aes(x = time_until_wo, y = mean_)) + xlab("days until work order") + 
    scale_x_reverse(lim = c(30,0)) + ylab(paste("mean", feature, sep = " ")) + facet_wrap(~ Turbine_id) +
    ggtitle(paste("mean", feature, "by days until work order start", sep = " "))
  
}

# plot the mean of each measurement x number of days before a work order in a pdf
pdf("rplts.pdf")
plot_days_until("power")
plot_days_until("wind_speed")
plot_days_until("gearbox_oil_temperature")
plot_days_until("generator_rpm_")
plot_days_until("hydraulic_pressure_")
plot_days_until("gearbox_hs_bearing_temp_")
plot_days_until("gearbox_ims_bearing_")
plot_days_until("ambient_temp")
dev.off()

# should we expand to the days after a work order?

