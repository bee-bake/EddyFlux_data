#Run the function and create targets dataset with daily flux mean averaged over...
#..all the available half hourly values without any cut-off
#Use a predefined function to import the data
library(patchwork)
source("generate_EddyFLux_ghg_targets_function_without_cutoff.R")
targets_without_stable_mean <- generate_EddyFlux_ghg_targets_function(
  
  flux_current_data_file <- "https://raw.githubusercontent.com/CareyLabVT/Reservoirs/master/Data/DataNotYetUploadedToEDI/EddyFlux_Processing/EddyPro_Cleaned_L1.csv",
  flux_edi_data_file <- "https://pasta-s.lternet.edu/package/data/eml/edi/692/11/e0976e7a6543fada4cbf5a1bb168713b",
  met_current_data_file <- "https://raw.githubusercontent.com/FLARE-forecast/FCRE-data/fcre-metstation-data-qaqc/FCRmet_L1.csv",
  met_edi_data_file <- "https://pasta.lternet.edu/package/data/eml/edi/389/8/d4c74bbb3b86ea293e5c52136347fbb0")

#Create a csv file and save it!
write.csv(targets_without_stable_mean, "targets_without_stable_mean.csv")

#Plot the time series with the daily mean values
targets_without_stable_mean_wider <- targets_without_stable_mean %>%
pivot_wider(names_from = variable, values_from = observation)

p1 <- ggplot(targets_without_stable_mean_wider, aes(x = as.Date(datetime)))+
  geom_point(aes(y = co2flux_umolm2s_mean), colour = "blue", alpha=0.5) +
  scale_x_date(date_breaks = "6 months", date_labels = "%Y-%m") +
  ylab("CO2 daily mean") + xlab("") + ggtitle("U star filtering and cut-off not applied") +
  ylim(-20, 20)

p2 <- ggplot(targets_without_stable_mean_wider, aes(x = as.Date(datetime)))+
  geom_point(aes(y = ch4flux_umolm2s_mean), colour = "red", alpha = 0.5) +
  scale_x_date(date_breaks = "6 months", date_labels = "%Y-%m") +
  ylab("CH4 daily mean") + xlab("") + ggtitle("U star filtering and cut-off not applied") +
  ylim(-0.045, 0.052)
p1/p2

#Now, let's run the function and create targets dataset with daily flux mean averaged over...
#..all the available half hourly values with the cut-off at 30!
#Use a predefined function to import the data
source("generate_EddyFLux_ghg_targets_function_with_cutoff.R")
targets_with_stable_mean <- generate_EddyFlux_ghg_targets_function(
  
  flux_current_data_file <- "https://raw.githubusercontent.com/CareyLabVT/Reservoirs/master/Data/DataNotYetUploadedToEDI/EddyFlux_Processing/EddyPro_Cleaned_L1.csv",
  flux_edi_data_file <- "https://pasta-s.lternet.edu/package/data/eml/edi/692/11/e0976e7a6543fada4cbf5a1bb168713b",
  met_current_data_file <- "https://raw.githubusercontent.com/FLARE-forecast/FCRE-data/fcre-metstation-data-qaqc/FCRmet_L1.csv",
  met_edi_data_file <- "https://pasta.lternet.edu/package/data/eml/edi/389/8/d4c74bbb3b86ea293e5c52136347fbb0")

#Create a csv file and save it!
write.csv(targets_with_stable_mean, "targets_with_stable_mean.csv")
#Only about 37% of data retained (half hours > 29) after all filters except u star threshold

#Plot the time series with the daily mean values
targets_with_stable_mean_wider <- targets_with_stable_mean %>%
  pivot_wider(names_from = variable, values_from = observation)

p3 <- ggplot(targets_with_stable_mean_wider, aes(x = as.Date(datetime)))+
  geom_point(aes(y = co2flux_umolm2s_mean), colour = "blue", alpha=0.5) +
  scale_x_date(date_breaks = "6 months", date_labels = "%Y-%m") +
  ylab("CO2 daily mean") + xlab("") + ggtitle("No U star filtering but cut-off applied at 30") +
  ylim(-20, 20)

p4 <- ggplot(targets_with_stable_mean_wider, aes(x = as.Date(datetime)))+
  geom_point(aes(y = ch4flux_umolm2s_mean), colour = "red", alpha = 0.5) +
  scale_x_date(date_breaks = "6 months", date_labels = "%Y-%m") +
  ylab("CH4 daily mean") + xlab("") + ggtitle("No U star filtering but cut-off applied at 30") +
  ylim(-0.045, 0.052)

p1/p3
p2/p4







