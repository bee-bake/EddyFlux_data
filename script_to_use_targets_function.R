#Run the function and create targets dataset with daily flux mean averaged over...
#..all the available half hourly values without any cut-off
#Use a predefined function to import the data
library(patchwork)
library(readr)
source("generate_EddyFLux_ghg_targets_function_without_cutoff.R")
targets_without_stable_mean <- generate_EddyFlux_ghg_targets_function(
  
  flux_current_data_file <- "https://raw.githubusercontent.com/CareyLabVT/Reservoirs/master/Data/DataNotYetUploadedToEDI/EddyFlux_Processing/EddyPro_Cleaned_L1.csv",
  flux_edi_data_file <- "https://pasta-s.lternet.edu/package/data/eml/edi/692/11/e0976e7a6543fada4cbf5a1bb168713b",
  met_current_data_file <- "https://raw.githubusercontent.com/FLARE-forecast/FCRE-data/fcre-metstation-data-qaqc/FCRmet_L1.csv",
  met_edi_data_file <- "https://pasta.lternet.edu/package/data/eml/edi/389/8/d4c74bbb3b86ea293e5c52136347fbb0")

#Create a csv file and save it!
write.csv(targets_without_stable_mean, "targets_without_stable_mean.csv")

################################################################################
#What does the data availability after wind, u star and cut-off filter look like?
#Load the dataset after wind directions and u star filtering with Brenda's code
u_star_applied <- read_csv("./2024-05-10_EC_processed_withBDScript.csv")

#Extract the date column and add to the dataset
u_star_applied$date <- date(u_star_applied$DateTime)

#Remove the NAs separately for columns NEE and CH4
flux_count_NEE <- u_star_applied %>%
  select(date, DateTime, NEE_uStar_orig)%>%
  na.omit()

flux_count_CH4 <- u_star_applied %>%
  select(date, DateTime, ch4_flux_uStar_orig)%>%
  na.omit()

#Count the number of half hourly values and apply the cut-off
#For CO2
daily_NEE <- flux_count_NEE  %>%
  select(date, DateTime, NEE_uStar_orig)%>%
  group_by(date)%>% # average if there are more than one sample taken during that day
  summarise(frequency = n(),
            co2flux_umolm2s_mean = mean(NEE_uStar_orig)) %>% #count hh values and filter >=20 only
  #filter(frequency >= 20) %>%
  ungroup()

#Format as per FLARE 
targets_df_ustarCO2 <- daily_NEE %>%
  filter(frequency >= 20) %>%
  #ungroup()%>%
  drop_na(date)%>% # drop when we have timezone issues with daylight savings
  mutate(datetime=(paste0(date," ","00:00:00")))%>%
  #drop_na(datetime) %>%
  mutate(Reservoir='fcre')%>% # change the name to the the reservoir code for FLARE
  mutate(Depth_m = NA)%>%
  select(-date, frequency)%>%
  rename(site_id=Reservoir, # rename the columns for standard notation
         depth=Depth_m)%>%
  pivot_longer(cols=c(co2flux_umolm2s_mean), # make the wide data frame into a long one so each observation has a depth
               names_to='variable',
               values_to='observation')%>%
  select(c('datetime', 'site_id', 'depth', "observation", 'variable')) # rearrange order of columns
#This way we only have 20% of the CO2 data left to use for forecasting!!!

#For CH4
daily_CH4 <- flux_count_CH4  %>%
  select(date, DateTime, ch4_flux_uStar_orig) %>%
  group_by(date)%>% # average if there are more than one sample taken during that day
  summarise(frequency = n(),
            ch4flux_umolm2s_mean = mean(ch4_flux_uStar_orig)) %>% #count hh values and filter >=20 only
  #filter(frequency >= 20) %>%
  ungroup()

#Format as per FLARE 
targets_df_ustarCH4 <- daily_CH4 %>%
  filter(frequency >= 20) %>%
  drop_na(date)%>% # drop when we have timezone issues with daylight savings
  mutate(datetime=(paste0(date," ","00:00:00")))%>%
  mutate(Reservoir='fcre')%>% # change the name to the the reservoir code for FLARE
  mutate(Depth_m = NA)%>%
  select(-date, frequency)%>%
  rename(site_id=Reservoir, # rename the columns for standard notation
         depth=Depth_m)%>%
  pivot_longer(cols=c(ch4flux_umolm2s_mean), # make the wide data frame into a long one so each observation has a depth
               names_to='variable',
               values_to='observation')%>%
  select(c('datetime', 'site_id', 'depth', "observation", 'variable')) # rearrange order of columns
#This way we only have 16.5% of the CH4 data left to use for forecasting!!!

#Make the datasets wider
targets_df_ustarCO2_wider <- targets_df_ustarCO2 %>%
  pivot_wider(names_from = variable, values_from = observation)

targets_df_ustarCH4_wider <- targets_df_ustarCH4 %>%
  pivot_wider(names_from = variable, values_from = observation)

p1 <- ggplot(targets_df_ustarCO2_wider, aes(x = as.Date(datetime)))+
  geom_point(aes(y = co2flux_umolm2s_mean), colour = "blue", alpha = 0.5) +
  scale_x_date(date_breaks = "6 months", date_labels = "%Y-%m") +
  ylab("CO2 daily mean") + xlab("") + ggtitle("Wind, U star and cut-off all applied") +
  ylim(-20, 20)

p2 <- ggplot(targets_df_ustarCH4_wider, aes(x = as.Date(datetime)))+
  geom_point(aes(y = ch4flux_umolm2s_mean), colour = "red", alpha = 0.5) +
  scale_x_date(date_breaks = "6 months", date_labels = "%Y-%m") +
  ylab("CH4 daily mean") + xlab("") + ggtitle("Wind, U star and cut-off all applied") +
  ylim(-0.045, 0.052)

###############################################################################

#Plot the time series with the daily mean values
targets_without_stable_mean_wider <- targets_without_stable_mean %>%
pivot_wider(names_from = variable, values_from = observation)

p3 <- ggplot(targets_without_stable_mean_wider, aes(x = as.Date(datetime)))+
  geom_point(aes(y = co2flux_umolm2s_mean), colour = "blue", alpha=0.5) +
  scale_x_date(date_breaks = "6 months", date_labels = "%Y-%m") +
  ylab("CO2 daily mean") + xlab("") + ggtitle("U star filtering and cut-off not applied") +
  ylim(-20, 20)

p4 <- ggplot(targets_without_stable_mean_wider, aes(x = as.Date(datetime)))+
  geom_point(aes(y = ch4flux_umolm2s_mean), colour = "red", alpha = 0.5) +
  scale_x_date(date_breaks = "6 months", date_labels = "%Y-%m") +
  ylab("CH4 daily mean") + xlab("") + ggtitle("U star filtering and cut-off not applied") +
  ylim(-0.045, 0.052)

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

p5 <- ggplot(targets_with_stable_mean_wider, aes(x = as.Date(datetime)))+
  geom_point(aes(y = co2flux_umolm2s_mean), colour = "blue", alpha=0.5) +
  scale_x_date(date_breaks = "6 months", date_labels = "%Y-%m") +
  ylab("CO2 daily mean") + xlab("") + ggtitle("No U star filtering but cut-off applied at 20") +
  ylim(-20, 20)

p6 <- ggplot(targets_with_stable_mean_wider, aes(x = as.Date(datetime)))+
  geom_point(aes(y = ch4flux_umolm2s_mean), colour = "red", alpha = 0.5) +
  scale_x_date(date_breaks = "6 months", date_labels = "%Y-%m") +
  ylab("CH4 daily mean") + xlab("") + ggtitle("No U star filtering but cut-off applied at 20") +
  ylim(-0.045, 0.052)

p3/p5/p1
p4/p6/p2







