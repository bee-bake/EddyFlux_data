#First version by Bibek Kandel on May 24, 2024
#This script uses ice_binary data output from ice_binary_qaqc.R
#Where do fluxes during ice cover come from?
library(ggpubr)
library(tidyverse)
#Load the ice data
Ice_2020_2024 <- read_csv("C:/Users/13188/Desktop/Data_repository/ice_L1.csv")

#Find the beginning and end of ice-on off periods
Ice_2020_2024$DOY <- yday(Ice_2020_2024$datetime) #find DOY

#Create a new table to store values later
Ice_period <- tibble(ice_on_date = rep(NA, times = (length(Ice_2020_2024$datetime)/2)), 
                        days_of_ice = rep(NA, times = (length(Ice_2020_2024$datetime)/2)))

#Loop to calculate the period of continuous ice-on at a time
j=0
for (i in seq(2, length(Ice_2020_2024$datetime), 2)) {
  j=j+1
  Ice_period$days_of_ice[j] = Ice_2020_2024$DOY[i] - Ice_2020_2024$DOY[i-1]  
  Ice_period$ice_on_date[j] = as.character(Ice_2020_2024$datetime[i-1]) 
}

#Find a way to replace negative DOY calculations; here I am replacing manually
Ice_period[c(18), 2] <- 9

#Define intermittent ice-on as ice periods for less than a day and continuous ice-on for the rest
Ice_period$ice_status <- ifelse(Ice_period$days_of_ice < 1, "Intermittent", "Continuous")

#Format datetime
Ice_period$ice_on_date = ifelse(nchar(Ice_period$ice_on_date) > 11,Ice_period$ice_on_date, paste0(Ice_period$ice_on_date," 00:00:00"))

#Bring in EC data 
ec <- read_csv("C:/Users/13188/Desktop/Data_repository/2024-06-01_EC_processed_withBDScript_for_IceAnalysis.csv") 

ec2 <- ec %>% 
  mutate(DateTime = as.POSIXct(DateTime, "%Y-%m-%dT%H:%M:%SZ", tz = "EST")) %>% 
  filter(DateTime >= as.POSIXct("2020-05-01 01:00:00") & DateTime < as.POSIXct("2024-05-01 01:00:00"))

#Create a dataset with all the time periods continuous ice cover
Ice_period |> filter(ice_status == "Continuous")
datetime_all <- NULL

for (k in 1:length(Ice_period$ice_on_date)) {
  datetime = seq.POSIXt(as.POSIXct(Ice_period$ice_on_date[k]), length.out = 24*6*(Ice_period$days_of_ice[k]), by="10 min")

  datetime <- data.frame(datetime)
  datetime_all <- bind_rows(datetime_all, datetime)
}

#Select the dates from EC dataset that match datetime of ice cover
flux_during_ice <- ec2[ec2$DateTime %in% datetime_all$datetime, ]
flux_during_no_ice <- ec2[ !(ec2$DateTime %in% datetime_all$datetime), ]

#To make our results comparable to existing wind-based models, k600 was related to wind speed
#at 10 m height (U10). To obtain U10, sonic wind speed measured at height zm=3.9 m was 
#extrapolated to 10 m height using the logarithmic wind law (Scholz, K., 2021)
#U10 = u*ln(10/z0)*1/(ln(zm/z0))
#where z0 represents the surface roughness length, which was set to 0.0001m (Vihma & Savijärvi, 1991).

#Select columns that you're interested in
flux_during_ice_U10 <- flux_during_ice |> 
  select(DateTime, NEE_uStar_orig, u) |>
  mutate(U10 = u*log(10/0.0001)*(1/log(2.9/0.0001))) |>
  na.omit() |> 
  mutate(wind_category = case_when(U10 > 5 ~ "Very High",
                                  U10 >= 4 ~ "High",
                                  U10 >= 3 ~ "Medium",
                                  U10 >= 2 ~ "Low",
                                  U10 < 2 ~ "Very Low"))

flux_during_ice_U10$wind_category <- factor(flux_during_ice_U10$wind_category, 
                                            levels=c("Very Low", "Low", "Medium", "High", "Very High"))
#Plot
ggplot(flux_during_ice_U10, aes(x=U10, y=NEE_uStar_orig))+
  #geom_boxplot(aes(x=wind_category, y=NEE_uStar_orig)) + ylim(-2,3)
  geom_point()+ geom_hline(yintercept=0, col = "red", lty = 2, lwd = 0.7)+
  geom_smooth()


####Filter U10 values greater than 4
flux_during_ice_low <- flux_during_ice |> 
  select(DateTime, NEE_uStar_orig, u) |>
  mutate(U10 = u*log(10/0.0001)*(1/log(2.9/0.0001))) |>
  filter(U10 <= 4) |> 
  na.omit()

flux_during_ice_high <- flux_during_ice |> 
  select(DateTime, NEE_uStar_orig, u) |>
  mutate(U10 = u*log(10/0.0001)*(1/log(2.9/0.0001))) |>
  filter(U10 > 4) |> 
  na.omit()

##########
flux_during_no_ice_U10 <- flux_during_no_ice |> 
  select(DateTime, NEE_uStar_orig, u) |>
  mutate(U10 = u*log(10/0.0001)*(1/log(2.9/0.0001))) |>
  na.omit() |> 
  mutate(wind_category = case_when(U10 > 5 ~ "Very High",
                                   U10 >= 4 ~ "High",
                                   U10 >= 3 ~ "Medium",
                                   U10 >= 2 ~ "Low",
                                   U10 < 2 ~ "Very Low"))

flux_during_no_ice_U10$wind_category <- factor(flux_during_no_ice_U10$wind_category, 
                                            levels=c("Very Low", "Low", "Medium", "High", "Very High"))

ggplot(flux_during_no_ice_U10, aes(x=U10, y=NEE_uStar_orig))+
  #geom_boxplot(aes(x=wind_category, y=NEE_uStar_orig)) + ylim(-2,3)
  geom_point()+ geom_hline(yintercept=0, col = "red", lty = 2, lwd = 0.7)+
  geom_smooth()

###Filter U10 values greater than 4
flux_during_no_ice_low <- flux_during_no_ice |> 
  select(DateTime, NEE_uStar_orig, u) |> 
  mutate(U10 = u*log(10/0.0001)*(1/log(2.9/0.0001))) |>
  filter(U10 <= 4) |> 
  na.omit()

flux_during_no_ice_high <- flux_during_no_ice |> 
  select(DateTime, NEE_uStar_orig, u) |> 
  mutate(U10 = u*log(10/0.0001)*(1/log(2.9/0.0001))) |>
  filter(U10 > 4) |> 
  na.omit()

p1 <- ggplot()+
  geom_boxplot(data = flux_during_ice_low, aes(x = U10, y = NEE_uStar_orig))+
  geom_boxplot(data = flux_during_ice_high, aes(x = U10, y = NEE_uStar_orig))+
  ylim(-4,4)+
  ylab(expression(paste("CO"[2]*" (",mu,"mol m"^-2*" s"^-1*")")))+
  xlab("U10 (m/s)")+
  labs(title="Ice On (n = 4608)")+
  annotate("text", x = 1, y=-7, label = "U10 < 4")+
  annotate("text", x = 4.5, y=-7, label = "U10 > 4")+
  geom_hline(yintercept=0, col = "red", lty = 2, lwd = 0.7)

p2 <- ggplot()+
  geom_boxplot(data = flux_during_no_ice_low, aes(x = U10, y = NEE_uStar_orig))+
  geom_boxplot(data = flux_during_no_ice_high, aes(x = U10, y = NEE_uStar_orig))+
  ylim(-4,4)+
  ylab(expression(paste("CO"[2]*" (",mu,"mol m"^-2*" s"^-1*")")))+
  xlab("U10 (m/s)")+
  labs(title="Ice Off (n = 12000)")+
  annotate("text", x = 0.8, y=-7, label = "U10 < 4")+
  annotate("text", x = 5, y=-7, label = "U10 > 4")+
  geom_hline(yintercept=0, col = "red", lty = 2, lwd = 0.7)

ggarrange(p1,p2,ncol=2,nrow=1,labels=c("A.","B."),font.label = list(face="plain",size=15))

#################################################################
#Check the humidity distribution at hourly timescales
# Select data every 30 minutes from Jan 2020 to end of met data
ec_humidity <- ec2 |> 
  select(DateTime, rH) |> 
  na.omit()

#Select the dates from EC dataset that match datetime of ice cover
rH_during_ice <- ec_humidity[ec_humidity$DateTime %in% datetime_all$datetime, ]
rH_during_no_ice <- ec_humidity[ !(ec_humidity$DateTime %in% datetime_all$datetime), ]

######################
#During Ice On period
#Create breaks at every hour 
rH_during_ice$Breaks <- cut(rH_during_ice$DateTime,breaks = "1 hour",right=FALSE)
rH_during_ice$Breaks <- ymd_hms(as.character(rH_during_ice$Breaks))

# Average data each hour
hourly_humidity_iceOn <- rH_during_ice %>% 
  group_by(Breaks) %>% 
  na.omit() %>%
  summarise(mean_rH = mean(rH))%>%
  rename("DateTime" = "Breaks")

#Normalize the dataset with a mean=0 and sd=1
hourly_humidity_iceOn <- hourly_humidity_iceOn %>% 
  mutate(normalized_rH = scale(mean_rH))

#Plot the distribution
#Resource: https://www.appsilon.com/post/ggplot2-histograms
#calculate skewness and kurtosis index
library(moments)
Skq <- skewness(hourly_humidity_iceOn$normalized_rH)
#Skewness is a measure of the asymmetry of a distribution.

Kur <- kurtosis(hourly_humidity_iceOn$normalized_rH)
#If a given distribution has a kurtosis less than 3, it is said to be playkurtic, 
#which means it tends to produce fewer and less extreme outliers than the normal distribution.

p1 <- ggplot(hourly_humidity_iceOn, aes(normalized_rH)) +
  geom_histogram(aes(y=..density..), breaks=seq(-3,2,0.05), alpha=1, 
                 position="identity", lwd=0.5, color = "#000000", fill = "#0099F8") +
  ggtitle("Humidity distribution - IceOn")+
  geom_density(color = "#000000", fill = "lightblue", alpha = 0.6)+
  annotate("text", x = -1, y=2, label = "Skq = -0.66 kurtosis = 2.61")

###########################
#During Ice Off period

#Create breaks at every hour 
rH_during_no_ice$Breaks <- cut(rH_during_no_ice$DateTime,breaks = "1 hour",right=FALSE)
rH_during_no_ice$Breaks <- ymd_hms(as.character(rH_during_no_ice$Breaks))

# Average data each hour
hourly_humidity_iceOff <- rH_during_no_ice %>% 
  group_by(Breaks) %>% 
  na.omit() %>%
  summarise(mean_rH = mean(rH))%>%
  rename("DateTime" = "Breaks")

#Normalize the dataset with a mean=0 and sd=1
hourly_humidity_iceOff <- hourly_humidity_iceOff %>% 
  mutate(normalized_rH = scale(mean_rH))

#Plot the distribution
#Resource: https://www.appsilon.com/post/ggplot2-histograms
#calculate skewness and kurtosis index
Skq <- skewness(hourly_humidity_iceOff$normalized_rH)
#Skewness is a measure of the asymmetry of a distribution.

Kur <- kurtosis(hourly_humidity_iceOff$normalized_rH)
#If a given distribution has a kurtosis less than 3, it is said to be playkurtic, 
#which means it tends to produce fewer and less extreme outliers than the normal distribution.

p2 <- ggplot(hourly_humidity_iceOff, aes(normalized_rH)) +
  geom_histogram(aes(y=..density..), breaks=seq(-3,2,0.05), alpha=1, 
                 position="identity", lwd=0.5, color = "#000000", fill = "#0099F8") +
  ggtitle("Humidity distribution - IceOff")+
  geom_density(color = "#000000", fill = "lightblue", alpha = 0.5)+
  annotate("text", x = -1.3, y=2, label = "Skq = -0.66 kurtosis = 2.67")

ggarrange(p1,p2,ncol=2,nrow=1,labels=c("A.","B."),font.label = list(face="plain",size=15))

################################################
#Use skewness function to calculate the range of skewness for all flux values
#skewness <-  function(x) {
  #m3 <- mean((x - mean(x))^3)
  #skewness <- m3/(sd(x)^3)
  #skewness}

#For periods with ice-on
#Select required columns
hourly_fluxes_iceOn <- flux_during_ice %>%
  select(DateTime, rH, u, NEE_uStar_orig) %>%
  na.omit()%>%
  mutate(normalized_flux = scale(NEE_uStar_orig))

mean_rH <- mean(hourly_fluxes_iceOn$rH, na.rm = TRUE)
sd_rH <- sd(hourly_fluxes_iceOn$rH, na.rm = TRUE)

#Calculate the skewness
hourly_fluxes_iceOn <- hourly_fluxes_iceOn |> 
  select(DateTime, rH, u, normalized_flux,NEE_uStar_orig) |>
  na.omit() |> 
  mutate(standardize = (rH - mean_rH)^3,
         Sk = (standardize)/sd_rH^3)

######################
#Binned data analysis
#Resources: https://lost-stats.github.io/Presentation/Figures/binscatter.html
#Data binning
hourly_fluxes_iceOn <- hourly_fluxes_iceOn |> 
  mutate(wind_cateory = case_when(u > 5 ~ "Very High",
                                  u >= 4 ~ "High",
                                  u >= 3 ~ "Medium",
                                  u >= 2 ~ "Low",
                                  u < 2 ~ "Very Low"))
#This will create 10 quantiles using y and assign the observations in each quantile to a separate bin
df <- hourly_fluxes_iceOn %>% 
  filter(Sk < -5 & Sk >= -15) %>%
  mutate(bin = ntile(NEE_uStar_orig, n=10))

new_df <- df %>% 
  group_by(bin) %>% 
  summarise(xmean = mean(Sk), ymean = mean(NEE_uStar_orig), #find the x and y mean of each bin
            u_xmean = mean(Sk), windspeed = mean(NEE_uStar_orig), #find the x and y mean of each bin
            xmin = min(Sk), xmax = max(Sk),
            ymin = min(NEE_uStar_orig), ymax = max(NEE_uStar_orig))


bp1 <- ggplot(new_df, aes(x=xmean, y=ymean, colour = windspeed)) + 
  geom_point(size = 3, shape = 19)+
  geom_errorbar(aes(ymin = ymin, ymax = ymax))+
  geom_errorbar(aes(xmin = xmin, xmax = xmax))+
  scale_colour_gradient(low = "green", high = "red")+
  ylab(expression(paste("CO"[2]*" (",mu,"mol m"^-2*" s"^-1*")")))+
  xlab("Skq") + ggtitle("Ice On")+
  xlim(-10,-5)

#Plot
p1 <- ggplot(hourly_fluxes_iceOn, aes(x=Sk, y=NEE_uStar_orig, color = u))+
  ggtitle("Ice On")+
  geom_point()+scale_colour_gradient(low = "green", high = "red")+
  geom_point(data = new_df, aes(x=xmean, y=ymean), shape=1, color = "red")+
  ylab(expression(paste("CO"[2]*" (",mu,"mol m"^-2*" s"^-1*")")))+
  xlab("Skq")

########################################################

#For periods with ice-off
#Select required columns
hourly_fluxes_iceOff <- flux_during_no_ice %>%
  select(DateTime, rH, u, NEE_uStar_orig) %>%
  na.omit()%>%
  mutate(normalized_flux = scale(NEE_uStar_orig))

mean_rH <- mean(hourly_fluxes_iceOff$rH, na.rm = TRUE)
sd_rH <- sd(hourly_fluxes_iceOff$rH, na.rm = TRUE)

#Calculate the skewness
hourly_fluxes_iceOff <- hourly_fluxes_iceOff |> 
  select(DateTime, rH, u, normalized_flux,NEE_uStar_orig) |>
  na.omit() |> 
  mutate(standardize = (rH - mean_rH)^3,
         Sk = (standardize)/sd_rH^3)


# This will create 10 quantiles using y and assign the observations in each quantile to a separate bin
df <- hourly_fluxes_iceOff %>% 
  filter(Sk < -5 & Sk >= -15) %>%
  mutate(bin = ntile(NEE_uStar_orig, n=10))

new_df <- df %>% 
  group_by(bin) %>% 
  summarise(xmean = mean(Sk), ymean = mean(NEE_uStar_orig), #find the x and y mean of each bin
            u_xmean = mean(Sk), windspeed = mean(NEE_uStar_orig), #find the x and y mean of each bin
            xmin = min(Sk), xmax = max(Sk),
            ymin = min(NEE_uStar_orig), ymax = max(NEE_uStar_orig))


bp2 <- ggplot(new_df, aes(x=xmean, y=ymean, colour = windspeed)) + 
  geom_point(size = 3, shape = 19)+
  geom_errorbar(aes(ymin = ymin, ymax = ymax))+
  geom_errorbar(aes(xmin = xmin, xmax = xmax))+
  scale_colour_gradient(low = "green", high = "red")+
  ylim(-10,10)+
  ylab(expression(paste("CO"[2]*" (",mu,"mol m"^-2*" s"^-1*")")))+
  xlab("Skq") + ggtitle("Ice Off") + xlim(-10,-5)

#Plot
p2 <- ggplot(hourly_fluxes_iceOff, aes(x=Sk, y=NEE_uStar_orig, color = u))+
  ggtitle("Ice Off")+
  ylim(-10,10)+
  ylab(expression(paste("CO"[2]*" (",mu,"mol m"^-2*" s"^-1*")")))+
  xlab("Skq")+
  geom_point()+scale_colour_gradient(low = "green",
                                     high = "red")+
  geom_hline(yintercept=10, linetype="solid", color = "purple", linewidth=1)+
  geom_hline(yintercept=-10, linetype="solid", color = "purple", linewidth=1)+
  geom_point(data = new_df, aes(x=xmean, y=ymean), shape=1, color = "red")

ggarrange(p1,p2,ncol=2,nrow=1,labels=c("A.","B."),common.legend = TRUE, font.label = list(face="plain",size=15))
ggarrange(bp1,bp2,ncol=2,nrow=1,labels=c("A.","B."),common.legend = TRUE, font.label = list(face="plain",size=15))

#About 11% of the data during ice-on has wind speed above the medium range (u>=3), which is the
#threshold when skewness starts accelerating. Should we filter our data by wind speed owing to 
#the footprint contribution from outside the reservoir?

################################
gap_unfilled_ec <- read.csv("C:/Users/13188/Desktop/Data_repository/Eddy_fcr_footprint_full.csv")

rH_during_ice <- gap_unfilled_ec[gap_unfilled_ec$datetime %in% datetime_all$datetime, ]
rH_during_no_ice <- gap_unfilled_ec[ !(gap_unfilled_ec$datetime %in% datetime_all$datetime), ]

Quad_analysis_ice <- rH_during_ice %>%
  select(datetime, co2_flux_umolm2s, w_var_ms)%>%
  na.omit()%>%
  mutate(quadrants = case_when(co2_flux_umolm2s > 0 & w_var_ms > 0 ~ "Q1",
                               co2_flux_umolm2s < 0 & w_var_ms > 0 ~ "Q2",
                               co2_flux_umolm2s < 0 & w_var_ms < 0 ~ "Q3",
                               co2_flux_umolm2s > 0 & w_var_ms < 0 ~ "Q4")) |> 
  mutate(wc_abs = abs(co2_flux_umolm2s*w_var_ms),
         wc = co2_flux_umolm2s*w_var_ms,
         H = wc_abs/wc_abs_mean)

wc_abs_mean <- mean(Quad_analysis_ice$wc_abs)
wc_mean <- mean(Quad_analysis_ice$wc)

Quad_analysis_ice <- Quad_analysis_ice %>%
  filter(quadrants == "Q1" & H >= 0)%>%
  mutate(flux_frac = wc/wc_mean)

Quad_analysis_ice$H_conditional = scale(Quad_analysis_ice$co2_flux_umolm2s)

ggplot(Quad_analysis_ice, aes(x=H, y=H_conditional))+
  geom_point()












