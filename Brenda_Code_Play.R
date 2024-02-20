library(tidyverse)
library(lubridate)


# Read compiled file: From Eddy Pro using basic processing
# Original file from Brenda on 11 May 2021
# Light data cleaning using EddyPro_CleanUp.R

dt1 <-read_csv("https://raw.githubusercontent.com/CareyLabVT/Reservoirs/master/Data/DataNotYetUploadedToEDI/EddyFlux_Processing/EddyPro_Cleaned_L1.csv")

#Read in met data 
met_all <- read.csv("FCRmet.csv")
#Remove all empty rows
met_all2 <- met_all[complete.cases(met_all), ]
#Extract date only
met_all2$Date <- as.Date(met_all2$TIMESTAMP)
#Extract time only
met_all2 <- met_all2 %>% 
  mutate(
    date = as.Date(TIMESTAMP),
    hour = hour(TIMESTAMP),
    minute = minute(TIMESTAMP)
  ) %>% 
  mutate(
    format_date = format(date, "%Y/%m/%d"),
    format_hour = paste(hour, minute, sep = ":"),
    format_hour = format(format_hour, "%H:%M")
  )

# read in historical data file 
# EDI
inUrl1 <- "https://pasta.lternet.edu/package/data/eml/edi/1061/2/f837d12dc12ab37a6772598578875e00"
infile1 <- tempfile()
try(download.file(inUrl1,infile1,method="curl"))
if (is.na(file.size(infile1))) download.file(inUrl1,infile1,method="auto")

# read in the data file downloaded from EDI
dt2 <-read_csv(infile1) 

#Bind rows from ec file to the dt1 file
ec <- dt2%>%
  bind_rows(.,dt1)

# Format time
ec$datetime <- as.POSIXct(paste(ec$date, ec$time), format="%Y-%m-%d %H:%M", tz="EST")
ec$datetime <- as_datetime(ec$datetime)

# Set new dataframe with list of dates+times:
# every 30 minutes
# Constrain to study time period: 2020-04-05 (time series start date) to
# last 30 minute period - UPDATE WITH EACH NEW SET OF DATA!
ts <- seq.POSIXt(as.POSIXct("2020-04-05 00:00:00",'%Y-%m-%d %H:%M:%S', tz="EST"), 
                 as.POSIXct("2024-01-29 09:30:00",'%Y-%m-%d %H:%M:%S', tz="EST"), by = "30 min")
ts2 <- data.frame(datetime = ts)

# Join Eddy Flux data with list of dates+time
ec2 <- merge(ts2, ec, by = 'datetime', all.x=T)

#Take out duplicated data points
ec2<-ec2[!duplicated(ec2$datetime),]

#Make sure time stamps are okay!
ec2 %>% group_by(year = year(datetime), month = factor(month.abb[month(datetime)], levels = c("Apr", "May", "Jun",
                                                                                              "Jul", "Aug", "Sep", "Oct", 'Nov', 
                                                                                              'Dec', 'Jan', 'Feb', 'Mar')), 
                 hour = hour(datetime)) %>% 
  summarise(air_temperature = mean(air_temperature_k, na.rm = TRUE)) %>% 
  ggplot(aes(hour, air_temperature, col = factor(year))) + geom_point() + 
  facet_wrap(~month) + theme_bw() + ylab('Air temperature') + xlab("Hours of day") +
  scale_color_brewer(palette = "Dark2")

#################################################################
# Count how many initial NAs are in CO2 and CH4 data
# Without any data processing!
#################################################################
sta<-ec2 %>% select(datetime, co2_flux_umolm2s, ch4_flux_umolm2s) %>% 
  summarise(co2_available = 100-sum(is.na(co2_flux_umolm2s))/n()*100,
            ch4_available = 100-sum(is.na(ch4_flux_umolm2s))/n()*100)

# Check data availability by month
ec2 %>% group_by(year(datetime), month(datetime)) %>% select(datetime, co2_flux_umolm2s, ch4_flux_umolm2s) %>% 
  summarise(co2_available = 100-sum(is.na(co2_flux_umolm2s))/n()*100,
            ch4_available = 100-sum(is.na(ch4_flux_umolm2s))/n()*100)


#Plot the timeseries of flux values
ggplot(ec2, aes(x=datetime, y=ch4_flux_umolm2s)) +
geom_point()

ggplot(ec2, aes(x=datetime, y=co2_flux_umolm2s)) +
  geom_point()

# Sum met data to the 30 min mark (for Total Rain and Total PAR)
met_30_rain <- met_all %>% 
  select(Rain_mm_Tot,PAR_Tot_Tot,Breaks) %>% 
  group_by(Breaks) %>% 
  summarise_all(sum,na.rm=TRUE)

# Combine averaged and summed data together
met_30_2 <- cbind.data.frame(met_30,met_30_rain)
















