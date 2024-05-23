#Script to calculate fall turnover dates
#Developed by: Bibek Kandel
#Fall turnover was defined as the first day in autumn when the temperature at 1m was <1°C of
#the temperature measured at 8m (1 November 2020 and 3 November 2021; McClure et al., 2018). 
#Spring mixing was harder to identify due to intermittent ice-on in 2021 and frequent mixing 
#during the winter period, but we defined spring mixing as the first day in spring after complete
#ice-off when the temperature at 1m was <1°C of the temperature measured at 8m (26 February 2021 and 10 February 2022).

#Fall turnover
#For 2020
FCR_Catwalk_2018_2024 <- read_csv("./Data/FCR_Catwalk_2020May_2024April.csv")
fall_turnover <- FCR_Catwalk_2018_2024 |> 
  select(DateTime, ThermistorTemp_C_1, ThermistorTemp_C_8) |> 
  filter(year(DateTime) == "2020",
         month(DateTime) == "09" | month(DateTime) == "10" | month(DateTime) == "11") |> 
         mutate(turnover_date = ifelse((ThermistorTemp_C_1 - ThermistorTemp_C_8) < 1, format(DateTime, "%Y-%m-%d"), NA)) |>  
na.omit()  
min(fall_turnover$turnover_date)
max(fall_turnover$turnover_date)

#For 2021
fall_turnover <- FCR_Catwalk_2018_2024 |> 
  select(DateTime, ThermistorTemp_C_1, ThermistorTemp_C_8) |> 
  filter(year(DateTime) == "2021",
         month(DateTime) == "09" | month(DateTime) == "10" | month(DateTime) == "11") |> 
  mutate(turnover_date = ifelse((ThermistorTemp_C_1 - ThermistorTemp_C_8) < 1, format(DateTime, "%Y-%m-%d"), NA)) |>  
  na.omit()  
min(fall_turnover$turnover_date)
max(fall_turnover$turnover_date)

#For 2022
fall_turnover <- FCR_Catwalk_2018_2024 |> 
  select(DateTime, ThermistorTemp_C_1, ThermistorTemp_C_8) |> 
  filter(year(DateTime) == "2022",
         month(DateTime) == "09" | month(DateTime) == "10" | month(DateTime) == "11") |> 
  mutate(turnover_date = ifelse((ThermistorTemp_C_1 - ThermistorTemp_C_8) < 1, format(DateTime, "%Y-%m-%d"), NA)) |>  
  na.omit()  
min(fall_turnover$turnover_date)
max(fall_turnover$turnover_date)

#For 2023
fall_turnover <- FCR_Catwalk_2018_2024 |> 
  select(DateTime, ThermistorTemp_C_1, ThermistorTemp_C_8) |> 
  filter(year(DateTime) == "2023",
         month(DateTime) == "09" | month(DateTime) == "10" | month(DateTime) == "11") |> 
  mutate(turnover_date = ifelse((ThermistorTemp_C_1 - ThermistorTemp_C_8) < 1, format(DateTime, "%Y-%m-%d"), NA)) |>  
  na.omit()  
min(fall_turnover$turnover_date)
max(fall_turnover$turnover_date)

#Spring mixing
#For 2021
spring_mixing <- FCR_Catwalk_2018_2024 |>
  filter(DateTime >= "2021-02-23 00:00:00") |> #first day of ice off
  select(DateTime, ThermistorTemp_C_1, ThermistorTemp_C_8) |> 
  mutate(mixing_date = ifelse((ThermistorTemp_C_1 - ThermistorTemp_C_8) < 1, format(DateTime, "%Y-%m-%d"), NA)) |>  
  na.omit()  
min(spring_mixing$mixing_date)
max(spring_mixing$mixing_date)

#For 2022
spring_mixing <- FCR_Catwalk_2018_2024 |>
  filter(DateTime >= "2022-02-10 00:00:00") |> #first day of ice off
  select(DateTime, ThermistorTemp_C_1, ThermistorTemp_C_8) |> 
  mutate(mixing_date = ifelse((ThermistorTemp_C_1 - ThermistorTemp_C_8) < 1, format(DateTime, "%Y-%m-%d"), NA)) |>  
  na.omit()  
min(spring_mixing$mixing_date)
max(spring_mixing$mixing_date)

#For 2023
spring_mixing <- FCR_Catwalk_2018_2024 |>
  filter(DateTime >= "2023-02-05 00:00:00") |> #first day of ice off
  select(DateTime, ThermistorTemp_C_1, ThermistorTemp_C_8) |> 
  mutate(mixing_date = ifelse((ThermistorTemp_C_1 - ThermistorTemp_C_8) < 1, format(DateTime, "%Y-%m-%d"), NA)) |>  
  na.omit()  
min(spring_mixing$mixing_date)
max(spring_mixing$mixing_date)

#For 2024
spring_mixing <- FCR_Catwalk_2018_2024 |>
  filter(DateTime >= "2024-01-26 00:00:00") |> #first day of ice off
  select(DateTime, ThermistorTemp_C_1, ThermistorTemp_C_8) |> 
  mutate(mixing_date = ifelse((ThermistorTemp_C_1 - ThermistorTemp_C_8) < 1, format(DateTime, "%Y-%m-%d"), NA)) |>  
  na.omit()  
min(spring_mixing$mixing_date)
max(spring_mixing$mixing_date)























