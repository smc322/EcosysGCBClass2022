## call functions
source("Hydro_Chemo_Graphs/Code/Functions/chemo_hydrograph.R")
source("Hydro_Chemo_Graphs/Code/Functions/wateryearplot.R")
source("Hydro_Chemo_Graphs/Code/Functions/cQ_graph.R")
source("Hydro_Chemo_Graphs/Code/Functions/tsDecompose.R")

library(tidyverse)
library(lubridate)



## call data and format
source("Hydro_Chemo_Graphs/Data/Call_Data.R") 

## filter water chemistry data 
Gl4stoich <- Gl4Chem |> #units for TDN, TDP are in umol/L
  mutate(TDN = as.numeric(TDN),
         TDP = as.numeric(TDP),
         NO3. = as.numeric(NO3.)) |>
  #mutate(tntp = as.numeric(TN)/as.numeric(TP)) |>
  mutate(tdn.tdp = TDN/TDP) |>
         #don.dop = as.numeric(DON)/as.numeric(DOP)) |>
  filter(!is.na(tdn.tdp),
         TDP > 0) |>
  select(year, date, NO3., TDN, TDP, tdn.tdp) 
  

## filter discharge data
Gl4Dis_a <- Gl4Dis |>
  mutate(year = year(date)) |>
  filter(year!= 2014,
         !is.na(discharge_rate)) |> # possible problems with discharge data in 2014 - personal correspondence with Sarah from Niwot LTER
  select(year, date, discharge_rate) #cms
  

Gl4Data <- full_join(Gl4stoich, Gl4Dis_a) |>
  arrange(date)


chemo_hydrograph(Gl4Data, Gl4Data$tdn.tdp, "TDN:TDP (molar ratio)", Gl4Data) +
  stat_smooth(aes(Gl4Data$date, Gl4Data$tdn.tdp)) +
  ylim(0,5000)
chemo_hydrograph(Gl4Data, Gl4Data$TDN, "Total Dissolved N"~(mu*mol~L^-1), Gl4Data) +
  stat_smooth(aes(Gl4Data$date, Gl4Data$TDN))
chemo_hydrograph(Gl4Data, Gl4Data$TDP, "Total Dissolved P"~(mu*mol~L^-1), Gl4Data) +
  stat_smooth(aes(Gl4Data$date, Gl4Data$TDP))
chemo_hydrograph(Gl4Data, Gl4Data$NO3., "Nitrate"~(mu*eq~L^-1), Gl4Data) +
  stat_smooth(aes(Gl4Data$date, Gl4Data$NO3.))
wateryearplot(Gl4Data, Gl4Data$tdn.tdp, "TDN:TDP")
wateryearplot(Gl4Data, Gl4Data$discharge_rate, "Streamflow" ~ (m^3~s^-1))
wateryearplot(Gl4Data, Gl4Data$TDN, "Total Dissolved N"~(mu*mol~L^-1))
wateryearplot(Gl4Data, Gl4Data$TDP, "Total Dissolved P"~(mu*mol~L^-1))
wateryearplot(Gl4Data, Gl4Data$NO3., "Nitrate"~(mu*eq~L^-1)) 

## adding water year 
Gl4Data_a <- Gl4Data |>
  rename(Date = date) |># must be named "Date" to work with the dataRetrieval function
  #seq along dates starting with the beginning of your water year
  mutate(CDate=as.Date(paste0(ifelse(month(Date) < 10, "1901", "1900"),
                              "-", month(Date), "-", day(Date))))

Gl4Data_b <- addWaterYear(Gl4Data_a) # dataRetrieval fuction


# ggplot(Gl4Data_b)+
#   geom_line(aes(CDate, tdn.tdp, group = waterYear, color = waterYear)) +
#   geom_point(aes(CDate, chemVar, group = waterYear, color = waterYear)) +
#   stat_smooth(method = "loess", aes(CDate, chemVar), color = "red", se = FALSE) +
#   theme_light() +
#   scale_color_viridis_c() +
#   scale_x_date(date_labels = "%b %d") +
#   labs(x = "") +
#   scale_y_continuous(
#     name = name_units
#   ) 



## timeseries decomposition 


ratio <- tsDecompose(Gl4Data, Gl4Data$tdn.tdp, "TDN:TDP (molar ratio)", "TDN_TDP")
tdn <- tsDecompose(Gl4Data, Gl4Data$TDN, "Total Dissolved N"~(mu*mol~L^-1), "TDN")
tdp <- tsDecompose(Gl4Data, Gl4Data$TDP, "Total Dissolved P"~(mu*mol~L^-1), "TDP")
streamflow <- tsDecompose(Gl4Data, Gl4Data$discharge_rate, "Streamflow" ~ (m^3~s^-1), "streamflow")
