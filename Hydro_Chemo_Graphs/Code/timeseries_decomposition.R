
##### Timeseries Decomposition for Green Lake 4 Outlet variables of interest ####

library(tidyverse)
library(lubridate)
source("Hydro_Chemo_Graphs/Code/Functions/tsDecompose.R")

## call data and format
source("Hydro_Chemo_Graphs/Data/Call_Data.R") 

## filter water chemistry data 
Gl4stoich <- Gl4Chem |> #units for TDN, TDP are in umol/L
  mutate(TDN = as.numeric(TDN), # umol/L
         TDP = as.numeric(TDP), # umol/L
         NO3. = as.numeric(NO3.)) |> # ueq/L
  mutate(tdn.tdp = TDN/TDP) |>
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


## Create and save plots using the function sourced at beginning of script. 
ratio <- tsDecompose(Gl4Data, Gl4Data$tdn.tdp, "TDN:TDP (molar ratio)", "TDN_TDP")
tdn <- tsDecompose(Gl4Data, Gl4Data$TDN, "Total Dissolved N"~(mu*mol~L^-1), "TDN")
tdp <- tsDecompose(Gl4Data, Gl4Data$TDP, "Total Dissolved P"~(mu*mol~L^-1), "TDP")
streamflow <- tsDecompose(Gl4Data, Gl4Data$discharge_rate, "Streamflow" ~ (m^3~s^-1), "streamflow")

