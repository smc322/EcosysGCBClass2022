
##### Timeseries Decomposition for Green Lake 4 Outlet variables of interest ####

library(tidyverse)
library(lubridate)
library(dataRetrieval)
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
  select(year, date, NO3., TDN, TDP, tdn.tdp) |>
  filter(tdn.tdp < 9000) # remove that one crazy outlier


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
no3 <- tsDecompose(Gl4Data, Gl4Data$NO3., "Nitrate"~(mu*eq~L^-1), "nitrate")


## Let's look at seasonality of the ratio and streamflow
### This doesn't really look like anything helpful
# ratio1 <- ratio |>
#   group_by(year(date), month(date)) |>
#   mutate(ave_szn = mean(season)) |>
#   ungroup() |>
#   rename(Date = date) |># must be named "Date" to work with the dataRetrieval function
#   #seq along dates starting with the beginning of your water year
#   mutate(CDate=as.Date(paste0(ifelse(month(Date) < 10, "1901", "1900"),
#                               "-", month(Date), "-", day(Date))))
# 
# ratio2 <- addWaterYear(ratio1) |>
#   select(waterYear, CDate, ave_szn) |>
#   mutate(CDate = floor_date(CDate, 'month')) |>
#   distinct() 
# 
# 
# ggplot(ratio2) +
#   geom_line(aes(CDate, ave_szn, group = waterYear, color = waterYear)) +
#   geom_point(aes(CDate, ave_szn, group = waterYear, color = waterYear)) +
#   stat_smooth(method = "loess", aes(CDate, ave_szn), color = "red4", se = FALSE) +
#   theme_light() +
#   scale_color_viridis_c("Water Year") +
#   scale_x_date(date_labels = "%b %d") +
#   labs(x = "") +
#   scale_y_continuous(
#     name = "TDN:TDP (molar ratio)")
# 
# 
# streamflow1 <- streamflow |>
#   group_by(year(date), month(date)) |>
#   mutate(ave_szn = mean(season)) |>
#   ungroup() |>
#   rename(Date = date) |># must be named "Date" to work with the dataRetrieval function
#   #seq along dates starting with the beginning of your water year
#   mutate(CDate=as.Date(paste0(ifelse(month(Date) < 10, "1901", "1900"),
#                               "-", month(Date), "-", day(Date))))
# 
# streamflow2 <- addWaterYear(streamflow1) |>
#   select(waterYear, CDate, ave_szn) |>
#   mutate(CDate = floor_date(CDate, 'month')) |>
#   distinct() 
# 
# 
# ggplot(streamflow2) +
#   geom_line(aes(CDate, ave_szn, group = waterYear, color = waterYear)) +
#   geom_point(aes(CDate, ave_szn, group = waterYear, color = waterYear)) +
#   stat_smooth(method = "loess", aes(CDate, ave_szn), color = "red4", se = FALSE) +
#   theme_light() +
#   scale_color_viridis_c("Water Year") +
#   scale_x_date(date_labels = "%b %d") +
#   labs(x = "") +
#   scale_y_continuous(
#     name = "Streamflow")
