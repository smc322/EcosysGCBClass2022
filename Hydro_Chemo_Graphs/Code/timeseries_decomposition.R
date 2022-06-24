
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


#check out timeseries trends for some of the other variables in the dataset
ph <- tsDecompose(Gl4Chem, Gl4Chem$pH, "pH", "pH")
cond <- tsDecompose(Gl4Chem, Gl4Chem$cond, "Specific Conductivity"~(mu*S~cm^-1), "cond")
NH4 <- tsDecompose(Gl4Chem, Gl4Chem$NH4., "Ammonium"~(mu*eq~L^-1), "NH4")
ca <- tsDecompose(Gl4Chem, Gl4Chem$Ca.., "Calcium" ~(mu*eq~L^-1), "calcium")
mg <- tsDecompose(Gl4Chem, Gl4Chem$Mg.., "Magnesium"~(mu*eq~L^-1), "mg")
na <- tsDecompose(Gl4Chem, Gl4Chem$Na., "Sodium"~(mu*eq~L^-1), "na")
k <- tsDecompose(Gl4Chem, Gl4Chem$K., "Potassium"~(mu*eq~L^-1), "k")
cl <- tsDecompose(Gl4Chem, Gl4Chem$Cl., "Chloride"~(mu*eq~L^-1), "cl")
So4 <- tsDecompose(Gl4Chem, Gl4Chem$SO4.., "Sulfate"~(mu*eq~L^-1), "so4")
po4 <- tsDecompose(Gl4Chem, Gl4Chem$PO4.., "Phosphate"~(mu*eq~L^-1), "po4")
si <- tsDecompose(Gl4Chem, Gl4Chem$Si, "Silica"~(mu*mol~L^-1), "mg")


# Other forms of N and P from the dataset
tn <- tsDecompose(Gl4Chem, Gl4Chem$TN, "Total N"~(mu*mol~L^-1), "TN")
tp <- tsDecompose(Gl4Chem, Gl4Chem$TP, "Total P"~(mu*mol~L^-1), "TP")
pn <- tsDecompose(Gl4Chem, Gl4Chem$PN, "Particulate N"~(mu*mol~L^-1), "PN")
pp <- tsDecompose(Gl4Chem, Gl4Chem$PP, "Particulate P"~(mu*mol~L^-1), "PP")
don <- tsDecompose(Gl4Chem, Gl4Chem$TN, "Dissolved Organic N"~(mu*mol~L^-1), "DON")
dop <- tsDecompose(Gl4Chem, Gl4Chem$TP, "Dissolved Organic P"~(mu*mol~L^-1), "DOP")
i_n <- tsDecompose(Gl4Chem, Gl4Chem$IN, "Inorganic N"~(mu*mol~L^-1), "IN")
i_p <- tsDecompose(Gl4Chem, Gl4Chem$IP, "Inorganic P"~(mu*mol~L^-1), "IP")

# Carbon
doc <- tsDecompose(Gl4Chem, Gl4Chem$DOC, "Dissolved Organic Carbon"~(mg~L^-1), "DOC")
toc <- tsDecompose(Gl4Chem, Gl4Chem$TOC, "Total Organic Carbon"~(mg~L^-1), "TOC")
poc <- tsDecompose(Gl4Chem, Gl4Chem$POC, "Particulate Organic Carbon"~(mg~L^-1), "POC")

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
