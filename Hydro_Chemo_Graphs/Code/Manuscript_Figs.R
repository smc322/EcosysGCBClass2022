## call functions
source("Hydro_Chemo_Graphs/Code/Functions/chemo_hydrograph.R")
source("Hydro_Chemo_Graphs/Code/Functions/wateryearplot.R")
source("Hydro_Chemo_Graphs/Code/Functions/cQ_graph.R")

library(tidyverse)
library(lubridate)



## call data and format
source("Hydro_Chemo_Graphs/Data/Call_Data.R") 

## filter water chemistry data 
Gl4stoich <- Gl4Chem |> #units for TDN, TDP are in umol/L
  mutate(TDN = as.numeric(TDN),
         TDP = as.numeric(TDP)) |>
  #mutate(tntp = as.numeric(TN)/as.numeric(TP)) |>
  mutate(tdn.tdp = TDN/TDP) |>
         #don.dop = as.numeric(DON)/as.numeric(DOP)) |>
  filter(!is.na(tdn.tdp),
         TDP > 0) |>
  select(year, date, TDN, TDP, tdn.tdp) 
  

## filter discharge data
Gl4Dis_a <- Gl4Dis |>
  mutate(year = year(date)) |>
  filter(year!= 2014,
         !is.na(discharge_rate)) |> # possible problems with discharge data in 2014 - personal correspondance with Sarah from Niwot LTER
  select(year, date, discharge_rate) #cms

Gl4Data <- full_join(Gl4stoich, Gl4Dis_a)


chemo_hydrograph(Gl4Data, Gl4Data$tdn.tdp, "TDN:TDP (molar ratio)", Gl4Data) 
chemo_hydrograph(Gl4Data, Gl4Data$TDN, "Total Dissolved N"~(mu~mol~L^-1), Gl4Data) 
chemo_hydrograph(Gl4Data, Gl4Data$TDP, "Total Dissolved P"~(mu~mol~L^-1), Gl4Data) 


## adding water year 
Gl4Data_a <- Gl4Data |>
  rename(Date = date) |># must be named "Date" to work with the dataRetrieval function
  #seq along dates starting with the beginning of your water year
  mutate(CDate=as.Date(paste0(ifelse(month(Date) < 10, "1901", "1900"),
                              "-", month(Date), "-", day(Date))))

Gl4Data_b <- addWaterYear(Gl4Data_a) # dataRetrieval fuction



wateryearplot(Gl4Data, Gl4Data$tdn.tdp, "TDN:TDP")
wateryearplot(Gl4Data, Gl4Data$discharge_rate, "Streamflow" ~ (m ^ 3 ~ s ^ -1))
wateryearplot(Gl4Data, Gl4Data$TDN, "Total Dissolved N"~(mu~mol~L^-1))
wateryearplot(Gl4Data, Gl4Data$TDP, "Total Dissolved P"~(mu~mol~L^-1))
