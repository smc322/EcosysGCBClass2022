### creating graphs with multiple years of data ###

library(tidyverse)
library(lubridate)

source("Hydro_Chemo_Graphs/Code/Functions/wateryearplot.R")

## prepare the data
source("Hydro_Chemo_Graphs/Data/Call_Data.R") 

Gl4stoich <- Gl4Chem |>
  # mutate(tntp = as.numeric(TN)/as.numeric(TP)) |>
  mutate(tdn.tdp = as.numeric(TDN)/as.numeric(TDP),
         don.dop = as.numeric(DON)/as.numeric(DOP)) |>
  filter(year(Date) != 2014)
  

  
lochvale <- read.csv("Hydro_Chemo_Graphs/Data/USGS_lochvaleoutlet.csv") |>
  select(-X, -X.1) |>
  mutate(Date = as.Date(Date)) 

lochvale <- addWaterYear(lochvale) |>
  rename(date = Date) |>
  filter(year(date) > 1995)

################################################################################

# TDN:TDP GL4
wateryearplot(Gl4stoich, Gl4stoich$tdn.tdp, "TDN:TDP (molar ratio)")
wateryearplot(Gl4Dis, Gl4Dis$discharge_rate, "Streamflow" ~ (m ^ 3 ~ s ^ -1))


wateryearplot(lochvale, lochvale$Nitrate_mgl, "nitrate")





