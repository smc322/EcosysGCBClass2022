# Collating data from Loch Vale, H.J. Andrews LTER

library(tidyverse)
library(macrosheds)
library(lubridate)
library(dataRetrieval)
library(scales)
library(patchwork)


# some colors for HCGs
nitr <- '#90C24C'
hydro <- '#4D6BBC'  

# Loch Vale streamflow and nitrate data (grabbed via USGS retrieval in Data/download_usgs.R)
lochvale <- read.csv("Data/USGS_lochvaleoutlet.csv") |>
  select(-X) |>
  mutate(date = as.Date(date)) |>
  rename(nitrate_mgL = Nitrate_mgL) |>
  mutate(site_code = 'LochVale') |>
  # add seasons to the dataframe (based on snow depth data - described in methods)
  mutate(mon = month(date)) |>
  mutate(season = case_when(mon %in% c(11,12,1,2,3) ~ "Winter",
                            mon %in% c(4,5,6)  ~ "Snowmelt runoff",
                            mon %in% c(7,8,9,10) ~ "Summer")) |>
  mutate(season = factor(season, levels = c('Winter','Snowmelt runoff','Summer')))


# Andrews LTER Macrosheds read-in
andrews <- read.csv('Data/macrosheds_andrews.csv') |>
  select(-X) |>
  mutate(date = as.Date(date))


str(lochvale)
str(andrews)

data <- rbind(andrews, lochvale) 


