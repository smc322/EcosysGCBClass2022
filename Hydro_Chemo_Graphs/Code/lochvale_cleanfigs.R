## Loch Vale Data Exploration

library(tidyverse)
library(lubridate)
library(dataRetrieval)
library(colorblindr)
library(patchwork)
source("Hydro_Chemo_Graphs/Code/Functions/tsDecompose.R")
source("Hydro_Chemo_Graphs/Code/Functions/chemo_hydrograph.R")
source("Hydro_Chemo_Graphs/Code/Functions/wateryearplot.R")
source("Hydro_Chemo_Graphs/Code/Functions/cQ_graph.R")


# Loch Vale
lochvale <- read.csv("Hydro_Chemo_Graphs/Data/USGS_lochvaleoutlet.csv") |>
  select(-X, -X.1) |>
  mutate(Date = as.Date(Date)) |>
  rename(date = Date) |>
  filter(year(date) > 1984)

# Andrews Stream
# andrews <- read.csv("Hydro_Chemo_Graphs/Data/USGS_andrewslochvale.csv") |>
#   select(-X) |>
#   mutate(Date = as.Date(Date)) |>
#   rename(date = Date) |>
#   select(-Discharge_cms)

chemo_hydrograph(lochvale, lochvale$Nitrate_mgl, "Nitrate"~(mg~~L^-1), lochvale)+
  geom_vline(xintercept = seq.Date(from = as.Date('1980-01-01'), to = as.Date('2021-01-01'),by = 'year'),
             linetype=2, alpha = 0.4, color = "grey70") 
ggsave("Hydro_Chemo_Graphs/Plots/Loch_Vale_Plots/HCG_lochvale_nitrate.jpg", width = 6.5, height = 4.5, dpi=500)


# chemo_hydrograph(andrews, andrews$Nitrate_mgl, "Nitrate"~(mg~~L^-1), andrews)+
#   geom_vline(xintercept = seq.Date(from = as.Date('1980-01-01'), to = as.Date('2021-01-01'),by = 'year'),
#              linetype=2, alpha = 0.4, color = "grey70") 
# ggsave("Hydro_Chemo_Graphs/Plots/Loch_Vale_Plots/HCG_andrews_nitrate.jpg", width = 6.5, height = 4.5, dpi=500)

################################################################################
# Make HCG of weekly average values 

lochvale_weekly <- lochvale |>
  mutate(x = round((day(date)/5))*5,
         x = ifelse(x == 0, 1, x), 
         date2 = paste(year(date), month(date), x, sep = "-")) |>
  mutate(Date = as.Date(date2)) |># must be named "Date" to work with the dataRetrieval function
  #seq along dates starting with the beginning of your water year
  mutate(CDate=as.Date(paste0(ifelse(month(Date) < 10, "1901", "1900"),
                              "-", month(Date), "-", day(Date)))) |>
  group_by(CDate) |>
  mutate(ave_weekly_nitrate = mean(Nitrate_mgl, na.rm = TRUE),
         ave_weekly_dis = mean(discharge_rate, na.rm = TRUE)) |>
  ungroup() |>
  addWaterYear() |>
  select(CDate, ave_weekly_nitrate, ave_weekly_dis) |>
  distinct() |>
  mutate(date = CDate) |>
  rename(discharge_rate = ave_weekly_dis)

chemo_hydrograph(lochvale_weekly, lochvale_weekly$ave_weekly_nitrate, "Average Nitrate"~(mg~L^-1), lochvale_weekly)
ggsave("Hydro_Chemo_Graphs/Plots/Loch_Vale_Plots/HCG_average_weekly.jpg", width = 6.5, height = 4.5, dpi=500)

################################################################################
# water year plots
p.1 <- wateryearplot(lochvale, lochvale$Nitrate_mgl, "Nitrate"~(mg~L~L^-1))
p.2 <- wateryearplot(lochvale,lochvale$discharge_rate, "Streamflow" ~ (m ^ 3 ~ s ^ -1))

p.1/p.2
ggsave("Hydro_Chemo_Graphs/Plots/Loch_Vale_Plots/wateryear.jpg", width = 6.5, height = 8.5, dpi=500)


