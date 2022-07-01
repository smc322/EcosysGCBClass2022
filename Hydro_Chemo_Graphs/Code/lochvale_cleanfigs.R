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
             linetype=2, alpha = 0.4, color = "grey70") +
  labs(caption = "Figure X. Nitrate concentration shown in the red line, and streamflow shown in the grey line\n for the Loch Vale outlet for years 1985-2019.") +
  theme(plot.caption = element_text(size = 10, hjust = 0))
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

################################################################################
# precip
precip <- read.csv("Hydro_Chemo_Graphs/Data/precip_lochvale.csv") |>
  mutate(date = as.Date(dateOn)) |>
  select(date, ppt) |>
  mutate(ppt = ifelse(ppt < 0, NA, ppt)) |>
  mutate(year = year(date)) |>
  group_by(year) |>
  mutate(annual = sum(ppt, na.rm = TRUE)) |>
  ungroup() |>
  filter(year < 2022) 


annual_precip <- precip |>
  mutate(season = NA) |>
  mutate(season = ifelse(between(month(date), 1, 3), "Jan-Mar",
                         ifelse(between(month(date), 4,6), "April-June",
                                ifelse(between(month(date), 7,9), "July-Sep",
                                       ifelse(between(month(date), 10,12), "Oct-Dec", season))))) |>
  group_by(year, season) |>
  mutate(seasonal_precip = sum(ppt, na.rm = TRUE)) |>
  select(year, season, seasonal_precip, annual) |>
  distinct() |>
  mutate(percentage = (seasonal_precip/annual)*100)

annual_precip$season = factor(annual_precip$season, levels = c("Jan-Mar", "April-June", "July-Sep", "Oct-Dec"))

ggplot(annual_precip, aes(year, seasonal_precip, fill = season)) +
  geom_bar(stat = "identity") +
  theme_light() +
  scale_fill_manual("", values = palette_OkabeIto[1:4]) +
  geom_hline(yintercept = mean(annual_precip$annual), linetype = "dashed") +
  labs(x = "",
       y = "Total Annual Precipitation (mm)") +
  annotate('text', label = 'Long-term annual average', x = 2000, y = 1200, hjust = 0, size = 3.5)

ggsave("Hydro_Chemo_Graphs/Plots/Loch_Vale_Plots/annual_precip.jpg", height = 4.5, width = 6.5, units = "in", dpi = 500)  


