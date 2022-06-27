## call functions
source("Hydro_Chemo_Graphs/Code/Functions/chemo_hydrograph.R")
source("Hydro_Chemo_Graphs/Code/Functions/wateryearplot.R")


library(tidyverse)
library(lubridate)
library(colorblindr)
library(patchwork)


## call data and format
source("Hydro_Chemo_Graphs/Data/Call_Data.R") 

#### format the datasets ####
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
  select(year, date, NO3., TDN, TDP, tdn.tdp) |>
  filter(tdn.tdp < 9000) # remove that one crazy outlier


## filter discharge data
Gl4Dis_a <- Gl4Dis |>
  mutate(year = year(date)) |>
  filter(year!= 2014,
         !is.na(discharge_rate)) |> # possible problems with discharge data in 2014 - personal correspondence with Sarah from Niwot LTER
  select(year, date, discharge_rate) #cms


Gl4Data <- full_join(Gl4stoich, Gl4Dis_a) |>
  arrange(date) |>
  filter(between(date, min(Gl4stoich$date), max(Gl4stoich$date)))

## create basic hydrochemograph
p.1 <- chemo_hydrograph(Gl4Data, Gl4Data$tdn.tdp, "TDN:TDP molar ratio", Gl4Data) +
  geom_vline(xintercept = seq.Date(from = as.Date('1980-01-01'), to = as.Date('2021-01-01'),by = 'year'),
             linetype=2, alpha = 0.4, color = "grey70") 


## create graph of TDN, TDP 
coef <- mean(as.numeric(Gl4Data$TDN), na.rm = TRUE) / mean(Gl4Data$TDP, na.rm = TRUE)


p.2 <- ggplot() +
  geom_line(Gl4Data |> filter(!is.na(TDP)), mapping = aes(date, TDP * coef), color = palette_OkabeIto[5]) +
  geom_line(Gl4Data |> filter(!is.na(TDN)), mapping = aes(date, TDN), color = palette_OkabeIto[7]) +
  theme_light() +
  labs(x = "") +
  scale_y_continuous(
    # first axis
    name =  "Total Dissolved N"~(mu*mol~L^-1),
    
    # second axis 
    sec.axis = sec_axis(~./coef, name =  "Total Dissolved P"~(mu*mol~L^-1))
  )  +
  theme(axis.title.y.right = element_text(color = palette_OkabeIto[5]),
        axis.title.y = element_text(color = palette_OkabeIto[7]),
        axis.line.y.left = element_line(color = palette_OkabeIto[7]))  +
  geom_vline(xintercept = seq.Date(from = as.Date('1980-01-01'), to = as.Date('2021-01-01'),by = 'year'),
             linetype=2, alpha = 0.4, color = "grey70") 


p.1/p.2


ggsave("Hydro_Chemo_Graphs/Plots/HCG_pluselements.jpg", width = 6.5, height = 8.5, dpi=500)


################################################################################
# Make HCG of weekly average values 

Gl4Data_weekly <- Gl4Data |>
  mutate(x = round((day(date)/5))*5,
         x = ifelse(x == 0, 1, x), 
         date2 = paste(year(date), month(date), x, sep = "-")) |>
  mutate(Date = as.Date(date2)) |># must be named "Date" to work with the dataRetrieval function
  #seq along dates starting with the beginning of your water year
  mutate(CDate=as.Date(paste0(ifelse(month(Date) < 10, "1901", "1900"),
                              "-", month(Date), "-", day(Date)))) |>
  group_by(CDate) |>
  mutate(ave_weekly_tdn = mean(TDN, na.rm = TRUE),
         ave_weekly_tdp = mean(TDP, na.rm = TRUE),
         ave_weekly_tdn.tdp = mean(tdn.tdp, na.rm = TRUE),
         ave_weekly_dis = mean(discharge_rate, na.rm = TRUE)) |>
  ungroup() |>
  addWaterYear() |>
  select(CDate, ave_weekly_tdn, ave_weekly_tdp, ave_weekly_tdn.tdp, ave_weekly_dis) |>
  distinct() |>
  mutate(date = CDate) |>
  rename(discharge_rate = ave_weekly_dis)

chemo_hydrograph(Gl4Data_weekly, Gl4Data_weekly$ave_weekly_tdn.tdp, "TDN:TDP molar ratio", Gl4Data_weekly) +
  stat_smooth(aes(Gl4Data_weekly$date, Gl4Data_weekly$ave_weekly_tdn.tdp), se = FALSE)
chemo_hydrograph(Gl4Data_weekly, Gl4Data_weekly$ave_weekly_tdn, "Total Dissolved N"~(mu*mol~L^-1), Gl4Data_weekly)+
  stat_smooth(aes(Gl4Data_weekly$date, Gl4Data_weekly$ave_weekly_tdn), se = FALSE)
chemo_hydrograph(Gl4Data_weekly, Gl4Data_weekly$ave_weekly_tdp, "Total Dissolved P"~(mu*mol~L^-1), Gl4Data_weekly) +
  stat_smooth(aes(Gl4Data_weekly$date, Gl4Data_weekly$ave_weekly_tdp), se = FALSE) 
