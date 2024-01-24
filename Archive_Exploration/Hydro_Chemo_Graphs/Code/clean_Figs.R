## call functions
source("Archive_Exploration/Hydro_Chemo_Graphs/Code/Functions/chemo_hydrograph.R")
source("Archive_Exploration/Hydro_Chemo_Graphs/Code/Functions/wateryearplot.R")
source("Archive_Exploration/Hydro_Chemo_Graphs/Code/Functions/cQ_graph.R")

library(tidyverse)
library(lubridate)
library(colorblindr)
library(patchwork)


## call data and format
source('C:/PhD_code/EcosysGCBClass2022/Archive_Exploration/Hydro_Chemo_Graphs/Data/Call_Data.R') 


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

gl4_chem <- Gl4Chem |>
  select(date, year, NO3.) |>
  mutate(NO3. = as.numeric(NO3.)) |>
  drop_na()
  

## filter discharge data
Gl4Dis_a <- Gl4Dis |>
  mutate(year = year(date)) |>
  filter(year!= 2014,
         !is.na(discharge_rate)) |> # possible problems with discharge data in 2014 - personal correspondence with Sarah from Niwot LTER
  select(year, date, discharge_rate) #cms
  

Gl4Data <- full_join(gl4_chem, Gl4Dis_a) |>
  arrange(date) |>
  drop_na() |>
  mutate(mon = month(date)) |> #and add seasons to the dataframe
  # mutate(season = case_when(mon %in% c(10,11,12) ~ "Oct-Dec",
  #                           mon %in% c(1,2,3) ~ "Jan-Mar",
  #                           mon %in% c(4,5,6)  ~ "Apr-Jun",
  #                           mon %in% c(7,8,9) ~ "Jul-Sep")) |>
  # mutate(season = factor(season, levels = c('Oct-Dec','Jan-Mar','Apr-Jun','Jul-Sep'))) |>
  mutate(season = case_when(mon %in% c(11,12,1,2,3) ~ "Winter",
                            mon %in% c(4,5,6)  ~ "Snowmelt runoff",
                            mon %in% c(7,8,9,10) ~ "Summer")) |>
  mutate(season = factor(season, levels = c('Winter','Snowmelt runoff','Summer'))) 

ggplot(Gl4Data) +
  geom_point(aes(date, NO3.)) 
  

count <- Gl4Data |>
  count(year, season)



#### Some basic plots ####
chemo_hydrograph(Gl4Data, Gl4Data$tdn.tdp, "TDN:TDP (molar ratio)", Gl4Data) +
  stat_smooth(aes(Gl4Data$date, Gl4Data$tdn.tdp))
ggsave("Hydro_Chemo_Graphs/Plots/timeseriesnp.jpg", height = 4.5, width = 6.5, units = "in", dpi = 500)  
chemo_hydrograph(Gl4Data, Gl4Data$TDN, "Total Dissolved N"~(mu*mol~L^-1), Gl4Data) +
  stat_smooth(aes(Gl4Data$date, Gl4Data$TDN))
ggsave("Hydro_Chemo_Graphs/Plots/timeseriesN.jpg", height = 4.5, width = 6.5, units = "in", dpi = 500)  
chemo_hydrograph(Gl4Data, Gl4Data$TDP, "Total Dissolved P"~(mu*mol~L^-1), Gl4Data) +
  stat_smooth(aes(Gl4Data$date, Gl4Data$TDP))
ggsave("Hydro_Chemo_Graphs/Plots/timeseriesP.jpg", height = 4.5, width = 6.5, units = "in", dpi = 500)  
chemo_hydrograph(Gl4Data, Gl4Data$NO3., "Nitrate"~(mu*eq~L^-1), Gl4Data) +
  stat_smooth(aes(Gl4Data$date, Gl4Data$NO3.))
wateryearplot(Gl4Data, Gl4Data$tdn.tdp, "TDN:TDP")
wateryearplot(Gl4Data, Gl4Data$discharge_rate, "Streamflow" ~ (m^3~s^-1))
wateryearplot(Gl4Data, Gl4Data$TDN, "Total Dissolved N"~(mu*mol~L^-1))
wateryearplot(Gl4Data, Gl4Data$TDP, "Total Dissolved P"~(mu*mol~L^-1))
wateryearplot(Gl4Data, Gl4Data$NO3., "Nitrate"~(mu*eq~L^-1)) 



#### make a nice figure?####
## adding water year 
Gl4Data_a <- Gl4Data |>
  rename(Date = date) |># must be named "Date" to work with the dataRetrieval function
  #seq along dates starting with the beginning of your water year
  mutate(CDate=as.Date(paste0(ifelse(month(Date) < 10, "1901", "1900"),
                              "-", month(Date), "-", day(Date))))

Gl4Data_b <- addWaterYear(Gl4Data_a) # dataRetrieval package function to add water year


## Plotting TDN:TDP water year 
p.inset <- ggplot(Gl4Data_b |> filter(!is.na(tdn.tdp))) +
  geom_line(aes(CDate, tdn.tdp, group = waterYear, color = waterYear)) +
  geom_point(aes(CDate, tdn.tdp, group = waterYear, color = waterYear)) +
  stat_smooth(method = "loess", aes(CDate, tdn.tdp), color = "red4", se = FALSE) +
  theme_light() +
  scale_color_viridis_c() +
  scale_x_date(date_labels = "%b %d") +
  labs(x = "") +
  scale_y_continuous(
    name = ""
  ) +
  theme(legend.position = "none",
        axis.text = element_text(size = 5))
#make another TDN:TDP, but limit the values because it is impossible to see the pattern. Make above an inset
p.limit <- ggplot(Gl4Data_b |> filter(!is.na(tdn.tdp), tdn.tdp < 5000)) +
  geom_line(aes(CDate, tdn.tdp, group = waterYear, color = waterYear)) +
  geom_point(aes(CDate, tdn.tdp, group = waterYear, color = waterYear)) +
  stat_smooth(method = "loess", aes(CDate, tdn.tdp), color = "red4", se = FALSE) +
  theme_light() +
  scale_color_viridis_c("Water Year") +
  scale_x_date(date_labels = "%b %d") +
  labs(x = "") +
  scale_y_continuous(
    name = "TDN:TDP (molar ratio)"
  ) 

p.limit + inset_element(p.inset,0,0.6,0.4,1)
ggsave("Hydro_Chemo_Graphs/Plots/ratio_wY.jpg", height = 4.5, width = 6.5, units = "in", dpi = 500)


## Plotting streamflow water year
mindate <- min(Gl4stoich$date)
maxdate <- max(Gl4stoich$date)

ggplot(Gl4Data_b |> filter(between(Date, mindate, maxdate))) +
  geom_line(aes(CDate, discharge_rate, group = waterYear, color = waterYear)) +
  geom_point(aes(CDate, discharge_rate, group = waterYear, color = waterYear)) +
  stat_smooth(method = "loess", aes(CDate, discharge_rate), color = "red4", se = FALSE) +
  theme_light() +
  scale_color_viridis_c("Water Year") +
  scale_x_date(date_labels = "%b %d") +
  labs(x = "") +
  scale_y_continuous(
    name = "Streamflow"~(m^3~s^-1)
  ) 

ggsave("Hydro_Chemo_Graphs/Plots/streamflow_wY.jpg", height = 4.5, width = 6.5, units = "in", dpi = 500)



#### Looking at some of the weird years ####

## Find year with high discharge January - May 
high_dis <- Gl4Dis_a |>
  filter(month(date) %in% c(1,2,3,4,5))
#(it is 2015)
high_dis <- Gl4Data_b |> 
  filter(waterYear == 2015,
  between(Date, mindate, maxdate))

coef <- mean(as.numeric(high_dis$tdn.tdp), na.rm = TRUE) / mean(high_dis$discharge_rate, na.rm = TRUE)

ggplot(high_dis) +
  geom_line(aes(CDate, discharge_rate * coef), color = '#336a98') +
  geom_point(aes(CDate, discharge_rate * coef), color = '#336a98') +
  geom_point(aes(CDate, tdn.tdp), color = 'red4')  +
  geom_line(high_dis |> filter(!is.na(tdn.tdp)), mapping = aes(CDate, tdn.tdp), color = 'red4')  +
  scale_x_date(date_labels = "%b %d") +
  theme_light() +
  labs(x = "", title = "Year 2015 - Abnormally high streamflow January-May") +
  scale_y_continuous(
    # first axis
    name = "TDN:TDP (molar ratio)",
    
    # second axis 
    sec.axis = sec_axis(~./coef, name = "Streamflow" ~ (m ^ 3 ~ s ^ -1))
  )  +
  theme(axis.title.y.right = element_text(color = "#336a98"),
        axis.line.y.right = element_line(color = "#336a98"),
        axis.title.y = element_text(color = "red4"),
        axis.line.y.left = element_line(color = "red4")) 

ggsave("Hydro_Chemo_Graphs/Plots/2015_plot.jpg", height = 4.5, width = 6.5, units = "in", dpi = 500)  




## Check out years 2015-2020

high_np <- Gl4Data_b |> 
  filter(between(waterYear, 2015, 2020))

coef <- mean(as.numeric(high_np$tdn.tdp), na.rm = TRUE) / mean(high_np$discharge_rate, na.rm = TRUE)

ggplot(high_np) +
  geom_line(aes(CDate, discharge_rate * coef, group = as.factor(waterYear)), color = "#9A9391") +
  geom_point(aes(CDate, discharge_rate * coef, group = as.factor(waterYear)), color = "#9A9391") +
  geom_point(aes(CDate, tdn.tdp, group = waterYear, color = as.factor(waterYear))) +
  geom_line(high_np |> filter(!is.na(tdn.tdp)), mapping = aes(CDate, tdn.tdp, group = as.factor(waterYear), color = as.factor(waterYear))) +
  scale_color_viridis_d("Water Year") +
  scale_x_date(date_labels = "%b %d") +
  theme_light() +
  labs(x = "", title = "2015-2020") +
  scale_y_continuous(
    # first axis
    name = "TDN:TDP (molar ratio)",
    
    # second axis 
    sec.axis = sec_axis(~./coef, name = "Streamflow" ~ (m ^ 3 ~ s ^ -1))
  ) # +
  # theme(axis.title.y.right = element_text(color = "#336a98"),
  #       axis.line.y.right = element_line(color = "#336a98"),
  #       axis.title.y = element_text(color = "red4"),
  #       axis.line.y.left = element_line(color = "red4")) 

ggsave("Hydro_Chemo_Graphs/Plots/2015_2020_plot.jpg", height = 4.5, width = 6.5, units = "in", dpi = 500)  


## Pinpoint year with high N:P ratio -- 2017
high_np17 <- Gl4Data_b |> 
  filter(waterYear == 2017)

coef <- mean(as.numeric(high_np17$tdn.tdp), na.rm = TRUE) / mean(high_np17$discharge_rate, na.rm = TRUE)

ggplot(high_np17) +
  geom_line(aes(CDate, discharge_rate * coef), color = '#336a98') +
  geom_point(aes(CDate, discharge_rate * coef), color = '#336a98') +
  geom_point(aes(CDate, tdn.tdp), color = 'red4')  +
  geom_line(high_np17 |> filter(!is.na(tdn.tdp)), mapping = aes(CDate, tdn.tdp), color = 'red4')  +
  scale_x_date(date_labels = "%b %d") +
  theme_light() +
  labs(x = "", title = "Year 2017 - Abnormally high N:P") +
  scale_y_continuous(
    # first axis
    name = "TDN:TDP (molar ratio)",
    
    # second axis 
    sec.axis = sec_axis(~./coef, name = "Streamflow" ~ (m ^ 3 ~ s ^ -1))
  )  +
  theme(axis.title.y.right = element_text(color = "#336a98"),
        axis.line.y.right = element_line(color = "#336a98"),
        axis.title.y = element_text(color = "red4"),
        axis.line.y.left = element_line(color = "red4")) 

ggsave("Hydro_Chemo_Graphs/Plots/2017_plot.jpg", height = 4.5, width = 6.5, units = "in", dpi = 500)  


################################################################################

#### Check out precipitation ###
precip <- read.csv("Hydro_Chemo_Graphs/Data/precip_lochvale.csv") |>
  mutate(date = as.Date(date)) |>
  filter(between(date, mindate, as.Date("2020-09-30")))

# ggplot(precip |> filter(year > 2015), aes(date, precip)) +
#   geom_bar(stat = "identity") +
#   #geom_point(precip |> filter(month(date) %in% c(1,2,3,4,5)), mapping = aes(date, precip), color = "#336a98") +
#   theme_light() +
#   ylim(0,40)  +
#   geom_vline(xintercept = seq.Date(from = as.Date('1980-01-01'), to = as.Date('2020-01-01'),by = 'year'),
#              linetype=2, alpha = 0.4) 


 annual_precip <- precip |>
   group_by(year) |>
   mutate(annual = sum(precip)) |>
   ungroup() |>
   mutate(season = NA) |>
   mutate(season = ifelse(between(month(date), 1, 3), "Jan-Mar",
                          ifelse(between(month(date), 4,6), "April-June",
                                 ifelse(between(month(date), 7,9), "July-Sep",
                                                ifelse(between(month(date), 10,12), "Oct-Dec", season))))) |>
   group_by(year, season) |>
   mutate(seasonal_precip = sum(precip)) |>
   select(year, season, seasonal_precip, annual) |>
   distinct() |>
   mutate(percentage = (seasonal_precip/annual)*100)
 
 annual_precip$season = factor(annual_precip$season, levels = c("Jan-Mar", "April-June", "July-Sep", "Oct-Dec"))
 
 ggplot(annual_precip, aes(year, seasonal_precip, fill = season)) +
   geom_bar(stat = "identity") +
   theme_minimal() +
   scale_fill_manual("", values = palette_OkabeIto[1:4]) +
   geom_hline(yintercept = mean(annual_precip$annual), linetype = "dashed") +
   labs(x = "",
        y = "Total Annual Precipitation (mm)") +
   annotate('text', label = 'Long-term annual average', x = 2000, y = 1200, hjust = 0, size = 3.5)
  
 ggsave("Hydro_Chemo_Graphs/Plots/Loch_Vale_Plots/annual_precip.png", height = 4.5, width = 6.5, units = "in", dpi = 500)  
 
 
 
 ggplot(annual_precip, aes(year, seasonal_precip, group = season, color = season)) +
   geom_point() +
   geom_smooth(method = "lm", se = FALSE) +
   theme_light() +
   scale_color_manual("", values = palette_OkabeIto[1:4]) +
   labs(x = "",
        y = "Seasonal Precipitation (mm)")  +
   scale_y_log10()
ggsave("Hydro_Chemo_Graphs/Plots/precip_seasonalchange_lm.jpg", height = 4.5, width = 6.5, units = "in", dpi = 500) 

ggplot(annual_precip, aes(year, seasonal_precip, group = season, color = season)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  theme_light() +
  scale_color_manual("", values = palette_OkabeIto[1:4]) +
  labs(x = "",
       y = "Seasonal Precipitation (mm)") 
ggsave("Hydro_Chemo_Graphs/Plots/precip_seasonalchange.jpg", height = 4.5, width = 6.5, units = "in", dpi = 500) #loess  

#testing the linear models if the precipitation change over the years is influenced by season?
szn.m <- lm(seasonal_precip~year*season, annual_precip) 
summary(szn.m) # only July-September change through the years is significant
# library(visreg)
# visreg(szn.m, xvar = "year", by = "season", annual_precip, overlay = TRUE)



################################################################################

#### check out ice thickness ####
ice <- read.csv("Hydro_Chemo_Graphs/Data/ice.csv") |>
  mutate(date = as.Date(date)) |>
  filter(between(date, mindate, as.Date("2020-09-30"))) |>
  rename(Date = date)

ice <- addWaterYear(ice) |>
  #seq along dates starting with the beginning of your water year
  mutate(CDate=as.Date(paste0(ifelse(month(Date) < 10, "1901", "1900"),
                              "-", month(Date), "-", day(Date))))


ggplot(ice, aes(CDate, thickness, group = waterYear, color = waterYear)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  scale_color_viridis_c() +
  labs(x = "",
       y = "Ice thickness (cm)") +
  theme_light()


################################################################################

#### check out air temperatures ####
temp <- read.csv("Hydro_Chemo_Graphs/Data/air_temp.csv") |>
  mutate(date = as.Date(date)) |>
  filter(between(date, mindate, as.Date("2020-09-30"))) |>
  rename(Date = date) |>
  mutate(airtemp_avg = as.numeric(airtemp_avg))

temp <- addWaterYear(temp) |>
  #seq along dates starting with the beginning of your water year
  mutate(CDate=as.Date(paste0(ifelse(month(Date) < 10, "1901", "1900"),
                              "-", month(Date), "-", day(Date))))


ggplot() +
  geom_point(temp, mapping = aes(CDate, airtemp_avg, group = waterYear), color = "grey70") +
  geom_point(temp |> filter(waterYear == "2015"), mapping = aes(CDate, airtemp_avg), color = "red4") +
  geom_smooth(temp |> filter(waterYear == "2015"), mapping = aes(CDate, airtemp_avg), color = "red4", se = FALSE) +
  #geom_smooth(se = FALSE) +
 # scale_color_viridis_c("Water Year") +
  labs(x = "",
       y = "Air Temperature"~degree*C) +
  theme_light()  +
  scale_x_date(date_labels = "%b %d")


ggplot(temp, aes(Date, airtemp_avg)) +
  geom_point(color = "grey60") +
  geom_smooth(se = FALSE, color = "red4") +
 # scale_color_viridis_c("Water Year") +
  labs(x = "",
       y = "Air Temperature"~degree*C) +
  theme_light()

ggsave("Hydro_Chemo_Graphs/Plots/air_temp.jpg", height = 4.5, width = 6.5, units = "in", dpi = 500) 

annual_temp <- temp |>
  group_by(year(Date)) |>
  mutate(annual = mean(airtemp_avg, na.rm = TRUE)) |>
  ungroup() |>
  mutate(season = NA) |>
  mutate(season = ifelse(between(month(Date), 1, 3), "Jan-Mar",
                         ifelse(between(month(Date), 4,6), "April-June",
                                ifelse(between(month(Date), 7,9), "July-Sep",
                                       ifelse(between(month(Date), 10,12), "Oct-Dec", season))))) |>
  group_by(year(Date), season) |>
  mutate(seasonal_temp = mean(airtemp_avg, na.rm = TRUE)) |>
  ungroup() |>
  mutate(year = year(Date)) |>
  select(year, season, seasonal_temp, annual) |>
  distinct()

annual_temp$season = factor(annual_temp$season, levels = c("Jan-Mar", "April-June", "July-Sep", "Oct-Dec"))




ggplot(annual_temp, aes(year, seasonal_temp, group = season, color = season)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  theme_light() +
  scale_color_manual("", values = palette_OkabeIto[1:4]) +
  labs(x = "",
       y = "Seasonal air temperature"~degree*C)
#ggsave("Hydro_Chemo_Graphs/Plots/temp_seasonalchange_lm.jpg", height = 4.5, width = 6.5, units = "in", dpi = 500) 

ggplot(annual_temp, aes(year, seasonal_temp, group = season, color = season)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  theme_light() +
  scale_color_manual("", values = palette_OkabeIto[1:4]) +
  labs(x = "",
       y = "Seasonal air temperature"~degree*C) 
#ggsave("Hydro_Chemo_Graphs/Plots/temp_seasonalchange.jpg", height = 4.5, width = 6.5, units = "in", dpi = 500) #loess  

#testing the linear models if the temps change over the years is influenced by season?
szn.m <- lm(seasonal_temp~year*season, annual_temp) 
summary(szn.m) # only July-September change through the years is significant
# library(visreg)
# visreg(szn.m, xvar = "year", by = "season", annual_precip, overlay = TRUE)

temp_szn <-tsDecompose(temp |> rename(date = Date), temp$airtemp_avg, "Air temperature"~degree*C, "temp")


temp_szn <- temp |>
  mutate(season = NA) |>
  mutate(season = ifelse(between(month(Date), 1, 3), "Jan-Mar",
                         ifelse(between(month(Date), 4,6), "April-June",
                                ifelse(between(month(Date), 7,9), "July-Sep",
                                       ifelse(between(month(Date), 10,12), "Oct-Dec", season)))))

temp_szn$season = factor(temp_szn$season, levels = c("Jan-Mar", "April-June", "July-Sep", "Oct-Dec"))



ggplot(temp_szn, aes(Date, airtemp_avg, color = season)) +
  geom_point(color = "grey70") +
  geom_smooth(se = FALSE, color = "red4") +
  geom_smooth(mapping = aes(Date, as.numeric(airtemp_min)), se = FALSE) +
  scale_color_manual("", values = palette_OkabeIto[1:4]) +
  # scale_color_viridis_c("Water Year") +
  labs(x = "",
       y = "Air Temperature"~degree*C) +
  theme_light() #+
  #geom_hline(yintercept = mean((temp_szn |> filter(season == "Jan-Mar"))$airtemp_avg, na.rm = TRUE), linetype = "dashed")















Gl4Data_all <- Gl4Data |>
  left_join(temp_szn  |>
              rename(date = Date)) |>
  filter(between(date, mindate, maxdate))


ggplot(Gl4Data_all) +
  geom_point(aes(airtemp_avg, TDP))



Gl4Data_c <- Gl4Data  |>
  mutate(season = NA) |>
  mutate(season = ifelse(between(month(date), 1, 3), "Jan-Mar",
                         ifelse(between(month(date), 4,6), "April-June",
                                ifelse(between(month(date), 7,9), "July-Sep",
                                       ifelse(between(month(date), 10,12), "Oct-Dec", season)))))

Gl4Data_c$season = factor(Gl4Data_c$season, levels = c("Jan-Mar", "April-June", "July-Sep", "Oct-Dec"))

  


ggplot(Gl4Data_c) +
  geom_boxplot(aes(season, TDN, fill = season)) +
  theme_light() +
  scale_fill_manual("", values = palette_OkabeIto[1:4]) +
  labs(x = "",
       y = "Total Dissolved N"~(mu*mol~L^-1))

ggplot(Gl4Data_c) +
  geom_boxplot(aes(season, TDP, fill = season)) +
  theme_light() +
  scale_fill_manual("", values = palette_OkabeIto[1:4]) +
  labs(x = "",
       y = "Total Dissolved P"~(mu*mol~L^-1))

ggplot(Gl4Data_c) +
  geom_boxplot(aes(season, tdn.tdp, fill = season)) +
  theme_light() +
  scale_fill_manual("", values = palette_OkabeIto[1:4]) +
  labs(x = "",
       y = "TDN:TDP molar ratio")




seasonal_dat <- Gl4Data_all |>
  group_by(season, year) |>
  summarise(mean(discharge_rate, na.rm = TRUE),
            mean(tdn.tdp, na.rm = TRUE),
            mean(TDN, na.rm = TRUE),
            mean(TDP, na.rm = TRUE))








