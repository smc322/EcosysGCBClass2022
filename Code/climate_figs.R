library(tidyverse)
#library(macrosheds)
library(lubridate)
library(dataRetrieval)
library(scales)
library(patchwork)
library(mcp)
library(readxl)

# 1. Determine seasons using average monthly snow depth data 1980-2019 ####
## 1a. Loch Vale ####
lv_szn <- read.csv('Data/LochValeClimate_IMERG_01302024/SWE_kgm2_monthly_loch.csv', skip=7) |>
  rename(date=1,
         SWE_kgm2=2) |>
  mutate(Date=as.Date(date)) |>
  mutate(mon = month(Date),
         Year = year(Date)) |>
  addWaterYear() |>
  #seq along dates starting with the beginning of your water year
  mutate(CDate=as.Date(paste0(ifelse(mon < 10, "1901", "1900"),
                              "-", mon, "-1"))) |>
  group_by(CDate) |>
  summarise(avesnow = mean(SWE_kgm2)) |>
  ungroup() |>
  ggplot() +
  geom_bar(stat='identity', aes(CDate, avesnow)) +
  labs(x='', y='',
       title = 'Loch Vale') +
  theme_classic() +
  theme(plot.title = element_text(face = 'bold', family = 'serif', size = rel(0.75),
                                  hjust = 0.5),
        text = element_text(family = 'serif')) +
  scale_x_date(labels = date_format('%b'))
# winter (snow increasing November-April)
# snowmelt runoff (snow decreasing May-July)
# summer (little to no snow Aug-October) 
# ggsave('Figures/LochVale_AndyGSWS08/snow_season_LochVALE.png', height = 4.5, width = 6.5, units = 'in', dpi=1200)
lv_szn

## 1b. Niwot ####
niwot_szn <- read.csv('Data/NiwotClimate_IMERG_02282024/SWE_kgm2_monthly_niwot.csv', skip=7) |>
  rename(date=1,
         SWE_kgm2=2) |>
  mutate(Date=as.Date(date)) |>
  mutate(mon = month(Date),
         Year = year(Date)) |>
  addWaterYear() |>
  #seq along dates starting with the beginning of your water year
  mutate(CDate=as.Date(paste0(ifelse(mon < 10, "1901", "1900"),
                              "-", mon, "-1"))) |>
  group_by(CDate) |>
  summarise(avesnow = mean(SWE_kgm2)) |>
  ungroup() |>
  ggplot() +
  geom_bar(stat='identity', aes(CDate, avesnow)) +
  labs(x='', y='Mean SWE 1980-2019'~(kg*~m^-2),
       title='Niwot Ridge') +
  theme_classic() +
  theme(plot.title = element_text(face = 'bold', family = 'serif', size = rel(0.75),
                                  hjust = 0.5),
        text = element_text(family = 'serif')) +
  scale_x_date(labels = date_format('%b'))
# winter (snow increasing November-May)
# snowmelt runoff (snow decreasing June-July)
# summer (little to no snow August-October) 
niwot_szn

## 1c. Andrews ####
andy_szn<-read.csv('Data/AndrewsClimate_IMERG_01302024/SWE_kgm2_monthly_andy.csv', skip=7) |>
  rename(date=1,
         SWE_kgm2=2) |>
  mutate(Date=as.Date(date)) |>
  mutate(mon = month(Date),
         Year = year(Date)) |>
  addWaterYear() |>
  #seq along dates starting with the beginning of your water year
  mutate(CDate=as.Date(paste0(ifelse(mon < 10, "1901", "1900"),
                              "-", mon, "-1"))) |>
  group_by(CDate) |>
  summarise(avesnow = mean(SWE_kgm2)) |>
  ungroup() |>
  ggplot() +
  geom_bar(stat='identity', aes(CDate, avesnow)) +
  labs(x='Month', y='',
       title = 'Andrews Forest') +
  theme_classic() +
  theme(plot.title = element_text(face = 'bold', family = 'serif', size = rel(0.75),
                                  hjust = 0.5),
        text = element_text(family = 'serif')) +
  scale_x_date(labels = date_format('%b'))
# winter (snow increasing December-February)
# snowmelt runoff (snow decreasing March-April)
# summer (little to no snow May-November) 
andy_szn

## 1d. patchwork season plots ####
lv_szn/niwot_szn/andy_szn +
  plot_annotation(tag_levels = 'a', tag_suffix = ')')


ggsave('Figures/szn_plot.png', dpi=1200, units='in', height=6.5, width=8.5)


# 2. Air temperature at each location ####

## 2a. Loch Vale ####
LV_temp <- read.csv('Data/LochValeClimate_IMERG_01302024/surfaceairtemp_C_monthly_loch.csv', skip=7) |>
  rename(date=1,
         temp=2) |>
  mutate(date=as.Date(date),
         Year=year(date)) |>
  mutate(mon = month(date)) |> 
  mutate(season = case_when(mon %in% c(12,1,2,3,4) ~ "Winter",
                            mon %in% c(5,6)  ~ "Snowmelt runoff",
                            mon %in% c(7,8,9,10,11) ~ "Summer")) |>
  mutate(season = factor(season, levels = c('Winter','Snowmelt runoff','Summer'))) |>
  group_by(Year, season) |>
  mutate(average_seasonal_temp = mean(temp)) |>
  ungroup()  

# average monthly air temperature
ggplot(LV_temp) +
  geom_line(aes(date, temp), color='grey70') +
  geom_point(aes(date, average_seasonal_temp, color=season)) +
  scale_color_manual('',values = c("#7EA8C4","#EFD15E","#E6A0C4")) +
  theme_classic() +
  labs(x='',y='Monthly temperature ('~degree*C*')')
#ggsave('Figures/LochVale/monthlytemp.png', width = 6.5, height = 4.5, dpi=1200)

# average annual temps
ggplot(LV_temp, aes(Year, average_seasonal_temp, color=season)) +
  geom_point() +
  geom_smooth(method='gam') +
  scale_color_manual('',values = c("#7EA8C4","#EFD15E","#E6A0C4")) 

temp_annual_LV <- LV_temp |>
  select(Year, season, average_seasonal_temp) |>
  distinct()

## looks like there might be a trend so let's test ####
### Mann-Kendall test ####
mk.model <- trend::mk.test((temp_annual_LV |> filter(season=='Summer'))$average_seasonal_temp)
mk.model # p == 0.015

mk.model <- trend::mk.test((temp_annual_LV |> filter(season=='Snowmelt runoff'))$average_seasonal_temp)
mk.model # p > 0.05

mk.model <- trend::mk.test((temp_annual_LV |> filter(season=='Winter'))$average_seasonal_temp)
mk.model # p > 0.05

### sen slope ####
LV_summerslope <- temp_annual_LV |> filter(season=='Summer')
sen.model <- zyp::zyp.sen(average_seasonal_temp ~ Year, LV_summerslope)
coef(sen.model)

LV_temp_inc <- ggplot(LV_summerslope, aes(Year, average_seasonal_temp)) +
  geom_point() +
  geom_abline(intercept = coef(sen.model)[[1]], 
              slope = coef(sen.model)[[2]], color = '#E6A0C4') +
  labs(x='', #y='Mean summertime air temperature ('~degree*C*')') +
       y='',
       title = 'Loch Vale - summer') +
  theme_classic() +
  annotate('text', x=2015, y=13.5, label = 'p-value < 0.05; slope = 0.029') +
  theme(plot.title = element_text(face = 'bold', family = 'serif', size = rel(0.75),
                                  hjust = 0.5),
        text = element_text(family = 'serif'))


## 2b. Niwot ####
niwot_temp <- read.csv('Data/NiwotClimate_IMERG_02282024/surfaceairtemp_C_monthly_niwot.csv', skip=7) |>
  rename(date=1,
         temp=2) |>
  mutate(date=as.Date(date),
         Year=year(date)) |>
  mutate(mon = month(date)) |> 
  mutate(season = case_when(mon %in% c(12,1,2,3,4,5) ~ "Winter",
                            mon %in% c(6,7)  ~ "Snowmelt runoff",
                            mon %in% c(8,9,10,11) ~ "Summer")) |>
  mutate(season = factor(season, levels = c('Winter','Snowmelt runoff','Summer'))) |>
  group_by(Year, season) |>
  mutate(average_seasonal_temp = mean(temp)) |>
  ungroup()  

# average monthly air temperature
ggplot(niwot_temp) +
  geom_line(aes(date, temp), color='grey70') +
  geom_point(aes(date, average_seasonal_temp, color=season)) +
  scale_color_manual('',values = c("#7EA8C4","#EFD15E","#E6A0C4")) +
  theme_classic() +
  labs(x='',y='Monthly temperature ('~degree*C*')')
#ggsave('Figures/LochVale/monthlytemp.png', width = 6.5, height = 4.5, dpi=1200)

# average annual temps
ggplot(niwot_temp, aes(Year, average_seasonal_temp, color=season)) +
  geom_point() +
  geom_smooth(method='gam') +
  scale_color_manual('',values = c("#7EA8C4","#EFD15E","#E6A0C4")) 

niwot_temp_annual <- niwot_temp |>
  select(Year, season, average_seasonal_temp) |>
  distinct()

## looks like there might be a trend so let's test ####
### Mann-Kendall test ####
mk.model <- trend::mk.test((niwot_temp_annual |> filter(season=='Summer'))$average_seasonal_temp)
mk.model # p == 0.005757

mk.model <- trend::mk.test((niwot_temp_annual |> filter(season=='Snowmelt runoff'))$average_seasonal_temp)
mk.model #p >0.05

mk.model <- trend::mk.test((niwot_temp_annual |> filter(season=='Winter'))$average_seasonal_temp)
mk.model # p > 0.05

### sen slope ####
niwot_summerslope <- niwot_temp_annual|> filter(season=='Summer')
sen.model <- zyp::zyp.sen(average_seasonal_temp ~ Year, niwot_summerslope)
coef(sen.model)

niwot_temp_inc <- ggplot(niwot_summerslope, aes(Year, average_seasonal_temp)) +
  geom_point() +
  geom_abline(intercept = coef(sen.model)[[1]], 
              slope = coef(sen.model)[[2]], color = '#E6A0C4') +
  labs(x='', #y='Mean summertime air temperature ('~degree*C*')') +
       y='',
       title = 'Niwot Ridge - summer') +
  theme_classic() +
  annotate('text', x=2015, y=13.5, label = 'p-value < 0.01; slope = 0.032') +
  theme(plot.title = element_text(face = 'bold', family = 'serif', size = rel(0.75),
                                  hjust = 0.5),
        text = element_text(family = 'serif'))


## 2c. Andrews ####
andy_temp <- read.csv('Data/AndrewsClimate_IMERG_01302024/surfaceairtemp_C_monthly_andy.csv', skip=7) |>
  rename(date=1,
         temp=2) |>
  mutate(date=as.Date(date),
         Year=year(date)) |>
  mutate(mon = month(date)) |> 
  mutate(season = case_when(mon %in% c(12,1,2) ~ "Winter",
                            mon %in% c(3,4)  ~ "Snowmelt runoff",
                            mon %in% c(5,6,7,8,9,10,11) ~ "Summer")) |>
  mutate(season = factor(season, levels = c('Winter','Snowmelt runoff','Summer'))) |>
  group_by(Year, season) |>
  mutate(average_seasonal_temp = mean(temp)) |>
  ungroup()  

# average monthly air temperature
ggplot(andy_temp) +
  geom_line(aes(date, temp), color='grey70') +
  geom_point(aes(date, average_seasonal_temp, color=season)) +
  scale_color_manual('',values = c("#7EA8C4","#EFD15E","#E6A0C4")) +
  theme_classic() +
  labs(x='',y='Monthly temperature ('~degree*C*')')
#ggsave('Figures/LochVale/monthlytemp.png', width = 6.5, height = 4.5, dpi=1200)

# average annual temps
ggplot(andy_temp, aes(Year, average_seasonal_temp, color=season)) +
  geom_point() +
  geom_smooth(method='gam') +
  scale_color_manual('',values = c("#7EA8C4","#EFD15E","#E6A0C4")) 

andy_temp_annual <- andy_temp |>
  select(Year, season, average_seasonal_temp) |>
  distinct()

## looks like there might be a trend so let's test ####
### Mann-Kendall test ####
mk.model <- trend::mk.test((andy_temp_annual |> filter(season=='Summer'))$average_seasonal_temp)
mk.model # p == 2.599e-05

mk.model <- trend::mk.test((andy_temp_annual |> filter(season=='Snowmelt runoff'))$average_seasonal_temp)
mk.model #p >0.05

mk.model <- trend::mk.test((andy_temp_annual |> filter(season=='Winter'))$average_seasonal_temp)
mk.model # p > 0.05

### sen slope ####
andy_summerslope <- andy_temp_annual |> filter(season=='Summer')
sen.model <- zyp::zyp.sen(average_seasonal_temp ~ Year, andy_summerslope)
coef(sen.model)

andy_summer <- ggplot(andy_summerslope, aes(Year, average_seasonal_temp)) +
  geom_point() +
  geom_abline(intercept = coef(sen.model)[[1]], 
              slope = coef(sen.model)[[2]], color = '#E6A0C4') +
  labs(x='', y='',
       title='Andrews Forest - summer') +
  theme_classic() +
  annotate('text', x=2015, y=13.5, label = 'p-value < 0.0001; slope = 0.048')  +
  theme(plot.title = element_text(face = 'bold', family = 'serif', size = rel(0.75),
                                  hjust = 0.5),
        text = element_text(family = 'serif'))


# andy_snowmeltslope <- andy_temp_annual |> filter(season=='Snowmelt runoff')
# sen.model <- zyp::zyp.sen(average_seasonal_temp ~ Year, andy_snowmeltslope)
# coef(sen.model)
# 
# andy_snowmelt <- ggplot(andy_snowmeltslope, aes(Year, average_seasonal_temp)) +
#   geom_point() +
#   geom_abline(intercept = coef(sen.model)[[1]], 
#               slope = coef(sen.model)[[2]], color = '#EFD15E') +
#   labs(x='', y='', title='Andrews Forest - snowmelt runoff') +
#   theme_classic() +
#   annotate('text', x=2005, y=9.5, label = 'p-value < 0.05; slope = 0.039')  +
#   theme(plot.title = element_text(face = 'bold', family = 'serif', size = rel(0.75),
#                                   hjust = 0.5),
#         text = element_text(family = 'serif'))


# 2d. Patchwork airtemp plots ####
LV_temp_inc/niwot_temp_inc/andy_summer +
  plot_annotation(tag_levels = 'a', tag_suffix = ')')


ggsave('Figures/airtemp_plot.png', dpi=1200, units='in', height=6.5, width=8.5)


# 3. LV and Niwot Snowpack ####

## 3a. Loch Vale ####
lv_snow <- read.csv('Data/LochValeClimate_IMERG_01302024/SWE_kgm2_monthly_loch.csv', skip=7)  |>
  rename(date=1,
         SWE_kgm2=2) |>
  mutate(Date=as.Date(date)) |>
  addWaterYear() |>
  filter(waterYear < 2020) |> #only includes a few days in 2020
  group_by(waterYear) |>
  reframe(Tot_snow = sum(SWE_kgm2)) |>
  mutate(mean_snow = mean(Tot_snow),
         min_snow = min(Tot_snow)) |>
  ggplot() +
  geom_bar(stat='identity', aes(waterYear, Tot_snow)) +
  labs(x='', y='', title='Loch Vale') +
  theme_classic() +
  theme(plot.title = element_text(face = 'bold', family = 'serif', size = rel(0.75),
                                  hjust = 0.5),
        text = element_text(family = 'serif'))

## 3b. Niwot Ridge ####
niwot_snow <- read.csv('Data/NiwotClimate_IMERG_02282024/SWE_kgm2_monthly_niwot.csv', skip=7) |>
  rename(date=1,
         SWE_kgm2=2) |>
  mutate(Date=as.Date(date)) |>
  addWaterYear() |>
  filter(waterYear < 2020) |> #only includes a few days in 2020
  group_by(waterYear) |>
  reframe(Tot_snow = sum(SWE_kgm2)) |>
  mutate(mean_snow = mean(Tot_snow),
         min_snow = min(Tot_snow)) |>
  ggplot() +
  geom_bar(stat='identity', aes(waterYear, Tot_snow)) +
  labs(x='', y='', title='Niwot Ridge') +
  theme_classic() +
  theme(plot.title = element_text(face = 'bold', family = 'serif', size = rel(0.75),
                                  hjust = 0.5),
        text = element_text(family = 'serif'))
# 2011 is highest snow year (8.189045m)
# lowest snow year is 1981 - but we don't have stream data that year - using lowest snow year on record after 1986 (when data starts): 1991 (2.695854m)

# 3c. patchwork snowpack plots ####
lv_snow/niwot_snow +
  plot_annotation(tag_levels = 'a', tag_suffix = ')')

ggsave('Figures/snowpack_plot.png', dpi=1200, units='in', height=6.5, width=8.5)


