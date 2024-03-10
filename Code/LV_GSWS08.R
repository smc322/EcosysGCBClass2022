#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Comparing Andrews GSWS08 to LochVale
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#


library(tidyverse)
#library(macrosheds)
library(lubridate)
library(dataRetrieval)
library(scales)
library(patchwork)


# 1. Determine seasons using average monthly snowpack data 1980-2019 ####
## 1a. Loch Vale ####
read.csv('Data/LochValeClimate_IMERG_01302024/snowdepth_m_monthly_loch.csv', skip=7) |>
  rename(date=1,
         snowpack_m=2) |>
  mutate(date=as.Date(date)) |>
  mutate(mon = month(date),
         Year = year(date)) |>
  group_by(mon) |>
  summarise(avesnow = mean(snowpack_m)) |>
  ungroup() |>
  mutate(mon = as.factor(mon)) |>
  ggplot() +
  geom_bar(stat='identity', aes(mon, avesnow)) +
  labs(x='Month', y='Average snowpack 1980-2019 (m)') +
  theme_classic()
# winter (snow increasing November-March)
# snowmelt runoff (snow decreasing April-June)
# summer (little to no snow July-October) 
# ggsave('Figures/LochVale_AndyGSWS08/snow_season_LochVALE.png', height = 4.5, width = 6.5, units = 'in', dpi=1200)


## 1b. Andrews ####
read.csv('Data/AndrewsClimate_IMERG_01302024/snowdepth_m_monthly_andy.csv', skip=7) |>
  rename(date=1,
         snowpack_m=2) |>
  mutate(date=as.Date(date)) |>
  mutate(mon = month(date),
         Year = year(date)) |>
  group_by(mon) |>
  summarise(avesnow = mean(snowpack_m)) |>
  ungroup() |>
  mutate(mon = as.factor(mon)) |>
  ggplot() +
  geom_bar(stat='identity', aes(mon, avesnow)) +
  labs(x='Month', y='Average snowpack 1990-2019 (m)') +
  theme_classic()
# winter (snow increasing December-February)
# snowmelt runoff (snow decreasing March-April)
# summer (little to no snow May-November) 
ggsave('Figures/LochVale_AndyGSWS08/snow_season_Andrews.png', height = 4.5, width = 6.5, units = 'in', dpi=1200)

## 1c. High/low snow years - in 'water years' ####
# Loch Vale
read.csv('Data/LochValeClimate_IMERG_01302024/snowdepth_m_monthly_loch.csv', skip=7) |>
  rename(date=1,
         snowpack_m=2) |>
  mutate(Date=as.Date(date)) |>
  addWaterYear() |>
  filter(waterYear < 2020) |> #only includes a few days in 2020
  group_by(waterYear) |>
  reframe(Tot_snow = sum(snowpack_m)) |>
  mutate(mean_snow = mean(Tot_snow),
         min_snow = min(Tot_snow)) |>
  ggplot() +
  geom_bar(stat='identity', aes(waterYear, Tot_snow)) +
  labs(x='Year', y='Total snowpack (m)') +
  theme_classic() 
# 2011 is highest snow year
# lowest snow year is 1981 - but we don't have stream data that year - using second lowest snow year on record 2018

# Andrews
read.csv('Data/AndrewsClimate_IMERG_01302024/snowdepth_m_monthly_andy.csv', skip=7)  |>
  rename(date=1,
         snowpack_m=2) |>
  mutate(Date=as.Date(date)) |>
  addWaterYear() |>
  filter(waterYear < 2020) |> #only includes a few days in 2020
  group_by(waterYear) |>
  reframe(Tot_snow = sum(snowpack_m)) |>
  mutate(mean_snow = mean(Tot_snow),
         min_snow = min(Tot_snow)) |>
  ggplot() +
  geom_bar(stat='identity', aes(waterYear, Tot_snow)) +
  labs(x='Year', y='Total snowpack (m)') +
  theme_classic() 
# 1993 is highest snow year
# Lowest snow year is 2003
  
  

# 2. Read in stream data and add seasons ####
## 2a. Loch Vale ####
lochvale <- read.csv("Data/USGS_lochvaleoutlet.csv") |>
  select(-X) |>
  mutate(Date = as.Date(date)) |>
  select(-date) |>
  rename(nitrate_mgL = Nitrate_mgL) |>
  mutate(site_code = 'LochVale') |>
  # add seasons to the dataframe (based on snow depth data)
  mutate(mon = month(Date)) |>
  mutate(season = case_when(mon %in% c(11,12,1,2,3) ~ "Winter",
                            mon %in% c(4,5,6)  ~ "Snowmelt runoff",
                            mon %in% c(7,8,9,10) ~ "Summer")) |>
  mutate(season = factor(season, levels = c('Winter','Snowmelt runoff','Summer')))


## 2b. Andrews LTER Macrosheds read-in ####
andrews <- read.csv('Data/macrosheds_andrews.csv') |>
  select(-X) |>
  filter(site_code == 'GSWS08') |>
  mutate(Date = as.Date(date)) |>   
  select(-date) |>
  # add seasons to the dataframe (based on snow depth data)
  mutate(mon = month(Date)) |>
  mutate(season = case_when(mon %in% c(12,1,2) ~ "Winter",
                            mon %in% c(3,4)  ~ "Snowmelt runoff",
                            mon %in% c(5,6,7,8,9,10,11) ~ "Summer")) |>
  mutate(season = factor(season, levels = c('Winter','Snowmelt runoff','Summer')))


all_data <- rbind(lochvale, andrews) |>
  drop_na() |>
  # add water year from dataRetrieval package
  addWaterYear() |>
  filter(waterYear < 2020) |> #only includes a few days in 2020
  mutate(discharge_Ls = ifelse(discharge_Ls < 0, 0, discharge_Ls)) # some negative values of discharge at Loch Vale


# 3. Average water year figs####
wtryr_ave <- all_data |>
  # create a weekly timestep to average on
  mutate(x = round((day(Date)/5))*5,
         x = ifelse(x == 0, 1, x), 
         Date2 = paste(year(Date), month(Date), x, sep = "-")) |>
  # can't round to Feb. 30 - change to Mar. 1
  mutate(Date2 = ifelse(is.na(Date2), paste0(year(Date), '-03-01'), Date2)) |>
  mutate(Date = as.Date(Date2)) |>
  #seq along dates starting with the beginning of your water year
  mutate(CDate=as.Date(paste0(ifelse(month(Date) < 10, "1901", "1900"),
                              "-", month(Date), "-", day(Date)))) |>
  # get weekly averages
  group_by(CDate, site_code) |>
  mutate(ave_weekly_nitrate = mean(nitrate_mgL, na.rm = TRUE),
         ave_weekly_dis = mean(discharge_Ls, na.rm = TRUE)) |>
  ungroup() |>
  # add water year from dataRetrieval package
  addWaterYear() |>
  # simplify dataset 
  select(site_code, CDate, ave_weekly_nitrate, ave_weekly_dis, season) |>
  distinct() 

## 3b. plot averages ####
# some colors for HCGs
nitr <- '#90C24C'
hydro <- '#4D6BBC'  

loch_coef <- mean(as.numeric((wtryr_ave |> filter(site_code=='LochVale'))$ave_weekly_nitrate), na.rm = TRUE) / mean((wtryr_ave |> filter(site_code=='LochVale'))$ave_weekly_dis, na.rm = TRUE)


loch_ave <- ggplot((wtryr_ave |> filter(site_code=='LochVale'))) +
  geom_line(aes(CDate, ave_weekly_dis * loch_coef), color = hydro) +
  geom_line(aes(CDate, ave_weekly_nitrate), color = nitr) +
  theme_classic() +
  labs (x = '', title = 'Loch Vale - elevation 4009m') +
  scale_y_continuous(
    # first axis
    name = "Average Weekly Nitrate "~(mg~L^-1),
    # second axis 
    sec.axis = sec_axis(~./loch_coef, name = "Average Weekly Streamflow " ~(L~s^-1))
  )  +
  guides(linetype = guide_legend(override.aes = list(color = "black"))) +
  theme(plot.caption.position = "plot",
        plot.caption = element_text(hjust = 0),
        axis.title.y.right = element_text(color = hydro),
        axis.title.y = element_text(color = nitr, vjust = -2),
        legend.title = element_blank(),
        legend.position = 'none') +
  theme(plot.title = element_text(face = 'bold', family = 'serif', size = rel(0.75),
                                  hjust = 0.5),
        text = element_text(family = 'serif'),
        axis.text = element_text(size = 9),
        axis.title = element_text(size = 9)) +
  # add snowmelt period
  geom_vline(xintercept= c(as.numeric(as.Date("1901-04-01")), as.numeric(as.Date('1901-07-01'))),
             linetype=4, colour="grey50") +
  # annotate('text', label = 'snowmelt runoff', x = as.Date("1901-05-15"), y = 0.05, size = 3, color = 'grey50') +
  scale_x_date(labels = date_format('%b'))
loch_ave


andy_coef <- mean(as.numeric((wtryr_ave |> filter(site_code=='GSWS08'))$ave_weekly_nitrate), na.rm = TRUE) / mean((wtryr_ave |> filter(site_code=='GSWS08'))$ave_weekly_dis, na.rm = TRUE)


andy_ave <- ggplot((wtryr_ave |> filter(site_code=='GSWS08'))) +
  geom_line(aes(CDate, ave_weekly_dis * andy_coef), color = hydro) +
  geom_line(aes(CDate, ave_weekly_nitrate), color = nitr) +
  theme_classic() +
  labs (x = '', title = 'Andrews - elevation 1182m') +
  scale_y_continuous(
    # first axis
    name = "Average Weekly Nitrate "~(mg~L^-1),
    # second axis 
    sec.axis = sec_axis(~./andy_coef, name = "Average Weekly Streamflow " ~(L~s^-1))
  )  +
  guides(linetype = guide_legend(override.aes = list(color = "black"))) +
  theme(plot.caption.position = "plot",
        plot.caption = element_text(hjust = 0),
        axis.title.y.right = element_text(color = hydro),
        axis.title.y = element_text(color = nitr, vjust = -2),
        legend.title = element_blank(),
        legend.position = 'none') +
  theme(plot.title = element_text(face = 'bold', family = 'serif', size = rel(0.75),
                                  hjust = 0.5),
        text = element_text(family = 'serif'),
        axis.text = element_text(size = 9),
        axis.title = element_text(size = 9)) +
  # add snowmelt period
  geom_vline(xintercept= c(as.numeric(as.Date("1901-03-01")), as.numeric(as.Date('1901-05-01'))),
             linetype=4, colour="grey50") +
  # annotate('text', label = 'snowmelt runoff', x = as.Date("1901-04-01"), y = 0.02, size = 3, color = 'grey50') +
  scale_x_date(labels = date_format('%b'))
andy_ave


# 4. High/low snow year figs ####
# for Loch Vale - high snow is 2011, low snow is 2018
# for Andrews - high snow is 1993, low snow is 2015
snow_years <- all_data |>
  mutate(snowyr = case_when(site_code=='LochVale' & waterYear==2011 ~ 'high',
                            site_code=='LochVale' & waterYear==2018 ~ 'low',
                            site_code=='GSWS08' & waterYear==1993 ~ 'high',
                            site_code=='GSWS08' & waterYear==2003 ~ 'low')) |>
  drop_na(snowyr) |>  #seq along dates starting with the beginning of your water year for plotting
mutate(CDate=as.Date(paste0(ifelse(month(Date) < 10, "1901", "1900"),
                            "-", month(Date), "-", day(Date)))) 


## 4b. plot ####
loch_coef_snow <- mean(as.numeric((snow_years |> filter(site_code=='LochVale'))$nitrate_mgL), na.rm = TRUE) / mean((snow_years |> filter(site_code=='LochVale'))$discharge_Ls, na.rm = TRUE)

loch_snowyrs <- ggplot((snow_years |> filter(site_code=='LochVale'))) +
  geom_line(aes(CDate, discharge_Ls * loch_coef_snow, linetype=snowyr), 
            color = hydro) +
  geom_line(aes(CDate, nitrate_mgL, linetype=snowyr), color = nitr) +
  theme_classic() +
  labs (x = '', title = 'Loch Vale - elevation 4009m') +
  scale_y_continuous(
    # first axis
    name = "Nitrate "~(mg~L^-1),
    # second axis 
    sec.axis = sec_axis(~./loch_coef_snow, name = "Streamflow " ~(L~s^-1))
  )  +
  guides(linetype = guide_legend(override.aes = list(color = "black"))) +
  theme(plot.caption.position = "plot",
        plot.caption = element_text(hjust = 0),
        axis.title.y.right = element_text(color = hydro),
        axis.title.y = element_text(color = nitr, vjust = -2),
        legend.title = element_blank()) +
  theme(plot.title = element_text(face = 'bold', family = 'serif', size = rel(0.75),
                                  hjust = 0.5),
        text = element_text(family = 'serif'),
        axis.text = element_text(size = 9),
        axis.title = element_text(size = 9)) +
  # add snowmelt period
  geom_vline(xintercept= c(as.numeric(as.Date("1901-04-01")), as.numeric(as.Date('1901-07-01'))),
             linetype=4, colour="grey50") +
  # annotate('text', label = 'snowmelt runoff', x = as.Date("1901-05-15"), y = 0.05, size = 3, color = 'grey50') +
  scale_x_date(labels = date_format('%b'))
loch_snowyrs




andy_coef_snow <- mean(as.numeric((snow_years |> filter(site_code=='GSWS08'))$nitrate_mgL), na.rm = TRUE) / mean((snow_years |> filter(site_code=='GSWS08'))$discharge_Ls, na.rm = TRUE)

andy_snowyrs <- ggplot((snow_years |> filter(site_code=='GSWS08'))) +
  geom_line(aes(CDate, discharge_Ls * andy_coef_snow, linetype=snowyr), 
            color = hydro) +
  geom_line(aes(CDate, nitrate_mgL, linetype=snowyr), color = nitr) +
  theme_classic() +
  labs (x = '', title = 'Andrews - elevation 1182m') +
  scale_y_continuous(
    # first axis
    name = "Nitrate "~(mg~L^-1),
    # second axis 
    sec.axis = sec_axis(~./andy_coef_snow, name = "Streamflow " ~(L~s^-1))
  )  +
  guides(linetype = guide_legend(override.aes = list(color = "black"))) +
  theme(plot.caption.position = "plot",
        plot.caption = element_text(hjust = 0),
        axis.title.y.right = element_text(color = hydro),
        axis.title.y = element_text(color = nitr, vjust = -2),
        legend.title = element_blank()) +
  theme(plot.title = element_text(face = 'bold', family = 'serif', size = rel(0.75),
                                  hjust = 0.5),
        text = element_text(family = 'serif'),
        axis.text = element_text(size = 9),
        axis.title = element_text(size = 9)) +
  # add snowmelt period
  geom_vline(xintercept= c(as.numeric(as.Date("1901-04-01")), as.numeric(as.Date('1901-07-01'))),
             linetype=4, colour="grey50") +
  # annotate('text', label = 'snowmelt runoff', x = as.Date("1901-05-15"), y = 0.05, size = 3, color = 'grey50') +
  scale_x_date(labels = date_format('%b'))
andy_snowyrs


# 5. Combine HGC plots into nice figure ####

(loch_ave + andy_ave) /
  (loch_snowyrs + andy_snowyrs) +
  plot_annotation(tag_levels = 'a', tag_suffix = ')') +
  plot_layout(guides = 'collect')

ggsave("Figures/LochVale_AndyGSWS08/HCG_average_snowyears.png", width = 7.5, height = 5.5, dpi=1200)


# 6. cQ plots ####
cq_slopes <- all_data |>
  group_by(site_code, waterYear, season) |>
  filter(!is.infinite(log10(discharge_Ls)),
         !is.infinite(log10(nitrate_mgL))) |>
  do({
    mod = lm(log10(nitrate_mgL) ~ log10(discharge_Ls), data = .)
    data.frame(Intercept = coef(mod)[1],
               Slope = coef(mod)[2],
               SE = as.numeric((coef(summary(mod))[, "Std. Error"])[2]),
               CI.up = confint(mod, 'log10(discharge_Ls)', level=0.95)[2],
               CI.down = confint(mod, 'log10(discharge_Ls)', level=0.95)[1]) 
  }) # some NA slopes in Andrews becuase of 0 value nitrate concentrations reported

cq_slopes$site_code = factor(cq_slopes$site_code, levels=c('LochVale', 'GSWS08'))

szn_cols <- c("#7EA8C4","#EFD15E","#E6A0C4")

ggplot(cq_slopes |> # Andrews 1990 snowmelt runoff slope is 84.6 
         filter(Slope <80,
                # Andrews 1996 snowmelt runoff slope is -9
                Slope > -8), aes(Slope, season, fill=season)) +
  geom_violin() +
  labs(x = "cQ slope", y = "") +
  annotate("rect", xmin = -0.05, xmax = 0.05, ymin = 0, ymax = Inf, alpha = 0.2, color = "grey") +
  annotate("text", label = 'chemostatic', x = 0, y = 0.2, size = 2,color = "black") +
  annotate("text", label = 'mobilization', x = 1, y = 0.2, size = 2,color = "black") +
  annotate("text", label = 'dilution', x = -1, y = 0.2, size = 2,color = "black") +
  scale_fill_manual('', values=szn_cols) +
  theme_bw()+
  theme(plot.title = element_text(face = 'bold', family = 'serif', size = rel(0.75),
                                  hjust = 0.5),
        text = element_text(family = 'serif'),
        axis.text = element_text(size = 8),
        axis.title = element_text(size =8),
        axis.text.y = element_blank()) +
  scale_y_discrete(limits=rev) +
  geom_jitter( shape=21, col='black', alpha=0.5) +
  facet_wrap(~site_code, ncol=1, labeller=labeller(site_code= c(LochVale='Loch Vale, elevation 4009m', GSWS08='Andrews - elevation 1182m'))) +
  # bring out high snow years
  geom_point(cq_slopes |> filter(site_code=='GSWS08' & waterYear==1993), mapping=aes(Slope, season), shape=8, size=3) +
geom_point(cq_slopes |> filter(site_code=='LochVale' & waterYear==2011), mapping=aes(Slope, season), shape=8, size=3) + 
  # bring out low snow years
  geom_point(cq_slopes |> filter(site_code=='GSWS08' & waterYear==2003), mapping=aes(Slope, season), shape=25, size=2, fill = 'black') +
  geom_point(cq_slopes |> filter(site_code=='LochVale' & waterYear==2018), mapping=aes(Slope, season), shape=25, size=2, fill = 'black')

ggsave("Figures/LochVale_AndyGSWS08/cq_slopes.png", width = 6.5, height = 4.5, dpi=1200)


# 7. Area-normalized flux ####
# watershed area: 
andy_ws_area_ha <- 16.01008 # from macrosheds site info
loch_ws_area_ha <- 660 # from LTER site description https://www2.nrel.colostate.edu/projects/lvws/site_description.html

# daily time interval
q_interval <- 86400 # seconds/day


flux <- all_data |>
  mutate(ws_area_ha = case_when(site_code=='LochVale'~loch_ws_area_ha,
                                site_code=='GSWS08'~andy_ws_area_ha),
         # calculate flux here
         # nitrate * discharge * interval(s/day) / convert mg to kg / ws area
         flux_kg_ha_day = nitrate_mgL*discharge_Ls*q_interval/1e6/ws_area_ha) |>
  # group_by(site_code, waterYear, season) |>
  # mutate(seasonal_flux = sum(flux_kg_ha_day)) |>
  # ungroup() |>
   group_by(site_code, waterYear) |>
   mutate(annual_flux = sum(flux_kg_ha_day))
flux$site_code = factor(flux$site_code, levels=c('LochVale', 'GSWS08'))


# fluxes at Loch Vale
loch_flux <- ggplot(flux |> filter(site_code == 'LochVale'), aes(waterYear,flux_kg_ha_day, fill=season)) +
  geom_bar(stat='identity') +
  labs(y = "Nitrate flux "~(kg~ha^-1~day^-1), x = "Water Year",
       title='Loch Vale, elevation 4009m') +
  scale_fill_manual('', values=szn_cols) +
  theme_bw()+
  theme(plot.title = element_text(face = 'bold', family = 'serif', size = rel(0.75),
                                  hjust = 0.5),
        text = element_text(family = 'serif'),
        axis.text = element_text(size = 8),
        axis.title = element_text(size =8)) +
  geom_point(cq_slopes |> filter(site_code=='LochVale' & waterYear==2011), mapping=aes(2011, 0.4), shape=8, size=2) + 
  # bring out low snow years
  geom_point(cq_slopes |> filter(site_code=='LochVale' & waterYear==2018), mapping=aes(2018, 0.4), shape=25, size=2, fill = 'black')
loch_flux



# fluxes at Andrews
andy_high_flux <- ggplot(flux |> filter(site_code == 'GSWS08', 
                                        waterYear%in%c(1986,1996,2010)) |>
                           mutate(waterYear=factor(waterYear)), 
                         aes(waterYear,flux_kg_ha_day, fill=season)) +
  geom_bar(stat='identity') +
  labs(y = "", x = "") +
  scale_fill_manual('', values=szn_cols) +
  theme_minimal()+
  theme(plot.title = element_text(face = 'bold', family = 'serif', size = rel(0.75),
                                  hjust = 0.5),
        text = element_text(family = 'serif'),
        axis.text = element_text(size = 8),
        axis.title = element_text(size =8),
        legend.position = 'none')
andy_high_flux



# create insert for fluxes at Andrews to view lower flux years
andy_low_flux <- ggplot(flux |> filter(site_code == 'GSWS08', 
                                       annual_flux < 0.02), aes(waterYear,flux_kg_ha_day, fill=season)) +
  geom_bar(stat='identity') +
  labs(y = "Nitrate flux "~(kg~ha^-1~day^-1), x = "Water Year", title='Andrews - elevation 1182m') +
  scale_fill_manual('', values=szn_cols) +
  theme_bw()+
  theme(plot.title = element_text(face = 'bold', family = 'serif', size = rel(0.75),
                                  hjust = 0.5),
        text = element_text(family = 'serif'),
        axis.text = element_text(size = 8),
        axis.title = element_text(size =8)) +
  # bring out high snow years
  geom_point(flux |> filter(site_code=='GSWS08' & waterYear==1993), mapping=aes(1993, 0.0025), shape=8, size=2) +
  # bring out low snow years
  geom_point(cq_slopes |> filter(site_code=='GSWS08' & waterYear==2003), mapping=aes(2003, 0.001), shape=25, size=2, fill = 'black')
andy_low_flux

# add insert to andy
andy_insert <- andy_low_flux +
  annotation_custom(ggplotGrob(andy_high_flux),
                    ymin=0.003, ymax=0.0095, xmin=2000, xmax=2020)

loch_flux/andy_insert  +
  plot_annotation(tag_levels = 'a', tag_suffix = ')') +
  plot_layout(guides = 'collect')

ggsave("Figures/LochVale_AndyGSWS08/nitrate_flux.png", width = 7.5, height = 5.5, dpi=1200)




