#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# High and low snow years at Niwot
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

library(tidyverse)
#library(macrosheds)
library(lubridate)
library(dataRetrieval)
library(scales)
library(patchwork)


# 1. Determine seasons using average monthly snowpack data 1980-2019 ####
read.csv('Data/NiwotClimate_IMERG_02282024/SWE_kgm2_monthly_niwot.csv', skip=7) |>
  rename(date=1,
         SWE_kgm2=2) |>
  mutate(date=as.Date(date)) |>
  mutate(mon = month(date),
         Year = year(date)) |>
  group_by(mon) |>
  summarise(avesnow = mean(SWE_kgm2)) |>
  ungroup() |>
  mutate(mon = as.factor(mon)) |>
  ggplot() +
  geom_bar(stat='identity', aes(mon, avesnow)) +
  labs(x='Month', y='Average snowpack 1990-2019 (m)') +
  theme_classic()
# winter (snow increasing November-May)
# snowmelt runoff (snow decreasing June-July)
# summer (little to no snow August-October) 
# ggsave('Figures/Niwot/snow_season_NIWOT.png', height = 4.5, width = 6.5, units = 'in', dpi=1200)



## 1a. Find high/low snow years - in 'water years' ####
# Niwot
chk<- read.csv('Data/NiwotClimate_IMERG_02282024/SWE_kgm2_monthly_niwot.csv', skip=7) |>
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
  labs(x='Year', y='Total snowpack (m)') +
  theme_classic() 
# 2011 is highest snow year (2121.7083 kg/m2)
# lowest snow year is 1981 - but we don't have stream data that year - using lowest snow year on record after 1986 (when data starts): 1991 (464.9719 kg/m2)


# 2. Read in stream data and add seasons ####
niwot <- read.csv('Data/macrosheds_niwot.csv') |>
  select(-X) |>
  mutate(Date = as.Date(date)) |>   
  select(-date) |>
  # add seasons to the dataframe (based on snow depth data)
  # add seasons to the dataframe (based on snow depth data)
  mutate(mon = month(Date)) |>
  mutate(season = case_when(mon %in% c(12,1,2,3,4,5) ~ "Winter",
                            mon %in% c(6,7)  ~ "Snowmelt runoff",
                            mon %in% c(8,9,10,11) ~ "Summer")) |>
  mutate(season = factor(season, levels = c('Winter','Snowmelt runoff','Summer'))) |>
  # add water year from dataRetrieval package
  addWaterYear() |>
  filter(waterYear < 2020) |> #only includes a few days in 2020
  mutate(discharge_Ls = ifelse(discharge_Ls < 0, 0, discharge_Ls))


# 3. Average water year figs####
wtryr_ave <- niwot |>
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

niwot_coef <- mean(as.numeric((wtryr_ave)$ave_weekly_nitrate), na.rm = TRUE) / mean((wtryr_ave)$ave_weekly_dis, na.rm = TRUE)


niwot_ave <- ggplot((wtryr_ave)) +
  geom_line(aes(CDate, ave_weekly_dis * niwot_coef), color = hydro) +
  geom_line(aes(CDate, ave_weekly_nitrate), color = nitr) +
  theme_classic() +
  labs (x = '', title = 'Niwot - elevation 3550m') +
  scale_y_continuous(
    # first axis
    name = "Average Weekly Nitrate "~(mg~L^-1),
    # second axis 
    sec.axis = sec_axis(~./niwot_coef, name = "Average Weekly Streamflow " ~(L~s^-1))
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
  geom_vline(xintercept= c(as.numeric(as.Date("1900-11-01")),as.numeric(as.Date("1901-05-01")), as.numeric(as.Date('1901-08-01'))),
             linetype=4, colour="grey50") +
  # annotate('text', label = 'snowmelt runoff', x = as.Date("1901-05-15"), y = 0.05, size = 3, color = 'grey50') +
  scale_x_date(labels = date_format('%b'))
niwot_ave


# 4. High/low snow year figs ####
snow_years <- niwot |>
  mutate(snowyr = case_when(waterYear==2011 ~ 'high',
                            waterYear==1991 ~ 'low')) |>
  drop_na(snowyr) |>  #seq along dates starting with the beginning of your water year for plotting
  mutate(CDate=as.Date(paste0(ifelse(month(Date) < 10, "1901", "1900"),
                              "-", month(Date), "-", day(Date)))) 


## 4b. plot ####
niwot_coef_snow <- mean(as.numeric((snow_years)$nitrate_mgL), na.rm = TRUE) / mean((snow_years)$discharge_Ls, na.rm = TRUE)

niwot_snowyrs <- ggplot((snow_years)) +
  geom_line(aes(CDate, discharge_Ls * niwot_coef_snow, linetype=snowyr), 
            color = hydro) +
  geom_line(aes(CDate, nitrate_mgL, linetype=snowyr), color = nitr) +
  theme_classic() +
  labs (x = '') +
  scale_y_continuous(
    # first axis
    name = "Nitrate "~(mg~L^-1),
    # second axis 
    sec.axis = sec_axis(~./niwot_coef_snow, name = "Streamflow " ~(L~s^-1))
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
  geom_vline(xintercept= c(as.numeric(as.Date("1900-11-01")),as.numeric(as.Date("1901-05-01")), as.numeric(as.Date('1901-08-01'))),
             linetype=4, colour="grey50") +
  # annotate('text', label = 'snowmelt runoff', x = as.Date("1901-05-15"), y = 0.05, size = 3, color = 'grey50') +
  scale_x_date(labels = date_format('%b'))
niwot_snowyrs



# 5. cQ plot ####
cq_wtryr_ave <- wtryr_ave |>
  group_by(season) |>
  filter(!is.infinite(log10(ave_weekly_dis)),
         !is.infinite(log10(ave_weekly_nitrate))) |>
  do({
    mod = lm(log10(ave_weekly_nitrate) ~ log10(ave_weekly_dis), data = .)
    data.frame(Intercept = coef(mod)[1],
               Slope = coef(mod)[2],
               SE = as.numeric((coef(summary(mod))[, "Std. Error"])[2]),
               CI.up = confint(mod, 'log10(ave_weekly_dis)', level=0.95)[2],
               CI.down = confint(mod, 'log10(ave_weekly_dis)', level=0.95)[1]) 
  }) |>
  mutate(snowyr = 'average') |>
  as.data.frame() |>
  ungroup()

cq_snowyrs <- snow_years |>
  group_by(snowyr, season) |>
  filter(!is.infinite(log10(discharge_Ls)),
         !is.infinite(log10(nitrate_mgL))) |>
  drop_na() |>
  do({
    mod = lm(log10(nitrate_mgL) ~ log10(discharge_Ls), data = .)
    data.frame(Intercept = coef(mod)[1],
               Slope = coef(mod)[2],
               SE = as.numeric((coef(summary(mod))[, "Std. Error"])[2]),
               CI.up = confint(mod, 'log10(discharge_Ls)', level=0.95)[2],
               CI.down = confint(mod, 'log10(discharge_Ls)', level=0.95)[1]) 
  }) |>
  as.data.frame() |>
  ungroup()


cq_slopes <- rbind(cq_snowyrs, cq_wtryr_ave)


szn_cols <- c("#7EA8C4","#EFD15E","#E6A0C4")


# # create an inset for the weird winter high snow year slope
# inset <- ggplot(cq_slopes |> filter(Slope < -1)) + # need to plot winter of high snow year separately
#   labs(x = "", y = "") +
#   geom_point(mapping = aes(x=Slope, y=season, shape = snowyr, fill = season, color = season),
#              size = 2.5, alpha = 0.7, position = position_dodge(width=0.5)) +
#   geom_errorbarh(mapping = aes(Slope, season, xmin=CI.down, xmax=CI.up, color = season, group = snowyr),
#                  height = 0.2, position=position_dodge(width=0.5)) +
#   scale_y_discrete(limits=rev) +
#   theme_bw() +
#   scale_shape_manual('', values = 22) +
#   scale_fill_manual('', values = c("#7EA8C4","#EFD15E","#E6A0C4")) +
#   scale_color_manual('', values = c("#7EA8C4","#EFD15E","#E6A0C4")) +
#   # guides(fill = 'none') + 
#   #theme(legend.position = 'none') +
#   theme(plot.title = element_text(face = 'bold', family = 'serif', size = rel(0.5),
#                                   hjust = 0.5),
#         text = element_text(family = 'serif'),
#         axis.text = element_text(size = 8),
#         axis.title = element_text(size =8),
#         axis.text.y = element_blank(),
#         legend.position = 'none')
# inset


# rest of the data
cqplot <- ggplot(cq_slopes) + 
  labs(x = "cQ slope", y = "") +
  annotate("rect", xmin = -0.05, xmax = 0.05, ymin = 0, ymax = Inf, alpha = 0.2, color = "grey") +
  annotate("text", label = 'chemostatic', x = 0, y = 0.2, size = 2.5,color = "black") +
  annotate("text", label = 'mobilization', x = 2, y = 0.2, size = 2.5,color = "black") +
  annotate("text", label = 'dilution', x = -2, y = 0.2, size = 2.5,color = "black") +
  geom_point(mapping = aes(x=Slope, y=season, shape = snowyr, fill = season, color = season),
             size = 2.5, alpha = 0.7, position = position_dodge(width=0.5)) +
  geom_errorbarh(mapping = aes(Slope, season, xmin=CI.down, xmax=CI.up, color = season, group = snowyr),
                 height = 0.2, position=position_dodge(width=0.5)) +
  scale_y_discrete(limits=rev) +
  theme_classic() +
  scale_shape_manual('', values = c(21,22,24)) +
  scale_fill_manual('', values = c("#7EA8C4","#EFD15E","#E6A0C4")) +
  scale_color_manual('', values = c("#7EA8C4","#EFD15E","#E6A0C4")) +
  # guides(fill = 'none') + 
  #theme(legend.position = 'none') +
  theme(plot.title = element_text(face = 'bold', family = 'serif', size = rel(0.5),
                                  hjust = 0.5),
        text = element_text(family = 'serif'),
        axis.text = element_text(size = 8),
        axis.title = element_text(size =8),
        axis.text.y = element_blank()) 
  # add the inset here
  # annotation_custom(ggplotGrob(inset),
  #                   ymin=2.25, ymax=3.5, xmin=-1.5, xmax=-0.5)

# 
# ggsave("Figures/Niwot/niwot_cq.png", width = 6, height = 4, dpi=1200)



# 6. Combine HGC and cq plots into nice figure ####
layout <- 
  'AACC
   BB##'

niwot_ave + niwot_snowyrs + cqplot +
  plot_annotation(tag_levels = 'a', tag_suffix = ')') +
  plot_layout(design=layout, guides = 'collect')

ggsave("Figures/Niwot/combo_hcg_cq.png", width = 8.5, height = 6.5, dpi=1200)

