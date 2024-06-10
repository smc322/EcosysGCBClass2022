#~~~~~~~~~~~~~~~~~~~~~~~~#
# comparing Andrews sites
#~~~~~~~~~~~~~~~~~~~~~~~~#
# GSWS08 is reference site with no logging 
# GSWS06 lost 100% overstory 1974


library(tidyverse)
library(lubridate)
library(dataRetrieval)
library(scales)
library(patchwork)


# 1. Load data ####
andrews <- read.csv('Data/macrosheds_andrews.csv') |>
  select(-X) |>
  mutate(Date = as.Date(date)) |>   
  select(-date) |>
  # add seasons to the dataframe (based on snow depth data)
  mutate(mon = month(Date)) |>
  mutate(season = case_when(mon %in% c(12,1,2) ~ "Winter",
                            mon %in% c(3,4)  ~ "Snowmelt runoff",
                            mon %in% c(5,6,7,8,9,10,11) ~ "Summer")) |>
  mutate(season = factor(season, levels = c('Winter','Snowmelt runoff','Summer'))) |>
  drop_na() |>
  # add water year from dataRetrieval package
  addWaterYear() |>
  filter(waterYear < 2020) |> #only includes a few days in 2020
  mutate(discharge_Ls = ifelse(discharge_Ls < 0, 0, discharge_Ls)) |>
  mutate(site_code = ifelse(site_code == 'GSWS08','Watershed 8', 'Watershed 6'))


# 2 Plot raw data to compare sites ####
# some colors for HCGs
nitr <- '#90C24C'
hydro <- '#4D6BBC'  

# filter data
log_years <- andrews |>
  filter(between(waterYear, 1972, 1984)) 

# for plotting NO3 and discharge on same plot
andy_coef <- mean(as.numeric((log_years)$nitrate_mgL), na.rm = TRUE) / mean((log_years)$discharge_Ls, na.rm = TRUE)

# plot!
rawdata <- ggplot(log_years) +
  geom_line(aes(Date, discharge_Ls * andy_coef, linetype=site_code), 
            color = hydro) +
  geom_line(aes(Date, nitrate_mgL, linetype=site_code), color = nitr) +
  theme_classic() +
  labs (x = '', title = 'Andrews - Watershed 8 elevation 1182m, Watershed 6 elevation 1029m') +
  scale_y_continuous(
    # first axis
    name = "Nitrate "~(mg~L^-1),
    # second axis 
    sec.axis = sec_axis(~./andy_coef, name = "Streamflow " ~(L~s^-1))
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
        axis.title = element_text(size = 9),
        legend.position = c(0.75,0.75)) +
  # add line at 1974
  geom_vline(xintercept= (as.Date("1974-01-01")),
             linetype=1, colour="grey50") +
   annotate('text', label = 'Watershed 6 logged 100% in 1974', x = as.Date("1977-08-15"), y = 0.12, size = 3, color = 'grey50') +
  scale_x_date(date_breaks = '1 year', date_labels = '%Y')

# ggsave('Figures/Andrews_logging_comparison.png',height=4.5,width=6.5,units='in',dpi=1200)








# 3. Area-normalized flux ####
# watershed area: 
andy08_ws_area_ha <- 16.01008 # from macrosheds site info
andy06_ws_area_ha <- 11.99134 # from macrosheds site info

# daily time interval
q_interval <- 86400 # seconds/day


flux <- andrews |>
  mutate(ws_area_ha = case_when(site_code=='Watershed 6'~andy06_ws_area_ha,
                                site_code=='Watershed 8'~andy08_ws_area_ha),
         # calculate flux here
         # nitrate * discharge * interval(s/day) / convert mg to kg / ws area
         flux_kg_ha_day = nitrate_mgL*discharge_Ls*q_interval/1e6/ws_area_ha) |>
  # group_by(site_code, waterYear, season) |>
  # mutate(seasonal_flux = sum(flux_kg_ha_day)) |>
  # ungroup() |>
  group_by(site_code, waterYear) |>
  mutate(annual_flux = sum(flux_kg_ha_day))

difference_flux <- flux |>
  select(-nitrate_mgL, -discharge_Ls,-ws_area_ha,-annual_flux) |>
  pivot_wider(names_from = site_code, values_from = flux_kg_ha_day) |>
  mutate(diff = `Watershed 6` - `Watershed 8`) |>
  filter(between(waterYear, 1972, 1984)) 

diffplot <- ggplot(difference_flux) +
  geom_line(aes(Date, diff)) +
  # add line at 1974
  geom_vline(xintercept= (as.Date("1974-01-01")),
             linetype=1, colour="grey50") +
  annotate('text', label = 'Watershed 6 logged 100% in 1974', x = as.Date("1977-08-15"), y = 0.016, size = 3, color = 'grey50') +
  scale_x_date(date_breaks = '1 year', date_labels = '%Y') +
  theme_classic() +
  labs(x='', y='Difference in nitrate flux'~(kg~ha^-1~day^-1)) +
  theme(text = element_text(family = 'serif'),
        axis.text = element_text(size = 9),
        axis.title = element_text(size = 9))

# 4. patchwork plots together ####
rawdata/diffplot +
  plot_annotation(tag_levels = 'a', tag_suffix = ')')

ggsave('Figures/Andrews_compare.png', width = 6.5, height = 4.5, dpi=1200)


# flux$site_code = factor(flux$site_code, levels=c('GSWS08', 'GSWS06'))
# 
# szn_cols <- c("#7EA8C4","#EFD15E","#E6A0C4")
# 
# # fluxes at Andrews
# ggplot(flux |> filter(site_code == 'GSWS08', 
#                                         waterYear%in%c(1986,1996,2010)) |>
#                            mutate(waterYear=factor(waterYear)), 
#                          aes(waterYear,flux_kg_ha_day, fill=season)) +
#   geom_bar(stat='identity') +
#   labs(y = "", x = "") +
#   scale_fill_manual('', values=szn_cols) +
#   theme_minimal()+
#   theme(plot.title = element_text(face = 'bold', family = 'serif', size = rel(0.75),
#                                   hjust = 0.5),
#         text = element_text(family = 'serif'),
#         axis.text = element_text(size = 8),
#         axis.title = element_text(size =8),
#         legend.position = 'none')
# 
# 
# ggplot(flux |> filter(site_code == 'GSWS06') |>
#          mutate(waterYear=factor(waterYear)) |>
#                   filter(annual_flux < 0.005), 
#        aes(waterYear,flux_kg_ha_day, fill=season)) +
#   geom_bar(stat='identity') +
#   labs(y = "", x = "") +
#   scale_fill_manual('', values=szn_cols) +
#   theme_minimal()+
#   theme(plot.title = element_text(face = 'bold', family = 'serif', size = rel(0.75),
#                                   hjust = 0.5),
#         text = element_text(family = 'serif'),
#         axis.text = element_text(size = 8),
#         axis.title = element_text(size =8),
#         legend.position = 'none')
# 
# 
# 
# 
# # create insert for fluxes at Andrews to view lower flux years
# ggplot(flux |> filter(site_code == 'GSWS08', 
#                                        annual_flux < 0.02), aes(waterYear,flux_kg_ha_day, fill=season)) +
#   geom_bar(stat='identity') +
#   labs(y = "Nitrate flux "~(kg~ha^-1~day^-1), x = "Water Year", title='Andrews - elevation 1182m') +
#   scale_fill_manual('', values=szn_cols) +
#   theme_bw()+
#   theme(plot.title = element_text(face = 'bold', family = 'serif', size = rel(0.75),
#                                   hjust = 0.5),
#         text = element_text(family = 'serif'),
#         axis.text = element_text(size = 8),
#         axis.title = element_text(size =8)) +
#   # bring out high snow years
#   geom_point(flux |> filter(site_code=='GSWS08' & waterYear==1993), mapping=aes(1993, 0.0025), shape=8, size=2) +
#   # bring out low snow years
#   geom_point(cq_slopes |> filter(site_code=='GSWS08' & waterYear==2003), mapping=aes(2003, 0.001), shape=25, size=2, fill = 'black')
# andy_low_flux
