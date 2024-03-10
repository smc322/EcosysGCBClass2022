# Climate figures
library(tidyverse)
library(readxl)

# Palmer Drought Severity Index ####
# greater than 4 is extreme wet, less than -4 is extreme drought

pdsi <- read_xlsx('Data/PDSI_SouthPlatte_HUC8.xlsx')

# average monthly PDSI
pdsi_long <- pivot_longer(pdsi, 2:13, names_to = 'month', values_to = 'pdsi') |>
  mutate(fakeDate = as.Date(paste(Year, month, '01', sep='-'), format='%Y-%b-%d')) 

ggplot(pdsi_long) +
  geom_line(aes(fakeDate,pdsi)) +
  geom_hline(yintercept = 0)  + 
  labs(x = '', y = 'Palmer Drought Severity Index') +
  theme_classic() +
  geom_vline(xintercept = as.numeric(ymd("2000-01-01")), color = 'red') +
  geom_vline(xintercept = as.numeric(ymd("2010-01-01")), color = 'red')

ggplot(pdsi_long, aes(fakeDate,pdsi)) +
  geom_point() +
  geom_smooth(method='gam')

# yearly average PDSI
pdsi_yearly <- pdsi_long |>
  group_by(Year) |>
  summarise(PDSI = mean(pdsi))

ggplot(pdsi_yearly) +
  geom_point(aes(Year, PDSI)) +
  geom_hline(yintercept = 0) + 
  labs(x = '', y = 'Palmer Drought Severity Index') +
  theme_classic()

### try segmented regression/breakpoint analysis ####
# library(segmented)
# lm.1 <- lm(pdsi~fakeDate, pdsi_long)
# 
# breakpoint <- segmented(lm.1, seg.Z = ~fakeDate,
#                         psi = list(fakeDate = c(as.Date('2000-01-01'), as.Date('2010-01-01'))))
# summary(breakpoint)
# # get breakpoints:
# breakpoint$psi
# 
# as.Date(13312.82)
# 
# as.Date(14518.00)
# 
# ### try it with annual data to see how that compares ####
# lm.2 <- lm(PDSI~Year, pdsi_yearly)
# 
# breakpoint <- segmented(lm.2, seg.Z = ~Year,
#                         psi = list(Year = c(2000, 2010)))
# summary(breakpoint)
# # get breakpoints:
# breakpoint$psi
# #1998 and 2000 - weird
# 
# # try again with psi = NA to see how that compares
# 
# breakpoint <- segmented(lm.2, seg.Z = ~Year,
#                         psi = NA)
# summary(breakpoint)
# # get breakpoints:
# breakpoint$psi
# # no points -- when I did this with all data, there were 10 breakpoints

## Try MCMC changepoint analysis ####
# https://lindeloev.github.io/mcp/articles/packages.html
library(mcp)
model = list(pdsi~1, 1~1, 1~1)  # three intercept-only segments
fit_mcp = mcp(model, data = pdsi_long, par_x = "Year")
summary(fit_mcp)
# Breaks at 1999.51 and 2006.50!!!!




library(patchwork)
plot(fit_mcp) + plot_pars(fit_mcp, pars = c("cp_1", "cp_2"), type = "dens_overlay")

ggsave("Figures/LochVale/breakpoint_MCMCfit.png", width = 7.5, height = 5.5, dpi=1200)


ggplot(pdsi_long) +
  geom_line(aes(fakeDate,pdsi)) +
  geom_hline(yintercept = 0)  + 
  labs(x = '', y = 'Palmer Drought Severity Index') +
  theme_classic() +
  geom_vline(xintercept = as.numeric(ymd("2000-01-01")), color = 'red4') +
  geom_vline(xintercept = as.numeric(ymd("2007-01-01")), color = 'red4')
ggsave("Figures/LochVale/PDSI_breakpoints.png", width = 6.5, height = 4.5, dpi=1200)



# Precip ####
# #p <- read.csv('Data/LochValeClimate_IMERG_07312023/totalprecip_mm_monthly.csv', skip=7) |>
#   rename(date=1,
#          precip=2) |>
#   mutate(date=as.Date(date)) |>
#   mutate(mon = month(date),
#          Year = year(date)) |> 
#   mutate(season = case_when(mon %in% c(10,11,12,1,2,3) ~ "Winter",
#                             mon %in% c(4,5,6)  ~ "Snowmelt runoff",
#                             mon %in% c(7,8,9) ~ "Summer")) |>
#   mutate(season = factor(season, levels = c('Winter','Snowmelt runoff','Summer'))) |>
#   mutate(decade = ifelse(year(date) <= 1990, 1, NA),                                       decade = ifelse(between(year(date), 1990, 2000), "1990-2000", decade),                                                                               decade = ifelse(between(year(date), 2000, 2006), "2001-2007 (Drought)", decade),  decade = ifelse(between(year(date), 2007, 2019), "2008-2019", decade)) |>
#   mutate(decade = as.factor(decade))

nadp_weekly <- read.csv('Data/WeeklyLochValeNADP.csv') |>
  select(dateOff, NO3, ppt) |>
  mutate(date = as.Date(dateOff)) |>
  select(-dateOff) |>
  mutate(mon = month(date),
               Year = year(date)) |>
           mutate(season = case_when(mon %in% c(10,11,12,1,2,3) ~ "Winter",
                                     mon %in% c(4,5,6)  ~ "Snowmelt runoff",
                                     mon %in% c(7,8,9) ~ "Summer")) |>
           mutate(season = factor(season, 
                                  levels = c('Winter','Snowmelt runoff','Summer'))) |>
           mutate(decade = ifelse(year(date) <= 1990, 1, NA),                                       decade = ifelse(between(year(date), 1990, 2000), "1990-2000", decade),                                                                               decade = ifelse(between(year(date), 2000, 2006), "2001-2007 (Drought)", decade),  decade = ifelse(between(year(date), 2007, 2019), "2008-2019", decade)) |>
           mutate(decade = as.factor(decade)) |>
  mutate(NO3=ifelse(NO3 == -9, NA, NO3),
         ppt=ifelse(ppt < 0, NA, ppt)) |>
  filter(decade != 1)



# # total monthy precip
# ggplot(p, mapping = aes(date, precip)) +
#   geom_bar(stat='identity', fill='lightgrey') +
#   geom_line(color='grey40') +
#   geom_point(aes(color=season)) +
#   scale_color_manual('',values = c("#7EA8C4","#EFD15E","#E6A0C4")) +
#   theme_classic() +
#   labs(x='',y='Total monthly precipitation (mm)')
# ggsave('Figures/LochVale/monthlyprecip.png', width = 6.5, height = 4.5, dpi=1200)

# total yearly precip
p_yearly <- nadp_weekly |>
  group_by(Year, season) |>
  summarise(PRECIP = sum(ppt, na.rm=TRUE)/100) #mm to cm 

precip <- ggplot(p_yearly, aes(Year, PRECIP, fill=season)) +
  geom_bar(stat='identity') +
  theme_classic() +
  labs(x='',y='Total annual precipitation (cm)')  +
  scale_fill_manual('',values = c("#7EA8C4","#EFD15E","#E6A0C4")) +
  geom_vline(xintercept = 2002, color = 'red4') +
  geom_vline(xintercept = 2009, color = 'red4')
#ggsave('Figures/LochVale/annualprecip.png', width = 6.5, height = 4.5, dpi=1200)


p_ann <-nadp_weekly |>
  group_by(Year, decade) |>
  summarise(PRECIP = sum(ppt, na.rm=TRUE)/100) #mm to cm 

ggplot(p_ann, aes(decade, PRECIP)) +
  geom_boxplot()
# no diff between 1990-2000 and 2008-2019

## Try MCMC changepoint analysis for precip ####
# https://lindeloev.github.io/mcp/articles/packages.html
library(mcp)
model = list(ppt~1, 1~1, 1~1)  # three intercept-only segments
fit_mcp = mcp(model, data = nadp_weekly, par_x = "Year")
summary(fit_mcp)
# Breaks at 2002 and 2009


library(patchwork)
plot(fit_mcp) + plot_pars(fit_mcp, pars = c("cp_1", "cp_2"), type = "dens_overlay")



## check for trend - unlikely ####
### Mann-Kendall test ####
mk.model <- trend::mk.test((nadp_weekly |> filter(season=='Summer') |>
                              drop_na(ppt))$ppt)
mk.model # p  = 0.057

mk.model <- trend::mk.test((nadp_weekly |> filter(season=='Snowmelt runoff') |>
                              drop_na(ppt))$ppt)
mk.model # p > 0.05

mk.model <- trend::mk.test((nadp_weekly |> filter(season=='Winter') |>
                              drop_na(ppt))$ppt)
mk.model # p = 0.053







# Air temp ####
temp <- read.csv('Data/LochValeClimate_IMERG_01302024/surfaceairtemp_C_monthly_loch.csv', skip=7) |>
  rename(date=1,
         temp=2) |>
  mutate(date=as.Date(date),
         Year=year(date)) |>
  mutate(mon = month(date)) |> 
  mutate(season = case_when(mon %in% c(10,11,12,1,2,3) ~ "Winter",
                            mon %in% c(4,5,6)  ~ "Snowmelt runoff",
                            mon %in% c(7,8,9) ~ "Summer")) |>
  mutate(season = factor(season, levels = c('Winter','Snowmelt runoff','Summer'))) |>
  group_by(Year, season) |>
  mutate(average_seasonal_temp = mean(temp)) |>
  ungroup()  

# average monthly air temperature
a<-ggplot(temp) +
  geom_line(aes(date, temp), color='grey70') +
  geom_point(aes(date, average_seasonal_temp, color=season)) +
  scale_color_manual('',values = c("#7EA8C4","#EFD15E","#E6A0C4")) +
  theme_classic() +
  labs(x='',y='Monthly temperature ('~degree*C*')')
#ggsave('Figures/LochVale/monthlytemp.png', width = 6.5, height = 4.5, dpi=1200)

# average annual temps
ggplot(temp, aes(Year, average_seasonal_temp, color=season)) +
  geom_point() +
  geom_smooth(method='gam') +
  scale_color_manual('',values = c("#7EA8C4","#EFD15E","#E6A0C4")) 

temp_annual <- temp |>
  select(Year, season, average_seasonal_temp) |>
  distinct()

## looks like there might be a trend so let's test ####
### Mann-Kendall test ####
mk.model <- trend::mk.test((temp_annual |> filter(season=='Summer'))$average_seasonal_temp)
mk.model # p == 0.04

mk.model <- trend::mk.test((temp_annual |> filter(season=='Snowmelt runoff'))$average_seasonal_temp)
mk.model # p > 0.05

mk.model <- trend::mk.test((temp_annual |> filter(season=='Winter'))$average_seasonal_temp)
mk.model # p > 0.05

### sen slope ####
summerslope <- temp_annual |> filter(season=='Summer')
sen.model <- zyp::zyp.sen(average_seasonal_temp ~ Year, summerslope)
coef(sen.model)

b<-ggplot(summerslope, aes(Year, average_seasonal_temp)) +
  geom_point() +
  geom_abline(intercept = coef(sen.model)[[1]], 
              slope = coef(sen.model)[[2]], color = '#E6A0C4') +
  labs(x='', y='Mean summertime air temperature ('~degree*C*')') +
  theme_classic() +
  annotate('text', x=2015, y=13.5, label = 'p-value < 0.05; slope = 0.037')
  
a/b +
  plot_annotation(tag_levels = 'a', tag_suffix = ')') 

ggsave('Figures/LochVale/tmp_combinedPlot.png', width = 8.5, height = 6.5, dpi=1200)




# andy Air temp ####
temp <- read.csv('Data/AndrewsClimate_IMERG_01302024/surfaceairtemp_C_monthly_andy.csv', skip=7) |>
  rename(date=1,
         temp=2) |>
  mutate(date=as.Date(date),
         Year=year(date)) |>
  mutate(mon = month(date)) |> 
  mutate(season = case_when(mon %in% c(10,11,12,1,2,3) ~ "Winter",
                            mon %in% c(4,5,6)  ~ "Snowmelt runoff",
                            mon %in% c(7,8,9) ~ "Summer")) |>
  mutate(season = factor(season, levels = c('Winter','Snowmelt runoff','Summer'))) |>
  group_by(Year, season) |>
  mutate(average_seasonal_temp = mean(temp)) |>
  ungroup()  

# average monthly air temperature
a<-ggplot(temp) +
  geom_line(aes(date, temp), color='grey70') +
  geom_point(aes(date, average_seasonal_temp, color=season)) +
  scale_color_manual('',values = c("#7EA8C4","#EFD15E","#E6A0C4")) +
  theme_classic() +
  labs(x='',y='Monthly temperature ('~degree*C*')')
#ggsave('Figures/LochVale/monthlytemp.png', width = 6.5, height = 4.5, dpi=1200)

# average annual temps
ggplot(temp, aes(Year, average_seasonal_temp, color=season)) +
  geom_point() +
  geom_smooth(method='gam') +
  scale_color_manual('',values = c("#7EA8C4","#EFD15E","#E6A0C4")) 

temp_annual <- temp |>
  select(Year, season, average_seasonal_temp) |>
  distinct()

## looks like there might be a trend so let's test ####
### Mann-Kendall test ####
mk.model <- trend::mk.test((temp_annual |> filter(season=='Summer'))$average_seasonal_temp)
mk.model # p == 4.323e-05

mk.model <- trend::mk.test((temp_annual |> filter(season=='Snowmelt runoff'))$average_seasonal_temp)
mk.model # 0.02309

mk.model <- trend::mk.test((temp_annual |> filter(season=='Winter'))$average_seasonal_temp)
mk.model # p > 0.05

### sen slope ####
summerslope <- temp_annual |> filter(season=='Summer')
sen.model <- zyp::zyp.sen(average_seasonal_temp ~ Year, summerslope)
coef(sen.model)

b<-ggplot(summerslope, aes(Year, average_seasonal_temp)) +
  geom_point() +
  geom_abline(intercept = coef(sen.model)[[1]], 
              slope = coef(sen.model)[[2]], color = '#E6A0C4') +
  labs(x='', y='Mean summertime ('~degree*C*')') +
  theme_classic() +
  annotate('text', x=2010, y=13.5, label = 'p-value < 0.0001; slope = 0.061')


snowmeltslope <- temp_annual |> filter(season=='Snowmelt runoff')
sen.model <- zyp::zyp.sen(average_seasonal_temp ~ Year, snowmeltslope)
coef(sen.model)

c<-ggplot(summerslope, aes(Year, average_seasonal_temp)) +
  geom_point() +
  geom_abline(intercept = coef(sen.model)[[1]], 
              slope = coef(sen.model)[[2]], color = '#EFD15E') +
  labs(x='', y='Mean snowmelt ('~degree*C*')') +
  theme_classic() +
  annotate('text', x=2010, y=13.5, label = 'p-value < 0.05; slope = 0.039')

a/b/c +
  plot_annotation(tag_levels = 'a', tag_suffix = ')') 

ggsave('Figures/LochVale/tmp_combinedPlotANDY.png', width = 8.5, height = 6.5, dpi=1200)


# ### look at a gam ####
# gam.model <- mgcv::gam(average_seasonal_temp ~ s(Year, k=12) * season,
#                        data = temp_annual,
#                        method= 'REML')
# summary(gam.model)
# broom::glance(gam.model)
# broom::tidy(gam.model)
# # p > 0.05 again
# 
# 
# ## look at timeseries analysis too ####
# temp_ts <- ts(temp |> select(date, temp), frequency=12)
# 
# plot.ts(temp_ts)
# comp <- decompose(temp_ts)
# 
# plot(comp)


# N-Deposition data ####
# nadp <- read.csv('Data/LochValeNADP.csv') |>
#   rename(mon = seas,
#          Year= yr) |>
#   select(mon, Year, NO3) |>
#   mutate(fakeDate = as.Date(paste(Year, mon, '01', sep='-'))) |>
#   mutate(NO3=ifelse(NO3 == -9, NA, NO3)) |>
#   filter(between(Year, 1990, 2019)) |>
#   mutate(season = case_when(mon %in% c(10,11,12,1,2,3) ~ "Winter",
#                             mon %in% c(4,5,6)  ~ "Snowmelt runoff",
#                             mon %in% c(7,8,9) ~ "Summer")) |>
#   mutate(season = factor(season, levels = c('Winter','Snowmelt runoff','Summer'))) 
# 
# nadp_yearly <- nadp |>
#   group_by(Year) |>
#   mutate(annualNO3 = mean(NO3, na.rm = TRUE)) |>
#   ungroup() |>
#   select(Year, annualNO3) |>
#   distinct() |>
#   mutate(fakeDate = as.Date(paste(Year, '01', '01', sep='-')))

no3_yearly <- nadp_weekly |>
  group_by(Year) |>
  mutate(annualNO3 = mean(NO3, na.rm = TRUE)) |>
  ungroup() |>
  select(Year, annualNO3) |>
  distinct() |>
  mutate(fakeDate = as.Date(paste(Year, '01', '01', sep='-')))

ndep <- ggplot() +
  geom_point(nadp_weekly, mapping=aes(date, NO3, color=season), show.legend = FALSE) +
  geom_point(no3_yearly, mapping=aes(fakeDate, annualNO3, shape='Annual average nitrate-deposition')) +
  scale_color_manual('',values = c("#7EA8C4","#EFD15E","#E6A0C4")) +
  scale_shape_manual('', values=c(0)) +
  theme_classic() +
  labs(x='',y='Nitrate deposition ('~mg*L^-1*')') +
  geom_vline(xintercept = as.numeric(ymd("2004-01-01")), color = 'red4') +
  geom_vline(xintercept = as.numeric(ymd("2009-01-01")), color = 'red4') + theme(
    legend.position = c(.95, .95),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6)
    )
# ggsave('Figures/LochVale/NO3dep.png', width = 6.5, height = 4.5, dpi=1200)
  

## Try MCMC changepoint analysis for nadp ####
# https://lindeloev.github.io/mcp/articles/packages.html
library(mcp)
model = list(NO3~1, 1~1, 1~1)  # three intercept-only segments
fit_mcp = mcp(model, data = nadp_weekly, par_x = "Year")
summary(fit_mcp)
# Breaks at 2003.66 and 2009


library(patchwork)
precip/ndep +
  plot_annotation(tag_levels = 'a', tag_suffix = ')') #  +
 # plot_layout(guides = 'collect')

ggsave('Figures/LochVale/ndep_precip.png', width = 8.5, height = 6.5, dpi=1200)



# snowpack ####
snowpack <- read.csv('Data/LochValeClimate_IMERG_01302024/snowdepth_m_monthly_loch.csv', skip=7) |>
  rename(date=1,
         snowpack_m=2) |>
  mutate(date=as.Date(date)) |>
  mutate(mon = month(date),
         Year = year(date)) |>
  mutate(season = case_when(mon %in% c(10,11,12,1,2,3) ~ "Winter",
                            mon %in% c(4,5,6)  ~ "Snowmelt runoff",
                            mon %in% c(7,8,9) ~ "Summer")) |>
  mutate(season = factor(season, levels = c('Winter','Snowmelt runoff','Summer'))) |>
  mutate(decade = ifelse(year(date) <= 1990, 1, NA),                                       decade = ifelse(between(year(date), 1990, 2000), "1990-2000", decade),                                                                               decade = ifelse(between(year(date), 2000, 2007), "2001-2007 (Drought)", decade),  decade = ifelse(between(year(date), 2008, 2019), "2008-2019", decade)) |>
  mutate(decade = as.factor(decade)) |>
  drop_na(decade)
library(plotly)
ggplotly(ggplot(snowpack, aes(date, snowpack_m)) +
  geom_line())

ggplot(snowpack, aes(Year, snowpack_m, fill=season)) +
  geom_bar(stat='identity') +
  scale_fill_manual('',values = c("#7EA8C4","#EFD15E","#E6A0C4")) +
  theme_classic() +
  labs(x = '', y = 'Snow depth (m)')

ggsave('Figures/LochVale/snowpack.png', width = 6.5, height = 4.5, units = 'in', dpi = 1200)

## is there trend in snowpack ####

annualsno <- snowpack |>
  group_by(Year) |>
  summarise(snow = sum(snowpack_m)) |>
  ungroup()


mk.model <- trend::mk.test(annualsno$snow)
mk.model # p > 0.05

ggplot(annualsno, aes(Year, snow)) +
  geom_point()

 snowpack |>
  group_by(mon) |>
  summarise(avesnow = mean(snowpack_m)) |>
  ungroup() |>
  mutate(mon = as.factor(mon)) |>
  ggplot() +
  geom_bar(stat='identity', aes(mon, avesnow)) +
  labs(x='Month', y='Average snowpack 1990-2019 (m)') +
  theme_classic()
 ggsave('Figures/LochVale/snow_season.png', height = 4.5, width = 6.5, units = 'in', dpi=1200)
 
 

 
 # andrews snowpack ####
 snowpack <- read.csv('Data/AndrewsClimate_IMERG_01302024/snowdepth_m_monthly_andy.csv', skip=7) |>
   rename(date=1,
          snowpack_m=2) |>
   mutate(date=as.Date(date)) |>
   mutate(mon = month(date),
          Year = year(date)) |>
   mutate(season = case_when(mon %in% c(10,11,12,1,2,3) ~ "Winter",
                             mon %in% c(4,5,6)  ~ "Snowmelt runoff",
                             mon %in% c(7,8,9) ~ "Summer")) |>
   mutate(season = factor(season, levels = c('Winter','Snowmelt runoff','Summer'))) |>
   mutate(decade = ifelse(year(date) <= 1990, 1, NA),                                       decade = ifelse(between(year(date), 1990, 2000), "1990-2000", decade),                                                                               decade = ifelse(between(year(date), 2000, 2007), "2001-2007 (Drought)", decade),  decade = ifelse(between(year(date), 2008, 2019), "2008-2019", decade)) |>
   mutate(decade = as.factor(decade)) |>
   drop_na(decade)

 ggplot(snowpack, aes(Year, snowpack_m, fill=season)) +
   geom_bar(stat='identity') +
   scale_fill_manual('',values = c("#7EA8C4","#EFD15E","#E6A0C4")) +
   theme_classic() +
   labs(x = '', y = 'Snow depth (m)')
 
 ggsave('Figures/LochVale/snowpackANDY.png', width = 6.5, height = 4.5, units = 'in', dpi = 1200)
 
 
