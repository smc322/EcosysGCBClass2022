# Climate data

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

## try segmented regression/breakpoint analysis ####
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

# Try MCMC changepoint analysis ####
# https://lindeloev.github.io/mcp/articles/packages.html
library(mcp)
model = list(pdsi~1, 1~1, 1~1)  # three intercept-only segments
fit_mcp = mcp(model, data = pdsi_long, par_x = "Year")
summary(fit_mcp)
# Breaks at 1999.51 and 2006.50!!!!




library(patchwork)
plot(fit_mcp) + plot_pars(fit_mcp, pars = c("cp_1", "cp_2"), type = "dens_overlay")

ggsave("Figures/breakpoint_MCMCfit.png", width = 7.5, height = 5.5, dpi=1200)


ggplot(pdsi_long) +
  geom_line(aes(fakeDate,pdsi)) +
  geom_hline(yintercept = 0)  + 
  labs(x = '', y = 'Palmer Drought Severity Index') +
  theme_classic() +
  geom_vline(xintercept = as.numeric(ymd("2000-01-01")), color = 'red4') +
  geom_vline(xintercept = as.numeric(ymd("2007-01-01")), color = 'red4')
ggsave("Figures/PDSI_breakpoints.png", width = 6.5, height = 4.5, dpi=1200)



# Precip ####
p <- read.csv('Data/LochValeClimate_IMERG_07312023/totalprecip_mm_monthly.csv', skip=7) |>
  rename(date=1,
         precip=2) |>
  mutate(date=as.Date(date)) |>
  mutate(mon = month(date)) |> 
  mutate(season = case_when(mon %in% c(10,11,12,1,2,3) ~ "Winter",
                            mon %in% c(4,5,6)  ~ "Snowmelt runoff",
                            mon %in% c(7,8,9) ~ "Summer")) |>
  mutate(season = factor(season, levels = c('Winter','Snowmelt runoff','Summer'))) 



# total monthy precip
ggplot(p, mapping = aes(date, precip)) +
  geom_bar(stat='identity', fill='lightgrey') +
  geom_line(color='grey40') +
  geom_point(aes(color=season)) +
  scale_color_manual('',values = c("#7EA8C4","#EFD15E","#E6A0C4")) +
  theme_classic() +
  labs(x='',y='Total monthly precipitation (mm)')
ggsave('Figures/monthlyprecip.png', width = 6.5, height = 4.5, dpi=1200)

# total yearly precip
p_yearly <- p |>
  mutate(Year = year(date)) |>
  group_by(Year, season) |>
  summarise(PRECIP = sum(precip)/100) #mm to cm 

ggplot(p_yearly, aes(Year, PRECIP, fill=season)) +
  geom_bar(stat='identity') +
  theme_classic() +
  labs(x='',y='Total annual precipitation (cm)')  +
  scale_fill_manual('',values = c("#7EA8C4","#EFD15E","#E6A0C4")) 
ggsave('Figures/annualprecip.png', width = 6.5, height = 4.5, dpi=1200)


# Air temp ####
temp <- read.csv('Data/LochValeClimate_IMERG_07312023/surfaceairtemp_C_monthly.csv', skip=7) |>
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
ggplot(temp) +
  geom_line(aes(date, temp), color='grey70') +
  geom_point(aes(date, average_seasonal_temp, color=season)) +
  scale_color_manual('',values = c("#7EA8C4","#EFD15E","#E6A0C4")) +
  theme_classic() +
  labs(x='',y='Monthly temperature ('~degree*C*')')
ggsave('Figures/monthlytemp.png', width = 6.5, height = 4.5, dpi=1200)

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
mk.model # p == 0.03

mk.model <- trend::mk.test((temp_annual |> filter(season=='Snowmelt runoff'))$average_seasonal_temp)
mk.model # p > 0.05

mk.model <- trend::mk.test((temp_annual |> filter(season=='Winter'))$average_seasonal_temp)
mk.model # p > 0.05

### sen slope ####
summerslope <- temp_annual |> filter(season=='Summer')
sen.model <- zyp::zyp.sen(average_seasonal_temp ~ Year, summerslope)
coef(sen.model)

ggplot(summerslope, aes(Year, average_seasonal_temp)) +
  geom_point() +
  geom_abline(intercept = coef(sen.model)[[1]], 
              slope = coef(sen.model)[[2]], color = '#E6A0C4') +
  labs(x='', y='Mean summertime air temperature ('~degree*C*')') +
  theme_classic() +
  annotate('text', x=2015, y=13.5, label = 'p-value < 0.05; slope = 0.037')
  
ggsave('Figures/summer_MKtest.png', width = 6.5, height = 4.5, dpi=1200)


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
nadp <- read.csv('Data/LochValeNADP.csv') |>
  rename(mon = seas,
         Year= yr) |>
  select(mon, Year, NO3) |>
  mutate(fakeDate = as.Date(paste(Year, mon, '01', sep='-'))) |>
  mutate(NO3=ifelse(NO3 == -9, NA, NO3)) |>
  filter(between(Year, 1990, 2019)) |>
  mutate(season = case_when(mon %in% c(10,11,12,1,2,3) ~ "Winter",
                            mon %in% c(4,5,6)  ~ "Snowmelt runoff",
                            mon %in% c(7,8,9) ~ "Summer")) |>
  mutate(season = factor(season, levels = c('Winter','Snowmelt runoff','Summer'))) 

nadp_yearly <- nadp |>
  group_by(Year) |>
  mutate(annualNO3 = mean(NO3, na.rm = TRUE)) |>
  ungroup() |>
  select(Year, annualNO3) |>
  distinct() |>
  mutate(fakeDate = as.Date(paste(Year, '01', '01', sep='-')))

ggplot() +
  geom_point(nadp, mapping=aes(fakeDate, NO3, color=season)) +
  geom_point(nadp_yearly, mapping=aes(fakeDate, annualNO3, shape='Annual average')) +
  scale_color_manual('',values = c("#7EA8C4","#EFD15E","#E6A0C4")) +
  scale_shape_manual('', values=c(0)) +
  theme_classic() +
  labs(x='',y='Nitrate wet deposition ('~mg*L^-1*')')
ggsave('Figures/wetNdep.png', width = 6.5, height = 4.5, dpi=1200)
  

## Try MCMC changepoint analysis for nadp ####
# https://lindeloev.github.io/mcp/articles/packages.html
library(mcp)
model = list(NO3~1, 1~1, 1~1)  # three intercept-only segments
fit_mcp = mcp(model, data = nadp, par_x = "Year")
summary(fit_mcp)
# Breaks at 1999.75 and 2008.09!!!!
