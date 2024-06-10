# Changepoint analysis for LochVale

library(tidyverse)
library(lubridate)
library(scales)
library(patchwork)
library(mcp)

# 1. LochVale Breakpoints ####
## 1a. PDSI breakpoint ####
# Palmer Drought Severity Index: greater than 4 is extreme wet, less than -4 is extreme drought

pdsi <- read_xlsx('Data/PDSI_SouthPlatte_HUC8.xlsx')

# average monthly PDSI
pdsi_long <- pivot_longer(pdsi, 2:13, names_to = 'month', values_to = 'pdsi') |>
  mutate(fakeDate = as.Date(paste(Year, month, '01', sep='-'), format='%Y-%b-%d')) 

ggplot(pdsi_long) +
  geom_line(aes(fakeDate,pdsi)) +
  geom_hline(yintercept = 0)  + 
  labs(x = '', y = 'Palmer Drought Severity Index') +
  theme_classic() 

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


### MCMC changepoint analysis - PDSI ####
# https://lindeloev.github.io/mcp/articles/packages.html
model = list(pdsi~1, 1~1, 1~1)  # three intercept-only segments
fit_mcp = mcp(model, data = pdsi_long, par_x = "Year")
summary(fit_mcp)
# Breaks at 1999.51 and 2006.51!!!!

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

## 1b. precip and NO3 breakpoints ####
nadp_weekly <- read.csv('Data/WeeklyLochValeNADP.csv') |>
  select(dateOff, NO3, ppt) |>
  mutate(date = as.Date(dateOff)) |>
  select(-dateOff) |>
  mutate(mon = month(date),
         Year = year(date)) |>
  mutate(season = case_when(mon %in% c(12,1,2,3,4) ~ "Winter",
                            mon %in% c(5,6)  ~ "Snowmelt runoff",
                            mon %in% c(7,8,9,10,11) ~ "Summer")) |>
  mutate(season = factor(season, 
                         levels = c('Winter','Snowmelt runoff','Summer'))) |>
  mutate(decade = ifelse(year(date) <= 1990, 1, NA),                                       decade = ifelse(between(year(date), 1990, 2000), "1990-2000", decade),                                                                               decade = ifelse(between(year(date), 2000, 2006), "2001-2007 (Drought)", decade),  decade = ifelse(between(year(date), 2007, 2019), "2008-2019", decade)) |>
  mutate(decade = as.factor(decade)) |>
  mutate(NO3=ifelse(NO3 == -9, NA, NO3),
         ppt=ifelse(ppt < 0, NA, ppt)) |>
  filter(decade != 1)

### precip ####
p_ann <-nadp_weekly |>
  group_by(Year, decade) |>
  summarise(PRECIP = sum(ppt, na.rm=TRUE)/100) #mm to cm 

ggplot(p_ann, aes(decade, PRECIP)) +
  geom_boxplot()
# no diff between 1990-2000 and 2008-2019

#### fit mcp precip ####
model = list(ppt~1, 1~1, 1~1)  # three intercept-only segments
fit_mcp = mcp(model, data = nadp_weekly, par_x = "Year")
summary(fit_mcp)
# Breaks at 2001 and 2007


plot(fit_mcp) + plot_pars(fit_mcp, pars = c("cp_1", "cp_2"), type = "dens_overlay")


# total yearly precip + breakpoints
p_yearly <- nadp_weekly |>
  group_by(Year, season) |>
  summarise(PRECIP = sum(ppt, na.rm=TRUE)/100) #mm to cm 

precip <- ggplot(p_yearly, aes(Year, PRECIP, fill=season)) +
  geom_bar(stat='identity') +
  theme_classic() +
  labs(x='',y='Total annual precipitation (cm)')  +
  scale_fill_manual('',values = c("#7EA8C4","#EFD15E","#E6A0C4")) +
  geom_vline(xintercept = 2001, color = 'red4') +
  geom_vline(xintercept = 2007, color = 'red4')

precip

## 1c. Nitrate deposition changepoints ####
no3_yearly <- nadp_weekly |>
  group_by(Year) |>
  mutate(annualNO3 = mean(NO3, na.rm = TRUE)) |>
  ungroup() |>
  select(Year, annualNO3) |>
  distinct() |>
  mutate(fakeDate = as.Date(paste(Year, '01', '01', sep='-')))


# fit mcp
model = list(NO3~1, 1~1, 1~1)  # three intercept-only segments
fit_mcp = mcp(model, data = nadp_weekly, par_x = "Year")
summary(fit_mcp)
# Breaks at 2005.01 and 2011.72


ndep <- ggplot() +
  geom_point(nadp_weekly, mapping=aes(date, NO3, color=season), show.legend = FALSE) +
  geom_point(no3_yearly, mapping=aes(fakeDate, annualNO3, shape='Annual average nitrate-deposition')) +
  scale_color_manual('',values = c("#7EA8C4","#EFD15E","#E6A0C4")) +
  scale_shape_manual('', values=c(0)) +
  theme_classic() +
  labs(x='',y='Nitrate deposition ('~mg*L^-1*')') +
  geom_vline(xintercept = as.numeric(ymd("2005-01-01")), color = 'red4') +
  geom_vline(xintercept = as.numeric(ymd("2012-01-01")), color = 'red4') + theme(
    legend.position = c(.95, .95),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6)
  )



# 2. create plot for SI ####
precip/ndep +
  plot_annotation(tag_levels = 'a', tag_suffix = ')') #  +
# plot_layout(guides = 'collect')

ggsave('Figures/LochVale/ndep_precip.png', width = 8.5, height = 6.5, dpi=1200)

