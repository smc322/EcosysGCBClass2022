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
library(segmented)
lm.1 <- lm(pdsi~fakeDate, pdsi_long)

breakpoint <- segmented(lm.1, seg.Z = ~fakeDate,
                        psi = list(fakeDate = c(as.Date('2000-01-01'), as.Date('2010-01-01'))))
summary(breakpoint)
# get breakpoints:
breakpoint$psi

as.Date(13312.82)

as.Date(14518.00)

### try it with annual data to see how that compares ####
lm.2 <- lm(PDSI~Year, pdsi_yearly)

breakpoint <- segmented(lm.2, seg.Z = ~Year,
                        psi = list(Year = c(2000, 2010)))
summary(breakpoint)
# get breakpoints:
breakpoint$psi
#1998 and 2000 - weird

# try again with psi = NA to see how that compares

breakpoint <- segmented(lm.2, seg.Z = ~Year,
                        psi = NA)
summary(breakpoint)
# get breakpoints:
breakpoint$psi
# no points -- when I did this with all data, there were 10 breakpoints

# Precip ####
p <- read.csv('Data/LochValeClimate_IMERG_07312023/totalprecip_mm_monthly.csv', skip=7) |>
  rename(date=1,
         precip=2) |>
  mutate(date=as.Date(date))

# total monthy precip
ggplot(p, aes(date, precip)) +
  geom_bar(stat='identity', fill='lightgrey') +
  geom_line()


# total yearly precip
p_yearly <- p |>
  mutate(Year = year(date)) |>
  group_by(Year) |>
  summarise(PRECIP = sum(precip)/100) #mm to cm

ggplot(p_yearly, aes(Year, PRECIP)) +
  geom_bar(stat='identity', fill='lightgrey') +
  theme_classic() +
  labs(x='',y='Total annual precipitation (cm)')


# Air temp ####
temp <- read.csv('Data/LochValeClimate_IMERG_07312023/surfaceairtemp_C_monthly.csv', skip=7) |>
  rename(date=1,
         temp=2) |>
  mutate(date=as.Date(date),
         Year=year(date)) |>
  group_by(Year) |>
  mutate(average_annual_temp = mean(temp)) |>
  ungroup()

# average monthly air temperature
ggplot(temp) +
  geom_line(aes(date, temp)) +
  geom_point(aes(date, average_annual_temp), color='red')

# average annual temps
ggplot(temp, aes(Year, average_annual_temp)) +
  geom_point() +
  geom_smooth(method='gam')

temp_annual <- temp |>
  select(Year, average_annual_temp) |>
  distinct()

## looks like there might be a trend so let's test ####
### Mann-Kendall test ####
mk.model <- trend::mk.test(temp_annual$average_annual_temp)
mk.model # p > 0.05

### sen slope ####
sen.model <- zyp::zyp.sen(average_annual_temp~Year, temp_annual)
coef(sen.model)

### look at a gam ####
gam.model <- mgcv::gam(average_annual_temp ~ s(Year, k=12),
                       data = temp_annual,
                       method= 'REML')
summary(gam.model)
broom::glance(gam.model)
broom::tidy(gam.model)
# p > 0.05 again


## look at timeseries analysis too ####
temp_ts <- ts(temp |> select(date, temp), frequency=12)

plot.ts(temp_ts)
comp <- decompose(temp_ts)

plot(comp)
