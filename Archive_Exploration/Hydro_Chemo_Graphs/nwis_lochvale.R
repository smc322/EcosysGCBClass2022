
# 00060 discharge cms
# 00618	Nutrient	Nitrate, water, filtered, milligrams per liter as nitrogen

library(tidyverse)
library(dataRetrieval)

# There are three potential sites we can use from Loch Vale for creating hydrochemographs
andrews_hydro <- whatNWISdata(siteNumbers = "401723105400000", service = "dv", statCd="00003")
andrews_wq <- whatNWISdata(siteNumbers = "401723105400000", service = "qw", statCd="00003")

# Andrews Creek has discharge data for 1991-09-30 to 2022-04-06 (at least daily values)
# Andrews Creek has (it has a lot more than these too):
##  TN mg/L - 00600 2010-10-19 to 2020-10-01
##  DON mg/L as N - 00607 2010-10-19 to 2020-10-01
##  Ammonia (NH3 and NH4) mg/L as N - 00608 1992-04-15 to 2003-09-30
##  NO3 mg/L as N - 00618 1992-04-15 to 2020-02-11
##  TDP mg/L - 00666 1994-09-16 to 2002-08-13
##  TP mg/L - 00665 2002-08-13 to 2022-03-02

icybrook_hydro <- whatNWISdata(siteNumbers = "401707105395000", service = "dv", statCd="00003")
icybrook_wq <- whatNWISdata(siteNumbers = "401707105395000", service = "qw", statCd="00003")

# Icy Brook has discharge data for 1993-10-01 to 2010-09-13
# Icy Brook has (it has a lot more than these too):
##  Ammonia (NH3 and NH4) mg/L as N - 00608 1992-05-05 to 2003-09-30
##  NO3 mg/L as N - 00618 1991-05-17 to 2016-07-26

lochvale_hydro <- whatNWISdata(siteNumbers = "401733105392404", service = "dv", statCd="00003")
lochvale_wq <- whatNWISdata(siteNumbers = "401733105392404", service = "qw", statCd="00003")
# Loch Vale has discharge data for 1983-09-30 to 2022-04-21
# Loch Vale has (it has a lot more than these too):
##  TN mg/L - 00600 2012-04-10 to 2020-10-01
##  TDN mg/L - 00602 1999-06-01 to 2018-12-18
##  Ammonia (NH3 and NH4) mg/L as N - 00608 1983-06-10 to 2018-12-18
##  NO3 mg/L as N - 00618 1982-05-11 to 2019-10-24
##  PO4 mg/L as P - 00671 1982-05-11 to 2018-12-18


andrews_discharge <- readNWISdv("401723105400000", "00060", "","")
andrews_nitrate <- readNWISqw("401723105400000", "00618", "","")

icybrook_discharge <- readNWISdv("401707105395000", "00060", "","")
icybrook_nitrate <- readNWISqw("401707105395000", "00618", "","")
  
lochvale_discharge <-readNWISdv("401733105392404", "00060", "","")
lochvale_wq <- readNWISqw("401733105392404", c("00618", "00671"), "","") 

lochvale <- lochvale_discharge |>
  select(Date, X_00060_00003) |>
  group_by(Date) |>
  summarise(Discharge_cms = mean(X_00060_00003)) |> # there were no overlapping observations in each date
  ungroup() |> 
  mutate(Discharge_cms = ifelse(Discharge_cms < 0, 0, Discharge_cms)) # some negative values between 2009-01-30 through 2009-04-17, possibly from ice cover or low water? 

lochvalewq1 <- lochvale_wq |>
  select(site_no, sample_dt, parm_cd, result_va) |>
  rename(Date = sample_dt) |>
  group_by(site_no, Date, parm_cd) |>
  summarise(result_va = mean(result_va)) |> # there are multiple observations per day 
  ungroup() |>
  pivot_wider(names_from = parm_cd, values_from = result_va) |>
  rename(Nitrate_mgl = `00618`,
         Phosphate_mgl = `00671`)

lochvale_fin <- left_join(lochvalewq1, lochvale)

# get molar masses of P and N 
library(biogas)
Pmol <- molMass("P")
Nmol <- molMass("N")

lochvale_fin1 <- lochvale_fin |>
  mutate(nitrate_umol = Nitrate_mgl/Nmol * 1000,
         phosphate_umol = Phosphate_mgl/Pmol * 1000) |>
  mutate(no3.po4 = nitrate_umol/phosphate_umol) |>
  rename(discharge_rate = Discharge_cms) # renaming to match the function

write.csv(lochvale_fin1, "Hydro_Chemo_Graphs/Data/USGS_lochvaleoutlet.csv")

citation(package = "dataRetrieval")






andrews <- andrews_discharge |>
  select(Date, X_00060_00003) |>
  group_by(Date) |>
  summarise(Discharge_cms = mean(X_00060_00003)) |> # there were no overlapping observations in each date
  ungroup() |> 
  mutate(discharge_rate = ifelse(Discharge_cms < 0, 0, Discharge_cms)) # some negative values between 2009-01-30 through 2009-04-17, possibly from ice cover or low water? 

andrewswq1 <- andrews_nitrate |>
  select(site_no, sample_dt, parm_cd, result_va) |>
  rename(Date = sample_dt) |>
  group_by(site_no, Date, parm_cd) |>
  summarise(result_va = mean(result_va)) |> # there are multiple observations per day 
  ungroup() |>
  pivot_wider(names_from = parm_cd, values_from = result_va) |>
  rename(Nitrate_mgl = `00618`)

andrews_fin <- left_join(andrewswq1, andrews)

write.csv(andrews_fin, "Hydro_Chemo_Graphs/Data/USGS_andrewslochvale.csv")




