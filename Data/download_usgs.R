# get Loch Vale data from USGS
library(tidyverse)
library(dataRetrieval)

data <- readNWISdata(siteNumber='401733105392404', 
                     parameterCd='00060',
                     startDate='1990-01-01',
                     endDate='2019-12-31') |>
  rename(date=dateTime, discharge_cfs=X_00060_00003) |>
  mutate(discharge_rate = discharge_cfs *0.028316846592) |> # convert cfs to cms
  select(date, discharge_rate)

nitrate <- readNWISqw(siteNumber='401733105392404', 
                      parameterCd='00618',
                      startDate='1990-01-01',
                      endDate='2019-12-31') |>
  rename(date=sample_dt, Nitrate_mgl=result_va) |>
  select(date, Nitrate_mgl)

lochvale_dat <- left_join(nitrate,data)

write.csv(lochvale_dat, 'Data/USGS_lochvaleoutlet.csv')
