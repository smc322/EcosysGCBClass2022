# get Loch Vale data from USGS
library(tidyverse)
library(dataRetrieval)

data <- readNWISdata(siteNumber='401733105392404', 
                     parameterCd='00060',
                     startDate='1980-01-01',
                     endDate='2019-12-31') |>
  rename(date=dateTime, discharge_cfs=X_00060_00003) |>
  mutate(discharge_Ls = discharge_cfs *28.316846592) |> # convert cfs to L/s
  select(date, discharge_Ls)

nitrate <- readNWISqw(siteNumber='401733105392404', 
                      parameterCd='00618',
                      startDate='1980-01-01',
                      endDate='2019-12-31') |>
  rename(date=sample_dt, Nitrate_mgL=result_va) |>
  select(date, Nitrate_mgL)

lochvale_dat <- left_join(nitrate,data)

write.csv(lochvale_dat, 'Data/USGS_lochvaleoutlet.csv')
