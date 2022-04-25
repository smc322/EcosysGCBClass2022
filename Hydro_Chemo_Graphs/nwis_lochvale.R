
# 00060 discharge cms
# 00618	Nutrient	Nitrate, water, filtered, milligrams per liter as nitrogen

library(tidyverse)
library(dataRetrieval)

# There are three potential sites we can use from Loch Vale for creating hydrochemographs
andrews_hydro <- whatNWISdata(siteNumbers = "401723105400000", service = "dv", statCd="00003")
andrews_wq <- whatNWISdata(siteNumbers = "401723105400000", service = "qw", statCd="00003")

# Andrews Creek has discharge data for 1991-09-30 to 2022-04-06 (at least daily values)
# Andrews Creek has:
##  DON mg/L as N - 00607 2010-10-19 to 2020-10-01
##  Ammonia (NH3 and NH4) mg/L as N - 00608 1992-04-15 to 2003-09-30
##  NO3 mg/L as N - 00618 1992-04-15 to 2020-02-11
##  PO4 mg/L as P - 00671 1994-09-16 to 2002-08-13

icybrook_hydro <- whatNWISdata(siteNumbers = "401707105395000", service = "dv", statCd="00003")
icybrook_wq <- whatNWISdata(siteNumbers = "401707105395000", service = "qw", statCd="00003")


lochvale_hydro <- whatNWISdata(siteNumbers = "401733105392404", service = "dv", statCd="00003")
lochvale_wq <- whatNWISdata(siteNumbers = "401733105392404", service = "qw", statCd="00003")




