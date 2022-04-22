

# 00618	Nutrient	Nitrate, water, filtered, milligrams per liter as nitrogen

library(tidyverse)
library(dataRetrieval)


tbl <- whatNWISdata(siteNumbers = "402114105350101", service = "dv", statCd="00003")
tbl <- whatNWISdata(siteNumbers = "402114105350101", service = "qw", statCd="00003")
tbl <- whatNWISdata(siteNumbers = "401733105392404", service = "dv", statCd="00003")
tbl <- whatNWISdata(siteNumbers = "401723105400000", service = "dv", statCd="00003")
tbl <- whatNWISdata(siteNumbers = "401723105400000", service = "qw")

