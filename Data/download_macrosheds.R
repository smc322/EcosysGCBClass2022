# download macrosheds data

library(tidyverse)
library(macrosheds)

?ms_download_core_data
ms_download_core_data(my_ms_dir, domains=c('hjandrews','niwot'))

# list available Macrosheds R package functions
help(package = macrosheds)

#ms_vars <- ms_load_variables()


?ms_load_sites
site_data <- ms_load_sites()

?ms_load_product
my_ms_dir <- "./data/macrosheds"
my_chem <- ms_load_product(
  my_ms_dir,
  prodname = "stream_chemistry",
  filter_vars = "NO3_N", # units == mg/L
  site_codes = c("GSWS06", "GSWS08",'GREEN4'),
  sort_result = TRUE,
  warn = TRUE
)


my_q <- ms_load_product(
  my_ms_dir,
  prodname = "discharge", # units == L/s
  site_codes = c("GSWS06", "GSWS08",'GREEN4'),
  sort_result = TRUE,
  warn = TRUE
)

my_chem_2 <- my_chem |>
  drop_na(val) |>
  rename(nitrate_mgL = val,
         date = datetime)  |>
  select(-var, -ms_status, -ms_interp, -val_err)

my_q_2 <- my_q |>
  rename(discharge_Ls = val,
         date=datetime) |>
  select(-var, -ms_status, -ms_interp, -val_err)


andrews <- left_join(my_chem_2, my_q_2) |>
  filter(year(date) >= 1971) |>
  filter(site_code %in% c('GSWS06', 'GSWS08'))

niwot <- left_join(my_chem_2, my_q_2) |>
  filter(year(date) >= 1971) |>
  filter(site_code %in% c('GREEN4'))


write.csv(andrews, 'Data/macrosheds_andrews.csv')
write.csv(niwot, 'Data/macrosheds_niwot.csv')
