
## Script to create bar graph with nutrient form proportions ##

### Call data
source("Hydro_Chemo_Graphs/Code/CallData.R")

library(zoo)

## Nitrogen ##

# albion #
albion_N <- left_join(AlbChem, AlbDis) |>
  select(local_site, date, NH4., NO3., TN, TDN, PN, DON, discharge, discharge_rate) |>
  mutate_at(3:8, as.numeric) |>
  mutate(NH4. = NH4. * 18.04 * 0.05544) |> # convert ueq/L to umol/L --- eq to g multiply by 18.04, then g to mol, multiply by 0.05544
  mutate(NO3. = NO3. * 62 * 0.01613) |> # convert ueq/L to umol/L --- eq to g multiply by 62, then g to mol, multiply by 0.01613 
  mutate(discharge_L = discharge * 1000) # convert daily discharge colume in cubic meters to liters

micro_to_dailymole <- function(x) { # function to convert micromols/L to daily load of mols 
  x * 0.0000010 * albion_N$discharge_L
}

albion_N1 <- albion_N |>
  mutate_at(3:8, micro_to_dailymole) |> # convert from micromol to mol and calculate the daily load in mol by multiplying by liters per day
  pivot_longer(3:8, names_to = 'Var', values_to = 'dailyload_mols') |>
  select(-3, -4, -5) |>
  drop_na(dailyload_mols)

albion_monthly <- albion_N1 |>
  #mutate(yearmon = )
  group_by(as.yearmon(date), Var) |>
  summarise(monthlyload_mols = sum(dailyload_mols))


ggplot(albion_monthly) +
  geom_col(aes(`as.yearmon(date)`, monthlyload_mols, fill = Var)) +
  theme_light()


ggplot(albion_monthly) +
  geom_line(aes(`as.yearmon(date)`, monthlyload_mols, color = Var)) +
  theme_light() +
  scale_color_viridis_d()





# green lake 4 outlet #
greenlake_N <- left_join(Gl4Chem, Gl4Dis) |>
  select(local_site, date, NH4., NO3., TN, TDN, PN, DON, discharge, discharge_rate) |>
  mutate_at(3:8, as.numeric) |>
  mutate(NH4. = NH4. * 18.04 * 0.05544) |> # convert ueq/L to umol/L --- eq to g multiply by 18.04, then g to mol, multiply by 0.05544
  mutate(NO3. = NO3. * 62 * 0.01613) |> # convert ueq/L to umol/L --- eq to g multiply by 62, then g to mol, multiply by 0.01613 
  mutate(discharge_L = discharge * 1000) # convert daily discharge colume in cubic meters to liters

micro_to_dailymole <- function(x) { # function to convert micromols/L to daily load of mols 
  x * 0.0000010 * greenlake_N$discharge_L
}

greenlake_N1 <- greenlake_N |>
  mutate_at(3:8, micro_to_dailymole) |> # convert from micromol to mol and calculate the daily load in mol by multiplying by liters per day
  pivot_longer(3:8, names_to = 'Var', values_to = 'dailyload_mols') |>
  select(-3, -4, -5) |>
  drop_na(dailyload_mols)

greenlake_monthly <- greenlake_N1 |>
  group_by(as.yearmon(date), Var) |>
  summarise(monthlyload_mols = sum(dailyload_mols))


ggplot(greenlake_monthly) +
  geom_col(aes(`as.yearmon(date)`, monthlyload_mols, fill = Var)) +
  theme_light()


ggplot(greenlake_monthly) +
  geom_line(aes(`as.yearmon(date)`, monthlyload_mols, color = Var)) +
  theme_light() +
  scale_color_viridis_d()



## Phosphourus ##

# albion #
albion_P <- left_join(AlbChem, AlbDis) |>
  select(local_site, date, PO4.., TP, TDP, PP, DOP , discharge, discharge_rate) |>
  mutate_at(3:8, as.numeric) |>
  mutate(NH4. = NH4. * 18.04 * 0.05544) |> # convert ueq/L to umol/L --- eq to g multiply by 18.04, then g to mol, multiply by 0.05544
  mutate(NO3. = NO3. * 62 * 0.01613) |> # convert ueq/L to umol/L --- eq to g multiply by 62, then g to mol, multiply by 0.01613 
  mutate(discharge_L = discharge * 1000) # convert daily discharge colume in cubic meters to liters

micro_to_dailymole <- function(x) { # function to convert micromols/L to daily load of mols 
  x * 0.0000010 * albion_N$discharge_L
}

albion_N1 <- albion_N |>
  mutate_at(3:8, micro_to_dailymole) |> # convert from micromol to mol and calculate the daily load in mol by multiplying by liters per day
  pivot_longer(3:8, names_to = 'Var', values_to = 'dailyload_mols') |>
  select(-3, -4, -5) |>
  drop_na(dailyload_mols)

albion_monthly <- albion_N1 |>
  #mutate(yearmon = )
  group_by(as.yearmon(date), Var) |>
  summarise(monthlyload_mols = sum(dailyload_mols))


ggplot(albion_monthly) +
  geom_col(aes(`as.yearmon(date)`, monthlyload_mols, fill = Var)) +
  theme_light()


ggplot(albion_monthly) +
  geom_line(aes(`as.yearmon(date)`, monthlyload_mols, color = Var)) +
  theme_light() +
  scale_color_viridis_d()





# green lake 4 outlet #
greenlake_P <- left_join(Gl4Chem, Gl4Dis) |>
  select(local_site, date, NH4., NO3., TN, TDN, PN, DON, discharge, discharge_rate) |>
  mutate_at(3:8, as.numeric) |>
  mutate(NH4. = NH4. * 18.04 * 0.05544) |> # convert ueq/L to umol/L --- eq to g multiply by 18.04, then g to mol, multiply by 0.05544
  mutate(NO3. = NO3. * 62 * 0.01613) |> # convert ueq/L to umol/L --- eq to g multiply by 62, then g to mol, multiply by 0.01613 
  mutate(discharge_L = discharge * 1000) # convert daily discharge colume in cubic meters to liters

micro_to_dailymole <- function(x) { # function to convert micromols/L to daily load of mols 
  x * 0.0000010 * greenlake_N$discharge_L
}

greenlake_N1 <- greenlake_N |>
  mutate_at(3:8, micro_to_dailymole) |> # convert from micromol to mol and calculate the daily load in mol by multiplying by liters per day
  pivot_longer(3:8, names_to = 'Var', values_to = 'dailyload_mols') |>
  select(-3, -4, -5) |>
  drop_na(dailyload_mols)

greenlake_monthly <- greenlake_N1 |>
  group_by(as.yearmon(date), Var) |>
  summarise(monthlyload_mols = sum(dailyload_mols))


ggplot(greenlake_monthly) +
  geom_col(aes(`as.yearmon(date)`, monthlyload_mols, fill = Var)) +
  theme_light()


ggplot(greenlake_monthly) +
  geom_line(aes(`as.yearmon(date)`, monthlyload_mols, color = Var)) +
  theme_light() +
  scale_color_viridis_d()