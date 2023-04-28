
library(zoo)
library(tidyverse)
library(remotes)
library(smwrBase)

### Chemograph function
chemo_hydrograph <- function(chemData, chemVar, name_units, disData) {
  
  chemData.1 <- chemData |>
    mutate(chemVar = as.numeric(chemVar)) |>
    filter(!is.na(chemVar))|>
    filter(chemVar != Inf)
  
  
  mindate <- min(chemData.1$date)
  maxdate <- max(chemData.1$date)
  
  coef <- mean(as.numeric(chemData.1$chemVar), na.rm = TRUE) / mean(disData$discharge_rate, na.rm = TRUE)
  
  
  ggplot() +
    geom_line(disData, mapping = aes(date, discharge_rate * coef), color = "#9A9391") +
    geom_line(chemData.1, mapping = aes(date, chemVar), color = "#d14411") +
    theme_light() +
    scale_x_date(limits = c(mindate, maxdate)) +
    labs(x = "") +
    scale_y_continuous(limits = c(0, 10000),
      # first axis
      name = name_units,
      
      # second axis 
      sec.axis = sec_axis(~./coef, name = "Streamflow" ~ (m ^ 3 ~ s ^ -1))
    )  +
    theme(axis.title.y.right = element_text(color = "#9A9391"),
          axis.title.y = element_text(color = "#d14411"),
          axis.line.y.left = element_line(color = "#d14411")) 
  
}


### Discharge function
cQ_graph <- function(chemData, chemVar, name_units, disData) {
    
    disData.1 <- disData |>
      select(local_site, date, discharge_rate)|>
      
    
    chemData.1 <- chemData |>
      mutate(chemVar = as.numeric(chemVar)) |>
      filter(!is.na(chemVar)) |>
      select(local_site, date, chemVar) 
      
    
    
    chem_dis <- inner_join(disData.1, chemData.1)
    
    
    ggplot(chem_dis) +
      geom_point(aes(log(discharge_rate), log(chemVar))) +
      geom_smooth(method = "loess", aes(log(discharge_rate), log(chemVar)), se = FALSE) +
      theme_light() +
      labs(x = "ln(Q)",
           y = name_units)
    
  }


### Data

#Green Lake 4
Gl4Dis <- read.csv("Data/Discharge/gl4disch.nc.data.csv") |>
  mutate(date = as.Date(date)) |>
  mutate(discharge_rate = discharge / 86400)  #convert cubic meters daily flow volume to cubic meters per second discharge
Gl4Dis$wateryear <- waterYear(Gl4Dis$date)

Gl4Chem <- read.csv("Data/WaterChem/gre4solu.nc.data.csv") |>
  mutate(date = as.Date(date)) |>
  mutate(local_site = "gl4")
Gl4Chem$wateryear <- waterYear(Gl4Chem$date)

# Albion
AlbDis <- read.csv("Data/Discharge/albdisch.nc.data.csv") |>
  mutate(date = as.Date(date)) |>
  mutate(discharge_rate = discharge / 86400) #convert cubic meters daily flow volume to cubic meters per second discharge
AlbDis$wateryear <- waterYear(AlbDis$date)

AlbChem <- read.csv("Data/WaterChem/albisolu.nc.data.csv") |>
  mutate(date = as.Date(date)) |>
  mutate(local_site = "alb")
AlbChem$wateryear <- waterYear(AlbChem$date)

lochvale <- read.csv("Data/USGS_lochvaleoutlet.csv") |>
  select(-X, -X.1) |>
  mutate(date = as.Date(Date))
lochvale$wateryear <- waterYear(lochvale$date)

# Merge for stoichiometry
Gl4stoich <- Gl4Chem |>
  # mutate(tntp = as.numeric(TN)/as.numeric(TP)) |>
  mutate(tdn.tdp = as.numeric(TDN)/as.numeric(TDP),
         don.dop = as.numeric(DON)/as.numeric(DOP))

Albstoich <- AlbChem |>
  # mutate(tntp = as.numeric(TN)/as.numeric(TP)) |>
  mutate(tdn.tdp = as.numeric(TDN)/as.numeric(TDP),
         don.dop = as.numeric(DON)/as.numeric(DOP))



### hydrochemographs
# Green Lake 4
Gl4stoich2002_1 <- filter(Gl4stoich, wateryear == 2002)
Gl4_2002 <- chemo_hydrograph(Gl4stoich2002, Gl4stoich2002$don.dop, "DON:DOP (molar ratio)", Gl4Dis)
Gl4_2002
Gl4stoich2011_1 <- filter(Gl4stoich, wateryear == 2011)
Gl4_2011 <- chemo_hydrograph(Gl4stoich2011, Gl4stoich2011$don.dop, "DON:DOP (molar ratio)", Gl4Dis)
Gl4_2011
Gl4stoich2017_1 <- filter(Gl4stoich, wateryear == 2017)
Gl4_2017 <- chemo_hydrograph(Gl4stoich2017, Gl4stoich2017$don.dop, "DON:DOP (molar ratio)", Gl4Dis)
Gl4_2017
Gl4p1 <- chemo_hydrograph(Gl4stoich, Gl4stoich$don.dop, "DON:DOP (molar ratio)", Gl4Dis)
Gl4p1
Gl4stoich2002_1 <- filter(Gl4stoich, wateryear == 2002)
Gl4_2002 <- chemo_hydrograph(Gl4stoich2002, Gl4stoich2002$tdn.tdp, "TDN:TDP (molar ratio)", Gl4Dis)
Gl4_2002
Gl4stoich2011_1 <- filter(Gl4stoich, wateryear == 2011)
Gl4_2011 <- chemo_hydrograph(Gl4stoich2011, Gl4stoich2011$tdn.tdp, "TDN:TDP (molar ratio)", Gl4Dis)
Gl4_2011
Gl4stoich2017_1 <- filter(Gl4stoich, wateryear == 2017)
Gl4_2017 <- chemo_hydrograph(Gl4stoich2017, Gl4stoich2017$tdn.tdp, "TDN:TDP (molar ratio)", Gl4Dis)
Gl4_2017

Gl4p2 <- chemo_hydrograph(Gl4stoich, Gl4stoich$tdn.tdp, "TDN:TDP (molar ratio)", Gl4Dis)
Gl4p3 <- chemo_hydrograph(Gl4Chem, Gl4Chem$NH4., "Ammonium"~(mu~eq~L^-1), Gl4Dis)
Gl4p4 <- chemo_hydrograph(Gl4Chem, Gl4Chem$NO3., "Nitrate"~(mu~eq~L^-1), Gl4Dis)
Gl4p5 <- chemo_hydrograph(Gl4Chem, Gl4Chem$TDN, "Total Dissolved N"~(mu~mol~L^-1), Gl4Dis)
Gl4p6 <- chemo_hydrograph(Gl4Chem, Gl4Chem$DON, "Dissolved Organic N"~(mu~mol~L^-1), Gl4Dis)
Gl4p7 <- chemo_hydrograph(Gl4Chem, Gl4Chem$IN, "Inorganic N"~(mu~eq~L^-1), Gl4Dis)
Gl4p8 <- chemo_hydrograph(Gl4Chem, Gl4Chem$TDP, "Total Dissolved P"~(mu~mol~L^-1), Gl4Dis)
Gl4p9 <- chemo_hydrograph(Gl4Chem, Gl4Chem$DOP, "Dissolved Organic P"~(mu~mol~L^-1), Gl4Dis)

# Albion
Albp1 <- chemo_hydrograph(Albstoich, Albstoich$don.dop, "DON:DOP (molar ratio)", AlbDis)
Albp2 <- chemo_hydrograph(Albstoich, Albstoich$tdn.tdp, "TDN:TDP (molar ratio)", AlbDis)
Albp3 <- chemo_hydrograph(AlbChem, AlbChem$NH4., "Ammonium"~(mu~eq~L^-1), AlbDis)
Albp4 <- chemo_hydrograph(AlbChem, AlbChem$NO3., "Nitrate"~(mu~eq~L^-1), AlbDis)
Albp5 <- chemo_hydrograph(AlbChem, AlbChem$TDN, "Total Dissolved N"~(mu~mol~L^-1), AlbDis)
Albp6 <- chemo_hydrograph(AlbChem, AlbChem$DON, "Dissolved Organic N"~(mu~mol~L^-1), AlbDis)
Albp7 <- chemo_hydrograph(AlbChem, AlbChem$IN, "Inorganic N"~(mu~eq~L^-1), AlbDis)
Albp8 <- chemo_hydrograph(AlbChem, AlbChem$TDP, "Total Dissolved P"~(mu~mol~L^-1), AlbDis)
Albp9 <- chemo_hydrograph(AlbChem, AlbChem$DOP, "Dissolved Organic P"~(mu~mol~L^-1), AlbDis)

# Lochvale
Lochp1 <- chemo_hydrograph(lochvale, lochvale$no3.po4, "NO3:PO4 (molar ratio)", lochvale)
Lochp2 <- chemo_hydrograph(lochvale, lochvale$Nitrate_mgl, "Nitrate"~(mg~L~L^-1), lochvale)
Lochp3 <- chemo_hydrograph(lochvale, lochvale$Phosphate_mgl, "Phosphate"~(mg~L~L^-1), lochvale)
