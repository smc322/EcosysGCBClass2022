 
### Call in data ###

library(tidyverse)


## Read in discharge data

# Albion 
# AlbDis <- read.csv("Hydro_Chemo_Graphs/Data/Discharge/albdisch.nc.data.csv") |>
#   mutate(date = as.Date(date)) |>
#   mutate(discharge_rate = discharge / 86400) #convert cubic meters daily flow volume to cubic meters per second discharge

#Green Lake 4
Gl4Dis <- read.csv("Archive_Exploration/Hydro_Chemo_Graphs/Data/Discharge/gl4disch.nc.data.csv") |>#read.csv("C:/Users/linne/Downloads/PhD_Code/EcosysGCBClass2022/Hydro_Chemo_Graphs/Data/Discharge/gl4disch.nc.data.csv") |>
  mutate(date = as.Date(date)) |>
  mutate(discharge_rate = discharge / 86400) #convert cubic meters daily flow volume to cubic meters per second discharge

# #Martinelli  
# MarDis <- read.csv("Hydro_Chemo_Graphs/Data/Discharge/mardisch.nc.data.csv") |>
#    mutate(date = as.Date(date)) |>
#    mutate(discharge_rate = discharge / 86400) #convert cubic meters daily flow volume to cubic meters per second discharge
# 
# #Saddle 
# SadDis <- read.csv("Hydro_Chemo_Graphs/Data/Discharge/saddisch.nc.data.csv") |>
#    mutate(date = as.Date(date)) |>
#    mutate(discharge_rate = discharge / 86400) #convert cubic meters daily flow volume to cubic meters per second discharge


## Read in chemistry data

# Albion 
# AlbChem <- read.csv("https://raw.githubusercontent.com/smc322/EcosysGCBClass2022/main/Hydro_Chemo_Graphs/Data/WaterChem/albisolu.nc.data.csv?token=GHSAT0AAAAAABNZX5S63Y6TJHLCF7OYRXOSYTX67WA") |>
#   mutate(date = as.Date(date)) |>
#   mutate(local_site = "alb")

#Green Lake 4
Gl4Chem <- read.csv("Archive_Exploration/Hydro_Chemo_Graphs/Data/WaterChem/gre4solu.nc.data.csv") |>#read.csv("C:/Users/linne/Downloads/PhD_Code/EcosysGCBClass2022/Hydro_Chemo_Graphs/Data/WaterChem/gre4solu.nc.data.csv") |>
  mutate(date = as.Date(date)) |>
  mutate(local_site = "gl4")

# #Martinelli  
# MarChem <- read.csv("Hydro_Chemo_Graphs/Data/WaterChem/martsolu.nc.data.csv") |>
#    mutate(date = as.Date(date)) |>
#    mutate(local_site = "mar")
# 
# #Saddle 
# SadChem <- read.csv("Hydro_Chemo_Graphs/Data/WaterChem/saddsolu.nc.data.csv")|>
#    mutate(date = as.Date(date)) |>
#    mutate(local_site = "sdl")
