
### Script to make hydrochemographs ###

## Call data
source("Hydro_Chemo_Graphs/Code/CallData.R")


### Create the graphs ###
source("Hydro_Chemo_Graphs/Code/Functions/chemo_hydrograph.R")


Gl4stoich <- Gl4Chem |>
 # mutate(tntp = as.numeric(TN)/as.numeric(TP)) |>
  mutate(tdn.tdp = as.numeric(TDN)/as.numeric(TDP),
         don.dop = as.numeric(DON)/as.numeric(DOP))

chemo_hydrograph(Gl4stoich, Gl4stoich$don.dop, "DON:DOP (molar ratio)", Gl4Dis)

chemo_hydrograph(Gl4stoich, Gl4stoich$tdn.tdp, "TDN:TDP (molar ratio)", Gl4Dis)

chemo_hydrograph(Gl4Chem, Gl4Chem$NH4., "Ammonium"~(mu~eq~L^-1), Gl4Dis)

chemo_hydrograph(Gl4Chem, Gl4Chem$NO3., "Nitrate"~(mu~eq~L^-1), Gl4Dis)

chemo_hydrograph(Gl4Chem, Gl4Chem$TDN, "Total Dissolved N"~(mu~mol~L^-1), Gl4Dis)

chemo_hydrograph(Gl4Chem, Gl4Chem$DON, "Dissolved Organic N"~(mu~mol~L^-1), Gl4Dis)

chemo_hydrograph(Gl4Chem, Gl4Chem$IN, "Inorganic N"~(mu~eq~L^-1), Gl4Dis)

chemo_hydrograph(Gl4Chem, Gl4Chem$TDP, "Total Dissolved P"~(mu~mol~L^-1), Gl4Dis)

chemo_hydrograph(Gl4Chem, Gl4Chem$DOP, "Dissolved Organic P"~(mu~mol~L^-1), Gl4Dis)