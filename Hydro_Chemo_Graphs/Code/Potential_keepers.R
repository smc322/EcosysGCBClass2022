
### Script to make hydrochemographs ###

## Call data
source("Hydro_Chemo_Graphs/Code/CallData.R")


### Create the graphs ###
source("Hydro_Chemo_Graphs/Code/Functions/chemo_hydrograph.R")


Gl4stoich <- Gl4Chem |>
 # mutate(tntp = as.numeric(TN)/as.numeric(TP)) |>
  mutate(tdn.tdp = as.numeric(TDN)/as.numeric(TDP),
         don.dop = as.numeric(DON)/as.numeric(DOP))

chemo_hydrograph(Gl4stoich, Gl4stoich$don.dop, "DON:DOP", Gl4Dis)

chemo_hydrograph(Gl4stoich, Gl4stoich$tdn.tdp, "TDN:TDP", Gl4Dis)
