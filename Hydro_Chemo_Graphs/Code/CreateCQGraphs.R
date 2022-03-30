
### cQ graphs ###

## Call data
source("Hydro_Chemo_Graphs/Code/CallData.R")


### Create the graphs ###
source("Hydro_Chemo_Graphs/Code/Functions/cQ_graph.R")


#NH4+
cQ_graph(AlbChem, AlbChem$NH4., "ln(Ammonium)", AlbDis)
cQ_graph(Gl4Chem, Gl4Chem$NH4., "ln(Ammonium)", Gl4Dis)
cQ_graph(MarChem, MarChem$NH4., "ln(Ammonium)", MarDis)
cQ_graph(SadChem, SadChem$NH4., "ln(Ammonium)", SadDis)

#NO3-
cQ_graph(AlbChem, AlbChem$NO3., "ln(Nitrate)", AlbDis)
cQ_graph(Gl4Chem, Gl4Chem$NO3., "ln(Nitrate)", Gl4Dis)
cQ_graph(MarChem, MarChem$NO3., "ln(Nitrate)", MarDis)
cQ_graph(SadChem, SadChem$NO3., "ln(Nitrate)", SadDis)

#PO4--
cQ_graph(AlbChem, AlbChem$PO4.., "ln(Phosphate)", AlbDis)
cQ_graph(Gl4Chem, Gl4Chem$PO4.., "ln(Phosphate)", Gl4Dis)
cQ_graph(MarChem, MarChem$PO4.., "ln(Phosphate)", MarDis)
cQ_graph(SadChem, SadChem$PO4.., "ln(Phosphate)", SadDis)

#TN
cQ_graph(AlbChem, AlbChem$TN, "ln(Total N)", AlbDis)
cQ_graph(Gl4Chem, Gl4Chem$TN, "ln(Total N)", Gl4Dis)
cQ_graph(MarChem, MarChem$TN, "ln(Total N)", MarDis)
cQ_graph(SadChem, SadChem$TN, "ln(Total N)", SadDis)

#TDN
cQ_graph(AlbChem, AlbChem$TDN, "ln(Total Dissolved N)", AlbDis)
cQ_graph(Gl4Chem, Gl4Chem$TDN, "ln(Total Dissolved N)", Gl4Dis)
cQ_graph(MarChem, MarChem$TDN, "ln(Total Dissolved N)", MarDis)
cQ_graph(SadChem, SadChem$TDN, "ln(Total Dissolved N)", SadDis)

#PN
cQ_graph(AlbChem, AlbChem$PN, "ln(Particulate N)", AlbDis)
cQ_graph(Gl4Chem, Gl4Chem$PN, "ln(Particulate N)", Gl4Dis)
cQ_graph(MarChem, MarChem$PN, "ln(Particulate N)", MarDis)
cQ_graph(SadChem, SadChem$PN, "ln(Particulate N)", SadDis)

#DON
cQ_graph(AlbChem, AlbChem$DON, "ln(Dissolved Organic N)", AlbDis)
cQ_graph(Gl4Chem, Gl4Chem$DON, "ln(Dissolved Organic N)", Gl4Dis)
cQ_graph(MarChem, MarChem$DON, "ln(Dissolved Organic N)", MarDis)
cQ_graph(SadChem, SadChem$DON, "ln(Dissolved Organic N)", SadDis)

#IN 
cQ_graph(AlbChem, AlbChem$IN, "ln(Inorganic N)", AlbDis)
cQ_graph(Gl4Chem, Gl4Chem$IN, "ln(Inorganic N)", Gl4Dis)
cQ_graph(MarChem, MarChem$IN, "ln(Inorganic N)", MarDis)
cQ_graph(SadChem, SadChem$IN, "ln(Inorganic N)", SadDis)

#TP 
cQ_graph(AlbChem, AlbChem$TP, "Total P"~(mu~mol~L^-1), AlbDis)
cQ_graph(Gl4Chem, Gl4Chem$TP, "Total P"~(mu~mol~L^-1), Gl4Dis)
cQ_graph(MarChem, MarChem$TP, "Total P"~(mu~mol~L^-1), MarDis)
cQ_graph(SadChem, SadChem$TP, "Total P"~(mu~mol~L^-1), SadDis)

#TDP
cQ_graph(AlbChem, AlbChem$TDP, "ln(Total Dissolved P)", AlbDis)
cQ_graph(Gl4Chem, Gl4Chem$TDP, "ln(Total Dissolved P)", Gl4Dis)
cQ_graph(MarChem, MarChem$TDP, "ln(Total Dissolved P)", MarDis)
cQ_graph(SadChem, SadChem$TDP, "ln(Total Dissolved P)", SadDis)

#PP
cQ_graph(AlbChem, AlbChem$PP, "ln(Particulate P)", AlbDis)
cQ_graph(Gl4Chem, Gl4Chem$PP, "ln(Particulate P)", Gl4Dis)
cQ_graph(MarChem, MarChem$PP, "ln(Particulate P)", MarDis)
cQ_graph(SadChem, SadChem$PP, "ln(Particulate P)", SadDis)

#DOP
cQ_graph(AlbChem, AlbChem$DOP, "ln(Dissolved Organic P)", AlbDis)
cQ_graph(Gl4Chem, Gl4Chem$DOP, "ln(Dissolved Organic P)", Gl4Dis)
cQ_graph(MarChem, MarChem$DOP, "ln(Dissolved Organic P)", MarDis)
cQ_graph(SadChem, SadChem$DOP, "ln(Dissolved Organic P)", SadDis)

#IP
cQ_graph(AlbChem, AlbChem$IP, "ln(Inorganic P)", AlbDis)
cQ_graph(Gl4Chem, Gl4Chem$IP, "ln(Inorganic P)", Gl4Dis)
cQ_graph(MarChem, MarChem$IP, "ln(Inorganic P)", MarDis)
cQ_graph(SadChem, SadChem$IP, "ln(Inorganic P)", SadDis)

#TOC 
cQ_graph(AlbChem, AlbChem$TOC, "ln(Total Organic P)", AlbDis) # - no data
cQ_graph(Gl4Chem, Gl4Chem$TOC, "ln(Total Organic P)", Gl4Dis) # limited data
cQ_graph(MarChem, MarChem$TOC, "ln(Total Organic P)", MarDis) # limited data
cQ_graph(SadChem, SadChem$TOC, "ln(Total Organic P)", SadDis) # limited data

#DOC
cQ_graph(AlbChem, AlbChem$DOC, "ln(Dissolved Organic P)", AlbDis)
cQ_graph(Gl4Chem, Gl4Chem$DOC, "ln(Dissolved Organic P)", Gl4Dis)
cQ_graph(MarChem, MarChem$DOC, "ln(Dissolved Organic P)", MarDis)
cQ_graph(SadChem, SadChem$DOC, "ln(Dissolved Organic P)", SadDis)

#POC
cQ_graph(AlbChem, AlbChem$POC, "ln(Particulate Organic P)", AlbDis) # - no data
cQ_graph(Gl4Chem, Gl4Chem$POC, "ln(Particulate Organic P)", Gl4Dis) # limited data
cQ_graph(MarChem, MarChem$POC, "ln(Particulate Organic P)", MarDis) # limited data
cQ_graph(SadChem, SadChem$POC, "ln(Particulate Organic P)", SadDis) # limited data


### Notes
# Question for the group - what parameters do we care most about? 

# Linnea thinks Albion and Green Lake 4 have the best data, most consistently collected and longest records.



