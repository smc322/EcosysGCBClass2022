
### Create hydrographs, chemographs ###

## Call data
source("Hydro_Chemo_Graphs/Code/CallData.R")


### Create the graphs ###
source("Hydro_Chemo_Graphs/Code/Functions/chemo_hydrograph.R")

#NH4+
chemo_hydrograph(AlbChem, AlbChem$NH4., "Ammonium"~(mu~eq~L^-1), AlbDis)
chemo_hydrograph(Gl4Chem, Gl4Chem$NH4., "Ammonium"~(mu~eq~L^-1), Gl4Dis)
# chemo_hydrograph(MarChem, MarChem$NH4., "Ammonium"~(mu~eq~L^-1), MarDis)
# chemo_hydrograph(SadChem, SadChem$NH4., "Ammonium"~(mu~eq~L^-1), SadDis)

#NO3-
chemo_hydrograph(AlbChem, AlbChem$NO3., "Nitrate"~(mu~eq~L^-1), AlbDis)
chemo_hydrograph(Gl4Chem, Gl4Chem$NO3., "Nitrate"~(mu~eq~L^-1), Gl4Dis)
# chemo_hydrograph(MarChem, MarChem$NO3., "Nitrate"~(mu~eq~L^-1), MarDis)
# chemo_hydrograph(SadChem, SadChem$NO3., "Nitrate"~(mu~eq~L^-1), SadDis)

#PO4--
chemo_hydrograph(AlbChem, AlbChem$PO4.., "Phosphate"~(mu~eq~L^-1), AlbDis)
chemo_hydrograph(Gl4Chem, Gl4Chem$PO4.., "Phosphate"~(mu~eq~L^-1), Gl4Dis)
# chemo_hydrograph(MarChem, MarChem$PO4.., "Phosphate"~(mu~eq~L^-1), MarDis)
# chemo_hydrograph(SadChem, SadChem$PO4.., "Phosphate"~(mu~eq~L^-1), SadDis)

#TN
chemo_hydrograph(AlbChem, AlbChem$TN, "Total N"~(mu~mol~L^-1), AlbDis)
chemo_hydrograph(Gl4Chem, Gl4Chem$TN, "Total N"~(mu~mol~L^-1), Gl4Dis)
# chemo_hydrograph(MarChem, MarChem$TN, "Total N"~(mu~mol~L^-1), MarDis)
# chemo_hydrograph(SadChem, SadChem$TN, "Total N"~(mu~mol~L^-1), SadDis)

#TDN
chemo_hydrograph(AlbChem, AlbChem$TDN, "Total Dissolved N"~(mu~mol~L^-1), AlbDis)
chemo_hydrograph(Gl4Chem, Gl4Chem$TDN, "Total Dissolved N"~(mu~mol~L^-1), Gl4Dis)
# chemo_hydrograph(MarChem, MarChem$TDN, "Total Dissolved N"~(mu~mol~L^-1), MarDis)
# chemo_hydrograph(SadChem, SadChem$TDN, "Total Dissolved N"~(mu~mol~L^-1), SadDis)

#PN
chemo_hydrograph(AlbChem, AlbChem$PN, "Particulate N"~(mu~mol~L^-1), AlbDis)
chemo_hydrograph(Gl4Chem, Gl4Chem$PN, "Particulate N"~(mu~mol~L^-1), Gl4Dis)
# chemo_hydrograph(MarChem, MarChem$PN, "Particulate N"~(mu~mol~L^-1), MarDis)
# chemo_hydrograph(SadChem, SadChem$PN, "Particulate N"~(mu~mol~L^-1), SadDis)

#DON
chemo_hydrograph(AlbChem, AlbChem$DON, "Dissolved Organic N"~(mu~mol~L^-1), AlbDis)
chemo_hydrograph(Gl4Chem, Gl4Chem$DON, "Dissolved Organic N"~(mu~mol~L^-1), Gl4Dis)
# chemo_hydrograph(MarChem, MarChem$DON, "Dissolved Organic N"~(mu~mol~L^-1), MarDis)
# chemo_hydrograph(SadChem, SadChem$DON, "Dissolved Organic N"~(mu~mol~L^-1), SadDis)

#IN 
chemo_hydrograph(AlbChem, AlbChem$IN, "Inorganic N"~(mu~eq~L^-1), AlbDis)
chemo_hydrograph(Gl4Chem, Gl4Chem$IN, "Inorganic N"~(mu~eq~L^-1), Gl4Dis)
# chemo_hydrograph(MarChem, MarChem$IN, "Inorganic N"~(mu~eq~L^-1), MarDis)
# chemo_hydrograph(SadChem, SadChem$IN, "Inorganic N"~(mu~eq~L^-1), SadDis)

#TP 
chemo_hydrograph(AlbChem, AlbChem$TP, "Total P"~(mu~mol~L^-1), AlbDis)
chemo_hydrograph(Gl4Chem, Gl4Chem$TP, "Total P"~(mu~mol~L^-1), Gl4Dis)
# chemo_hydrograph(MarChem, MarChem$TP, "Total P"~(mu~mol~L^-1), MarDis)
# chemo_hydrograph(SadChem, SadChem$TP, "Total P"~(mu~mol~L^-1), SadDis)

#TDP
chemo_hydrograph(AlbChem, AlbChem$TDP, "Total Dissolved P"~(mu~mol~L^-1), AlbDis)
chemo_hydrograph(Gl4Chem, Gl4Chem$TDP, "Total Dissolved P"~(mu~mol~L^-1), Gl4Dis)
# chemo_hydrograph(MarChem, MarChem$TDP, "Total Dissolved P"~(mu~mol~L^-1), MarDis)
# chemo_hydrograph(SadChem, SadChem$TDP, "Total Dissolved P"~(mu~mol~L^-1), SadDis)

#PP
chemo_hydrograph(AlbChem, AlbChem$PP, "Particulate P"~(mu~mol~L^-1), AlbDis)
chemo_hydrograph(Gl4Chem, Gl4Chem$PP, "Particulate P"~(mu~mol~L^-1), Gl4Dis)
# chemo_hydrograph(MarChem, MarChem$PP, "Particulate P"~(mu~mol~L^-1), MarDis)
# chemo_hydrograph(SadChem, SadChem$PP, "Particulate P"~(mu~mol~L^-1), SadDis)

#DOP
chemo_hydrograph(AlbChem, AlbChem$DOP, "Dissolved Organic P"~(mu~mol~L^-1), AlbDis)
chemo_hydrograph(Gl4Chem, Gl4Chem$DOP, "Dissolved Organic P"~(mu~mol~L^-1), Gl4Dis)
# chemo_hydrograph(MarChem, MarChem$DOP, "Dissolved Organic P"~(mu~mol~L^-1), MarDis)
# chemo_hydrograph(SadChem, SadChem$DOP, "Dissolved Organic P"~(mu~mol~L^-1), SadDis)

#IP
chemo_hydrograph(AlbChem, AlbChem$IP, "Inorganic P"~(mu~mol~L^-1), AlbDis)
chemo_hydrograph(Gl4Chem, Gl4Chem$IP, "Inorganic P"~(mu~mol~L^-1), Gl4Dis)
# chemo_hydrograph(MarChem, MarChem$IP, "Inorganic P"~(mu~mol~L^-1), MarDis)
# chemo_hydrograph(SadChem, SadChem$IP, "Inorganic P"~(mu~mol~L^-1), SadDis)

#TOC 
chemo_hydrograph(AlbChem, AlbChem$TOC, "Total Organic C"~(mg~L^-1), AlbDis) # - no data
chemo_hydrograph(Gl4Chem, Gl4Chem$TOC, "Total Organic C"~(mg~L^-1), Gl4Dis) # limited data
# chemo_hydrograph(MarChem, MarChem$TOC, "Total Organic P"~(mg~L^-1), MarDis) # limited data
# chemo_hydrograph(SadChem, SadChem$TOC, "Total Organic P"~(mg~L^-1), SadDis) # limited data

#DOC
chemo_hydrograph(AlbChem, AlbChem$DOC, "Dissolved Organic C"~(mg~L^-1), AlbDis)
chemo_hydrograph(Gl4Chem, Gl4Chem$DOC, "Dissolved Organic C"~(mg~L^-1), Gl4Dis)
# chemo_hydrograph(MarChem, MarChem$DOC, "Dissolved Organic P"~(mg~L^-1), MarDis)
# chemo_hydrograph(SadChem, SadChem$DOC, "Dissolved Organic P"~(mg~L^-1), SadDis)

#POC
chemo_hydrograph(AlbChem, AlbChem$POC, "Particulate Organic C"~(mg~L^-1), AlbDis) # - no data
chemo_hydrograph(Gl4Chem, Gl4Chem$POC, "Particulate Organic C"~(mg~L^-1), Gl4Dis) # limited data
# chemo_hydrograph(MarChem, MarChem$POC, "Particulate Organic P"~(mg~L^-1), MarDis) # limited data
# chemo_hydrograph(SadChem, SadChem$POC, "Particulate Organic P"~(mg~L^-1), SadDis) # limited data


### Notes
# Question for the group - what parameters do we care most about? -- N!!!

# Linnea thinks Albion and Green Lake 4 have the best data, most consistently collected and longest records.





