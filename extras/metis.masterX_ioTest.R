
#----------------------------
# Install necessary packages
#----------------------------
if("devtools" %in% rownames(installed.packages()) == F){install.packages("devtools")}
library(devtools)
if("metis" %in% rownames(installed.packages()) == F){install_github(repo="JGCRI/metis")}
library(metis)
if("rgcam" %in% rownames(installed.packages()) == F){install_github(repo="JGCRI/rgcam")}
library(rgcam)
if("tibble" %in% rownames(installed.packages()) == F){install.packages("tibble")}
library(tibble)
if("dplyr" %in% rownames(installed.packages()) == F){install.packages("dlpyr")}
library(dplyr)
if("rgdal" %in% rownames(installed.packages()) == F){install.packages("rgdal")}
library(rgdal)
if("tmap" %in% rownames(installed.packages()) == F){install.packages("tmap")}
library(tmap)
if("rgeos" %in% rownames(installed.packages()) == F){install.packages("rgeos")}
library(rgeos)
library(ggplot2)
if("ggalluvial" %in% rownames(installed.packages()) == F){install.packages("ggalluvial")}
library(ggalluvial)


#----------------------------
# Read GCAM Data
#---------------------------

# # Example 1
# ioTable0=tibble::tribble( # Initial Flows
#  ~supplySubSector,    ~ag,         ~man, ~total,
#   "ag",            500,         350,  1000,
#   "man",           320,         360,  800);ioTable0
#
# ioTable0=tibble::tribble( # Initial Flows
#   ~supplySubSector,     ~supplySector, ~w_groundWater, ~w_surface,   ~Ag_corn,  ~Ag_rice, ~E_biofuel, ~E_solar, ~other,
#   "w_groundWater"     ,  "water",            0,                0,            5,        10,         5,       1,        20,
#   "w_surface"         ,  "water",            0,                0,            1,        2,          3,       1,        5,
#   "Ag_biofuel"        ,  "ag",               0,                0,            0,        0,          1,       0,        0,
#   "Ag_rice"           ,  "ag",               0,                0,            0,        0,          0,       0,        10,
#   "E_biofuel"         ,  "elec",             6,                0.6,          1,        1,          0,       0,        10,
#   "E_solar"           ,  "elec",             3,                0.3,          0.5,      0.5,        0,       0,        5);ioTable0

ioTable0=tibble::tribble( # Initial Flows
  ~supplySubSector,  ~water,  ~ag,   ~elec,  ~domestic, ~export, ~mining, ~other,
   "water",            0,    30,      5,        8,         5,       5,    5,
   "ag",               0,    0,      1,        0,          10,      0,    0,
   "elec",             3,    0.3,    0,        0.5,        1,       2,    5);ioTable0


io<-metis.io(ioTable0 = ioTable0, folderName = "test",plotSankeys=F)
io$A
io$L

# Example Using Fixed A table
# Increase irrigation efficiency i.e. decrease water used in ag by half
A_test = io$A; A_test
A_test = A_test %>% dplyr::mutate(ag = case_when(supplySubSector=="water"~ag*0.5, TRUE~ag)); A_test
io1<-metis.io(ioTable0 = ioTable0, A0 = A_test, useIntensity = 1, folderName = "test_fixedA", plotSankeys=F)
io1$A
io1$L

# Compare Scenarios
io$ioTbl_Output
io1$ioTbl_Output
ioTable1 = io$ioTbl_Output %>%
  dplyr::select(names(ioTable0)) %>%
  dplyr::mutate(subRegion="Orig") %>%
  dplyr::filter(!grepl("_all",supplySubSector)) %>%
  dplyr::bind_rows(io1$ioTbl_Output %>% dplyr::select(names(ioTable0)) %>% dplyr::mutate(subRegion="New_A")
                   %>% dplyr::filter(!grepl("_all",supplySubSector))); ioTable1

io2<-metis.io(ioTable0 = ioTable1, folderName = "test_comparefixedA", plotSankeys=T)
io2$A
io2$L


# Example Multi Sub_regions
ioTable0=tibble::tribble( # Initial Flows
  ~supplySubSector,    ~ag,         ~man, ~total, ~ subRegion,
  "ag",            500,         350,  1000, "subRegA",
  "man",           320,         360,  800,  "subRegA",
  "ag",            500,         350,  1000, "subRegB",
  "man",           320,         360,  800,  "subRegB");ioTable0


io1<-metis.io(ioTable0 = ioTable0, folderName = "MultiReg_test")
io1$A
io1$L



# Commodities
# 1. Water demands
# 2. Agricultural production by Crop
# 3. Electricity Production by Type

# Downscaled Outputs:
# 1. Ag production by crop
# 2. Elec production by fuel
# 3. Water demands by ag, elec, other

# Initial Coefficient Assumptions
# A0

# Steps
# 1. Use A0 and D0 (Demand other) to find Intermediate flows and total production
# 2. Compare Aggregated demands to initial assumption
# 3. Adjust A to reflect actual data
# 4.


A0=tibble::tribble( # Initial Flows
  ~sector     ,    ~W,   ~Ag_corn,  ~Ag_rice, ~E_coal, ~E_solar, ~E,
  "W"         ,    0,       0.1,     2,     0.5,    0.01,   0,
  "Ag_corn"     ,    0,        0,      0,     0,      0,      0,
  "Ag_rice"     ,    0,        0,      0,     0,      0,      0,
  "E_coal"     ,    0,        0,      0,     0,      0,      0,
  "E_solar"    ,    0,        0,      0,     0,      0,      0,
  "E"         ,    1,      0.3,    0.4,   0,      0,      0);A0

Dreal = tibble::tribble( # From tethys
  ~sector, ~W,
  "Ag"   ,  1000,
  "E"    ,    20);Dreal



D0=tibble::tribble( # Other demands (Not internal flows)
  ~other,
  1000, # W
  10,  # Acorn
  10,  # Arice
  50, # Ecoal
  50,   # Esolar
  100 # E
);D0


io<-metis.io(D0=D0,A0=A0, D=c(10,0,0,0,1,1))
io$A
io$L

install.packages("corrplot")
library(corrplot)
M<-as.matrix(A0%>%dplyr::select(-sector))
rownames(M)<-colnames(M);M
col<- colorRampPalette(metis.colors()$pal_div_wet)(20)
corrplot(M, method="circle", is.corr=F, type="upper",addCoef.col="red", col=col, add=F, cl.length=20, cl.lim=c(0,2))

