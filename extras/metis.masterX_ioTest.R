
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


# IO test 1 - Simple
ioTable0=tibble::tribble( # Initial Flows
  ~supplySubSector,  ~water,  ~ag,   ~elec,  ~domestic, ~export, ~mining, ~other, ~units,
   "water",            0,    30,      5,        8,         5,       5,    5, "km3",
   "ag",               0,    0,      1,        0,          10,      0,    0, "ton",
   "elec",             3,    0.3,    0,        0.5,        1,       2,    5, "TWh",
   "livestock",        1,    0.1,    0,        0.3,        2,       3,    2, "head");ioTable0


io<-metis.io(ioTable0 = ioTable0, folderName = "test",plotSankeys=T)
# View Outputs
io$A
io$L
io$ioTbl


# IO test 2 with subSectors
ioTable0=tibble::tribble( # Initial Flows
  ~supplySubSector,  ~supplySector, ~w_GW,  ~ag_Wheat,   ~elec_Coal,  ~domestic, ~export, ~mining, ~other, ~units,
  "w_GW",            "water",     0,    10,      1,        3,         1,       2,    1, "km3",
  "w_SW",            "water",     0,    30,      5,        8,         5,       5,    5, "km3",
  "ag_Biofuel",      "ag",      0,    0,       2,        0,         8,       0,    0, "ton",
  "ag_Wheat",        "ag",      0,    0,       0,        6,         9,       0,    0, "ton",
  "ag_Rice",         "ag",     0,    0,       0,        5,         20,      0,    0, "ton",
  "elec_Coal",       "elec",     3,    0.3,     0,        0.6,       1,       3,    6, "TWh",
  "elec_Wind",       "elec",     2,    0.2,     0,        0.4,       0.677,       2,    4, "TWh",
  "elec_Oil",        "elec",     1,    0.1,     0,        0.2,       0.333,       1,    2, "TWh",
  "livestock_Cow",   "livestock",     0,    0,     0,        0,    30,       60,    0, "head",
  "livestock_Chicken", "livestock",   0,    0,     0,        0,    50,       90,    0, "head");ioTable0


io_sub<-metis.io(ioTable0 = ioTable0, folderName = "test_subSectors",plotSankeys=T)
# View Outputs
io_sub$A
io_sub$L
io_sub$ioTbl


# Example Using Fixed A table
# Increase irrigation efficiency i.e. decrease water used in ag by half
A_test = io$A; A_test
A_test = A_test %>% dplyr::mutate(ag = case_when(supplySubSector=="water"~ag*0.5, TRUE~ag)); A_test
io1<-metis.io(ioTable0 = ioTable0, A0 = A_test, useIntensity = 1, folderName = "test_fixedA", plotSankeys=F)
io1$A
io1$L

# Compare Scenarios
io$ioTbl
io1$ioTbl
ioTable1 = io$ioTbl %>%
  dplyr::select(names(ioTable0)) %>%
  dplyr::mutate(subRegion="Orig") %>%
  dplyr::filter(!grepl("_all",supplySubSector)) %>%
  dplyr::bind_rows(io1$ioTbl %>% dplyr::select(names(ioTable0)) %>% dplyr::mutate(subRegion="New_A")
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



