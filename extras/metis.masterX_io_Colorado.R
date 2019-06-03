
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
if("ggplot2" %in% rownames(installed.packages()) == F){install.packages("ggplot2")}
library(ggplot2)
if("ggalluvial" %in% rownames(installed.packages()) == F){install.packages("ggaaluvial")}
library(ggalluvial)


#-------------
# Workflow for Metis I/O Analysis

# Small Examples

# Intensity
A0=tibble::tribble( # Initial total demand
  ~supplySector, ~W, ~W_import, ~E, ~E_import, ~Ag, ~Ag_import,
  "W",            0,         0 ,0.404     ,0     ,0         ,0,
  "W_import",     0,         0 ,0         ,0     ,0         ,0,
  "E",            0.27778,   0 ,0         ,0     ,0         ,0,
  "E_import",     0,         0 ,0         ,0     ,0         ,0,
  "Ag",           0,         0 ,0.0202    ,0     ,0         ,0,
  "Ag_import",    0,         0 ,0         ,0     ,0         ,0
);A0


# Demands and Exports
ioTable0=tibble::tribble( # Initial total demand
  ~supplySector,    ~W,    ~E,  ~industry, ~transport, ~misc, ~export, ~resid, ~cap,
  "W",               0,    200, 20,        40,         50,     0,      500,    1000,
  "W_import",        0,    20,  30,        20,         70,     300,    10,      0,
  "E",               225,  0,   10,        30,         0,      200,    30,      10,
  "E_import",        50,   0,   20,        40,         50,     670,    20,      350,
  "Ag",              0,    10,  20,        0,          50,     0,      0,      100,
  "Ag_import",       0,    20,  20,        0,          50,     30,     0,       20
);ioTable0


# Original Test
io1 <- metis.io(ioTable0=ioTable0, nameAppend = "_test"); io1$ioTbl_Output %>% as.data.frame() ; io1$A_Output %>% as.data.frame()
io1a <- metis.io(ioTable0=ioTable0, A0=A0, useIntensity = 1,nameAppend = "_A"); io1a$ioTbl_Output %>% as.data.frame() ; io1a$A_Output %>% as.data.frame()

# High Electric water intensity
A1<-A0; A1[1,4] <- 1.2; A1
io1b <- metis.io(ioTable0=ioTable0, A0=A1, useIntensity = 1,nameAppend = "_A_highWatinElec"); io1b$ioTbl_Output %>% as.data.frame() ; io1b$A_Output %>% as.data.frame()

# Low Electric water intensity
A2<-A0; A2[1,4] <- 0.2; A2
io1c <- metis.io(ioTable0=ioTable0, A0=A2, useIntensity = 1,nameAppend = "_A_lowWatinElec"); io1c$ioTbl_Output %>% as.data.frame() ; io1c$A_Output %>% as.data.frame()


#---------------------
# Multi Scenario Examples
#----------------------

A0=tibble::tribble( # Initial Flows
  ~supplySector ,    ~W,         ~E,    ~scenario,  ~subRegion,  ~year,
  "W"     ,    0,           0.23,  "ScenA",   "SubRegA",  2010,
  "E"     ,    0.13,          0,   "ScenA",   "SubRegA",  2010,
  "W"     ,    0,           0.3,  "ScenB",   "SubRegA",  2010,
  "E"     ,    0.2,          0,   "ScenB",   "SubRegA",  2010,
  "W"     ,    0,           0.1,  "ScenA",   "SubRegB",  2010,
  "E"     ,    0.05,          0,   "ScenA",   "SubRegB",  2010,
  "W"     ,    0,           0.15,  "ScenB",   "SubRegB",  2010,
  "E"     ,    0.25,          0,   "ScenB",   "SubRegB",  2010,
  "W"     ,    0,           0.4,  "ScenA",   "SubRegA",  2015,
  "E"     ,    0.1,          0,   "ScenA",   "SubRegA",  2015,
  "W"     ,    0,           0.35,  "ScenB",   "SubRegA",  2015,
  "E"     ,    0.25,          0,   "ScenB",   "SubRegA",  2015,
  "W"     ,    0,           0.17,  "ScenA",   "SubRegB",  2015,
  "E"     ,    0.15,          0,   "ScenA",   "SubRegB",  2015,
  "W"     ,    0,           0.45,  "ScenB",   "SubRegB",  2015,
  "E"     ,    0.03,          0,   "ScenB",   "SubRegB",  2015
);A0




ioTable0=tibble::tribble( # Initial total demand
  ~supplySector ,    ~localProduction,        ~scenario,  ~subRegion,  ~year,
  "W"     ,    1000,           "ScenA",   "SubRegA",  2010,
  "E"     ,    130,          "ScenA",   "SubRegA",  2010,
  "W"     ,    10,           "ScenB",   "SubRegA",  2010,
  "E"     ,    200,          "ScenB",   "SubRegA",  2010,
  "W"     ,    100,           "ScenA",   "SubRegB",  2010,
  "E"     ,    150,           "ScenA",   "SubRegB",  2010,
  "W"     ,    100,           "ScenB",   "SubRegB",  2010,
  "E"     ,    20,          "ScenB",   "SubRegB",  2010,
  "W"     ,    10,           "ScenA",   "SubRegA",  2015,
  "E"     ,    110,          "ScenA",   "SubRegA",  2015,
  "W"     ,    250,           "ScenB",   "SubRegA",  2015,
  "E"     ,    250,          "ScenB",   "SubRegA",  2015,
  "W"     ,    200,           "ScenA",   "SubRegB",  2015,
  "E"     ,    200,          "ScenA",   "SubRegB",  2015,
  "W"     ,    100,           "ScenB",   "SubRegB",  2015,
  "E"     ,    10,          "ScenB",   "SubRegB",  2015
);ioTable0




# Original Test
io1 <- metis.io(ioTable0=ioTable0, nameAppend = "_Multi_test"); io1$ioTbl_Output %>% as.data.frame() ; io1$A_Output %>% as.data.frame()
io1a <- metis.io(ioTable0=ioTable0, A0=A0, useIntensity = 1,nameAppend = "_Multi_A"); io1a$ioTbl_Output %>% as.data.frame() ; io1a$A_Output %>% as.data.frame()

# High Electric water intensity
A1<-A0; A1[1,4] <- 1.2; A1
io1b <- metis.io(ioTable0=ioTable0, A0=A1, useIntensity = 1,nameAppend = "_Multi_A_highWatinElec"); io1b$ioTbl_Output %>% as.data.frame() ; io1b$A_Output %>% as.data.frame()

# Low Electric water intensity
A2<-A0; A2[1,4] <- 0.2; A2
io1c <- metis.io(ioTable0=ioTable0, A0=A2, useIntensity = 1,nameAppend = "_Multi_A_lowWatinElec"); io1c$ioTbl_Output %>% as.data.frame() ; io1c$A_Output %>% as.data.frame()



#------------------------------------------------
# Import supply/demand/capacity files
# Import demands across sectors, with corresponding supplies specified
# Import base data
demand_data_file = 'C:/Users/twild/Dropbox/Argentina and metis workplan/Metis_Colorado/colorado_demand_data.csv'
capacity_data_file = 'C:/Users/twild/Dropbox/Argentina and metis workplan/Metis_Colorado/colorado_capacity_data.csv'
demand_data <- read.csv(demand_data_file)
demand_data <- demand_data %>% as_tibble()
capacity_data <- read.csv(capacity_data_file)
# Manipulate/rearrange demand/supply data frame to wide format
ioTable0 <- demand_data %>% select(-units, -localData, -dataSource, -year, -param) %>%
  mutate(demandClassCombined=ifelse(class2=="", paste0(demandClass, class2), paste0(demandClass,"_", class2))) %>%
  select(-class2, -demandClass) %>% spread(demandClassCombined, localDataSubdivided)
ioTable0[,c(which(colnames(ioTable0)=="supplySector"),which(colnames(ioTable0)!="supplySector"))]  # Shift supply to first
# Manipulate/rearrange supply capacity data frame to wide format, and integrate it with existing ioTable0 dataframe
capTable <- capacity_data %>% select(-year, -units) %>% rename(cap=data)
# Merge ioTable0 and capTable
ioTable0 <- ioTable0 %>% left_join(capTable, by=c('subRegion', 'supplySector'))
# Filter for one sub-region for testing purposes
ioTable0_SR <- ioTable0 %>% filter(subRegion=='Mendoza_alta_barrancas')
io1 <- metis.io(ioTable0=ioTable0, nameAppend = "_MultiScenario")  # ioTable0=ioTable0
io1 <- metis.io(ioTable0=ioTable0_SR, nameAppend = "_MultiScenario")  # ioTable0=ioTable0
io1$ioTbl_Output %>% as.data.frame()
io1$A_Output %>% as.data.frame()
