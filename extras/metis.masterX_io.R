
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
if("grid" %in% rownames(installed.packages()) == F){install.packages("grid")}
library(grid)


#-------------
# Workflow for Metis I/O Analysis

# Small Examples


# Intensity
A0=tibble::tribble( # Initial total demand
  ~supplySubSector, ~supplySector, ~water_sw, ~water_import, ~elec_all, ~elec_import, ~irri_all, ~irri_import,
  "water_sw",        "water",     0,         0 ,0.404     ,0     ,0         ,0,
  "water_import",     "water",     0,         0 ,0         ,0     ,0         ,0,
  "elec_all",         "elec",      0.27778,   0 ,0         ,0     ,0         ,0,
  "elec_import",      "elec",      0,         0 ,0         ,0     ,0         ,0,
  "irri_all",         "irri",      0,         0 ,0.05    ,0     ,0         ,0,
  "irri_import",      "irri",      0,         0 ,0         ,0     ,0         ,0
);A0



# Demands and Exports
ioTable0=tibble::tribble( # Initial total demand
  ~supplySubSector,   ~supplySector, ~water_all,    ~elec_all,  ~industry, ~transport, ~misc, ~export, ~resid, ~cap, ~units, ~x,
  "water_sw",        "water",       0,    200, 20,        40,         50,     0,      500,    1000, "km3", 2010,
  "water_import",     "water",       0,    20,  30,        20,         70,     300,    10,      0,  "km3", 2010,
  "elec_all",         "elec",        225,  0,   30,        40,         70,     1200,    30,     410, "TWh", 2010,
  "elec_import",      "elec",        50,   0,   20,        40,         50,     670,    20,      350, "TWh", 2010,
  "irri_corn",         "irri",        0,    30,  20,        0,          50,     0,      0,      100, "tons", 2010,
  "irri_import",      "irri",        0,    20,  20,        0,          50,     30,     0,       20, "tons", 2010,
  "water_sw",        "water",        0,    150, 10,        30,         70,     0,      200,    2000, "km3", 2020,
  "water_import",     "water",       30,    10,  20,        50,         20,     200,    30,      0,  "km3", 2020,
  "elec_all",         "elec",        125,  0,   20,        70,         40,     1800,    50,     810, "TWh", 2020,
  "elec_import",      "elec",        30,   0,   10,        60,         30,     870,    10,      750, "TWh", 2020,
  "irri_corn",         "irri",        0,    30,  50,        0,          20,     0,      0,      50, "tons", 2020,
  "irri_import",      "irri",        0,    20,  70,        0,          10,     10,     0,       10, "tons", 2020
);ioTable0


# Original Test
io1 <- metis.io(ioTable0=ioTable0, nameAppend = "_test"); io1$ioTbl_Output %>% as.data.frame() ; io1$A_Output %>% as.data.frame()
io1 <- metis.io(ioTable0=ioTable0 %>%dplyr::filter(x!=2020), nameAppend = "_2010"); io1$ioTbl_Output %>% as.data.frame() ; io1$A_Output %>% as.data.frame()

#---------------------
# Multi Scenario Examples
#----------------------

A0=tibble::tribble( # Initial Flows
  ~supplySubSector ,    ~W,         ~E,    ~scenario,  ~subRegion,  ~year,
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




# Demands and Exports
ioTable0=tibble::tribble( # Initial total demand
  ~supplySubSector,   ~supplySector, ~water_all,    ~elec_all,  ~industry, ~transport, ~misc, ~export, ~resid, ~cap, ~units, ~scenario, ~subRegion, ~x, ~region,
  "water_sw",        "water",        0,    200, 20,        40,         50,     0,      500,    1000, "km3", "ScenA", "SubRegionA", 2010, "testRegion",
  "water_import",     "water",       0,    20,  30,        20,         70,     300,    10,      0,  "km3", "ScenA", "SubRegionA", 2010, "testRegion",
  "elec_all",         "elec",        225,  0,   30,        40,         70,     1200,    30,     410, "TWh", "ScenA", "SubRegionA", 2010, "testRegion",
  "elec_import",      "elec",        50,   0,   20,        40,         50,     670,    20,      350, "TWh", "ScenA", "SubRegionA", 2010, "testRegion",
  "irri_corn",         "irri",       0,    30,  20,        0,          50,     0,      0,      100, "tons", "ScenA", "SubRegionA", 2010, "testRegion",
  "irri_import",      "irri",        0,    40,  20,        0,          50,     30,     0,       20, "tons", "ScenA", "SubRegionA", 2010, "testRegion",
  #
  "water_sw",        "water",        0,    100, 20,        40,         50,     0,      500,    1000, "km3", "ScenB", "SubRegionA", 2010, "testRegion",
  "water_import",     "water",       50,   40,  30,        20,         70,     300,    10,      0,  "km3", "ScenB", "SubRegionA", 2010, "testRegion",
  "elec_all",         "elec",        125,  0,   30,        40,         70,     1200,    30,     410, "TWh", "ScenB", "SubRegionA", 2010, "testRegion",
  "elec_import",      "elec",        30,   0,   20,        40,         50,     670,    20,      350, "TWh", "ScenB", "SubRegionA", 2010, "testRegion",
  "irri_corn",         "irri",       0,    70,  20,        0,          50,     0,      0,      100, "tons", "ScenB", "SubRegionA", 2010, "testRegion",
  "irri_import",      "irri",        0,    10,  20,        0,          50,     30,     0,       20, "tons", "ScenB", "SubRegionA", 2010, "testRegion",
  #
  "water_sw",        "water",        0,    400, 20,        40,         50,     0,      500,    1000, "km3", "ScenA", "SubRegionB", 2010, "testRegion",
  "water_import",     "water",       0,    10,  30,        20,         70,     300,    10,      0,  "km3",  "ScenA", "SubRegionB", 2010, "testRegion",
  "elec_all",         "elec",        425,  0,   30,        40,         70,     1200,    30,     410, "TWh", "ScenA", "SubRegionB", 2010, "testRegion",
  "elec_import",      "elec",        60,   0,   20,        40,         50,     670,    20,      350, "TWh", "ScenA", "SubRegionB", 2010, "testRegion",
  "irri_corn",         "irri",       0,    30,  20,        0,          50,     0,      0,      100, "tons", "ScenA", "SubRegionB", 2010, "testRegion",
  "irri_import",      "irri",        0,    10,  20,        0,          50,     30,     0,       20, "tons", "ScenA", "SubRegionB", 2010, "testRegion",
  #
  "water_sw",        "water",        0,    300, 20,        40,         50,     0,      500,    1000, "km3", "ScenB", "SubRegionB", 2010, "testRegion",
  "water_import",     "water",       0,    60,  30,        20,         70,     300,    10,      0,  "km3",  "ScenB", "SubRegionB", 2010, "testRegion",
  "elec_all",         "elec",        25,   0,   30,        40,         70,     1200,    30,     410, "TWh", "ScenB", "SubRegionB", 2010, "testRegion",
  "elec_import",      "elec",        10,  0,   20,        40,         50,     670,    20,      350, "TWh", "ScenB", "SubRegionB", 2010, "testRegion",
  "irri_corn",         "irri",       0,    70,  20,        0,          50,     0,      0,      100, "tons", "ScenB", "SubRegionB", 2010, "testRegion",
  # 2020
  "irri_import",      "irri",        0,    35,  20,        0,          50,     30,     0,       20, "tons", "ScenB", "SubRegionB", 2020, "testRegion",
  "water_sw",        "water",        0,    200, 20,        40,         50,     0,      500,    1000, "km3", "ScenA", "SubRegionA", 2020, "testRegion",
  "water_import",     "water",       0,    20,  30,        20,         70,     300,    10,      0,  "km3", "ScenA", "SubRegionA", 2020, "testRegion",
  "elec_all",         "elec",        225,  0,   30,        40,         70,     1200,    30,     410, "TWh", "ScenA", "SubRegionA", 2020, "testRegion",
  "elec_import",      "elec",        50,   0,   20,        40,         50,     670,    20,      350, "TWh", "ScenA", "SubRegionA", 2020, "testRegion",
  "irri_corn",         "irri",       0,    30,  20,        0,          50,     0,      0,      100, "tons", "ScenA", "SubRegionA", 2020, "testRegion",
  "irri_import",      "irri",        0,    40,  20,        0,          50,     30,     0,       20, "tons", "ScenA", "SubRegionA", 2020, "testRegion",
  #
  "water_sw",        "water",        0,    100, 20,        40,         50,     0,      500,    1000, "km3", "ScenB", "SubRegionA", 2020, "testRegion",
  "water_import",     "water",       50,   40,  30,        20,         70,     300,    10,      0,  "km3", "ScenB", "SubRegionA", 2020, "testRegion",
  "elec_all",         "elec",        125,  0,   30,        40,         70,     1200,    30,     410, "TWh", "ScenB", "SubRegionA", 2020, "testRegion",
  "elec_import",      "elec",        30,   0,   20,        40,         50,     670,    20,      350, "TWh", "ScenB", "SubRegionA", 2020, "testRegion",
  "irri_corn",         "irri",       0,    70,  20,        0,          50,     0,      0,      100, "tons", "ScenB", "SubRegionA", 2020, "testRegion",
  "irri_import",      "irri",        0,    10,  20,        0,          50,     30,     0,       20, "tons", "ScenB", "SubRegionA", 2020, "testRegion",
  #
  "water_sw",        "water",        0,    400, 20,        40,         50,     0,      500,    1000, "km3", "ScenA", "SubRegionB", 2020, "testRegion",
  "water_import",     "water",       0,    10,  30,        20,         70,     300,    10,      0,  "km3",  "ScenA", "SubRegionB", 2020, "testRegion",
  "elec_all",         "elec",        425,  0,   30,        40,         70,     1200,    30,     410, "TWh", "ScenA", "SubRegionB", 2020, "testRegion",
  "elec_import",      "elec",        60,   0,   20,        40,         50,     670,    20,      350, "TWh", "ScenA", "SubRegionB", 2020, "testRegion",
  "irri_corn",         "irri",       0,    30,  20,        0,          50,     0,      0,      100, "tons", "ScenA", "SubRegionB", 2020, "testRegion",
  "irri_import",      "irri",        0,    10,  20,        0,          50,     30,     0,       20, "tons", "ScenA", "SubRegionB", 2020, "testRegion",
  #
  "water_sw",        "water",        0,    300, 20,        40,         50,     0,      500,    1000, "km3", "ScenB", "SubRegionB", 2020, "testRegion",
  "water_import",     "water",       0,    60,  30,        20,         70,     300,    10,      0,  "km3",  "ScenB", "SubRegionB", 2020, "testRegion",
  "elec_all",         "elec",        25,   0,   30,        40,         70,     1200,    30,     410, "TWh", "ScenB", "SubRegionB", 2020, "testRegion",
  "elec_import",      "elec",        10,  0,   20,        40,         50,     670,    20,      350, "TWh", "ScenB", "SubRegionB", 2020, "testRegion",
  "irri_corn",         "irri",       0,    70,  20,        0,          50,     0,      0,      100, "tons", "ScenB", "SubRegionB", 2020, "testRegion",
  "irri_import",      "irri",        0,    35,  20,        0,          50,     30,     0,       20, "tons", "ScenB", "SubRegionB", 2020, "testRegion"
);ioTable0



# Original Test
io1 <- metis.io(ioTable0=ioTable0, nameAppend = "_Multi_test"); io1$ioTbl_Output %>% as.data.frame() ; io1$A_Output %>% as.data.frame()
