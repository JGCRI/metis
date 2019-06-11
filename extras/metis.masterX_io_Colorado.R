
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
if("tidyverse" %in% rownames(installed.packages()) == F){install.packages("tidyverse")}
library(tidyverse)



# This script uses MetisWatMod to build the simulation network then balance water flows in a historical year

source(paste(getwd(),'/extras/MetisWatMod.R',sep=""))
library(tidyverse)
data <- paste(getwd(),'/datafiles/io/network_data.csv',sep="")

# Simulation network/order
output <- network_main(data)
network_data <- output$network_data
network_order <- output$network_order
from_to <- output$from_to

#------------------------------------------------
# Import user-defined supply/demand/capacity files
# Import demands across sectors, with corresponding supplies specified
# Import base data
demand_data_file = paste(getwd(),'/datafiles/io/colorado_demand_data.csv',sep="")
capacity_data_file = paste(getwd(),'/datafiles/io/colorado_capacity_data.csv',sep="")
demand_data <- read.csv(demand_data_file)
demand_data <- demand_data %>% as_tibble()
capacity_data <- read.csv(capacity_data_file)
# Manipulate/rearrange demand/supply data frame to wide format
ioTable0 <- demand_data %>% select(-localData, -dataSource, -year, -param) %>%
  mutate(demandClassCombined=ifelse(class2=="", paste0(demandClass, class2), paste0(demandClass,"_", class2))) %>%
  select(-class2, -demandClass) %>% spread(demandClassCombined, localDataSubdivided)
ioTable0[,c(which(colnames(ioTable0)=="supplySector"),which(colnames(ioTable0)!="supplySector"))]  # Shift supply to first column
# Manipulate/rearrange supply capacity data frame to wide format, and integrate it with existing ioTable0 dataframe
capTable <- capacity_data %>% select(-year, -units) %>% rename(cap=data)
# Merge ioTable0 and capTable
ioTable0 <- ioTable0 %>% left_join(capTable, by=c('subRegion', 'supplySector', 'supplySubSector'))
#------------------------------------------------

# Balancing water flows
output_water <- subReg_water_balance(ioTable0, network_order, network_data, from_to)
ioTable0 <- output_water$supply_demand_table  # updated demand-supply table that includes proper natural water exports, and capacities
ioTable0$units <- as.character(ioTable0$units)

# Aggregate regions up to the level of a single region
ioTable0
t1 <- ioTable0
t2 <- ioTable0[0,]
downstream_elem_list <- character(0)
singleSubRegName <- 'Colorado'
downstream_flow <- 0

# determine downstream-most element, so can account correctly for single subregion outflows

for (subreg in unique(ioTable0$subRegion)){
  counter = 0
  for(name in names(from_to)){
    counter = counter + 1
    if(subreg %in% from_to[[name]]){
      break
    }
    if(counter==length(names(from_to))){
      downstream_elem_list <- c(downstream_elem_list, subreg)
    }
  }
}

# This only works if there are more than 1 subregion.
for(reg in unique(ioTable0$region)){
  # Loop through regions
  t1 <- t1 %>% filter(region == reg)  # Filter out region of interest
  # Reset W_SW_Upstream to W_SW_Runoff for these intermediate sub-regions (i.e., not headwater or downstream-most elements)
  # Needs to be done before we sum up all the categories below.
  for (subreg in unique(ioTable0$subRegion)){
    if(subreg %in% names(from_to)){
      if(subreg %in% downstream_elem_list){
        dsf <- (t1 %>% filter(supplySubSector=='W_SW_Upstream', subRegion==subreg))$downstream[1]
        downstream_flow <- downstream_flow + dsf
      }
      t1 <- t1 %>% mutate(supplySubSector = if_else(supplySubSector=='W_SW_Upstream' & subRegion==subreg, 'W_SW_Runoff', supplySubSector))
    }
  }
  View(t1)
  print("1")
  for (sup_sub_sec in unique(t1$supplySubSector)){
    # First loop through all subsectors, and sum up demands across all subregions
    non_numer <- t1 %>% filter(supplySubSector==sup_sub_sec) %>%
      select(region, subRegion, units, supplySector, supplySubSector)  #select LHS non-numeric columns
    lhs <- non_numer[1,] # select only the first row, so you can keep values of non-numeric columns
    numer <- t1 %>% filter(supplySubSector==sup_sub_sec) %>%
      select(-region, -subRegion, -units, -supplySector, -supplySubSector)
    rhs <- data.frame(colname=names(numer), colSums_d=colSums(numer, na.rm=TRUE)) %>% spread(colname, colSums_d)  # sum up and spread numeric columns
    new_row_subsector_single_reg <- cbind(lhs,rhs)  # combine lhs and rhs
    new_row_subsector_single_reg['subRegion'] <- singleSubRegName  # rename to new single subregion
    new_row_subsector_single_reg['downstream'] <- 0  # set to zero; we deal with this later
    if (dim(numer)[1]>0){
      t2 <- rbind(t2, new_row_subsector_single_reg)  # add new subsector single region value to final DF (t2)
    }
  }
  print("2")
  # Next deal with downstream flows and upstream flows

  # First, get rid of W_SW_Upstream; reflects incorrect accounting. Already reassigned some to runoff.
  t2 <- t2 %>% filter(!supplySubSector=='W_SW_Upstream')

  # Handle upstream flows
  # Only for headwater subregion, add up any flows coming from upstream
  non_numer <- t1 %>% filter(!subRegion %in% names(from_to), supplySector=='Water', supplySubSector=='W_SW_Upstream') %>%
    select(region, subRegion, units, supplySector, supplySubSector)  #select LHS non-numeric columns
  lhs <- non_numer[1,] # select only the first row, so you can keep values of non-numeric columns
  numer <- t1 %>% filter(!subRegion %in% names(from_to), supplySector=='Water', supplySubSector=='W_SW_Upstream') %>%
    select(-region, -subRegion, -units, -supplySector, -supplySubSector)
  rhs <- data.frame(colname=names(numer), colSums_d=colSums(numer, na.rm=TRUE)) %>% spread(colname, colSums_d)  # sum up and spread numeric columns
  new_row_subsector_single_reg <- cbind(lhs,rhs)  # combine lhs and rhs
  new_row_subsector_single_reg['subRegion'] <- singleSubRegName  # rename to new single subregion
  if (dim(numer)[1]>0){
    t2 <- rbind(t2, new_row_subsector_single_reg)  # put new row in for W_SW_Upstream
  }
  print("3")


  # Element is a downstream-most element
  # Handle downstream flows
  # Add row dealing with downstream flows

  t2 <- t2 %>%
    mutate(downstream=if_else(supplySubSector=='W_SW_Runoff', downstream_flow, downstream))
}




# Run Metis IO model
io1 <- metis.io(ioTable0=ioTable0, nameAppend = "_MultiScenario")  # ioTable0=ioTable0
io1$ioTbl_Output %>% as.data.frame()
io1$A_Output %>% as.data.frame()






non_numer <- t1 %>% filter(subRegion %in% downstream_elem_list, supplySector=='Water', supplySubSector=='W_SW_Upstream') %>%
  select(region, subRegion, units, supplySector, supplySubSector)  #select LHS non-numeric columns
lhs <- non_numer[1,] # select only the first row, so you can keep values of non-numeric columns
numer <- t1 %>% filter(subRegion %in% downstream_elem_list, supplySector=='Water', supplySubSector=='W_SW_Upstream') %>%
  select(-region, -subRegion, -units, -supplySector, -supplySubSector)
rhs <- data.frame(colname=names(numer), colSums_d=colSums(numer, na.rm=TRUE)) %>% spread(colname, colSums_d)  # sum up and spread numeric columns
new_row_subsector_single_reg <- cbind(lhs,rhs)  # combine lhs and rhs
new_row_subsector_single_reg['subRegion'] <- singleSubRegName  # rename to new single subregion
if (dim(numer)[1]>0){
  t2 <- t2 %>%
    mutate(downstream=if_else(supplySubSector=='W_SW_Runoff', new_row_subsector_single_reg$downstream[1], downstream))
}
