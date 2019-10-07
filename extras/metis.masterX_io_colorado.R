
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
scenarios <- c('Reference', 'Policy')
for(scen in scenarios){
  scenario_name <- scen  # 'Reference'
  demand_data_file = paste(getwd(),'/datafiles/io/colorado_', scenario_name, '_', 'NEW.csv',sep="")  # _AgPolicy
  capacity_data_file = paste(getwd(),'/datafiles/io/colorado_capacity_data.csv',sep="")
  demand_data <- read.csv(demand_data_file)
  demand_data <- demand_data %>% as_tibble()
  capacity_data <- read.csv(capacity_data_file)
  # Manipulate/rearrange demand/supply data frame to wide format
  ioTable0 <- demand_data %>%
    select(-dataSource, -year, -param) %>%
    spread(demandClass, localData)
  ioTable0[,c(which(colnames(ioTable0)=="supplySector"),which(colnames(ioTable0)!="supplySector"))]  # Shift supply to first column
  # Manipulate/rearrange supply capacity data frame to wide format, and integrate it with existing ioTable0 dataframe
  capTable <- capacity_data %>% select(-year, -units) %>% rename(cap=data)
  # Merge ioTable0 and capTable
  ioTable0 <- ioTable0 %>% left_join(capTable, by=c('subRegion', 'supplySector', 'supplySubSector'))
  #-----------------------------------------------------------------------------------------------------------------------
  # Balancing water flows
  output_water <- subReg_water_balance(ioTable0, network_order, network_data, from_to)
  ioTable0 <- output_water$supply_demand_table  # updated demand-supply table that includes proper natural water exports, and capacities
  ioTable0$units <- as.character(ioTable0$units)
  #-----------------------------------------------------------------------------------------------------------------------
  # Create a single dataframe to dump relevant results into that you want to map
  subregions <- as.character(unique(ioTable0$subRegion))
  num_subreg <- length(subregions)
  colnames <- c('scenario', 'region', 'subRegion', 'sources', 'param', 'units', 'class', 'x', 'value', 'subRegType',
                'classPalette1', 'classLabel1')
  mapping_df <- data.frame(matrix(ncol = 12, nrow = num_subreg))
  colnames(mapping_df) <- colnames
  params <- 'griddedScarcity'
  mapping_df['region'] <- 'Argentina'
  mapping_df['subRegion'] <- subregions
  mapping_df['scenario'] <- scenario_name
  mapping_df['sources'] <- 'localData'
  mapping_df['param'] <- params
  mapping_df['units'] <- 'unitless'
  mapping_df['class'] <- 'scarcity'
  mapping_df['subRegType'] <- 'localBasin'
  mapping_df['x'] <- 2010
  mapping_df['value'] <- 0
  mapping_df['classPalette'] <- 'pal_ScarcityCat'
  mapping_df['classLabel'] <- 'Scarcity'

  # Calculate scarcity
  # Process supplies/demands to create scarcity value for each of the 10 sub-regions
  for (subReg in subregions){
    df <- ioTable0 %>%
      filter(subRegion==subReg, supplySector %in% c('Water')) %>%
      filter(scenario==scenario_name)
    df2 <- df %>%
      select(-scenario, -subRegion, -supplySector, -supplySubSector, -cap, -downstream, -region, -units) %>%
      mutate(rowsum=rowSums(., na.rm=TRUE))
    demand <- sum((df2)$rowsum, na.rm=TRUE)
    df3 <- ioTable0 %>%
      filter(subRegion==subReg, supplySubSector %in% c('W_SW_Runoff', 'W_SW_Upstream'))
    df4 <- df3 %>%
      select(cap) %>%
      mutate(rowsum=rowSums(., na.rm=TRUE))
    surfaceSupply <- sum((df4)$rowsum, na.rm=TRUE)
    sr <- c(subReg)
    mapping_df <- mapping_df %>% mutate(value = if_else(subRegion==sr, demand/surfaceSupply, value))
  }
  save_dir <- 'C:/Users/twild/all_git_repositories/metis/metis/outputs/Maps/Tables'
  export_df <- mapping_df %>%
    filter(param %in% params) %>%
    select(-classPalette1)  # Deal with Palette separately in map
  write.csv(export_df, file=paste0(save_dir, '/', params, '_', scenario_name, '.csv'), row.names=FALSE)
  #-----------------------------------------------------------------------------------------------------------------------

  # Store/export data related to runoff
  colnames <- c('scenario', 'region', 'subRegion', 'sources', 'param', 'units', 'class', 'x', 'value', 'subRegType',
                'classPalette1', 'classLabel1')
  new_df_append <- data.frame(matrix(ncol = 12, nrow = num_subreg))
  colnames(new_df_append) <- colnames
  new_df_append['region'] <- 'Argentina'
  new_df_append['subRegion'] <- subregions
  new_df_append['scenario'] <- scenario_name
  new_df_append['sources'] <- 'localData'
  params <- c('runoff')
  new_df_append['param'] <- params
  new_df_append['units'] <- 'km3'
  new_df_append['class'] <- 'runoff'
  new_df_append['subRegType'] <- 'localBasin'
  new_df_append['x'] <- 2010
  new_df_append['value'] <- 0
  new_df_append['classPalette'] <- 'pal_ColoradoWater'
  new_df_append['classLabel'] <- 'Runoff'
  for (subReg in subregions){
    df <- ioTable0 %>%
      filter(subRegion==subReg, supplySubSector %in% c('W_SW_Runoff')) %>%
      filter(scenario==scenario_name) %>%
      select(-scenario, -subRegion, -supplySector, -supplySubSector, -cap, -region, -units) %>%
      mutate(rowsum=rowSums(., na.rm=TRUE))
    runoff_value <- sum((df)$rowsum, na.rm=TRUE)
    new_df_append <- new_df_append %>%
      mutate(value = if_else(subRegion==subReg, runoff_value, value))
  }
  mapping_df <- rbind(mapping_df %>% filter(!param==params), new_df_append)
  export_df <- mapping_df %>%
    filter(param %in% params)
  write.csv(export_df, file=paste0(save_dir, '/', params, '_', scenario_name, '.csv'), row.names=FALSE)

  # Store/export data related to irrigation water demand
  params <- c('irrigation_water_demand')
  new_df_append['param'] <- params
  new_df_append['units'] <- 'km3'
  new_df_append['class'] <- 'Irrigation'
  new_df_append['subRegType'] <- 'localBasin'
  new_df_append['x'] <- 2010
  new_df_append['value'] <- 0
  new_df_append['classPalette'] <- 'pal_ColoradoWater'
  new_df_append['classLabel'] <- 'IrrigationDemand'
  for (subReg in subregions){
    df <- ioTable0 %>%
      filter(subRegion==subReg) %>%
      filter(scenario==scenario_name) %>%
      filter(grepl('W_', supplySubSector)) %>%
      select(-scenario, -subRegion, -supplySector, -supplySubSector, -cap, -region, -units) %>%
      select(contains("Ag_")) %>%
      mutate(rowsum=rowSums(., na.rm=TRUE))
    irrig_wat_dem_val <- sum((df)$rowsum, na.rm=TRUE)
    new_df_append <- new_df_append %>% mutate(value = if_else(subRegion==subReg, irrig_wat_dem_val, value))
  }
  mapping_df <- rbind(mapping_df %>% filter(!param==params), new_df_append)
  export_df <- mapping_df %>% filter(param %in% params)
  write.csv(export_df, file=paste0(save_dir, '/', params, '_', scenario_name, '.csv'), row.names=FALSE)

  # Store/export data related to municipal water demand
  params <- c('municipal_water_demand')
  new_df_append['param'] <- params
  new_df_append['units'] <- 'km3'
  new_df_append['class'] <- 'Municipal'
  new_df_append['subRegType'] <- 'localBasin'
  new_df_append['x'] <- 2010
  new_df_append['value'] <- 0
  new_df_append['classPalette'] <- 'pal_ColoradoWater'
  new_df_append['classLabel'] <- 'MunicipalWatDemand'
  for (subReg in subregions){
    df <- ioTable0 %>%
      filter(subRegion==subReg) %>%
      filter(scenario==scenario_name) %>%
      filter(grepl('W_', supplySubSector)) %>%
      select(-scenario, -subRegion, -supplySector, -supplySubSector, -cap, -region, -units) %>%
      select(contains("municipal")) %>%
      mutate(rowsum=rowSums(., na.rm=TRUE))
    municipal_water_demand <- sum((df)$rowsum, na.rm=TRUE)
    new_df_append <- new_df_append %>%
      mutate(value = if_else(subRegion==subReg, municipal_water_demand, value))
  }
  mapping_df <- rbind(mapping_df %>% filter(!param==params), new_df_append)
  export_df <- mapping_df %>% filter(param %in% params)
  write.csv(export_df, file=paste0(save_dir, '/', params, '_', scenario_name, '.csv'), row.names=FALSE)

  # Store/export data related to electricity water demand
  params <- c('electricity_water_demand')
  new_df_append['param'] <- params
  new_df_append['units'] <- 'km3'
  new_df_append['class'] <- 'Electricity'
  new_df_append['subRegType'] <- 'localBasin'
  new_df_append['x'] <- 2010
  new_df_append['value'] <- 0
  new_df_append['classPalette'] <- 'pal_ColoradoWater'
  new_df_append['classLabel'] <- 'ElectricityWatDemand'
  for (subReg in subregions){
    df <- ioTable0 %>%
      filter(subRegion==subReg) %>%
      filter(scenario==scenario_name) %>%
      filter(grepl('W_', supplySubSector)) %>%
      select(-scenario, -subRegion, -supplySector, -supplySubSector, -cap, -region, -units) %>%
      select(contains("Electricity_")) %>%
      mutate(rowsum=rowSums(., na.rm=TRUE))
    electricity_water_demand <- sum((df)$rowsum, na.rm=TRUE)
    new_df_append <- new_df_append %>%
      mutate(value = if_else(subRegion==subReg, electricity_water_demand, value))
  }
  mapping_df <- rbind(mapping_df %>% filter(!param==params), new_df_append)
  export_df <- mapping_df %>% filter(param %in% params)
  write.csv(export_df, file=paste0(save_dir, '/', params, '_', scenario_name, '.csv'), row.names=FALSE)

  # Store/export data related to livestock water demand
  params <- c('livestock_water_demand')
  new_df_append['param'] <- params
  new_df_append['units'] <- 'km3'
  new_df_append['class'] <- 'Livestock'
  new_df_append['subRegType'] <- 'localBasin'
  new_df_append['x'] <- 2010
  new_df_append['value'] <- 0
  new_df_append['classPalette'] <- 'pal_ColoradoWater'
  new_df_append['classLabel'] <- 'LivestockWatDemand'
  for (subReg in subregions){
    df <- ioTable0 %>%
      filter(subRegion==subReg) %>%
      filter(scenario==scenario_name) %>%
      filter(grepl('W_', supplySubSector)) %>%
      select(-scenario, -subRegion, -supplySector, -supplySubSector, -cap, -region, -units) %>%
      select(contains("Livestock_")) %>%
      mutate(rowsum=rowSums(., na.rm=TRUE))
    livestock_water_demand <- sum((df)$rowsum, na.rm=TRUE)
    new_df_append <- new_df_append %>% mutate(value = if_else(subRegion==subReg, livestock_water_demand, value))
  }
  mapping_df <- rbind(mapping_df %>% filter(!param==params), new_df_append)
  export_df <- mapping_df %>% filter(param %in% params)
  write.csv(export_df, file=paste0(save_dir, '/', params, '_', scenario_name, '.csv'), row.names=FALSE)

  # Store/export data related to available water (routed/consumption taken into account)
  params <- c('available_water')
  new_df_append['param'] <- params
  new_df_append['units'] <- 'km3'
  new_df_append['class'] <- 'AvailableWater'
  new_df_append['subRegType'] <- 'localBasin'
  new_df_append['x'] <- 2010
  new_df_append['value'] <- 0
  new_df_append['classPalette'] <- 'pal_ColoradoWater'
  new_df_append['classLabel'] <- 'AvailableWater'
  for (subReg in subregions){
    df <- ioTable0 %>%
      filter(subRegion==subReg) %>%
      filter(scenario==scenario_name) %>%
      filter(supplySubSector %in% c('W_SW_Upstream', 'W_SW_Runoff')) %>%
      select(cap) %>% mutate(rowsum=rowSums(., na.rm=TRUE))
    available_water <- sum((df)$rowsum, na.rm=TRUE)
    new_df_append <- new_df_append %>%
      mutate(value = if_else(subRegion==subReg, available_water, value))
  }
  mapping_df <- rbind(mapping_df %>% filter(!param==params), new_df_append)
  export_df <- mapping_df %>%
    filter(param %in% params)
  write.csv(export_df, file=paste0(save_dir, '/', params, '_', scenario_name, '.csv'), row.names=FALSE)

  # Store/export data related to total water demand
  params <- c('total_water_demand')
  new_df_append['param'] <- params
  new_df_append['units'] <- 'km3'
  new_df_append['class'] <- 'WaterDemand'
  new_df_append['subRegType'] <- 'localBasin'
  new_df_append['x'] <- 2010
  new_df_append['value'] <- 0
  new_df_append['classPalette'] <- 'pal_ColoradoWater'
  new_df_append['classLabel'] <- 'WaterDemand'
  for (subReg in subregions){
    df <- ioTable0 %>%
      filter(subRegion==subReg) %>%
      filter(scenario==scenario_name) %>%
      filter(grepl("W_", supplySubSector)) %>%
      select(-scenario, -subRegion, -supplySector, -supplySubSector, -cap, -downstream, -region, -units) %>%
      mutate(rowsum=rowSums(., na.rm=TRUE))
    total_demand <- sum((df)$rowsum, na.rm=TRUE)
    new_df_append <- new_df_append %>%
      mutate(value = if_else(subRegion==subReg, total_demand, value))
  }
  mapping_df <- rbind(mapping_df %>% filter(!param==params), new_df_append)
  export_df <- mapping_df %>% filter(param %in% params)
  write.csv(export_df, file=paste0(save_dir, '/', params, '_', scenario_name, '.csv'), row.names=FALSE)

  # Store/export data related to total electricity supply
  params <- c('total_elec_supply')
  new_df_append['param'] <- params
  new_df_append['units'] <- 'GWh'
  new_df_append['class'] <- 'ElecSupply'
  new_df_append['subRegType'] <- 'localBasin'
  new_df_append['x'] <- 2010
  new_df_append['value'] <- 0
  new_df_append['classPalette'] <- 'pal_ColoradoTotalElecDemand'
  new_df_append['classLabel'] <- 'ElecSupply'
  for (subReg in subregions){
    df <- ioTable0 %>%
      filter(subRegion==subReg) %>%
      filter(scenario==scenario_name) %>%
      filter(grepl("Electricity_", supplySubSector)) %>%
      filter(!grepl("Electricity_Import", supplySubSector)) %>%
      select(-scenario, -subRegion, -supplySector, -supplySubSector, -cap, -downstream, -region, -units) %>%
      mutate(rowsum=rowSums(., na.rm=TRUE))
    total_elec_supply <- sum((df)$rowsum, na.rm=TRUE)
    new_df_append <- new_df_append %>%
      mutate(value = if_else(subRegion==subReg, total_elec_supply, value))
  }
  mapping_df <- rbind(mapping_df %>% filter(!param==params), new_df_append)
  export_df <- mapping_df %>% filter(param %in% params)
  write.csv(export_df, file=paste0(save_dir, '/', params, '_', scenario_name, '.csv'), row.names=FALSE)

  # Store/export data related to electricity demand by subsector
  # Get a list of electricity demand sectors included in data set
  demData <- demand_data
  demData$demandClass <- as.character(demData$demandClass)
  demandSubSec_List <- unique((demData %>%
                                 filter(scenario==scenario_name) %>%
                                 filter(grepl("Electricity_", supplySubSector)))$demandClass)
  for (demSec in demandSubSec_List){
    params <- c(paste0('ElecDemand_', demSec))
    new_df_append['param'] <- params
    new_df_append['units'] <- 'GWh'
    new_df_append['class'] <- params
    new_df_append['subRegType'] <- 'localBasin'
    new_df_append['x'] <- 2010
    new_df_append['value'] <- 0
    new_df_append['classPalette'] <- 'pal_ColoradoElecDemand'
    new_df_append['classLabel'] <- params
    for (subReg in subregions){
      df <- demData %>%
        filter(subRegion==subReg) %>%
        filter(scenario==scenario_name) %>%
        filter(grepl(demSec, demandClass)) %>%
        filter(grepl("Electricity_", supplySubSector)) %>%
        select(-scenario, -subRegion, -supplySector, -supplySubSector, -dataSource, -region, -units, -param, -year)
      if (dim(df)[1] > 0){
        total_elec_demand_tbl <- df %>%
          group_by(demandClass) %>%
          summarize(total=sum(localData, na.rm=TRUE)) %>%
          ungroup()
        total_elec_demand <- total_elec_demand_tbl$total
        new_df_append <- new_df_append %>%
          mutate(value = if_else(subRegion==subReg, total_elec_demand, value))
      }
    }
    mapping_df <- rbind(mapping_df %>% filter(!param==params), new_df_append)
    export_df <- mapping_df %>% filter(param %in% params)
    write.csv(export_df, file=paste0(save_dir, '/', params, '_', scenario_name, '.csv'), row.names=FALSE)
  }

  # Store/export data related to total electricity demand
  params <- c('ElecDemand_Total')
  new_df_append['param'] <- params
  new_df_append['units'] <- 'GWh'
  new_df_append['class'] <- params
  new_df_append['subRegType'] <- 'localBasin'
  new_df_append['x'] <- 2010
  new_df_append['value'] <- 0
  new_df_append['classPalette'] <- 'pal_ColoradoTotalElecDemand'
  new_df_append['classLabel'] <- params
  for (subReg in subregions){
    df <- demData %>%
      filter(subRegion==subReg) %>%
      filter(scenario==scenario_name) %>%
      filter(grepl("Electricity_", supplySubSector)) %>%
      select(-scenario, -subRegion, -supplySector, -supplySubSector, -dataSource, -region, -units, -param, -year)
    if (dim(df)[1] > 0){
      total_elec_demand_tbl <- df %>%
        group_by(demandClass) %>%
        summarize(total=sum(localData, na.rm=TRUE)) %>%
        ungroup()
      total_elec_demand <- sum(total_elec_demand_tbl$total)
      new_df_append <- new_df_append %>%
        mutate(value = if_else(subRegion==subReg, total_elec_demand, value))
    }
  }
  mapping_df <- rbind(mapping_df %>% filter(!param==params), new_df_append)
  export_df <- mapping_df %>% filter(param %in% params)
  write.csv(export_df, file=paste0(save_dir, '/', params, '_', scenario_name, '.csv'), row.names=FALSE)

  # Store/export data related to Land allocation
  params <- c('total_ag_supply')
  new_df_append['param'] <- params
  new_df_append['units'] <- 'land (km^2)'
  new_df_append['class'] <- 'AgSupply'
  new_df_append['subRegType'] <- 'localBasin'
  new_df_append['x'] <- 2010
  new_df_append['value'] <- 0
  new_df_append['classPalette'] <- 'Greens'
  new_df_append['classLabel'] <- 'AgSupply'
  for (subReg in subregions){
    df <- ioTable0 %>%
      filter(subRegion==subReg) %>%
      filter(scenario==scenario_name) %>%
      filter(grepl("Ag_", supplySubSector)) %>%
      select(-scenario, -subRegion, -supplySector, -supplySubSector, -cap, -downstream, -region, -units) %>%
      mutate(rowsum=rowSums(., na.rm=TRUE))
    total_ag_supply <- sum((df)$rowsum, na.rm=TRUE)
    new_df_append <- new_df_append %>%
      mutate(value = if_else(subRegion==subReg, total_ag_supply, value))
  }
  mapping_df <- rbind(mapping_df %>% filter(!param==params), new_df_append)
  export_df <- mapping_df %>%
    filter(param %in% params)
  write.csv(export_df, file=paste0(save_dir, '/', params, '_', scenario_name, '.csv'), row.names=FALSE)

  # Store/export data related to total livestock
  params <- c('total_livestock_supply')
  new_df_append['param'] <- params
  new_df_append['units'] <- 'livestock (head)'
  new_df_append['class'] <- 'total_livestock_supply'
  new_df_append['subRegType'] <- 'localBasin'
  new_df_append['x'] <- 2010
  new_df_append['value'] <- 0
  new_df_append['classPalette'] <- 'Greens'
  new_df_append['classLabel'] <- 'AgSupply'
  for (subReg in subregions){
    df <- ioTable0 %>%
      filter(subRegion==subReg) %>%
      filter(scenario==scenario_name) %>%
      filter(grepl("Livestock_", supplySubSector)) %>%
      select(-scenario, -subRegion, -supplySector, -supplySubSector, -cap, -downstream, -region, -units) %>%
      mutate(rowsum=rowSums(., na.rm=TRUE))
    total_ag_supply <- sum((df)$rowsum, na.rm=TRUE)
    new_df_append <- new_df_append %>%
      mutate(value = if_else(subRegion==subReg, total_ag_supply, value))
  }
  mapping_df <- rbind(mapping_df %>% filter(!param==params), new_df_append)
  export_df <- mapping_df %>%
    filter(param %in% params)
  write.csv(export_df, file=paste0(save_dir, '/', params, '_', scenario_name, '.csv'), row.names=FALSE)


  # Store/export data related to Land allocation for individual crops

  # Get list of unique crops across all sub-regions
  new <- ioTable0 %>%
    filter(scenario==scenario_name) %>%
    filter(grepl("Ag_", supplySubSector))
  crop_names <- unique(new$supplySubSector)

  # Loop through and save individual crop files.
  for(crop in crop_names){
    params <- c(paste0(crop, '_supply'))
    new_df_append['param'] <- params
    new_df_append['units'] <- 'land (km^2)'
    new_df_append['class'] <- params
    new_df_append['subRegType'] <- 'localBasin'
    new_df_append['x'] <- 2010
    new_df_append['value'] <- 0
    new_df_append['classPalette'] <- 'pal_ColoradoLandAlloc'
    new_df_append['classLabel'] <- params
    for (subReg in subregions){
      df <- ioTable0 %>%
        filter(scenario==scenario_name) %>%
        filter(grepl(crop, supplySubSector)) %>%
        filter(subRegion==subReg) %>%
        select(-scenario, -subRegion, -supplySector, -supplySubSector, -cap, -downstream, -region, -units) %>%
        mutate(rowsum=rowSums(., na.rm=TRUE))
      total_ag_supply <- sum((df)$rowsum, na.rm=TRUE)
      new_df_append <- new_df_append %>%
        mutate(value = if_else(subRegion==subReg, total_ag_supply, value))
    }
    mapping_df <- rbind(mapping_df %>% filter(!param==params), new_df_append)
    export_df <- mapping_df %>%
      filter(param %in% params)
    write.csv(export_df, file=paste0(save_dir, '/', params, '_', scenario_name, '.csv'), row.names=FALSE)
}


  #-----------------------------------------------------------------------------------------------------------------------

  # Run Metis IO model
  # Make sure region and subregion are character types
  ioTable0$region <- as.character(ioTable0$region)
  ioTable0$subRegion <- as.character(ioTable0$subRegion)
  ioTable0$scenario <- as.character(ioTable0$scenario)

  io1 <- metis.io(ioTable0=ioTable0, nameAppend = "_MultiScenario", combSubRegionPlots = 0,
                  folderName="ColoradoSubRegFinal", pdfpng='pdf', sankeyAxis1Label = 'Supply',
                  sankeyAxis2Label = 'Demand')  # ioTable0=ioTable0
  io1$ioTbl_Output %>% as.data.frame()
  io1$A_Output %>% as.data.frame()

  # Re-run Metis with A matrix output as input
  # First, add supply sectors back into A matrix. They were dropped, and metis checks for it and crashes if it's not there.
  #Ao <- io1$A_Output %>% as.data.frame() %>% cbind(io1$ioTbl_Output %>% as.data.frame() %>% select(supplySector))
  #io1 <- metis.io(ioTable0=ioTable0, A0=Ao, nameAppend = "_MultiScenario", useIntensity = 1)

  #----------------------------------------------------------------------------------------------------------------------#

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
    for (sup_sub_sec in unique(t1$supplySubSector)){
      # First loop through all subsectors, and sum up demands across all subregions
      non_numer <- t1 %>% filter(supplySubSector==sup_sub_sec) %>%
        select(scenario, region, subRegion, units, supplySector, supplySubSector)  #select LHS non-numeric columns
      lhs <- non_numer[1,] # select only the first row, so you can keep values of non-numeric columns
      numer <- t1 %>% filter(supplySubSector==sup_sub_sec) %>%
        select(-scenario, -region, -subRegion, -units, -supplySector, -supplySubSector)
      rhs <- data.frame(colname=names(numer), colSums_d=colSums(numer, na.rm=TRUE)) %>% spread(colname, colSums_d)  # sum up and spread numeric columns
      new_row_subsector_single_reg <- cbind(lhs,rhs)  # combine lhs and rhs
      new_row_subsector_single_reg['subRegion'] <- singleSubRegName  # rename to new single subregion
      new_row_subsector_single_reg['downstream'] <- 0  # set to zero; we deal with this later
      if (dim(numer)[1]>0){
        t2 <- rbind(t2, new_row_subsector_single_reg)  # add new subsector single region value to final DF (t2)
      }
    }
    # Next deal with downstream flows and upstream flows

    # First, get rid of W_SW_Upstream; reflects incorrect accounting. Already reassigned some to runoff.
    t2 <- t2 %>% filter(!supplySubSector=='W_SW_Upstream')

    # Also get rid of W_SW_Reservoir, as in a single basin analysis a reservoir cannot "produce" supply. It just redistributes.
    t2 <- t2 %>% filter(!supplySubSector=='W_SW_Reservoir')

    # Handle upstream flows
    # Only for headwater subregion, add up any flows coming from upstream
    non_numer <- t1 %>% filter(!subRegion %in% names(from_to), supplySector=='Water', supplySubSector=='W_SW_Upstream') %>%
      select(scenario, region, subRegion, units, supplySector, supplySubSector)  #select LHS non-numeric columns
    lhs <- non_numer[1,] # select only the first row, so you can keep values of non-numeric columns
    numer <- t1 %>% filter(!subRegion %in% names(from_to), supplySector=='Water', supplySubSector=='W_SW_Upstream') %>%
      select(-scenario, -region, -subRegion, -units, -supplySector, -supplySubSector)
    rhs <- data.frame(colname=names(numer), colSums_d=colSums(numer, na.rm=TRUE)) %>% spread(colname, colSums_d)  # sum up and spread numeric columns
    new_row_subsector_single_reg <- cbind(lhs,rhs)  # combine lhs and rhs
    new_row_subsector_single_reg['subRegion'] <- singleSubRegName  # rename to new single subregion
    if (dim(numer)[1]>0){
      t2 <- rbind(t2, new_row_subsector_single_reg)  # put new row in for W_SW_Upstream
    }

    # Element is a downstream-most element
    # Handle downstream flows
    # Add row dealing with downstream flows
    t2 <- t2 %>%
      mutate(downstream=if_else(supplySubSector=='W_SW_Runoff', downstream_flow, downstream))

    # Deal with electricity now
    imp <- t2 %>% filter(supplySubSector == 'Electricity_Import') %>% select(-cap, -scenario, -region, -subRegion, -units, -supplySector, -supplySubSector) %>%
      mutate(rowsum=rowSums(., na.rm=TRUE))
    imp <- imp$rowsum[1]

    exp <- t2 %>% filter(grepl('Electricity_', supplySubSector))
    exp <- sum(exp$export)

    if (exp>=imp){
      # Net exports from the region must occur, but they are reduced by local consumption of value="imp"
      actual_regional_exports <- exp-imp
      actual_regional_imports <- 0
      # Adjust t2 table to reflect correct exports.Proportionally change each category.
      t2 <- t2 %>% mutate(export=if_else(grepl('Electricity_', supplySubSector), actual_regional_exports*(export/exp), export))
    }else{
      # Net imports into the region must occur
      actual_regional_exports <- 0
      actual_regional_imports <- imp-exp
      #t2 <- t2 %>% filter(grepl('Electricity_Imports', supplySubSector) %>% mutate() actual_regional_exports*(export/exp), export))

      # Follow same approach as below.
      # Need to reset imports to zero in t2.

      protected_list <- c('scenario', 'region', 'subRegion', 'units', 'supplySector', 'supplySubSector', 'cap', 'downstream', 'export')
      for (colname in names(t2)){
        if(!colname %in% protected_list){
          index <- t2$supplySubSector=='Electricity_Import'  # index of electricity imports row
          orig_imp <- t2[[colname]][index]  # Store original electricity imports in this colname demand sector, so we can track the change ("diff")
          fraction <- orig_imp/imp  # fraction (out of 1) of required overall imports reduction that will be taken from this demand sector
          t2[[colname]][index] <- actual_regional_imports*fraction  # scale down imported values to reflect that some local supply exists
          diff <- orig_imp - t2[[colname]][index]  # difference to be allocated from exports back to demand sectors
          # add this diff back to the demand sector, but from a supply subsector row
          #Reassign exports to demand sectors
          index_supply_subsec <- grepl('Electricity_', t2$supplySubSector) & !grepl('_Import', t2$supplySubSector) # index of electricity imports row
          t2[[colname]][index_supply_subsec] <- t2[[colname]][index_supply_subsec] + t2$export[index_supply_subsec]*fraction
        }
      }
      index_elec <- grepl('Electricity_', t2$supplySubSector)  # need to set exports to zero in all electricity supply subsectors
      t2$export[index_elec] <- 0  # Reset export to zero
    }
    # Run Metis IO model
    ioTable0 <- t2
    io1 <- metis.io(ioTable0=ioTable0, nameAppend = "_MultiScenario", folderName="ColoradoFinal", pdfpng='pdf',
                    sankeyAxis1Label = 'Supply', sankeyAxis2Label = 'Demand')  # ioTable0=ioTable0
    io1$ioTbl_Output %>% as.data.frame()
    io1$A_Output %>% as.data.frame()
  }
}
